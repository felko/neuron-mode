;;; zettel-mode.el --- description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 felko
;;
;; Author: felko <http://github/felko>
;; Homepage: https://github.com/felko/zettel-mode
;; Package-Requires: ((emacs 26.3) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;

;;; Commentary:

;; Editing zettelkasten notes using neuron
;; https://neuron.srid.ca/

;;; Code:

(require 'f)
(require 'markdown-mode)
(require 'ivy)
(require 'subr-x)
(require 'url-parse)
(require 'counsel)
(require 'json)

(defgroup zettel-mode nil
  "A major mode for editing Zettelkasten notes with neuron."
  :link '(url-link "https://github.com/felko/zettel-mode")
  :group 'markdown)

(defcustom neuron-default-zettelkasten-directory (expand-file-name "~/zettelkasten")
  "The location of the default Zettelkasten directory."
  :group 'zettel-mode
  :type  'string
  :safe  'f-directory?)

(defcustom neuron-generate-on-save nil
  "Whether to generate the necessary zettels when a buffer is saved."
  :group 'zettel-mode
  :type  'boolean
  :safe  'booleanp)

(defcustom neuron-use-short-links t
  "Whether to use <ID> or [ID](z:/) syntax when inserting zettel links."
  :group 'zettel-mode
  :type  'boolean
  :safe  'booleanp)

(defvar neuron-zettelkasten neuron-default-zettelkasten-directory
  "The location of the current Zettelkasten directory.")

(defun neuron--make-command (cmd &rest args)
  "Construct a neuron command CMD with argument ARGS.
The command contains a `--zettelkasten-dir' argument if `neuron-zettelkasten' is non-nil."
  (mapconcat
   #'shell-quote-argument
   (append (list "neuron" "--zettelkasten-dir" neuron-zettelkasten cmd) args) " "))

(defun neuron--make-query-uri-command (uri)
  "Construct a neuron query command that queries the zettelkasten from URI.
URI is expected to have a zquery:/ scheme."
  (concat (neuron--make-command "query") " --uri " (format "'%s'" uri)))

(defun neuron--run-command (cmd)
  "Run the CMD neuron command with arguments ARGS in the current zettekasten.
The command is executed as a synchronous process and the standard output is
returned as a string."
  (let* ((result    (with-temp-buffer
                      (list (call-process-shell-command cmd nil t) (buffer-string))))
         (exit-code (nth 0 result))
         (output    (nth 1 result)))
    (if (equal exit-code 0)
        (string-trim-right output)
      (and (message "Command \"%s\" exited with code %d: %s" cmd exit-code output)
           nil))))

(defun neuron--json-extract-info (match)
  "Extract Zettel ID and title from MATCH, as a JSON string."
  (let ((zettels (json-read-from-string match)))
    (mapcar (lambda (obj) (list (map-elt obj 'id) (map-elt obj 'title))) zettels)))

(defun neuron--query-url-command (uri)
  "Run a neuron query from a zquery URI."
  (neuron--json-extract-info (neuron--run-command (neuron--make-query-uri-command uri))))

(defun neuron--run-rib-process (&rest args)
  "Run an asynchronous neuron process spawned by the rib command with arguments ARGS."
  (start-process-shell-command "rib" "*rib*" (apply #'neuron--make-command "rib" args)))

(defun neuron--run-rib-compile (&rest args)
  "Run an synchronous neuron command spawned by the rib command with arguments ARGS."
  (compile (apply #'neuron--make-command "rib" args)))

(defun neuron-select-zettelkasten ()
  "Select the active zettelkasten."
  (interactive)
  (setq neuron-zettelkasten (counsel-read-directory-name "Select Zettelkasten: ")))

(defun neuron-new-zettel ()
  "Create a new zettel in the current zettelkasten."
  (interactive)
  (when-let* ((path   (neuron--run-command (neuron--make-command "new" "Untitled")))
              (buffer (find-file-noselect path)))
    (and
     (pop-to-buffer-same-window buffer)
     (zettel-mode)
     (forward-line 1)
     (end-of-line)
     (message (concat "Created " path)))))

(defun neuron-select-zettel ()
  "Find a zettel in the current zettelkasten and return its ID."
  (interactive)
  (let ((match (counsel-rg "title: " neuron-zettelkasten "--no-line-number --no-heading --sort path" "Select Zettel: ")))
    (f-base (car (split-string match ":")))))

(defun neuron--select-zettel-from-query (URI)
  "Select a zettel from the match of URI.
Return the ID of the selected zettel."
  (ivy-read "Select Zettel: "
            (mapcar (lambda (z) (propertize (format "[%s] %s" (nth 0 z) (nth 1 z)) 'id (nth 0 z)))
                    (neuron--query-url-command URI))
                                        ; :predicate  (lambda (path) (not (string-prefix-p "." path)))
            :action (lambda (z) (neuron--edit-zettel-from-id (get-text-property 0 'id z)))
            :caller 'neuron-select-zettel-from-query))

(defun neuron-edit-zettel ()
  "Select and edit a zettel from the currently active zettelkasten."
  (interactive)
  (let* ((id (neuron-select-zettel))
         (path (f-join "/" neuron-zettelkasten (concat id ".md")))
         (buffer (find-file-noselect path)))
    (and
     (pop-to-buffer-same-window buffer)
     (zettel-mode))))

(defun neuron--insert-zettel-link-from-id (id)
  "Insert a zettel link.
Depending on the value of `neuron-use-short-links',
the inserted link will either be of the form <ID> or
[ID](z:/)."
  (insert
   (if neuron-use-short-links
       (format "<%s>" id)
     (format "[%s](z:/)" id))))

(defun neuron-insert-zettel-link ()
  "Insert a markdown hypertext link to another zettel."
  (interactive)
  (neuron--insert-zettel-link-from-id (neuron-select-zettel)))

(defun neuron-insert-new-zettel ()
  "Create a new zettel."
  (interactive)
  (when-let* ((path   (neuron--run-command (neuron--make-command "new" "Untitled")))
              (id     (f-base (f-no-ext path)))
              (buffer (find-file-noselect path)))
    (and
     (neuron--insert-zettel-link-from-id id)
     (pop-to-buffer-same-window buffer)
     (zettel-mode)
     (forward-line 1)
     (end-of-line)
     (message (concat "Created " path)))))

(defun neuron--edit-zettel-from-path (path &optional before after)
  "Open a neuron zettel from PATH.
Execute BEFORE just before popping the buffer and AFTER just after enabling `zettel-mode'."
  (let* ((buffer (find-file-noselect path)))
    (and
     (if before (funcall before) t)
     (pop-to-buffer-same-window buffer)
     (zettel-mode)
     (if after (funcall after) t))))

(defun neuron--edit-zettel-from-id (id &optional before after)
  "Open a neuron zettel from ID.
Execute BEFORE just before popping the buffer and AFTER just after enabling `zettel-mode'."
  (neuron--edit-zettel-from-path
   (f-join "/" neuron-zettelkasten (format "%s.md" id))
   before
   after))

(defun neuron--get-current-zettel-id ()
  "Extract the zettel ID of the current file."
  (f-base (buffer-name)))

(defun neuron--open-zettel-from-id (id)
  "Open the generated HTML file from the zettel ID."
  (let* ((path (f-join "/" neuron-zettelkasten ".neuron" "output" (format "%s.html" id)))
         (url (format "file://%s" path)))
    (browse-url url)))

(defun neuron-open-zettel ()
  "Select a zettel and open the corresponding generated HTML file in the browser."
  (neuron--open-zettel-from-id (neuron-select-zettel)))

(defun neuron-open-current-zettel ()
  "Open the current zettel's HTML file in the browser."
  (interactive)
  (neuron--open-zettel-from-id (neuron--get-current-zettel-id)))

(defun neuron-follow-thing-at-point ()
  "Open the zettel link at point."
  (interactive)
  ;; short links (from the `thing-at-point' demo)
  (if (thing-at-point-looking-at
       (rx (+ alphanumeric))
       ;; limit to current line
       (max (- (point) (line-beginning-position))
            (- (line-end-position) (point))))
      (neuron--edit-zettel-from-id (match-string 0))
    ;; markdown links
    (let* ((link   (markdown-link-at-pos (point)))
           (id     (nth 2 link))
           (url    (nth 3 link))
           (struct (url-generic-parse-url url))
           (type   (url-type struct)))
      (pcase type
        ("z"        (neuron--edit-zettel-from-id id))
        ("zcf"      (neuron--edit-zettel-from-id id))
        ("zquery"   (neuron--select-zettel-from-query url))
        ("zcfquery" (neuron--select-zettel-from-query url))
        (_          (markdown-follow-thing-at-point link))))))

(defun neuron-rib-watch ()
  "Start a web app for browsing the zettelkasten."
  (interactive)
  (if (neuron--run-rib-process "-w")
      (message "Watching %s for changes..." neuron-zettelkasten)
    (message "Rib command failed")))

(defun neuron-rib-serve ()
  "Start a web app for browsing the zettelkasten."
  (interactive)
  (if (neuron--run-rib-process "-wS")
      (message "Started web application on localhost:8080")
    (message "Rib command failed")))

(defun neuron-rib-generate ()
  "Do an one-off generation of the web interface of the zettelkasten."
  (interactive)
  (if (neuron--run-rib-compile)
      (message "Generated HTML files")
    (message "Rib command failed")))

(defun neuron-rib-open-page (page)
  "Open the web-application at page PAGE."
  (browse-url (format "http://localhost:8080/%s" page)))

(defun neuron-rib-open-z-index ()
  "Open the web application in the web browser at z-index."
  (interactive)
  (neuron-rib-open-page "z-index.html"))

(defun neuron-rib-open-current-zettel ()
  "Open the web application in the web browser at the current zettel note."
  (interactive)
  (let ((zettel-id (f-base (buffer-file-name))))
    (neuron-rib-open-page (concat zettel-id ".html"))))

(defun neuron-rib-open-zettel ()
  "Open a zettel in the web application."
  (interactive)
  (let ((zettel-id (neuron-select-zettel)))
    (neuron-rib-open-page (concat zettel-id ".html"))))

(defun neuron-rib-kill ()
  "Stop the web application."
  (interactive)
  (kill-buffer "*rib*"))

(defvar zettel-mode-map nil "Keymap for `zettel-mode'.")

(progn
  (setq zettel-mode-map (make-sparse-keymap))

  (define-key zettel-mode-map (kbd "C-c C-z")   #'neuron-new-zettel)
  (define-key zettel-mode-map (kbd "C-c C-e")   #'neuron-edit-zettel)
  (define-key zettel-mode-map (kbd "C-c C-l")   #'neuron-insert-zettel-link)
  (define-key zettel-mode-map (kbd "C-c C-S-L") #'neuron-insert-new-zettel)
  (define-key zettel-mode-map (kbd "C-c C-r")   #'neuron-open-current-zettel)
  (define-key zettel-mode-map (kbd "C-c C-o")   #'neuron-follow-thing-at-point))

(defvar zettel-mode-hook nil
  "Hook run when entering zettel-mode.")

(defun zettel-mode--setup-hooks ()
  "Initialize all local hooks in zettel-mode."
  (when neuron-generate-on-save
    (add-hook 'after-save-hook #'neuron-rib-generate t t)))

(add-hook 'zettel-mode-hook #'zettel-mode--setup-hooks)

;;;###autoload
(define-derived-mode zettel-mode markdown-mode "Zettel"
  "A major mode to edit Zettelkasten notes with neuron."
  (use-local-map zettel-mode-map))

(provide 'zettel-mode)

;;; zettel-mode.el ends here
