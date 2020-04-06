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
(require 'counsel)
(require 'json)

(defvar neuron-zettelkasten (expand-file-name "~/.zettle")
  "The zettle location.")

;; (defun neuron-zettelkasten-list ()
;;   "Return the list of zettelkastens."
;;   (mapcar (lambda (path) (f-relative path neuron-dir)) (f-directories neuron-dir)))

(defun neuron--command (cmd)
  "Run a neuron command in the current zettekasten.
CMD should be a string representing the command"
  (make-directory neuron-zettelkasten :parents)
  (shell-command-to-string (concat "neuron " neuron-zettelkasten " " cmd)))

(defun neuron--json-extract-info (match)
  "Extract Zettel ID and title from MATCH, as a JSON string."
  (let ((lst (json-read-from-string match)))
    (mapcar (lambda (obj) (list (map-elt obj 'id) (map-elt obj 'title))) lst)))

(defun neuron--query-url-command (query-url)
  "Run a neuron query from a zquery QUERY-URL."
  (let* ((res (shell-command-to-string (concat "neuron " neuron-zettelkasten " query --uri '" query-url "'"))))
    (neuron--json-extract-info res)))

(defun neuron--rib-command (cmd)
  "Run a neuron command to manage the web application.
CMD is a string representing a neuron rib command."
  (start-process "neuron-rib" "*neuron-rib*" "neuron" neuron-zettelkasten "rib" cmd))

(defun neuron-select-zettelkasten ()
  "Select the active zettelkasten."
  (interactive)
  (setq neuron-zettelkasten (counsel-read-directory-name "Select Zettelkasten: ")))

(defun neuron-new-zettel ()
  "Create a new zettel."
  (interactive)
  (let* ((path   (string-trim-right (neuron--command "new \"\"")))
         (buffer (find-file-noselect path)))
    (and
     (pop-to-buffer-same-window buffer)
     (zettel-mode)
     (forward-line 1)
     (end-of-line)
     (message (concat "Created " path)))))

(defun neuron-select-zettel ()
  "Find a zettel."
  (interactive)
  (let ((match (counsel-rg "title: " neuron-zettelkasten "--no-line-number --no-heading --sort path" "Select Zettel: ")))
    (f-base (car (split-string match ":")))))

(defun neuron--select-zettel-from-query (query-url)
  "Select a zettel from the match of QUERY-URL."
  (ivy-read "Select Zettel: "
            (mapcar (lambda (z) (propertize (format "[%s] %s" (nth 0 z) (nth 1 z)) 'id (nth 0 z)))
             (neuron--query-url-command query-url))
            ; :predicate  (lambda (path) (not (string-prefix-p "." path)))
            :action (lambda (z) (neuron--edit-zettel-from-id (get-text-property 0 'id z)))
            :caller 'neuron-select-zettel-from-query))

(defun neuron-edit-zettel ()
  "Edit a zettel."
  (interactive)
  (let* ((id (neuron-select-zettel))
         (path (f-join "/" neuron-zettelkasten (concat id ".md")))
         (buffer (find-file-noselect path)))
    (and
     (pop-to-buffer-same-window buffer)
     (zettel-mode))))

(defun neuron-insert-zettel-link ()
  "Insert a markdown hypertext link to another zettel."
  (interactive)
  (let ((id (neuron-select-zettel)))
    (insert (format "[%s](z:/)" id))))

(defun neuron-insert-new-zettel ()
  "Create a new zettel."
  (interactive)
  (let* ((path   (string-trim-right (neuron--command "new \"\"")))
         (id     (f-base (f-no-ext path)))
         (buffer (find-file-noselect path)))
    (and
     (insert (format "[%s](z:/)" id))
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

(defun neuron-follow-thing-at-point ()
  "Open the zettel link at point."
  (interactive)
  (let* ((link   (markdown-link-at-pos (point)))
         (id     (nth 2 link))
         (url    (nth 3 link))
         (struct (url-generic-parse-url url))
         (type   (url-type struct)))
    (pcase type
      ("z"        (neuron--edit-zettel-from-id id))
      ("zquery"   (neuron--select-zettel-from-query url))
      ("zcfquery" (neuron--select-zettel-from-query url))
      (_          (markdown-follow-thing-at-point link)))))

(defun neuron-rib-serve ()
  "Start a web app for browsing the zettelkasten."
  (interactive)
  (neuron--rib-command "serve")
  (message "Started web application on localhost:8080"))

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
  (kill-buffer "*neuron-rib*"))

(defvar zettel-mode-map nil "Keymap for `zettel-mode'.")

(progn
  (setq zettel-mode-map (make-sparse-keymap))

  (define-key zettel-mode-map (kbd "C-c C-z")   #'neuron-new-zettel)
  (define-key zettel-mode-map (kbd "C-c C-e")   #'neuron-edit-zettel)
  (define-key zettel-mode-map (kbd "C-c C-l")   #'neuron-insert-zettel-link)
  (define-key zettel-mode-map (kbd "C-c C-S-L") #'neuron-insert-new-zettel)
  (define-key zettel-mode-map (kbd "C-c C-r")   #'neuron-rib-open-current-zettel)
  (define-key zettel-mode-map (kbd "C-c C-o")   #'neuron-follow-thing-at-point))

;;;###autoload
(define-derived-mode zettel-mode markdown-mode "Zettel"
  "A major mode to edit Zettelkasten notes with neuron."
  (use-local-map zettel-mode-map))
; (add-to-list 'auto-mode-alist '("\\.\\'" . packet-mode))

(provide 'zettel-mode)

;;; zettel-mode.el ends here
