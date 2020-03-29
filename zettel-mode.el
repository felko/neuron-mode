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

(defconst neuron-dir (getenv "ZETTELPATH"))

(defvar neuron-zettelkasten-name "test")

(defvar neuron-rib-has-running-instance nil)

(defun neuron-zettelkasten-list ()
  "Return the list of zettelkastens."
  (mapcar (lambda (path) (f-relative path neuron-dir)) (f-directories neuron-dir)))

(defun neuron-zettelkasten-path ()
  "Return the path of the zettelkasten."
  (f-join "/" neuron-dir neuron-zettelkasten-name))

(defun neuron-command (cmd)
  "Run a neuron command in the current zettekasten.
CMD should be a string representing the command"
  (shell-command-to-string (concat "neuron " (neuron-zettelkasten-path) " " cmd)))

(defun neuron-rib-command (cmd)
  "Run a neuron command to manage the web application.
CMD is a string representing a neuron rib command."
  (start-process "neuron-rib" "*neuron-rib*" "neuron" (neuron-zettelkasten-path) "rib" cmd))

(defun neuron-select-zettelkasten ()
  "Select the active zettelkasten."
  (interactive)
  (ivy-read "Select Zettelkasten: "
            (neuron-zettelkasten-list)
            :predicate  (lambda (path) (not (string-prefix-p "." path)))
            :action (lambda (path) (setq neuron-zettelkasten-name (f-base path)))
            :caller 'neuron-select-zettelkasten))

(defun neuron-new-zettel ()
  "Create a new zettel."
  (interactive)
  (let* ((zettel-path   (string-trim-right (neuron-command "new \"\"")))
         (zettel-buffer (find-file-noselect zettel-path)))
    (and
     (pop-to-buffer-same-window zettel-buffer)
     (zettel-mode)
     (forward-line 1)
     (end-of-line)
     (message (concat "Created " zettel-path)))))

(defun neuron-select-zettel ()
  "Find a zettel."
  (interactive)
  (let ((match (counsel-rg "title: " (neuron-zettelkasten-path) "--no-line-number --no-heading --sort path" "Select Zettel: ")))
    (f-base (car (split-string match ":")))))

(defun neuron-edit-zettel ()
  "Edit a zettel."
  (interactive)
  (let* ((id (neuron-select-zettel))
         (path (f-join "/" (neuron-zettelkasten-path) (concat id ".md")))
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
  (let* ((path   (string-trim-right (neuron-command "new \"\"")))
         (id     (f-base (f-no-ext path)))
         (buffer (find-file-noselect path)))
    (and
     (insert (format "[%s](z:/)" id))
     (pop-to-buffer-same-window buffer)
     (zettel-mode)
     (forward-line 1)
     (end-of-line)
     (message (concat "Created " path)))))

(defun neuron-edit-zettel-from-path (path &optional before after)
  "Open a neuron zettel from PATH.
Execute BEFORE just before popping the buffer and AFTER just after enabling `zettel-mode'."
  (let* ((buffer (find-file-noselect path)))
    (and
     (if before (funcall before) t)
     (pop-to-buffer-same-window buffer)
     (zettel-mode)
     (if after (funcall after) t))))

(defun neuron-edit-zettel-from-id (id &optional before after)
  "Open a neuron zettel from ID.
Execute BEFORE just before popping the buffer and AFTER just after enabling `zettel-mode'."
  (neuron-edit-zettel-from-path
   (f-join "/" (neuron-zettelkasten-path) (format "%s.md" id))
   before
   after))

(defun neuron-follow-thing-at-point ()
  "Open the zettel link at POS."
  (interactive)
  (let* ((link   (markdown-link-at-pos (point)))
         (id     (nth 2 link))
         (url    (nth 3 link))
         (struct (url-generic-parse-url url))
         (type   (url-type struct)))
    (if (string-equal type "z")
        (neuron-edit-zettel-from-id id)
      (markdown-follow-thing-at-point (point)))
    (message "%S" (list type id)))
  )

;; TODO: ask srid why he chose [id](z:/) syntax instead of [link display](z:/id)

(defun neuron-rib-serve ()
  "Start a web app for browsing the zettelkasten."
  (interactive)
  (if neuron-rib-has-running-instance
      (message "A rib instance is already running")
    (and
     (neuron-rib-command "serve")
     (setq neuron-rib-has-running-instance t)
     (message "Started web application on localhost:8080"))))

(defun neuron-rib-open-page (page)
  "Open the web-application at page PAGE."
  (if neuron-rib-has-running-instance
    (browse-url (format "http://localhost:8080/%s" page))
    (message "No rib instance detected, start one first")))

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
  (if neuron-rib-has-running-instance
      (and
       (kill-buffer "*neuron-rib*")
       (setq neuron-rib-has-running-instance nil))
    (message "No rib instance to kill")))

(defvar zettel-mode-map nil "Keymap for `zettel-mode'.")

(progn
  (setq zettel-mode-map (make-sparse-keymap))

  (define-key zettel-mode-map (kbd "C-c C-z")   #'neuron-new-zettel)
  (define-key zettel-mode-map (kbd "C-c C-e")   #'neuron-edit-zettel)
  (define-key zettel-mode-map (kbd "C-c C-l")   #'neuron-insert-zettel-link)
  (define-key zettel-mode-map (kbd "C-c C-S-L") #'neuron-insert-new-zettel)
  (define-key zettel-mode-map (kbd "C-c C-r")   #'neuron-rib-open-current-zettel)
  (define-key zettel-mode-map (kbd "C-c C-o")   #'neuron-follow-thing-at-point))

(define-derived-mode zettel-mode markdown-mode "Zettel"
  "A major mode to edit Zettelkasten notes with neuron."
  (use-local-map zettel-mode-map))
; (add-to-list 'auto-mode-alist '("\\.\\'" . packet-mode))

(provide 'zettel-neuron)

;;; zettel-mode.el ends here
