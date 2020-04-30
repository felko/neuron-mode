;;; neuron-mode.el --- Major mode for editing zettelkasten notes using neuron -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 felko
;;
;; Author: felko <http://github/felko>
;; Homepage: https://github.com/felko/neuron-mode
;; Keywords: outlines
;; Package-Version: 0.1
;; Package-Requires: ((emacs "26.3") (f "0.20.0") (counsel "0.13.0") (markdown-mode "2.3"))
;;
;; This file is not part of GNU Emacs.

;;; License: GNU General Public License v3.0
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Editing zettelkasten notes using the neuron zettelkasten manager
;; https://neuron.zettel.page/

;;; Code:

(require 'f)
(require 'subr-x)
(require 'json)
(require 'url-parse)
(require 'counsel)
(require 'markdown-mode)

(defgroup neuron nil
  "A major mode for editing Zettelkasten notes with neuron."
  :prefix "neuron-"
  :link '(url-link "https://github.com/felko/neuron-mode")
  :group 'markdown)

(defcustom neuron-default-zettelkasten-directory (expand-file-name "~/zettelkasten")
  "The location of the default Zettelkasten directory."
  :group 'neuron
  :type  'string
  :safe  'f-directory?)

(defcustom neuron-generate-on-save nil
  "Whether to generate the necessary zettels when a buffer is saved."
  :group 'neuron
  :type  'boolean
  :safe  'booleanp)

(defcustom neuron-use-short-links t
  "Whether to use <ID> or [ID](z:/) syntax when inserting zettel links."
  :group 'neuron
  :type  'boolean
  :safe  'booleanp)

(defgroup neuron-faces nil
  "Faces used in neuron-mode."
  :group 'neuron
  :group 'faces)

(defface neuron-zettel-id-face
  '((((class color) (min-colors 88) (background dark)) :foreground "orange1")
    (((class color) (min-colors 88) (background light)) :foreground "orange3")
    (t :inherit link))
  "Face for zettel IDs in zettels and ivy-read prompts"
  :group 'neuron-faces)

(defface neuron-unknown-zettel-id-face
  '((t :inherit error))
  "Face for zettel IDs in zettels and ivy-read prompts"
  :group 'neuron-faces)

(defface neuron-zettel-tag-face
  '((t :inherit shadow))
  "Face for zettel IDs in zettels and ivy-read prompts"
  :group 'neuron-faces)

(defface neuron-title-overlay-face
  '((((class color) (min-colors 88) (background dark)) :foreground "LightSalmon1")
    (((class color) (min-colors 88) (background light)) :foreground "LightSalmon4")
    ((class color) :foreground "yellow")
    (t :italic t))
  "Face for title overlays displayed next to short links."
  :group 'neuron-face)

(defface neuron-unknown-face
  '((t :inherit error))
  "Face for the 'Unknown' label dislayed next to short links with unknown IDs."
  :group 'neuron-face)

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

(defun neuron--read-query-result (output)
  "Parse the OUTPUT of a query command in JSON.
Extract only the result itself, so the query type is lost."
  (map-elt (json-read-from-string output) 'result))

(defun neuron--query-url-command (uri)
  "Run a neuron query from a zquery URI."
  (neuron--read-query-result (neuron--run-command (neuron--make-query-uri-command uri))))

(defun neuron--run-rib-process (&rest args)
  "Run an asynchronous neuron process spawned by the rib command with arguments ARGS."
  (start-process-shell-command "rib" "*rib*" (apply #'neuron--make-command "rib" args)))

(defun neuron--run-rib-compile (&rest args)
  "Run an synchronous neuron command spawned by the rib command with arguments ARGS."
  (compile (apply #'neuron--make-command "rib" args)))

(defvar neuron--zettel-cache nil
  "Map containing all zettels indexed by their ID.")

(defun neuron--rebuild-cache ()
  "Rebuild the zettel cache with the current zettelkasten."
  (let ((zettels (neuron--query-url-command "zquery://search"))
        (assoc-id (lambda (zettel) (cons (intern (map-elt zettel 'id)) zettel))))
    (setq neuron--zettel-cache (mapcar assoc-id zettels))))

(defun neuron-refresh-buffer ()
  "Regenerate the zettel cache and the title overlays in the current buffer."
  (interactive)
  (neuron--rebuild-cache)
  (neuron--setup-overlays))

(defun neuron-select-zettelkasten ()
  "Select the active zettelkasten."
  (interactive)
  (setq neuron-zettelkasten (counsel-read-directory-name "Select Zettelkasten: "))
  (neuron--rebuild-cache))

(defun neuron-new-zettel ()
  "Create a new zettel in the current zettelkasten."
  (interactive)
  (when-let* ((title  (read-string "Title: "))
              (path   (neuron--run-command (neuron--make-command "new" title)))
              (buffer (find-file-noselect path)))
    (and
     (neuron--rebuild-cache)
     (pop-to-buffer-same-window buffer)
     (neuron-mode)
     (message (concat "Created " path)))))

(defun neuron--style-zettel-id (zid)
  "Style a ZID as shown in the ivy prompt."
  (propertize (format "<%s>" zid) 'face 'neuron-zettel-id-face))

(defun neuron--style-tags (tags)
  "Style TAGS as shown in the ivy prompt when selecting a zettel."
  (if (eq tags [])
      ""
    (propertize (format "(%s)" (s-join ", " tags)) 'face 'neuron-zettel-tag-face)))

(defun neuron--propertize-zettel (zettel)
  "Format ZETTEL as shown in the selection prompt."
  (let* ((zid (map-elt zettel 'id))
         (display
          (format "%s %s %s"
                  (neuron--style-zettel-id zid)
                  (map-elt zettel 'title)
                  (neuron--style-tags (map-elt zettel 'tags)))))
    (propertize display 'zettel zettel)))

(defun neuron--select-zettel-from-list (zettels &optional prompt)
  "Select a zettel from a given list.
ZETTELS is a list of maps containing zettels (keys: id, title, tags, path)
PROMPT is the prompt passed to `ivy-read'."
  (let* ((selection
          (ivy-read (or prompt "Select Zettel: ")
                    (mapcar #'neuron--propertize-zettel zettels)
                    :caller 'neuron--select-zettel-from-list)))
    (get-text-property 0 'zettel selection)))

(defun neuron--select-zettel-from-cache (&optional prompt)
  "Select a zettel from the current cache.
PROMPT is the prompt passed to `ivy-read'."
  (neuron--select-zettel-from-list neuron--zettel-cache prompt))

(defun neuron--select-zettel-from-query (uri)
  "Select a zettel from the match of URI."
  (neuron--select-zettel-from-list (neuron--query-url-command uri)))

(defun neuron-select-zettel (&optional prompt)
  "Find a zettel in the current zettelkasten.
PROMPT is the prompt passed to `ivy-read'."
  (neuron--select-zettel-from-cache prompt))

(defun neuron-edit-zettel ()
  "Select and edit a zettel from the currently active zettelkasten."
  (interactive)
  (let* ((zettel (neuron-select-zettel "Edit zettel: "))
         (path   (map-elt zettel 'path))
         (buffer (find-file-noselect path)))
    (and
     (pop-to-buffer-same-window buffer)
     (neuron-mode))))

(defun neuron--insert-zettel-link-from-id (id)
  "Insert a zettel link.
Depending on the value of `neuron-use-short-links',
the inserted link will either be of the form <ID> or
[ID](z:/)."
  (if neuron-use-short-links
      (progn
        (insert (format "<%s>" id))
        (neuron--setup-overlays))
    (format "[%s](z:/)" id)))

(defun neuron-insert-zettel-link ()
  "Insert a markdown hypertext link to another zettel."
  (interactive)
  (neuron--insert-zettel-link-from-id (map-elt (neuron-select-zettel) 'id)))

(defun neuron-insert-new-zettel ()
  "Create a new zettel."
  (interactive)
  (when-let* ((title  (read-string "Title: "))
              (path   (neuron--run-command (neuron--make-command "new" title)))
              (id     (f-base (f-no-ext path)))
              (buffer (find-file-noselect path)))
    (progn
      (neuron--rebuild-cache)
      (neuron--insert-zettel-link-from-id id)
      (pop-to-buffer-same-window buffer)
      (neuron-mode)
      (message (concat "Created " path)))))

(defun neuron--flatten-tag-node (node &optional root)
  "Flatten NODE into a list of tags.
Each element is a map containing 'tag and 'count keys.
The full tag is retrieved from the ROOT argument that is passed recursively.
See `neuron--flatten-tag-tree'."
  (let* ((name  (map-elt node 'name))
         (count (map-elt node 'count))
         (children (map-elt node 'children))
         (tag   (if root (concat root "/" name) name))
         (elem  (list (cons 'count count) (cons 'tag tag))))
    (cons elem (neuron--flatten-tag-tree children tag))))

(defun neuron--flatten-tag-tree (tree &optional root)
  "Flatten TREE into a list of tags.
Each element is a map containing 'tag and 'count keys.
The full tag is retrieved from the ROOT argument that is passed recursively."
  (apply #'append (mapcar (lambda (node) (neuron--flatten-tag-node node root)) tree)))

(defun neuron--propertize-tag (elem)
  "Format ELEM as shown in the tag selection prompt.
ELEM is a map containing the name of the tag and the number of associated zettels."
  (let ((tag   (map-elt elem 'tag))
        (count (map-elt elem 'count))
        (display-count (propertize (format "(%d)" count) 'face 'shadow)))
    (propertize (format "%s %s" tag display-count) 'tag tag 'count count)))

(defun neuron--select-tag-from-query (uri)
  "Prompt for a tag that is matched by the zquery URI."
  (let* ((tags (neuron--flatten-tag-tree (neuron--query-url-command uri)))
         (selection
          (ivy-read "Select tag: "
                    (mapcar #'neuron--propertize-tag tags)
                    :predicate (lambda (tag) (not (zerop (get-text-property 0  'count tag))))
                    :caller 'neuron-select-tag)))
    (get-text-property 0 'tag selection)))

(defun neuron-select-tag ()
  "Prompt for a tag that is already used in the zettelkasten."
  (neuron--select-tag-from-query "zquery://tags"))

(defun neuron-insert-tag ()
  "Select and insert a tag that is already used in the zettelkasten."
  (interactive)
  (insert (neuron-select-tag)))

(defun neuron-query-tags (&rest tags)
  "Select and edit a zettel from those that are tagged by TAGS."
  (interactive (list (neuron-select-tag)))
  (let ((query (mapconcat (lambda (tag) (format "tag=%s" tag)) tags "&")))
    (neuron--edit-zettel-from-query (format "zquery://search?%s" query))))

(defun neuron--edit-zettel-from-path (path)
  "Open a neuron zettel from PATH."
  (pop-to-buffer-same-window (find-file-noselect path))
  (neuron-mode))

(defun neuron--query-zettel-from-id (id)
  "Query a single zettel from the active zettelkasten from its ID.
Returns a map containing its title, tag and full path."
  (neuron--read-query-result (neuron--run-command (neuron--make-command "query" "--id" id))))

(defun neuron--get-cached-zettel-from-id (id &optional failed)
  "Fetch a zettel from the cache from its ID.
When FAILED is non-nil, the cache is regenerated first.
This is called internally to automatically refresh the cache when the ID
is not found."
  (when failed
    (neuron--rebuild-cache))
  (if-let ((zettel (map-elt neuron--zettel-cache (intern id))))
      zettel
    (if failed
        (neuron--get-current-zettel-id id t)
      (message "Cannot find zettel with ID %s" id)
      nil)))

(defun neuron--edit-zettel-from-id (id)
  "Open a neuron zettel from ID."
  (let ((zettel (neuron--get-cached-zettel-from-id id)))
    (neuron--edit-zettel-from-path
     (map-elt zettel 'path))))

(defun neuron--edit-zettel-from-query (uri)
  "Select and edit a zettel from a neuron query URI."
  (let ((struct (url-generic-parse-url uri)))
    (pcase (url-host struct)
      ("search" (neuron--edit-zettel-from-path (map-elt (neuron--select-zettel-from-query uri) 'path)))
      ("tags"   (neuron-query-tags (neuron--select-tag-from-query uri))))))

(defun neuron--get-current-zettel-id ()
  "Extract the zettel ID of the current file."
  (f-base (buffer-name)))

(defun neuron--open-page (rel-path)
  "Open the REL-PATH in the browser.
The path is relative to the neuron output directory."
  (let* ((path (f-join "/" neuron-zettelkasten ".neuron" "output" rel-path))
         (url (format "file://%s" path)))
    (browse-url url)))

(defun neuron--open-zettel-from-id (id)
  "Open the generated HTML file from the zettel ID."
  (neuron--open-page (format "%s.html" id)))

(defun neuron-open-zettel ()
  "Select a zettel and open the associated HTML file."
  (interactive)
  (neuron--open-zettel-from-id (map-elt (neuron-select-zettel "Open zettel: ") 'id)))

(defun neuron-open-index ()
  "Open the index.html file."
  (interactive)
  (neuron--open-page "index.html"))

(defun neuron-open-current-zettel ()
  "Open the current zettel's HTML file in the browser."
  (interactive)
  (neuron--open-zettel-from-id (neuron--get-current-zettel-id)))

(defconst neuron-short-link-regex (rx "<" (group (one-or-more alphanumeric)) ">")
  "Regex mathcing zettel links like <ID>.
Group 1 is the matched ID.")

(defun neuron-follow-thing-at-point ()
  "Open the zettel link at point."
  (interactive)
  ;; short links (from the `thing-at-point' demo)
  (if (thing-at-point-looking-at
       neuron-short-link-regex
       ;; limit to current line
       (max (- (point) (line-beginning-position))
            (- (line-end-position) (point))))
      (neuron--edit-zettel-from-id (match-string 1))
    ;; markdown links
    (let* ((link   (markdown-link-at-pos (point)))
           (id     (nth 2 link))
           (url    (nth 3 link))
           (struct (url-generic-parse-url url))
           (type   (url-type struct)))
      (pcase type
        ("z"        (neuron--edit-zettel-from-id id))
        ("zcf"      (neuron--edit-zettel-from-id id))
        ("zquery"   (neuron--edit-zettel-from-query url))
        ("zcfquery" (neuron--edit-zettel-from-query url))
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
  (let ((zid (f-base (buffer-file-name))))
    (neuron-rib-open-page (concat zid ".html"))))

(defun neuron-rib-open-zettel ()
  "Open a zettel in the web application."
  (interactive)
  (let ((zettel (neuron-select-zettel)))
    (neuron-rib-open-page (concat (map-elt zettel 'id) ".html"))))

(defun neuron-rib-kill ()
  "Stop the web application."
  (interactive)
  (kill-buffer "*rib*"))

(defun neuron--setup-overlay-from-id (ov zid)
  "Setup a single title overlay from a zettel ID.
OV is the overay to setup or update and ZID is the zettel ID."
  (overlay-put ov 'evaporate t)
  ;; (overlay-put ov 'display zid)
  (overlay-put ov 'face 'neuron-zettel-id-face)
  (overlay-put ov 'modification-hooks (list #'neuron--title-overlay-update))
  (if-let* ((zettel (neuron--get-cached-zettel-from-id zid))
            (title  (map-elt zettel 'title)))
      (overlay-put ov 'after-string (format " %s" (propertize title 'face 'neuron-title-overlay-face)))
    (overlay-put ov 'after-string (format " %s" (propertize "Unknown" 'face 'neuron-unknown-face)))
    (overlay-put ov 'face 'neuron-unknown-zettel-id-face)))

(defun neuron--title-overlay-update (ov after &rest _)
  "Delete the title overlay OV on modification.
When AFTER is non-nil, this hook is being called after the update occurs."
  (let ((short-link (buffer-substring (overlay-start ov) (overlay-end ov))))
    (when after
      (if (string-match neuron-short-link-regex short-link)
          (let ((zid (match-string 1 short-link)))
            (neuron--setup-overlay-from-id ov zid))
        (delete-overlay ov)))))

(defun neuron--setup-overlays ()
  "Setup title overlays on zettel links."
  (remove-overlays)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward neuron-short-link-regex nil t)
      (let* ((ov    (make-overlay (match-beginning 0) (match-end 0) nil t nil))
             (zid   (match-string 1)))
        (neuron--setup-overlay-from-id ov zid)))))

(defvar neuron-mode-map nil "Keymap for `neuron-mode'.")

(progn
  (setq neuron-mode-map (make-sparse-keymap))

  (define-key neuron-mode-map (kbd "C-c C-z")   #'neuron-new-zettel)
  (define-key neuron-mode-map (kbd "C-c C-e")   #'neuron-edit-zettel)
  (define-key neuron-mode-map (kbd "C-c C-t")   #'neuron-insert-tag)
  (define-key neuron-mode-map (kbd "C-c C-S-t") #'neuron-query-tags)
  (define-key neuron-mode-map (kbd "C-c C-l")   #'neuron-insert-zettel-link)
  (define-key neuron-mode-map (kbd "C-c C-S-L") #'neuron-insert-new-zettel)
  (define-key neuron-mode-map (kbd "C-c C-r")   #'neuron-open-current-zettel)
  (define-key neuron-mode-map (kbd "C-c C-S-r") #'neuron-refresh-buffer)
  (define-key neuron-mode-map (kbd "C-c C-o")   #'neuron-follow-thing-at-point))

(defvar neuron-mode-hook nil
  "Hook run when entering `neuron-mode'.")

(defun neuron-mode--setup-hooks ()
  "Initialize all local hooks in `neuron-mode'."
  (when neuron-generate-on-save
    (add-hook 'after-save-hook #'neuron-rib-generate t t))
  (neuron--setup-overlays)
  (add-hook 'after-save-hook #'neuron--setup-overlays t t))

(add-hook 'neuron-mode-hook #'neuron-mode--setup-hooks)

;;;###autoload
(define-derived-mode neuron-mode markdown-mode "Neuron"
  "A major mode to edit Zettelkasten notes with neuron."
  (use-local-map neuron-mode-map))

(neuron--rebuild-cache)

(provide 'neuron-mode)

;;; neuron-mode.el ends here
