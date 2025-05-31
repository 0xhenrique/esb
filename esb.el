;;; esb.el --- Emacs Simple Bookmark -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Henrique Marques

;; Author: Henrique Marques <hm2030master@proton.me>
;; URL: https://github.com/0xhenrique/esb
;; Version: 0.2
;; Package-Requires: ((emacs "27.1"))
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Commentary:
;; This is a simple encrypted bookmark manager for Emacs that
;; stores bookmarks in an encrypted file suitable for syncing via Git.
;; It uses GPG encryption to keep your bookmarks secure while allowing
;; you to store them in public repositories.

;;; Code:

(require 'epa-file)
(require 'json)
(require 'seq)
(require 'url-parse)

;;; Customization

(defgroup esb nil
  "Emacs Simple Bookmark."
  :group 'tools)

(defcustom esb-bookmarks-file "~/.bookmarks.gpg"
  "Path to the encrypted bookmarks file."
  :type 'string
  :group 'esb)

(defcustom esb-storage-backend 'gpg
  "Storage backend for bookmarks."
  :type '(choice (const :tag "GPG encrypted" gpg)
                 (const :tag "Plain text" plain)
                 (function :tag "Custom backend"))
  :group 'esb)

;;; Internal Variables

(defvar esb-bookmarks-cache nil
  "In-memory cache of decrypted bookmarks.")

(defvar esb-cache-dirty nil
  "Flag indicating if cache needs to be saved.")

;;; Utility Functions

(defun esb--ensure-epa-setup ()
  "Ensure EPA file encryption is properly configured."
  (unless (member epa-file-handler file-name-handler-alist)
    (epa-file-enable)))

(defun esb--valid-url-p (url)
  "Check if URL is valid."
  (and (stringp url)
       (not (string-empty-p url))
       (string-match-p "^https?://" url)
       (url-generic-parse-url url)))

(defun esb--valid-bookmark-p (bookmark)
  "Check if BOOKMARK has valid structure."
  (and (listp bookmark)
       (esb--valid-url-p (alist-get 'url bookmark))
       (let ((tags (alist-get 'tags bookmark)))
         (or (null tags)
             (and (listp tags)
                  (seq-every-p #'stringp tags))))))

(defun esb--normalize-tags (tags)
  "Normalize TAGS input to a list of strings."
  (cond
   ((null tags) nil)
   ((stringp tags)
    (if (string-empty-p tags)
        nil
      (mapcar #'string-trim (split-string tags "[,[:space:]]+" t))))
   ((listp tags) (seq-filter (lambda (tag) (and (stringp tag) (not (string-empty-p tag)))) tags))
   (t nil)))

;;; Storage Backend Functions

(defun esb--read-bookmarks-gpg ()
  "Read bookmarks from GPG encrypted file."
  (esb--ensure-epa-setup)
  (if (file-exists-p esb-bookmarks-file)
      (condition-case err
          (with-temp-buffer
            (insert-file-contents esb-bookmarks-file)
            (let ((content (string-trim (buffer-string))))
              (if (string-empty-p content)
                  '()
                (let ((parsed (json-parse-string content :array-type 'list :object-type 'alist)))
                  (if (eq parsed :null) '() parsed)))))
        (file-error
         (user-error "Cannot read bookmark file: %s" (error-message-string err)))
        (json-error
         (user-error "Invalid JSON in bookmark file: %s" (error-message-string err)))
        (error
         (user-error "GPG decryption failed: %s" (error-message-string err))))
    '()))

(defun esb--write-bookmarks-gpg (bookmarks)
  "Write BOOKMARKS to GPG encrypted file."
  (esb--ensure-epa-setup)
  (condition-case err
      (with-temp-buffer
        (insert (json-encode bookmarks))
        (write-file esb-bookmarks-file))
    (error
     (user-error "Failed to write bookmark file: %s" (error-message-string err)))))

(defun esb--read-bookmarks-plain ()
  "Read bookmarks from plain text file."
  (if (file-exists-p esb-bookmarks-file)
      (condition-case err
          (with-temp-buffer
            (insert-file-contents esb-bookmarks-file)
            (let ((content (string-trim (buffer-string))))
              (if (string-empty-p content)
                  '()
                (let ((parsed (json-parse-string content :array-type 'list :object-type 'alist)))
                  (if (eq parsed :null) '() parsed)))))
        (file-error
         (user-error "Cannot read bookmark file: %s" (error-message-string err)))
        (json-error
         (user-error "Invalid JSON in bookmark file: %s" (error-message-string err))))
    '()))

(defun esb--write-bookmarks-plain (bookmarks)
  "Write BOOKMARKS to plain text file."
  (condition-case err
      (with-temp-buffer
        (insert (json-encode bookmarks))
        (write-file esb-bookmarks-file))
    (error
     (user-error "Failed to write bookmark file: %s" (error-message-string err)))))

;;; Core Storage Functions

(defun esb--read-bookmarks ()
  "Read bookmarks using configured backend."
  (let ((bookmarks
         (pcase esb-storage-backend
           ('gpg (esb--read-bookmarks-gpg))
           ('plain (esb--read-bookmarks-plain))
           ((pred functionp) (funcall esb-storage-backend 'read))
           (_ (user-error "Unknown storage backend: %s" esb-storage-backend)))))
    (seq-filter #'esb--valid-bookmark-p bookmarks)))

(defun esb--write-bookmarks (bookmarks)
  "Write BOOKMARKS using configured backend."
  (let ((valid-bookmarks (seq-filter #'esb--valid-bookmark-p bookmarks)))
    (pcase esb-storage-backend
      ('gpg (esb--write-bookmarks-gpg valid-bookmarks))
      ('plain (esb--write-bookmarks-plain valid-bookmarks))
      ((pred functionp) (funcall esb-storage-backend 'write valid-bookmarks))
      (_ (user-error "Unknown storage backend: %s" esb-storage-backend))))
  (setq esb-cache-dirty nil))

(defun esb--get-bookmarks ()
  "Get bookmarks from cache or file."
  (unless esb-bookmarks-cache
    (setq esb-bookmarks-cache (or (esb--read-bookmarks) '())))
  esb-bookmarks-cache)

(defun esb--save-if-dirty ()
  "Save bookmarks to file if cache is dirty."
  (when esb-cache-dirty
    (esb--write-bookmarks esb-bookmarks-cache)))

;;; Bookmark Query Functions

(defun esb--find-bookmark-by-url (url)
  "Find bookmark by URL."
  (seq-find (lambda (bookmark) (string= (alist-get 'url bookmark) url))
            (esb--get-bookmarks)))

(defun esb--bookmark-exists-p (url)
  "Check if bookmark with URL already exists."
  (not (null (esb--find-bookmark-by-url url))))

(defun esb--get-all-tags ()
  "Get all unique tags from bookmarks."
  (let ((all-tags '()))
    (dolist (bookmark (esb--get-bookmarks))
      (let ((tags (alist-get 'tags bookmark)))
        (when tags
          (setq all-tags (append all-tags tags)))))
    (seq-uniq all-tags)))

(defun esb--filter-bookmarks-by-tag (tag)
  "Filter bookmarks that contain TAG."
  (seq-filter (lambda (bookmark)
                (member tag (alist-get 'tags bookmark)))
              (esb--get-bookmarks)))

;;; Interactive Functions

;;;###autoload
(defun esb-add-bookmark (url &optional description tags)
  "Add a new bookmark with URL, optional DESCRIPTION and TAGS."
  (interactive
   (let ((url (read-string "Bookmark URL: "))
         (description (read-string "Description (optional): "))
         (tags (read-string "Tags (comma-separated, optional): ")))
     (list url
           (if (string-empty-p description) nil description)
           tags)))
  
  (unless (esb--valid-url-p url)
    (user-error "Invalid URL: %s" url))
  
  (when (esb--bookmark-exists-p url)
    (user-error "Bookmark already exists: %s" url))
  
  (let* ((normalized-tags (esb--normalize-tags tags))
         (new-bookmark `((url . ,url)
                        (description . ,description)
                        (tags . ,normalized-tags)))
         (bookmarks (esb--get-bookmarks)))
    
    (setq esb-bookmarks-cache (append bookmarks (list new-bookmark)))
    (setq esb-cache-dirty t)
    (esb--save-if-dirty)
    (message "Added bookmark: %s%s"
             url
             (if normalized-tags (format " [%s]" (string-join normalized-tags ", ")) ""))))

;;;###autoload
(defun esb-delete-bookmark ()
  "Delete a bookmark by selecting from list."
  (interactive)
  (let ((bookmarks (esb--get-bookmarks)))
    (when (null bookmarks)
      (user-error "No bookmarks found"))
    
    (let* ((choices (mapcar (lambda (bookmark)
                             (let ((url (alist-get 'url bookmark))
                                   (desc (alist-get 'description bookmark))
                                   (tags (alist-get 'tags bookmark)))
                               (format "%s%s%s"
                                      url
                                      (if desc (format " - %s" desc) "")
                                      (if tags (format " [%s]" (string-join tags ", ")) ""))))
                           bookmarks))
           (selected (completing-read "Delete bookmark: " choices nil t))
           (selected-url (car (split-string selected " ")))
           (updated-bookmarks (seq-remove (lambda (bookmark)
                                         (string= (alist-get 'url bookmark) selected-url))
                                       bookmarks)))
      
      (setq esb-bookmarks-cache updated-bookmarks)
      (setq esb-cache-dirty t)
      (esb--save-if-dirty)
      (message "Deleted bookmark: %s" selected-url))))

;;;###autoload
(defun esb-list-bookmarks (&optional tag)
  "Display all bookmarks in a buffer, optionally filtered by TAG."
  (interactive
   (when current-prefix-arg
     (list (completing-read "Filter by tag: " (esb--get-all-tags) nil t))))
  
  (let ((bookmarks (if tag
                      (esb--filter-bookmarks-by-tag tag)
                    (esb--get-bookmarks))))
    (when (null bookmarks)
      (user-error "No bookmarks found%s" (if tag (format " with tag '%s'" tag) "")))
    
    (with-output-to-temp-buffer "*ESB Bookmarks*"
      (princ (format "Bookmarks%s:\n\n" (if tag (format " tagged '%s'" tag) "")))
      (dolist (bookmark bookmarks)
        (let ((url (alist-get 'url bookmark))
              (desc (alist-get 'description bookmark))
              (tags (alist-get 'tags bookmark)))
          (princ (format "• %s\n" url))
          (when desc
            (princ (format "  %s\n" desc)))
          (when tags
            (princ (format "  Tags: %s\n" (string-join tags ", "))))
          (princ "\n"))))))

;;;###autoload
(defun esb-select-bookmark (&optional tag)
  "Select a bookmark and copy URL to clipboard, optionally filtered by TAG."
  (interactive
   (when current-prefix-arg
     (list (completing-read "Filter by tag: " (esb--get-all-tags) nil t))))
  
  (let ((bookmarks (if tag
                      (esb--filter-bookmarks-by-tag tag)
                    (esb--get-bookmarks))))
    (when (null bookmarks)
      (user-error "No bookmarks found%s" (if tag (format " with tag '%s'" tag) "")))
    
    (let* ((choices (mapcar (lambda (bookmark)
                             (let ((url (alist-get 'url bookmark))
                                   (desc (alist-get 'description bookmark))
                                   (tags (alist-get 'tags bookmark)))
                               (format "%s%s%s"
                                      url
                                      (if desc (format " - %s" desc) "")
                                      (if tags (format " [%s]" (string-join tags ", ")) ""))))
                           bookmarks))
           (selected (completing-read "Select bookmark: " choices nil t))
           (selected-url (car (split-string selected " "))))
      
      (kill-new selected-url)
      (message "Copied to clipboard: %s" selected-url))))

;;;###autoload
(defun esb-edit-bookmark ()
  "Edit an existing bookmark."
  (interactive)
  (let ((bookmarks (esb--get-bookmarks)))
    (when (null bookmarks)
      (user-error "No bookmarks found"))
    
    (let* ((choices (mapcar (lambda (bookmark)
                             (let ((url (alist-get 'url bookmark))
                                   (desc (alist-get 'description bookmark))
                                   (tags (alist-get 'tags bookmark)))
                               (format "%s%s%s"
                                      url
                                      (if desc (format " - %s" desc) "")
                                      (if tags (format " [%s]" (string-join tags ", ")) ""))))
                           bookmarks))
           (selected (completing-read "Edit bookmark: " choices nil t))
           (selected-url (car (split-string selected " ")))
           (bookmark (esb--find-bookmark-by-url selected-url))
           (current-desc (or (alist-get 'description bookmark) ""))
           (current-tags (alist-get 'tags bookmark))
           (new-desc (read-string "Description: " current-desc))
           (new-tags-str (read-string "Tags (comma-separated): "
                                     (if current-tags (string-join current-tags ", ") "")))
           (new-tags (esb--normalize-tags new-tags-str)))
      
      (setf (alist-get 'description bookmark) (if (string-empty-p new-desc) nil new-desc))
      (setf (alist-get 'tags bookmark) new-tags)
      (setq esb-cache-dirty t)
      (esb--save-if-dirty)
      (message "Updated bookmark: %s" selected-url))))

;;;###autoload
(defun esb-list-tags ()
  "Display all available tags."
  (interactive)
  (let ((tags (esb--get-all-tags)))
    (if (null tags)
        (message "No tags found")
      (with-output-to-temp-buffer "*ESB Tags*"
        (princ "Available tags:\n\n")
        (dolist (tag (sort tags #'string<))
          (let ((count (length (esb--filter-bookmarks-by-tag tag))))
            (princ (format "• %s (%d bookmark%s)\n"
                          tag count (if (= count 1) "" "s")))))))))

;;;###autoload
(defun esb-reload-bookmarks ()
  "Reload bookmarks from file (useful after git pull)."
  (interactive)
  (setq esb-bookmarks-cache nil)
  (setq esb-cache-dirty nil)
  (esb--get-bookmarks)
  (message "Bookmarks reloaded from %s" esb-bookmarks-file))

;;;###autoload
(defun esb-initialize ()
  "Initialize bookmark file if it doesn't exist."
  (interactive)
  (if (file-exists-p esb-bookmarks-file)
      (message "Bookmark file already exists at: %s" esb-bookmarks-file)
    (esb--write-bookmarks '())
    (message "Initialized empty bookmark file at: %s" esb-bookmarks-file)))

(provide 'esb)

;;; esb.el ends here
