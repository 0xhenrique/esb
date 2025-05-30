;;; esb.el --- Emacs Simple Bookmark -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Henrique Marques

;; Author: Henrique Marques <hm2030master@proton.me>
;; URL: https://github.com/0xhenrique/esb
;; Version: 0.1
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

(defgroup esb nil
  "Emacs Simple Bookmark."
  :group 'tools)

(defcustom esb-bookmarks-file "~/.bookmarks.gpg"
  "Path to the encrypted bookmarks file."
  :type 'string
  :group 'esb)

(defvar esb-bookmarks-cache nil
  "In-memory cache of decrypted bookmarks.")

(defvar esb-cache-dirty nil
  "Flag indicating if cache needs to be saved.")

;;; Core functions

(defun esb--ensure-epa-setup ()
  "Ensure EPA file encryption is properly configured."
  (unless (member epa-file-handler file-name-handler-alist)
    (epa-file-enable)))

(defun esb--read-bookmarks ()
  "Read and decrypt bookmarks from file."
  (esb--ensure-epa-setup)
  (if (file-exists-p esb-bookmarks-file)
      (with-temp-buffer
        (insert-file-contents esb-bookmarks-file)
        (condition-case err
            (json-parse-string (buffer-string) :array-type 'list :object-type 'alist)
          (json-error
           (message "Error parsing bookmarks file: %s" err)
           nil)))
    nil))

(defun esb--write-bookmarks (bookmarks)
  "Encrypt and write BOOKMARKS to file."
  (esb--ensure-epa-setup)
  (with-temp-buffer
    (insert (json-encode bookmarks))
    (write-file esb-bookmarks-file))
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

(defun esb--bookmark-urls ()
  "Get list of bookmark URLs."
  (mapcar (lambda (bookmark) (alist-get 'url bookmark)) (esb--get-bookmarks)))

(defun esb--find-bookmark-by-url (url)
  "Find bookmark by URL."
  (seq-find (lambda (bookmark) (string= (alist-get 'url bookmark) url))
            (esb--get-bookmarks)))

;;; Interactive functions

;;;###autoload
(defun esb-add-bookmark (url &optional description)
  "Add a new bookmark with URL and optional DESCRIPTION."
  (interactive "sBookmark URL: \nsDescription (optional): ")
  (let* ((bookmarks (esb--get-bookmarks))
         (existing (esb--find-bookmark-by-url url)))
    (if existing
        (message "Bookmark already exists: %s" url)
      (let ((new-bookmark `((url . ,url)
                           (description . ,(if (string-empty-p description) nil description)))))
        (setq esb-bookmarks-cache (append bookmarks (list new-bookmark)))
        (setq esb-cache-dirty t)
        (esb--save-if-dirty)
        (message "Added bookmark: %s" url)))))

;;;###autoload
(defun esb-delete-bookmark ()
  "Delete a bookmark by selecting from list."
  (interactive)
  (let* ((bookmarks (esb--get-bookmarks))
         (urls (esb--bookmark-urls)))
    (if (null urls)
        (message "No bookmarks found")
      (let* ((selected-url (completing-read "Delete bookmark: " urls nil t))
             (updated-bookmarks (seq-remove (lambda (bookmark)
                                           (string= (alist-get 'url bookmark) selected-url))
                                         bookmarks)))
        (setq esb-bookmarks-cache updated-bookmarks)
        (setq esb-cache-dirty t)
        (esb--save-if-dirty)
        (message "Deleted bookmark: %s" selected-url)))))

;;;###autoload
(defun esb-list-bookmarks ()
  "Display all bookmarks in a buffer."
  (interactive)
  (let ((bookmarks (esb--get-bookmarks)))
    (if (null bookmarks)
        (message "No bookmarks found")
      (with-output-to-temp-buffer "*Esb Bookmarks*"
        (princ "Bookmarks:\n\n")
        (dolist (bookmark bookmarks)
          (let ((url (alist-get 'url bookmark))
                (desc (alist-get 'description bookmark)))
            (princ (format "• %s\n" url))
            (when desc
              (princ (format "  %s\n" desc)))
            (princ "\n")))))))

;;;###autoload
(defun esb-select-bookmark ()
  "Select a bookmark and copy URL to clipboard."
  (interactive)
  (let ((urls (esb--bookmark-urls)))
    (if (null urls)
        (message "No bookmarks found")
      (let ((selected-url (completing-read "Select bookmark: " urls nil t)))
        (kill-new selected-url)
        (message "Copied to clipboard: %s" selected-url)))))

;;;###autoload
(defun esb-edit-bookmark ()
  "Edit description of an existing bookmark."
  (interactive)
  (let* ((urls (esb--bookmark-urls)))
    (if (null urls)
        (message "No bookmarks found")
      (let* ((selected-url (completing-read "Edit bookmark: " urls nil t))
             (bookmark (esb--find-bookmark-by-url selected-url))
             (current-desc (or (alist-get 'description bookmark) ""))
             (new-desc (read-string "Description: " current-desc)))
        (setf (alist-get 'description bookmark) (if (string-empty-p new-desc) nil new-desc))
        (setq esb-cache-dirty t)
        (esb--save-if-dirty)
        (message "Updated bookmark: %s" selected-url)))))

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



;;;###autoload
(define-minor-mode esb-mode
  "Minor mode for encrypted bookmark management.
This mode provides no key bindings by default.
Users should define their own key bindings for ESB functions."
  :global t
  :group 'esb)

(provide 'esb)

;;; esb.el ends here
