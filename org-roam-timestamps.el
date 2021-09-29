;;; org-roam-timestamps.el --- m/ctime properties for org-roam -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Thomas F. K. Jorna
;;
;; Author: Thomas F. K. Jorna <https://github.com/thomas>
;; Maintainer: Thomas F. K. Jorna <jorna@jtrialerror.com>
;; Created: September 27, 2021
;; Modified: September 27, 2021
;; Version: 0.0.1
;; Keywords: org-roam zettelkasten time
;; Homepage: https://github.com/thomas/org-roam-timestamps
;; Package-Requires: ((emacs "26.1") org-roam)
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(require 'org-roam)

(defgroup org-roam-timestamps nil
  "Creation and modification timestamps in Org-roam."
  :group 'org-roam
  :prefix "org-roam-timestamps-"
  :link '(url-link :tag "Github" "https://github.com/ThomasFKJorna/org-roam-timestamps"))

;;;###autoload
(define-minor-mode org-roam-timestamps-mode
  "Automatically add creation and modification timestamps to org-roam nodes."
  :global t
  :group 'org-roam-timestamps
  :lighter " org-roam-timestamps"
  :init-value nil
  (when org-roam-timestamps-mode
    (message "Stamping")))

(defun org-roam-timestamps-decode (mtime)
  "Decode a list of seconds since 1970 MTIME to an org-roam-timestamp."
  (let ((time (decode-time mtime))
        dec-time
        el)
  (dotimes (i 6 )
    (setq el (number-to-string (nth i time)))
      (when (length= el 1) (setq el (concat "0" el)))
    (setq dec-time (concat
                    el
                    dec-time)))
  dec-time))

(defun org-roam-timestamps-all ()
  "Go through all nodes and add timestamps to them."
  (interactive)
  (when (yes-or-no-p "This will modify all your current notes by adding a ctime and mtime property
to all property drawers. We will make a backup of your notes first.
This might take a second. Are you sure you want to continue?")
    (let ((backup-dir (expand-file-name "org-roam-timestamp.bak"
                                         (file-name-directory (directory-file-name org-roam-directory)))))
      (message "Backing up files to %s" backup-dir)
      (copy-directory org-roam-directory backup-dir))
  (let ((nodes (org-roam-db-query [:select id :from nodes])))
    (dolist (node nodes)
      (let* ((n (org-roam-populate (org-roam-node-create :id (car node))))
             (file (org-roam-node-file n))
             (mtime (org-roam-timestamps-decode (org-roam-node-file-mtime n)))
             (pos (org-roam-node-point n))
             (props (org-roam-node-properties n))
             (level (org-roam-node-level n))
             (title (org-roam-node-title n)))
            (org-roam-with-file file nil
              (goto-char pos)
              (unless (assoc-default "MTIME" props)
              (org-roam-add-property mtime "mtime"))
              (unless (assoc-default "CTIME" props)
                  (if-let ((filename (file-name-base file))
                           (index (string-match "^[0-9]\\{14\\}" filename))
                           (timestamp (substring filename index (+ index 14))))
                      (org-roam-add-property timestamp "ctime")
                    (org-roam-add-property mtime "ctime")))
                  (save-buffer))))))
  (org-roam-db-sync))

(provide 'org-roam-timestamps)
;;; org-roam-timestamps.el ends here
