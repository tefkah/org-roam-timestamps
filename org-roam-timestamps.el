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

(defcustom org-roam-timestamps-timestamp-parent-file t
  "Whether to timestamp the parent file when modifying a child node."
  :group 'org-roam-timestamps
  :type 'boolean)

(defcustom org-roam-timestamps-remember-timestamps t
  "Whether to keep previous timestamps when updating the current one.
This allows you to see when you modified said file,
but will increase note and db file size."
  :group 'org-roam-timestamps
  :type 'boolean)

(defcustom org-roam-timestamps-minimum-gap 3600
  "Minimal timedelay between successive mtime recordings in seconds.
Only does something when `org-roam-timestamps-remember-timestamps' is t.
Defaults to one hour."
  :group 'org-roam-timestamps
  :type 'number)

;;;###autoload
(define-minor-mode org-roam-timestamps-mode
  "Automatically add creation and modification timestamps to org-roam nodes."
  :global t
  :group 'org-roam-timestamps
  :lighter " org-roam-timestamps"
  :init-value nil
  (if org-roam-timestamps-mode
      (add-hook 'after-save-hook #'org-roam-timestamps--on-save)
    (remove-hook 'after-save-hook #'org-roam-timestamps--on-save)))

(defun org-roam-timestamps--on-save ()
  "Set the MTIME property of the current org-roam-node to the current time."
  (when (org-roam-buffer-p)
    (let* ((node (org-roam-populate (org-roam-node-create :id (org-roam-id-at-point))))
           (file (org-roam-node-file node))
           (mtime (org-roam-timestamps--get-mtime node)))
      (when org-roam-timestamps-timestamp-parent-file
        (org-roam-with-file file nil
          (save-excursion
            (goto-char (buffer-end -1))
            (let* ((pnode (org-roam-populate (org-roam-node-create :id (org-roam-id-at-point))))
                   (pfile (org-roam-node-file node))
                   (pmtime (org-roam-timestamps--get-mtime node)))
              (unless org-roam-timestamps-remember-timestamps
                (org-roam-timestamps--remove-current-mtime))
              (org-roam-timestamps--add-mtime pmtime)))))
      (unless org-roam-timestamps-remember-timestamps
        (org-roam-timestamps--remove-current-mtime))
      (org-roam-timestamps--add-mtime mtime))))

(defun org-roam-timestamps--add-mtime (&optional mtime)
  "Add the current time MTIME to the node.
Optionally checks the minimum time interval you want between mod times."
  (let ((curr (org-roam-timestamps-decode (current-time))))
    (if (and org-roam-timestamps-remember-timestamps mtime)
        (when (> (org-roam-timestamps-subtract curr mtime t) org-roam-timestamps-minimum-gap)
          (org-roam-add-property (org-roam-timestamps-decode (current-time)) "mtime")
          (save-buffer))
      (org-roam-add-property curr "mtime")
      (save-buffer))))

(defun org-roam-timestamps--get-mtime (node)
  "Get the mtime of the org-roam node NODE."
  (assoc-default "MTIME" (org-roam-node-properties
                          node)))

(defun org-roam-timestamps--remove-current-mtime ()
  "Remove the timestamps for the node at the current point."
  (if-let ((mtime (org-roam-timestamps--get-mtime
                   ((org-roam-populate (org-roam-node-create :id (org-roam-id-at-point))))))
           (org-roam-remove-property "mtime" mtime))))

(defun org-roam-timestamps-decode (mtime)
  "Decode a list of seconds since 1970 MTIME to an org-roam-timestamp."
  (let ((time (decode-time mtime))
        dec-time
        el)
    (dotimes (i 6 )
      (setq el (number-to-string (nth i time)))
      (when (length= el 1) (setq el (concat "0" el)))
      (setq dec-time
            (concat el dec-time)))
    dec-time))

(defun org-roam-timestamps-encode (mtime)
  "Encode the current YYYYMMDDHHMMSS MTIME string to an Emacs format."
  (encode-time `(,(string-to-number (substring mtime 12 14))
                 ,(string-to-number (substring mtime 10 12))
                 ,(string-to-number (substring mtime 8 10))
                 ,(string-to-number (substring mtime 6 8))
                 ,(string-to-number (substring mtime 4 6))
                 ,(string-to-number (substring mtime 0 4))
                 nil -1 nil)))

(defun org-roam-timestamps-subtract (t1 t2 &optional abs)
  "Return the difference between two timestamps T1 and T2, as a time value.
If ABS is non-nil, return the absolute value."
  (let ((time
         (subtract-time (org-roam-timestamps-encode t1) (org-roam-timestamps-encode t2))))
    (if abs
        (abs time)
      time)))


(defun org-roam-timestamps-all ()
  "Go through all nodes and add timestamps to them."
  (interactive)
  (when (yes-or-no-p "This will modify all your current notes by adding a ctime and mtime property
to all property drawers. We will make a backup of your notes and db first.
This might take a second. Are you sure you want to continue?")
    (let ((backup-dir (expand-file-name "org-roam-timestamp.bak"
                                        (file-name-directory (directory-file-name org-roam-directory))))
          (backup-db (expand-file-name "org-roam-db.bak" (file-name-directory org-roam-db-location))))
      (message "Backing up files to %s" backup-dir)
      (copy-directory org-roam-directory backup-dir)
      (message "Backing up db to %s" backup-db)
      (copy-file org-roam-db-location backup-db))
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
