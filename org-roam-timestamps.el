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
;; Package-Requires: ((emacs "26.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

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


(defun org-roam-timestamps-all ()
  "Go through all nodes and add timestamps to them."
  (message "stampin"))

(provide 'org-roam-timestamps)
;;; org-roam-timestamps.el ends here
