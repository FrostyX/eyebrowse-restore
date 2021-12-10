;;; eyebrowse-restore.el --- Persistent Eyebrowse for all frames   -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Jakub Kadlčík

;; Author: Jakub Kadlčík <frostyx@email.cz>
;; URL: https://github.com/FrostyX/eyebrowse-restore
;; Version: 0.1-pre
;; Package-Requires: ((emacs "26.3"))
;; Keywords: eyebrowse, helm, persistent

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Never lose your Eyebrowse window configurations again


;;; Code:

;;;; Customization

(defcustom eyebrowse-restore-dir
  (concat user-emacs-directory "eyebrowse-restore")
  "Path to the directory where to store Eyebrowse window
configurations."
  :type 'directory)

(defcustom eyebrowse-restore-save-interval 300
  "How often (in seconds) to save all Eyebrowse window
configurations."
  :type 'number)

(defcustom eyebrowse-restore-keep-old-backups 10
  "How many old backups should we keep."
  :type 'integer)

;;;; Variables

(defvar eyebrowse-restore-timer nil
  "Timer to automatically save Eyebrowse window configurations")

;;;; Modes

(define-minor-mode eyebrowse-restore-mode
  "Toggle `eyebrowse-restore-mode'.
This global minor mode provides a timer to automatically
save Eyebrowse window configurations for all Emacs frames.
It also provides a hook to save window configuration for a
frame before closing it. "
  :global t
  (if eyebrowse-restore-mode
      (progn
        (add-to-list 'delete-frame-functions #'eyebrowse-restore-save)
        (setq eyebrowse-restore-timer
              (run-at-time 0 eyebrowse-restore-save-interval
                           #'eyebrowse-restore-save-all)))
    (progn
      (setq delete-frame-functions
            (delete #'eyebrowse-restore-save delete-frame-functions))
      (cancel-timer eyebrowse-restore-timer))))

;;;; Commands

;;;###autoload
(defun eyebrowse-restore-save-all ()
  "Save the Eyebrowse window configurations for all frames"
  (interactive)
  (make-directory eyebrowse-restore-dir t)
  (dolist (frame (frame-list))
    (eyebrowse-restore-save frame)))

;;;###autoload
(defun eyebrowse-restore-save (frame)
  "Save the Eyebrowse window configurations for the current
frame."
  (interactive)
  (let* ((name (frame-parameter frame 'name))
         (path (concat (file-name-as-directory eyebrowse-restore-dir) name))
         (window-configs (eyebrowse--get 'window-configs frame)))

    (with-temp-file path
      (prin1 window-configs (current-buffer))))
  (eyebrowse-restore--remove-unused-backups))

;;;###autoload
(defun eyebrowse-restore ()
  "Select a backup of an Eyebrowse window configurations and
apply them to the current frame.

Warning! The current Eyebrowse window configurations for the
active frame will be destroyed."
  (interactive)
  (let* ((name (completing-read
                "Eyebrowse backups: "
                (eyebrowse-restore--list-backups)))
         (path (concat (file-name-as-directory eyebrowse-restore-dir) name)))

    (with-temp-buffer
      (insert-file-contents path)
      (eyebrowse--set 'window-configs
        (read (buffer-string))))))

;;;; Functions

;;;;; Private

(defun eyebrowse-restore--list-backups ()
  "List all files stored in the `eyebrowse-restore-dir'
directory."
  (let* ((with-attrs (directory-files-and-attributes eyebrowse-restore-dir))
		 (sort-by-date #'(lambda (x y) (time-less-p (nth 5 y) (nth 5 x))))
		 (sorted (sort with-attrs sort-by-date))
		 (files (mapcar #'car sorted)))
	(seq-filter (lambda (x) (not (member x '("." "..")))) files)))

(defun eyebrowse-restore--list-unused-backups ()
  "Return a list of unused backups while respecting the
`eyebrowse-restore-keep-old-backups' number of unused
backups to keep."
  (-slice (seq-filter #'eyebrowse-restore--unused-backup-p
					  (eyebrowse-restore--list-backups))
		  eyebrowse-restore-keep-old-backups))

(defun eyebrowse-restore--unused-backup-p (name)
  "Return `t' if there isn't any frame with this `name'."
  (not (member
        name
        (mapcar (lambda (x) (frame-parameter x 'name))
                (frame-list)))))

(defun eyebrowse-restore--remove-unused-backups ()
  "Remove all files from the `eyebrowse-restore-dir' that
doesn't correspond with any of the active frames or is
older than `eyebrowse-restore-keep-old-backups'."
  (dolist (name (eyebrowse-restore--list-unused-backups))
    (delete-file (concat (file-name-as-directory eyebrowse-restore-dir) name))))

;;;; Footer

(provide 'eyebrowse-restore)

;;; eyebrowse-restore.el ends here
