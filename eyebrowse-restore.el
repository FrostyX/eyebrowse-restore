(defcustom eyebrowse-restore-dir
  (concat user-emacs-directory "eyebrowse-restore")
  "Path to the directory where to store Eyebrowse window
configurations."
  :type 'directory)

(defcustom eyebrowse-restore-save-interval 300
  "How often (in seconds) to save all Eyebrowse window
configurations."
  :type 'number)


(defun eyebrowse-restore-save-all ()
  "Save the Eyebrowse window configurations for all frames"
  (interactive)
  (make-directory eyebrowse-restore-dir t)
  (dolist (frame (frame-list))
    (eyebrowse-restore-save frame)))


(defun eyebrowse-restore-save (frame)
  "Save the Eyebrowse window configurations for the current
frame."
  (let* ((name (frame-parameter frame 'name))
         (path (concat (file-name-as-directory eyebrowse-restore-dir) name))
         (window-configs (eyebrowse--get 'window-configs frame)))

    (with-temp-file path
      (prin1 window-configs (current-buffer))))
  (eyebrowse-restore--remove-unused-backups))


(defun eyebrowse-restore-restore ()
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


(defun eyebrowse-restore--list-backups ()
  "List all files stored in the `eyebrowse-restore-dir'
directory."
  (seq-filter
   (lambda (x)
     (not (member x '("." ".."))))
   (directory-files eyebrowse-restore-dir)))


(defun eyebrowse-restore--unused-backup-p (name)
  "Return `t' if there isn't any frame with this `name'."
  (not (member
        name
        (mapcar (lambda (x) (frame-parameter x 'name))
                (frame-list)))))


(defun eyebrowse-restore--remove-unused-backups ()
  "Remove all files from the `eyebrowse-restore-dir' that
doesn't correspond with any of the active frames."
  (dolist (name (eyebrowse-restore--list-backups))
    (if (eyebrowse-restore--unused-backup-p name)
        (delete-file (concat (file-name-as-directory eyebrowse-restore-dir) name)))))



(add-to-list 'delete-frame-functions #'eyebrowse-restore-save)
(run-at-time 0 eyebrowse-restore-save-interval #'eyebrowse-restore-save-all)



;; Just scratch, drop this ...
(eyebrowse-restore-save-all)
(eyebrowse-restore-restore)

(frame-parameter nil 'name)
(set-frame-parameter (car (frame-list)) 'name "PIM")
(set-frame-parameter nil 'name "Main")
(set-frame-parameter nil 'name "Test-3")
(set-frame-parameter nil 'name "Test-4")
