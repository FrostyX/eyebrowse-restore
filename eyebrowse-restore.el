(defcustom eyebrowse-restore-dir
  (concat user-emacs-directory "eyebrowse-restore")
  "TODO"
  :type 'directory)

(defcustom eyebrowse-restore-save-interval 300
  "TODO"
  :type 'number)


(defun eyebrowse-restore-save-window-configs ()
  (interactive)
  (make-directory eyebrowse-restore-dir t)
  (dolist (frame (frame-list))
    (eyebrowse-restore-save-window-config frame)))


(defun eyebrowse-restore-save-window-config (frame)
  (let* ((name (frame-parameter frame 'name))
         (path (concat (file-name-as-directory eyebrowse-restore-dir) name))
         (window-configs (eyebrowse--get 'window-configs frame)))

    (with-temp-file path
      (prin1 window-configs (current-buffer))))
  (eyebrowse-restore--remove-unused-backups))


(defun eyebrowse-restore-restore ()
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
  (seq-filter
   (lambda (x)
     (not (member x '("." ".."))))
   (directory-files eyebrowse-restore-dir)))



(defun eyebrowse-restore-unused-backup-p (name)
  (not (member
        name
        (mapcar (lambda (x) (frame-parameter x 'name))
                (frame-list)))))


(defun eyebrowse-restore--remove-unused-backups ()
  (dolist (name (eyebrowse-restore--list-backups))
    (if (eyebrowse-restore-unused-backup-p name)
        (delete-file (concat (file-name-as-directory eyebrowse-restore-dir) name)))))



(add-to-list 'delete-frame-functions #'eyebrowse-restore-save-window-config)
(run-at-time 0 eyebrowse-restore-save-interval #'eyebrowse-restore-save-window-configs)



;; Just scratch, drop this ...
(eyebrowse-restore-save-window-configs)
(eyebrowse-restore-restore)

(frame-parameter nil 'name)
(set-frame-parameter (car (frame-list)) 'name "PIM")
(set-frame-parameter nil 'name "Main")
(set-frame-parameter nil 'name "Test-3")
(set-frame-parameter nil 'name "Test-4")
