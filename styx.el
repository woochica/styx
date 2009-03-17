(defcustom styx/restore-file "~/.styx-restore"
  "Path of restore file containing saved buffer points."
  :type 'file)

(defvar styx/buffer-positions nil
  "List of saved buffer points.")

(defun stxy/read-buffer-positions ()
  "Read buffer positions from `styx/restore-file'.

Once file was read, save it in `styx/buffer-positions' and return
it when future calls."
  (or
   styx/buffer-positions
   (let ((db (find-file-noselect styx/restore-file)))
     (if (= (buffer-size db) 0)
         ()
       (read db)))))

(defun styx/save-buffer-position ()
  "Save point of current buffer."
  (let* ((db (stxy/read-buffer-positions))
         (file (buffer-file-name (current-buffer))))
    (if (or (not file) (string= file styx/restore-file))
        ()
      (if (assoc file db)
          (setf (cdr (assoc file db)) (point))
        (add-to-list 'db (cons file (point))))
      (setq styx/buffer-positions db)
      (styx/write-buffer-positions))))

(defun styx/write-buffer-positions ()
  "Write value of `styx/buffer-positions' to `styx/restore-file'."
  (with-temp-file styx/restore-file
    (insert (format "%S" styx/buffer-positions))))

(defun styx/restore-buffer-position ()
  "Restore point in current buffer."
  (let* ((buff (current-buffer))
         (db (stxy/read-buffer-positions))
         (node (assoc (expand-file-name (buffer-file-name buff)) db)))
    (when node
      (switch-to-buffer buff)
      (goto-char (cdr node)))))

(add-hook 'find-file-hook 'styx/restore-buffer-position)
(add-hook 'kill-buffer-hook 'styx/save-buffer-position)
