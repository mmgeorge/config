
;;; Code:
(setq backup-inhibited t)  ;; Disable backups
(remove-unwanted-buffers)   ;; Remove unwanted buffers


;;; Helpers
(defun switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))


(defun bol-or-indentation()
  "move to beginning of line or indentation"
  (interactive
   (if (bolp)
       (back-to-indentation)
     (beginning-of-line))))


;;; emacs24-root ends here
