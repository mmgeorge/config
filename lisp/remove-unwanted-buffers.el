
;;; Code:
  
(defun remove-scratch-buffer ()
  (if (get-buffer "*scratch*")
      (kill-buffer "*scratch*")))

(defun remove-unwanted-buffers ()
  (setq initial-scratch-message "")  ;; Makes *scratch* empty.
  (add-hook 'after-change-major-mode-hook 'remove-scratch-buffer)
  (setq-default message-log-max nil) ;; Removes *messages* from the buffer.
  (kill-buffer "*Messages*")
  (add-hook 'minibuffer-exit-hook ;; Removes *Completions* 
            '(lambda ()
               (let ((buffer "*Completions*"))
                 (and (get-buffer buffer)
                      (kill-buffer buffer)))))
  ;; Don't show *Buffer list* when opening multiple files at the same time.
  (setq inhibit-startup-buffer-menu t)
  ;; Show only one active window when opening multiple files at the same time.
  (add-hook 'window-setup-hook 'delete-other-windows)
  ;; No more typing the whole yes or no. Just y or n will do.
  (fset 'yes-or-no-p 'y-or-n-p))

