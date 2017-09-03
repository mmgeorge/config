

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


;;; Code:
;; cua-mode
(cua-mode t)
(setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
(setq cua-keep-region-after-copy t) ;; Standard Windows behaviour
(transient-mark-mode 1)


;; ido-mode
(run-with-idle-timer 0 nil (lambda () (ido-mode t)))
(setq ido-separator "\n")
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point 'guess)
(setq ido-ignore-files '(".exe" ".bat"))


;; dired
(progn
  (require 'dired)
  (define-key dired-mode-map (kbd "a") (lambda()(interactive)(find-alternate-file "..")))
  (define-key dired-mode-map (kbd "d") 'dired-find-alternate-file)
  (define-key dired-mode-map (kbd "w") 'dired-previous-line)
  (define-key dired-mode-map (kbd "s") 'dired-next-line))

(put 'dired-find-alternate-file 'disabled nil)
(put 'erase-buffer 'disabled nil)


