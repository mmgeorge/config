
;;------------------------------------------------------------------------------------
;; Performance
;;------------------------------------------------------------------------------------

(setq gc-cons-threshold 200000000) ;; Increase gc threshold
(setq read-process-output-max (* 2048 2048)) ;; Increase emacs process data read (for lsp)

;;------------------------------------------------------------------------------------
;; Misc
;;------------------------------------------------------------------------------------

(setq create-lockfiles nil) ;; for typescript/webpack errors
(setq confirm-kill-processes nil)
(setq backup-inhibited t)  ;; Disable backups

;;------------------------------------------------------------------------------------
;; Default modes
;;------------------------------------------------------------------------------------

;; Duplicate parens, etc
(electric-pair-mode 1)

;; Highlight match parens, etc
(show-paren-mode 1)

;; Enable code abbrevations. These are defined per-mode using `define-abbrev-table`.
;; See `haskell-mode-abbrev-table` for example usage
(setq-default abbrev-mode t)

;; Automatically keep modified buffers up to day (e.g., when modified by another program)
(auto-revert-mode 1)

(auto-encryption-mode 0)

(semantic-mode 0)

;; cua-mode
(setq cua-remap-control-v nil)

(cua-mode t)
(setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
(setq cua-keep-region-after-copy t) ;; Standard Windows behaviour
(transient-mark-mode 1)
(setq cua-highlight-region-shift-only t)

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

;; bookmarks
(setq bookmark-menu-confirm-deletion nil)

;;------------------------------------------------------------------------------------
;; Remove unwanted buffers
;;------------------------------------------------------------------------------------

(defun remove-scratch-buffer ()
  (if (get-buffer "*scratch*")
      (kill-buffer "*scratch*")))

(defun remove-unwanted-buffers ()
  (setq initial-scratch-message "")  ;; Makes *scratch* empty.
;;  (remove-scratch-buffer)
  (add-hook 'after-change-major-mode-hook 'remove-scratch-buffer)
  ;;(setq-default message-log-max nil) ;; Removes *messages* from the buffer.
  ;; (kill-buffer "*Messages*")
  ;; (add-hook 'minibuffer-exit-hook ;; Removes *Completions* 
  ;;           '(lambda ()
  ;;              (let ((buffer "*Completions*"))
  ;;                (and (get-buffer buffer)
  ;;                     (kill-buffer buffer)))))
  ;; Don't show *Buffer list* when opening multiple files at the same time.
  ;;(setq inhibit-startup-buffer-menu t)
  ;; Show only one active window when opening multiple files at the same time.
  (add-hook 'window-setup-hook 'delete-other-windows)
  ;; No more typing the whole yes or no. Just y or n will do.
  (fset 'yes-or-no-p 'y-or-n-p))

(remove-unwanted-buffers)

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



