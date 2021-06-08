(define-key input-decode-map "\e\eOA" [(meta up)])
(define-key input-decode-map "\e\eOB" [(meta down)])

;;; Code:
;; Hotkey rebinds
(global-set-key (kbd "M-1") 'helm-find-files)
(global-set-key (kbd "M-2") 'switch-to-buffer)
(global-set-key (kbd "C-<tab>") 'other-window)
(global-set-key (kbd "M-o") 'other-window)

(global-set-key (kbd "C-t") 'next-error)

;;(global-set-key (kbd "C-v") 'yank)
(global-set-key (kbd "M-q") 'back-to-indentation)
(global-set-key (kbd "M-q") 'back-to-indentation)
(global-set-key (kbd "M-q") 'back-to-indentation)
(global-set-key (kbd "M-r") 'move-end-of-line)
(global-set-key (kbd "M-r") 'move-end-of-line)
(global-set-key (kbd "C-h") 'kill-whole-line)
(global-set-key (kbd "M-h") 'kill-word)
(global-set-key (kbd "M-u") 'kill-ring-save)
(global-set-key (kbd "M-y") 'cua-paste)
(global-set-key (kbd "C-u") 'kill-region)
;;(global-set-key (kbd "M-z") 'undo)
(global-set-key (kbd "M-c") 'company-complete)
  
;;(global-set-key (kbd "M-l") 'View-scroll-half-page-forward) 
;;(global-set-key (kbd "M-k") 'View-scroll-half-page-backward)   

(global-set-key (kbd "M-l") 'View-scroll-half-page-forward) 
(global-set-key (kbd "M-k") 'View-scroll-half-page-backward)   
;(global-set-key (kbd "M-k") 'previous-line)
;(global-set-key (kbd "M-l") 'next-line)

(global-set-key (kbd "C-;") 'forward-word)
(global-set-key (kbd "C-:") 'forward-word)
;(global-set-key (kbd "C-s") 'backward-word)
;(global-set-key (kbd "C-k") 'View-scroll-half-page-backward)
;(global-set-key (kbd "C-l") 'View-scroll-half-page-forward)
;;(global-set-key (kbd "M-4") 'delete-other-windows)

(global-set-key (kbd "C-x )") 'delete-window)

(global-set-key (kbd "M-3") 'kill-buffer)
(global-set-key (kbd "M-s") 'previous-line)
(global-set-key (kbd "M-d") 'next-line)
(global-set-key (kbd "M-f") 'forward-word)
;;(global-set-key (kbd "C-d") 'forward-char)
(global-set-key (kbd "M-a") 'backward-word)
(global-set-key (kbd "C-c v") 'erase-buffer)
(global-set-key (kbd "C-x f") 'set-mark-command)
(global-set-key (kbd "M-<down>") 'shrink-window)
(global-set-key (kbd "M-<up>") 'enlarge-window)

(global-set-key (kbd "C-k") 'shrink-window)
(global-set-key (kbd "C-l") 'enlarge-window)

(global-set-key (kbd "M-;") 'forward-char)
(global-set-key (kbd "M-:") 'forward-char)
(global-set-key (kbd "M-j") 'backward-char)

;(define-key dired-mode-map (kbd "w") 'dired-previous-line)
;(define-key dired-mode-map (kbd "s") 'dired-next-line)



(provide 'emacs-commands)
;;; emacs-commands ends here

