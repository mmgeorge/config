(define-key input-decode-map "\e\eOA" [(meta up)])
(define-key input-decode-map "\e\eOB" [(meta down)])

;;; Code:
;; Hotkey rebinds
(global-set-key (kbd "M-1") 'find-file)
(global-set-key (kbd "M-2") 'switch-to-buffer)
(global-set-key (kbd "C-<tab>") 'other-window)
(global-set-key (kbd "C-v") 'yank)
(global-set-key (kbd "M-q") 'back-to-indentation) 
(global-set-key (kbd "M-e") 'move-end-of-line)
(global-set-key (kbd "C-f") 'kill-whole-line)
(global-set-key (kbd "M-f") 'kill-word)

;;(global-set-key (kbd "M-l") 'View-scroll-half-page-forward) 
;;(global-set-key (kbd "M-k") 'View-scroll-half-page-backward)   

(global-set-key (kbd "M-o") 'View-scroll-half-page-forward) 
(global-set-key (kbd "M-i") 'View-scroll-half-page-backward)   
(global-set-key (kbd "M-k") 'previous-line)
(global-set-key (kbd "M-l") 'next-line)

(global-set-key (kbd "C-;") 'forward-word)
(global-set-key (kbd "C-:") 'forward-word)
(global-set-key (kbd "C-j") 'backward-word)
(global-set-key (kbd "C-k") 'View-scroll-half-page-backward)
(global-set-key (kbd "C-l") 'View-scroll-half-page-forward)


;;(global-set-key (kbd "M-4") 'delete-other-windows)
(global-set-key (kbd "M-3") 'kill-buffer)
(global-set-key (kbd "M-w") 'previous-line)
(global-set-key (kbd "M-s") 'next-line)
(global-set-key (kbd "M-d") 'forward-word)
;;(global-set-key (kbd "C-d") 'forward-char)
(global-set-key (kbd "M-a") 'backward-word)
(global-set-key (kbd "C-c v") 'erase-buffer)
(global-set-key (kbd "C-x f") 'set-mark-command)
(global-set-key (kbd "M-<down>") 'shrink-window)
(global-set-key (kbd "M-<up>") 'enlarge-window)

(global-set-key (kbd "M-;") 'forward-char)
(global-set-key (kbd "M-:") 'forward-char)
(global-set-key (kbd "M-j") 'backward-char)

;(define-key dired-mode-map (kbd "w") 'dired-previous-line)
;(define-key dired-mode-map (kbd "s") 'dired-next-line)



(provide 'emacs-commands)
;;; emacs-commands ends here

