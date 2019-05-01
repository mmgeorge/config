(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "unspecified-bg" :foreground "brightwhite" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 1 :width normal :foundry "default" :family "default"))))
 '(flycheck-error ((t (:foreground "brightred" :underline "brightred"))))
 '(font-lock-comment-face ((t (:foreground "color-248"))))
 '(font-lock-function-name-face ((t (:foreground "color-197"))))
 '(font-lock-keyword-face ((t (:foreground "color-221"))))
 '(font-lock-string-face ((t (:foreground "color-39"))))
 '(font-lock-variable-name-face ((t (:foreground "color-197")))))


;; Colors
;; (custom-set-variables
;;  '(ansi-color-faces-vector
;;    [default default default italic underline success warning error])
;;  '(custom-enabled-themes (quote (wombat)))
;;  '(inhibit-startup-screen t)
;;  '(safe-local-variable-values (quote ((Package . LISP-UNIT)))))

;; (custom-set-faces
;;  '(custom-variable-tag ((t (:foreground "old lace" :weight bold))))
;;  '(font-lock-keyword-face ((t (:foreground "gold" :weight bold))))
;;  '(py-def-class-face ((t (:inherit font-lock-keyword-face :foreground "sandy brown"))))
;;  '(variable-pitch ((t (:foreground "sky blue" :family "Sans Serif")))))

;; (setq initial-frame-alist
;;       `((background-color . ,(face-background 'default))
;;         (foreground-color . ,(face-foreground 'default))
;;         (horizontal-scroll-bars . nil)
;;         (top . 50)    
;;         (left . 550)       
;;         (height . 58);)
;;         (cursor-color . "white")
;;         (mouse-color . "white")))


(setq visible-bell 1)
;;(set-face-attribute 'default nil :height 100)
(menu-bar-mode -1)
;;(set-face-attribute 'region nil :background "#766")

;; Linum-mode
(global-linum-mode t)
(setq linum-format "%d ")


;; (defun simple-mode-line-render (left right)
;;     "Return a string of `window-width' length containing LEFT, and RIGHT
;;  aligned respectively."
;;     (let* ((available-width (- (window-width) (length left) 2)))
;;       (format (format " %%s %%%ds " available-width) left right)))

;; (setq mode-line-format
;;       '((:eval (simple-mode-line-render
;;                 ;; left
;;                 (format-mode-line "%b [%m] [%*]")
;;                 ;; right
;;                 (format-mode-line "Line: %l/%i Column: %c")))))


(defvar *path-length 5)
(defun shorten-path-when-long ()
  ;; Displays only the last 5 elements of the path
  (let ((dir-list (split-string default-directory "/")))
    (if (> (length dir-list) *path-length)
        (mapconcat 'identity
                   (cons "{..}"
                         (nthcdr (- (length dir-list) *path-length)
                                 (split-string default-directory "/"))) "/")
      default-directory)))


;; (defun shorten-path-when-long2 ()
;;     (if (> (length default-directory) 35)
;;         (mapconcat 'identity
;;                    (cons "{..}"
;;                          (cddr (cddr (split-string default-directory "/")))) "/")
;;       default-directory))

(defun mode-line-fill (face reserve)
  "Return empty space using FACE and leaving RESERVE space on the right."
  (unless reserve
    (setq reserve 20))
  (when (and window-system (eq 'right (get-scroll-bar-mode)))
    (setq reserve (- reserve 3)))
  (propertize " "
              'display `((space :align-to (- (+ right right-fringe right-margin) ,reserve)))
              'face face))


;; Set the modeline to tell me the filename, hostname, etc..
(setq-default
 mode-line-format
 (list
  " "
  'mode-line-modified
  'mode-line-buffer-identification
  '(:eval (propertize "<%m>" 'help-echo buffer-file-coding-system))
     ""
  '(:eval (when buffer-read-only
            (concat  (propertize "(RO)"
                                 'help-echo "Buffer is read-only"))))
  ": "
  (shorten-path-when-long)
  ;;default-directory
  
  ;; Fill until the end of line but 10 characters
  (mode-line-fill 'mode-line 10)
  "L/C: %l/%c"
  ))


;; Code
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)


;; TABS FORMATING

;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(ansi-color-faces-vector
;;    [default default default italic underline success warning error])
;;  '(custom-enabled-themes (quote (wombat)))
;;  '(inhibit-startup-screen t)
;;  '(safe-local-variable-values (quote ((Package . LISP-UNIT)))))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(cua-rectangle-noselect ((t (:background "dimgray" :foreground "white"))))
;;  '(custom-variable-tag ((t (:foreground "old lace" :weight bold))))
;;  '(flycheck-warning ((t (:underline (:color "red" :style wave)))))
;;  '(font-lock-function-name-face ((t (:foreground "color-24"))))
;;  '(font-lock-keyword-face ((t (:foreground "gold" :weight bold))))
;;  '(font-lock-string-face ((t (:foreground "color-98"))))
;;  '(font-lock-type-face ((t (:foreground "red" :weight bold))))
;;  '(helm-selection ((t (:background "color-66" :distant-foreground "color-18"))))
;;  '(linum ((t (:inherit (shadow default) :foreground "color-102"))))
;;  '(mode-line ((t (:background "red" :foreground "blue"))))
;;  '(mode-line-buffer-id ((t (:foreground "color-16"))))
;;  '(py-def-class-face ((t (:inherit font-lock-keyword-face :foreground "sandy brown"))))
;;  '(variable-pitch ((t (:foreground "sky blue" :family "Sans Serif")))))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "unspecified-bg" :foreground "brightwhite" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 1 :width normal :foundry "default" :family "default"))))
 '(flycheck-error ((t (:foreground "brightred" :underline "brightred"))))
 '(font-lock-function-name-face ((t (:foreground "color-197"))))
 '(font-lock-keyword-face ((t (:foreground "color-221"))))
 '(font-lock-string-face ((t (:foreground "color-39"))))
 '(font-lock-variable-name-face ((t (:foreground "color-197")))))






;;; emacs24-root ends here
