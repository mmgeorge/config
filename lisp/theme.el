
;;------------------------------------------------------------------------------------
;; Visual modes
;;------------------------------------------------------------------------------------

(setq visible-bell 1)

(menu-bar-mode -1)

;;------------------------------------------------------------------------------------
;; Modeline
;;------------------------------------------------------------------------------------

(defvar *path-length 8)

(defun shorten-path-when-long ()
  ;; Displays only the last 5 elements of the path
  (let ((dir-list (split-string default-directory "/")))
    (if (> (length dir-list) *path-length)
        (mapconcat 'identity
                   (cons "{..}"
                         (nthcdr (- (length dir-list) *path-length)
                                 (split-string default-directory "/"))) "/")
      default-directory)))


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
  ;; 'lsp-modeline--code-actions-string
  'global-mode-string
  ;; 'lsp-modeline--diagnostics-string

  ;; Fill until the end of line but 10 characters
  (mode-line-fill 'mode-line 10)
  "L/C: %l/%c"
  ))

;;------------------------------------------------------------------------------------
;; Code
;;------------------------------------------------------------------------------------

(setq-default tab-width 4)

(setq-default indent-tabs-mode nil)

;;------------------------------------------------------------------------------------
;; Faces
;;------------------------------------------------------------------------------------

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "unspecified-bg" :foreground "brightwhite" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 1 :width normal :foundry "default" :family "default"))))
 '(flycheck-error ((t (:foreground "brightred" :underline "brightred"))))
 '(font-lock-function-name-face ((t (:foreground "deeppink2"))))
 '(font-lock-keyword-face ((t (:foreground "lightgoldenrod2"))))
 '(font-lock-string-face ((t (:foreground "deepskyblue1"))))
 '(font-lock-type-face ((t (:foreground "springgreen1"))))
 '(font-lock-comment-face ((t (:foreground "snow3"))))

'(flymake-error ((t (:foreground "brightred" :underline t))))
'(haskell-error-face ((t (:foreground "brightred" :underline t))))

 '(typescript-jsdoc-tag ((t (:foreground "lightgoldenrod2"))))
 '(typescript-jsdoc-value ((t (:foreground "ivory1"))))
 '(font-lock-variable-name-face ((t (:foreground "deeppink2"))))

 `(company-tooltip ((t (:foreground "brightwhite" :background "gray13"))))
 `(company-tooltip-selection ((t (:background "gray24"))))
 `(company-tooltip-common ((t (:foreground "brightwhite"))))
 `(company-tooltip-common-selection ((t (:foreground "brightwhite"))))

 '(popup-tip-face ((t (:foreground "brightwhite" :background "gray13"))))

 ;;`(rustic-errno-face ((t (:foreground "deeppink2"))))
 ;;`(rustic-compilation-warning ((t (:foreground "deeppink2"))))
 ;;`(rustic-compilation-error-face ((t (:inherit compilation-error-face))))
 ;;`(rustic-compilation-error ((t (:inherit compilation-error))))
 ;;`(rustic-compilation-warning-face ((t (:inherit compilation-warning-face))))
 ;;`(rustic-compilation-warning ((t (:inherit compilation-warning))))
 
 `(company-tooltip-annotation ((t (:foreground "brightwhite"))))
 `(company-tooltip-annotation-selection ((t (:foreground "brightwhite"))))
 `(company-scrollbar-fg ((t (:background "brightwhite"))))
 `(company-scrollbar-bg ((t (:background "gray"))))

 `(helm-ff-file-extension ((t (:foreground "deepskyblue1"))))
 `(helm-ff-file ((t (:foreground "deepskyblue1"))))
 `(helm-ff-directory ((t (:foreground "brightwhite"))))
 `(helm-buffer-file ((t (:foreground "deepskyblue1"))))
 `(helm-buffer-directory ((t (:foreground "brightwhite"))))
 `(helm-buffer-process ((t (:foreground "deepskyblue1"))))
 `(helm-selection ((t (:background "gray24"))))
 `(helm-source-header ((t (:foreground "black" :background "gray72"))))
 `(helm-M-x-key ((t (:foreground "deepskyblue1")))))

