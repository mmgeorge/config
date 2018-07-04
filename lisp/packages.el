(setq package-list
      '(helm
        helm-projectile
        flycheck
        company
        autopair
        markdown-mode

        ;; javascript
        ;;js2-mode
        ;;company-tern ;; requires tern installed (npm install -g tern)
        ;; npm install -g eslint babel-eslint eslint-plugin-react

        ;;tern
        ;;js2-refactor
        ;;rjsx-mode
        web-mode
        js2-mode
        
        ;; typescript
        tide
        typescript-mode

        
        ;; lisp
        slime
        slime-company
        ))


(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

;; Fetch list of available packages
(unless package-archive-contents
  (package-refresh-contents))


;; Install missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))



;; flycheck
(global-flycheck-mode)
(add-hook 'after-init-hook 'global-company-mode)
(global-set-key (kbd "C-e") 'company-complete)


(require 'helm-config)
(helm-mode 1)
(semantic-mode 1)
(global-semanticdb-minor-mode)
(setq helm-semantic-fuzzy-match t)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-2") 'helm-buffers-list)


(projectile-mode)
(require 'helm-projectile)
(helm-projectile-on)


(global-unset-key (kbd "C-p"))


(define-key projectile-mode-map (kbd "C-p g") 'helm-projectile-grep)
(define-key projectile-mode-map (kbd "C-p f") 'helm-projectile-find-file)

;; markdown-mode
(add-hook 'markdown-mode-hook
          (lambda ()
            (local-set-key (kbd "C-l l") 'markdown-insert-link) 
            (local-set-key (kbd "C-l p") 'markdown-live-preview-mode)))


;;(locate-dominating-file (cua--M/o) )

;; (defun find-system-name (asd-file)
;;   (with-temp-buffer
;;     (insert-file-contents (concat asd-file "system.asd"))
;;     (when (string-match "asdf:defsystem :\\(.*\\)" (buffer-string))
;;       (message (match-string 1 (buffer-string)))
;;       (match-string 1 (buffer-string))
;;     )))

(defun project-system-name ()
  (interactive)
  (file-name-base (car (directory-files (projectile-project-root) t "asd"))))


(defun reload-project ()
  (interactive)
  (message "hello project")
  (slime-reload-system (project-system-name)))

  ;; (let ((root (locate-dominating-file
  ;;                  (file-name-directory buffer-file-name)
  ;;                  (lambda (parent) (directory-files parent nil "\\(*.\\).asd")))))
  ;;   (if root
  ;;       (message root
  ;;        ;(car (directory-files root nil "asd"))
  ;;        ))))
        ;(find-system-name asd-file))))

;;slime
;; (defun start-slime ()
;;   (interactive)
;;   (if (string-equal "lisp" (file-name-extension buffer-file-name))
;;       (unless (slime-connected-p)
;;         (slime)
;;         )))


;;(setq shackle-rules '(:align 'below :size 0.2)))
;;(setq *project-path(projectile-project-pa) (projectile-project-info))


(require 'slime-autoloads)
;; This causes new packages not to be found!!!
;; Use this and bad thing will happen to you!
;; (setq slime-lisp-implementations
;;       '((sbcl ("sbcl" "--core" "/home/matt/config/lisp/sbcl.core-with-swank")
;;               :init (lambda (port-file _)
;;                       (format "(swank:start-server %S)\n" port-file))
;;               )))
(setq inferior-lisp-program "/usr/bin/sbcl")
(slime-setup '(slime-company))
(setq slime-contribs '(slime-fancy slime-asdf slime-cl-indent))
(setq slime-kill-without-query-p t)
                                        ;(start-slime)

                                        ;(run-with-idle-timer 0 nil (lambda () ))
                                        ;(run-with-idle-timer 1 nil (lambda () (slime-load-system (project-system-name))))
(global-set-key (kbd "C-p c") 'reload-project)

(defun slime-hook ()
  (run-with-idle-timer 0.25 nil (lambda ()
                                  (slime)
                                  (other-window 1)
                                 (slime-load-system (project-system-name))
                                 ))

 )

(setq display-buffer-alist
      '(("\\*inferior-lisp\\*" display-buffer-below-selected (window-height . 15)   

         (nil))))


(defun slime-repl-hook ()
  (define-key slime-repl-mode-map (kbd "M-s") nil)
  (define-key slime-repl-mode-map (kbd "M-z") 'slime-repl-clear-buffer))

(add-hook 'slime-repl-mode-hook 'slime-repl-hook)
;(add-hook 'slime-mode-hook 'slime-hook)
;(add-hook 'lisp-mode-hook 'slime-hook)

(add-hook 'lisp-mode-hook (lambda () (slime-hook)))

;; js2-mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))


;; Enable helm-gtags-mode
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

(global-set-key (kbd "M-.") 'helm-gtags-dwim)
(global-set-key (kbd "M-,") 'helm-gtags-find-tag)

;; C/C++
;; HideShow
;; (setq HS-MODE-HIDDEN nil)
;; (defun toggle-hide-all ()
;;   (interactive)
;;   (if HS-MODE-HIDDEN
;;       (progn (hs-show-all)
;;              (setq HS-MODE-HIDDEN nil))
;;     (progn (hs-hide-all)
;;            (setq HS-MODE-HIDDEN t))))

;; (setq hs-hide-comments-when-hiding-all nil)
;; (add-hook 'c-mode-hook 'hs-minor-mode)
;; (add-hook 'c++-mode-hook 'hs-minor-mode)
;; (global-set-key (kbd "M-k") 'hs-toggle-hiding)
;; (global-set-key (kbd "M-l") 'toggle-hide-all)

;; (defvar hs-special-modes-alist
;;   (mapcar 'purecopy
;;           '((c-mode "{" "}" "/[*/]" "#ifdef" "#endif" nil nil)
;;             (c++-mode "{" "}" "/[*/]" nil nil)
;;             (bibtex-mode ("@\\S(*\\(\\s(\\)" 1))
;;             (js-mode "{" "}" "/[*/]" nil)))")")


;; Set key bindings
;; (eval-after-load "helm-gtags"
;;   '(progn
;;      (define-key helm-gtags-mode-map (kbd "M-t") 'helm-gtags-find-tag)
;;      (define-key helm-gtags-mode-map (kbd "M-r") 'helm-gtags-find-rtag)
;;      (define-key helm-gtags-mode-map (kbd "M-s") 'helm-gtags-find-symbol)
;;      (define-key helm-gtags-mode-map (kbd "M-g M-p") 'helm-gtags-parse-file)
;;      (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
;;      (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
;;      (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)))

;;;; WEB Dev

;; web-mode
(require 'web-mode)
(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2))
(add-hook 'web-mode-hook  'my-web-mode-hook)

;; js2-mode
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
(setq js-indent-level 2)

;; typescript
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

(setq tide-format-options
      '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions
        t
        :placeOpenBraceOnNewLineForFunctions
        nil
        :InsertSpaceAfterOpeningAndBeforeClosingNonemptyBrackets t))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
;;(add-hook 'before-save-hook 'tide-format-before-save)
(setq typescript-indent-level 2)

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))
(add-hook 'typescript-mode-hook #'setup-tide-mode)


;;;; GLSL
(flycheck-define-checker glsl-lang-validator
  "A GLSL checker using glslangValidator.
   See URL https://www.khronos.org/opengles/sdk/tools/Reference-Compiler/"
  :command ("glslangValidator" source)
  :error-patterns
  ((error line-start "ERROR: " column ":" line ": " (message) line-end))
  :modes glsl-mode)
(add-to-list 'flycheck-checkers 'glsl-lang-validator)


;; autopair
(run-with-idle-timer 0 nil (lambda () (require 'autopair)))
(run-with-idle-timer 0 nil (lambda () (autopair-global-mode)))


;;; emacs-packages ends here
