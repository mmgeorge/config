;;(add-to-list 'load-path "~/.emacs.d/lisp/slime")
(add-to-list 'load-path "~/.emacs.d/lisp/sly")
(add-to-list 'load-path "~/.emacs.d/lisp/sly-asdf")

(require 'package)
(require 'cl-lib)

(setq package-list
      '(use-package
         lsp-mode
        helm
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

        ;; sly
        ;; lisp
        ;; slime
        ;; slime-company
        ))

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(setq load-prefer-newer t)

;; Fetch list of available packages
(unless package-archive-contents
  (package-refresh-contents))


;; Install missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))


;;(setq show-paren-delay 0)
(show-paren-mode 1)


;; flymake

;; (use-package flymake-diagnostic-at-point
;;   :after flymake
;;   :config
;;     (add-hook 'flymake-mode-hook #'flymake-diagnostic-at-point-mode))

(setq confirm-kill-processes nil)


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

;;;; ELisp

(defun elisp-bindings ()
  (define-key emacs-lisp-mode-map (kbd "M-.") 'xref-find-definitions)
  (define-key emacs-lisp-mode-map (kbd "M-,") 'xref-pop-marker-stack))

(add-hook 'emacs-lisp-mode-hook 'elisp-bindings)


;;;; Common Lisp

;; When using local copy of sly
(require 'sly-autoloads)
(require 'sly-asdf)

(add-to-list 'sly-contribs 'sly-asdf 'append)

(setq sly-asdf-enable-experimental-syntax-checking t)

;; End local sly


(setq inferior-lisp-program "sbcl")

(add-hook 'sly-mrepl-mode-hook 'sly-repl-bindings)
(add-hook 'sly-mode-hook 'sly-bindings)


(defun find-current-system ()
  "Find the name of the current asd system."
  (let ((system-file (find-system-file default-directory)))
    (when system-file
      (file-name-base system-file))))


(defun find-system-file (directory)
  "Find the first file in the current DIRECTORY or a parent of DIRECTORY that includes a .asd file."
  (let ((fname (directory-file-name directory)))
    (or
     (cl-find-if #'(lambda (file) (string-equal "asd" (file-name-extension file)))
                 (directory-files directory))
     (and (file-name-directory fname) 
          (find-system-file (file-name-directory fname))))))


(defun sly-reload-project ()
  (interactive)
                                        ;(sly-mrepl--eval-for-repl `(asdf:load-system ,(find-current-system))))
  (sly-asdf-load-system (find-current-system)))


(defun sly-repl-bindings ()
  (define-key sly-mrepl-mode-map (kbd "C-p c") 'sly-reload-project)
  (define-key sly-mrepl-mode-map (kbd "C-c l") 'sly-mrepl-sync)
  (define-key sly-mrepl-mode-map (kbd "M-z") 'sly-mrepl-clear-repl))


(defun sly-bindings ()
  (define-key sly-mode-map (kbd "C-p c") 'sly-reload-project)
  (define-key sly-mode-map (kbd "C-c l") 'sly-mrepl-sync)
  (define-key sly-mode-map (kbd "M-z") 'sly-mrepl-clear-repl))


;; (defun slime-reload-project ()
;;   (interactive)
;;   (message "hello project")
;;   (slime-reload-system (find-current-system)))

;; (defun slime-bindings ()
;;   (define-key slime-repl-mode-map (kbd "C-p c") 'slime-reload-project))

;; ;; (add-hook 'slime-repl-mode-hook 'slime-bindings)


;; (require 'slime-autoloads)
;; (setq slime-contribs '(slime-fancy slime-asdf slime-cl-indent))
;; ;;(setq slime-kill-without-query-p t)

;; (defun slime-repl-hook ()
;;   (define-key sly-mode-map (kbd "C-p c") 'slime-reload-project)
;;   (define-key slime-repl-mode-map (kbd "M-s") nil)
;;   (define-key slime-repl-mode-map (kbd "M-z") 'slime-repl-clear-buffer))

;; (add-hook 'slime-repl-mode-hook 'slime-repl-hook)
;;(add-hook 'lisp-mode-hook 'slime-hook)

                                        ;(slime-setup '(slime-company))


;;(global-set-key (kbd "C-p c") 'reload-project)
                                        ; (global-set-key (kbd "M-r") 'slime-compile-region)

;;(defun slime-hook ()
;;(run-with-idle-timer 0.25 nil (lambda ()
;;(slime)
;;(other-window 1)
;;(slime-load-system (project-system-name))
;;)))

(setq display-buffer-alist
      '(("\\*inferior-lisp\\*" display-buffer-below-selected (window-height . 15)   

         (nil))))



(defface decorator-default-face
  '((t ( :foreground "color-230")))
  "Face for `decorators'.")

(defvar decorator-face 'decorator-default-face)

(defun clx-region-for-defun-at-point ()
  (let ((start (point)))
    (save-excursion
      (save-match-data
        (end-of-defun)
        (let ((end (point))
              (start-defun (progn (beginning-of-defun) (point))))
          (list (if (< start start-defun) start start-defun)
                end))))))


(defun clx-compile-defun ()
  (interactive)
  (apply #'slime-compile-region (clx-region-for-defun-at-point)))


(defconst clx-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?[ "(" table)
                         (modify-syntax-entry ?] ")" table)
    table))

(setq clx-locks
                                        ; Should probably refactor this...
      '(("(\\(local-nicknames\\)" (1 font-lock-keyword-face)) ;; Don't highlight the import
        ( "#\\[[^][]*\\]" . decorator-face ) 
        ;; Match @dtype(([TYPE]*) [TYPE]) ;;\|\(\\(&[a-zA-Z0-9\s_-]*\\)\)
        ( "#.*\(\s*dtype\s*\(\\([\)\(&:a-zA-Z0-9\s_-]*\\)\)\\([a-zA-Z0-9\s\(\)_-]*\\)?\).*"
          (0 decorator-face)
          (1 font-lock-type-face prepend)
          (2 font-lock-type-face prepend))

        ( "#.*\(\s*dtype.*\\(&[a-zA-Z0-9\s_-]*\\) \\(\(\\(:[a-zA-Z0-9_-]*\\)\\([a-zA-Z0-9\s_-]*\\)\)\\).*"
          (1 decorator-face prepend) ;; Highlight & token
          (2 decorator-face prepend) ;; Highlight keywords
          (3 decorator-face prepend)
          (4 font-lock-type-face prepend))

        ;; ( "#.*\(\s*dtype.*\\(&[a-zA-Z0-9_-]*\\).*"
        ;;   (1 font-lock-builtin-face prepend)
        ;; )

        ))



(define-derived-mode clx-mode lisp-mode "clx mode" 
  (set-syntax-table clx-mode-syntax-table)
                                        ;(substitute-key-definition 'slime-compile-defun 'clx-compile-defun clx-mode-map)
                                        ;(use-local-map clx-mode-map)
  (define-key slime-mode-indirect-map (kbd "C-c C-c") 'clx-compile-defun)
  
  (font-lock-add-keywords lisp-mode clx-locks))

;;(setq-default lisp-mode 'clx-mode)
                                        ;(add-to-list 'auto-mode-alist '("\\.lisp\\'" . clx-mode))


;; (defun clx-mode-decorator-matcher (limit)
;;   (let (res)
;;     (while
;;         (and (setq res (re-search-forward "\(\\([a-zA-Z0-9\s_-]+\\)\)" limit t))
;;              (not (eql (syntax-ppss-context (syntax-ppss)) 'comment)) ;; Continue, unless in a comment

;;              ))
;;     res))


;; (defun clx-mode-decorator-matcher (limit)
;;   "Match @dtype((MATCH+) [ret-type]) group 0 is the entire construct, 1 the symbol."
;;   (let (res)
;;     (while
;;         (setq res (re-search-forward "\)\\([a-zA-Z0-9\s_-]+\\)\)" limit t)))
;;     res))

;; (defun clx-mode-decorator-matcher-2 (limit)
;;   "Match @dtype(([arg-type]+) MATCH) group 0 is the entire construct, 1 the symbol."
;;   (let (res)
;;     (while
;;         (and (setq res (re-search-forward "\)\\([a-zA-Z0-9\s_-]+\\)\)" limit t))
;;              (not (nth 4 (syntax-ppss)))))
;;     res))



;; elpy
(setq use-package-always-demand t)
(setq use-package-always-ensure t)
(setq lsp-enable-snippet nil)

(use-package lsp-mode
  :ensure t
  :hook (python-mode . lsp)
  :commands lsp)

(use-package lsp-ui
  ;:bind (("M-." . lsp-ui-peek-find-references)
                                        ;("M-," . lsp-ui-peek-find-definitions))
  :commands lsp-ui-mode
  )
(use-package company-lsp :commands company-lsp)
(use-package helm-lsp :commands helm-lsp-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

(define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
(define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)


;; js
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))


;; Enable helm-gtags-mode
;(add-hook 'c-mode-hook 'helm-gtags-mode)
;(add-hook 'c++-mode-hook 'helm-gtags-mode)
;(add-hook 'asm-mode-hook 'helm-gtags-mode)

;(global-set-key (kbd "M-.") 'helm-gtags-dwim)
;(global-set-key (kbd "M-,") 'helm-gtags-find-tag)

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
