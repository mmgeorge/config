(require 'package)
(require 'cl-lib)

(add-to-list
 'package-archives
 '("melpa" . "http://melpa.org/packages/")
 '("gnu" . "http://elpa.gnu.org/packages/"))

(setq package-list
      '(use-package
         ;;lsp-mode
         ;;lsp-haskell

         helm
         helm-projectile
         flycheck
         company
         ;; autopair
         markdown-mode
         web-mode
         tide
         typescript-mode
         s

         ;; rust
         ;;rustic
         ;;racer 

         sly
         ;;sly-asdf

         
         ;; lisp
         ;; slime
         ;; slime-company
         ;;paredit
         ))

(package-initialize)

;; Fetch list of available packages
;; (unless package-archive-contents
;;   (package-refresh-contents))


;; Install missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; elpy

;;(setq use-package-always-ensure t)

;; Performance

(setq gc-cons-threshold 800000) ;; Increase gc threshold
(setq read-process-output-max (* 2048 2048)) ;; Increase emacs process data read (for lsp)

;; Misc settings

(setq create-lockfiles nil) ;; for typescript/webpack errors
;;(setq load-prefer-newer t)
(setq confirm-kill-processes nil)


(electric-pair-mode 1)
(auto-revert-mode 1)
(show-paren-mode 1)
;;(auto-compression-mode 0)
(auto-encryption-mode 0)
(semantic-mode 0)

;; flycheck

;;(global-flycheck-mode)
(setq flycheck-check-syntax-automatically '(save mode-enable)) ;; editing is very slow otherwise
;;(setq flycheck-check-syntax-automatically '(save idle-change mode-enable)) ;; editing is very slow otherwise
(setq flycheck-idle-change-delay 4)

;; Autopair

;; (require 'autopair)
;; (autopair-global-mode)

;; company

(add-hook 'after-init-hook 'global-company-mode)
(global-set-key (kbd "C-e") 'company-complete)
;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; helm

(require 'helm-config)
(helm-mode 1)

(global-semanticdb-minor-mode)
(setq helm-semantic-fuzzy-match t)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-2") 'helm-buffers-list)

;; projectile

(projectile-mode)
(require 'helm-projectile)
(helm-projectile-on)
(global-unset-key (kbd "C-p"))
(define-key projectile-mode-map (kbd "C-p g") 'helm-projectile-grep)
(define-key projectile-mode-map (kbd "C-p f") 'helm-projectile-find-file)
(setq projectile-enable-caching t)

;; compilation-mode
(setq compilation-scroll-output t)
(setq compilation-scroll-output 'first-error)

;; markdown-mode

(add-hook 'markdown-mode-hook
          (lambda ()
            (local-set-key (kbd "C-l l") 'markdown-insert-link) 
            (local-set-key (kbd "C-l p") 'markdown-live-preview-mode)))


;;;; elisp

(defun elisp-bindings ()
  (define-key emacs-lisp-mode-map (kbd "M-.") 'xref-find-definitions)
  (define-key emacs-lisp-mode-map (kbd "M-,") 'xref-pop-marker-stack))

(add-hook 'emacs-lisp-mode-hook 'elisp-bindings)


;; lsp-mode


;;(setq lsp-completion-sort-initial-results nil) 
;;(setq lsp-completion--no-reordering t)

;; (use-package lsp-mode
;;   :custom
;;   (lsp-log-io t)
;;   (lsp-keep-workspace-alive nil)
;;   (lsp-enable-snippet nil)
;;   (lsp--auto-configure t )
;;   (lsp-imenu-sort-methods '(position))
;;   :hook (python-mode . lsp)
;;   :commands lsp)


;; (use-package lsp-ui :commands lsp-ui-mode)
;; (use-package helm-lsp :commands helm-lsp-workspace-symbol)
;;(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;;(define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
;;(define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)

;;;; rust

(defun my-project-try-cargo-toml (dir)
  (when-let* ((output
               (let ((default-directory dir))
                 (shell-command-to-string "cargo metadata --no-deps --format-version 1")))
              (js (ignore-errors (json-read-from-string output)))
              (found (cdr (assq 'workspace_root js))))
    (cons 'eglot-project found)))

(cl-defmethod project-roots ((project (head eglot-project)))
  (list (cdr project)))


(use-package rust-mode
  :ensure t
  :hook (rust-mode . lsp)
  :bind
  ("C-c k" . rust-compile)
  ("C-c t" . rust-test)
  ("C-c f" . helm-lsp-code-actions)
  :custom
  (lsp-rust-analyzer-diagnostics-enable nil)
  :init (progn
          ;; Warning! This seems fairly buggy 2020-08-10 is the last version that seems to work for me
          (setq lsp-rust-server 'rust-analyzer)
          (setq lsp-restart 'ignore)
          ;;(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
          ;;(add-to-list 'projectile-project-root-files-bottom-up "Cargo.toml")
          ))


;; Haskell

(use-package haskell-mode
  ;;:ensure t
  :mode (("\\.hs\\'" . haskell-mode)))

(add-hook 'haskell-mode-hook 'interactive-haskell-mode)


(defun cabal-root (dir)
  (message default-directory)
  (when-let ((root (find-cabal-directory default-directory)))
    (message root)
    (cons 'cabal root)))

(add-hook 'project-find-functions #'cabal-root)


(cl-defmethod project-roots ((project (head cabal)))
  (list (cdr project)))


(cl-defun find-cabal-directory (directory &optional (depth 10))
  "Find the first file in the current DIRECTORY or a parent of DIRECTORY that includes a .cabal file."
  (message "Search" directory)
  (message directory)
  (let ((fname (directory-file-name directory)))
    (or
     (and (cl-find-if #'(lambda (file) (string-equal "cabal" (file-name-extension file)))
                      (directory-files directory))
          directory)
     (and (> depth 0)
          (file-name-directory fname)
          (find-cabal-directory (file-name-directory fname) (1- depth))))))


(use-package eglot-mode
  ;;:ensure t
  :bind
  ("C-c f" . eglot-code-actions)
  ("C-c l" . haskell-process-load-file)
  ("M-z" . haskell-interactive-mode-clear)
  :init
  (setq project-current-inhibit-prompt nil))

(setq eglot-workspace-configuration
      '((haskell
         (formattingProvider . "stylish-haskell"))))

(defun eglot-code-action-import (beg &optional end)
  ;;(format "Execute '%s' code actions between BEG and END." kind)
  (interactive (eglot--region-bounds))
  (eglot-code-actions beg end "quickfix.import.extend.list.topLevel"))

(use-package flymake-diagnostic-at-point
  :after flymake
  :custom
  (flymake-diagnostic-at-point-timer-delay 0.1)
  (flymake-diagnostic-at-point-error-prefix "> ")
  (flymake-diagnostic-at-point-display-diagnostic-function 'flymake-diagnostic-at-point-display-popup)
  :hook
    (flymake-mode . flymake-diagnostic-at-point-mode))


;; (use-package dap-mode
;;   :ensure t :after lsp-mode
;;   :config
;;   (dap-mode t)
;;   (dap-ui-mode t))


;; (use-package rustic
;;   :ensure t
;;   :bind
;;   ("C-c k" . rustic-recompile)
;;   ;;("C-c t" . rust-test)
;;   ("C-c f" . helm-lsp-code-actions)
;;   :custom
;;   (rustic-ansi-faces
;;    ["black"
;;     "deeppink2"
;;     "springgreen1"
;;     "lightgoldenrod2"
;;     "deepskyblue1"
;;     "magenta3"
;;     "cyan3"
;;     "white"])
;;   :init 
;;   ;;(add-to-list 'projectile-project-root-files-bottom-up "Cargo.toml")
;;   )
;; ;;(add-to-list 'projectile-project-root-files-bottom-up "Cargo.toml")


(use-package web-mode
  :mode (("\\.tsx\\'" . web-mode)
         ("\\.html\\'" . web-mode)
         ("\\.json\\'" . web-mode))
  :config
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2))

(add-hook 'web-mode-hook
          (lambda ()
            (define-key web-mode-map (kbd "M-;") nil)
            (define-key web-mode-map (kbd "M-:") nil)
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (tide-mode)
              (tide-setup)
              )))

;;(flycheck-add-mode 'typescript-tslint 'web-mode)


;;;; Typescript

(defun eslint-fix ()
  "Apply linter."
  (interactive)
  (shell-command (concat "npx tslint --fix " (buffer-file-name)))
  ;;(shell-command (concat "npx prettier --write " (buffer-file-name)))
  (revert-buffer t t))


(use-package tide
  :bind
  ("C-c f" . tide-fix)
  ("C-c l" . eslint-fix)
  :custom
  ;; Use global install if applicable
  (when (file-exists-p "/usr/bin/tsserver")
    (tide-tsserver-executable "/usr/bin/tsserver"))
  ;;(tide-completion-detailed t)
  (typescript-indent-level 2)
  (tide-format-options
   '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions
     t
     :placeOpenBraceOnNewLineForFunctions
     nil
     :InsertSpaceAfterOpeningAndBeforeClosingNonemptyBrackets t))
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)))

;; autopair
;;(run-with-idle-timer 0 nil (lambda () (require 'autopair)))
;;(run-with-idle-timer 0 nil (lambda () (autopair-global-mode)))


;;; emacs-packages ends here
