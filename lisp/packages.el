(require 'package)
(require 'cl-lib)

;;------------------------------------------------------------------------------------
;; Package Loading
;;------------------------------------------------------------------------------------

(add-to-list
 'package-archives
 '("melpa" . "http://melpa.org/packages/")
 '("gnu" . "http://elpa.gnu.org/packages/"))

(require 'use-package-ensure)

(setq use-package-always-ensure t) ;; Always make sure we have any package that we use

;;------------------------------------------------------------------------------------
;; Common modes
;;------------------------------------------------------------------------------------

(setq *lsp-server* 'lsp) ;; set to 'lsp or 'eglot

;; Syntax checking. Flymake (builtin) used instead for some modes
(use-package flycheck
  ;; :init (global-flycheck-mode)
  :custom ((flycheck-check-syntax-automatically '(save mode-enable))
           (flycheck-idle-change-delay 1)))


;; Syntax checking UI library for flymake
(use-package flymake-diagnostic-at-point
  :after flymake
  :custom
  (flymake-diagnostic-at-point-timer-delay 0.1)
  (flymake-diagnostic-at-point-error-prefix "> ")
  (flymake-diagnostic-at-point-display-diagnostic-function 'flymake-diagnostic-at-point-display-popup)
  :hook
    (flymake-mode . flymake-diagnostic-at-point-mode))


;; Autocompletion
(use-package company
  :bind (("C-e" . company-complete))
  :init (global-company-mode)
  :custom ((company-tooltip-align-annotations t)))


;;(use-package yasnippet
  ;;:init (yas-global-mode 1))


;; Allows for viewing project files (e.g.., find a file within git project)
(use-package projectile
  :bind (("C-p g" . helm-projectile-grep)
         ("C-p f" . helm-projectile-find-file)
         ("M-o f" . helm-projectile-find-file)
         )
  :init (projectile-mode)
  :custom ((projectile-enable-caching t)))

;; Mode for interacting with language servers that implement the Language Server Protocol
(when (eq *lsp-server* 'lsp)
  (use-package lsp-mode
    :bind
    (("C-c f" . helm-lsp-code-actions))
    :custom
    ((lsp-headerline-breadcrumb-enable nil)
     ;;(lsp-completion-enable-additional-text-edit nil)
     ;;(lsp-log-io t)
     (lsp-ui-doc-enable t)
     (lsp-keep-workspace-alive nil)
     (lsp-enable-snippet nil)
     (lsp--auto-configure t )
     (lsp-imenu-sort-methods '(position))
     (lsp-ui-sideline-show-code-actions nil)
     (lsp-ui-doc-max-height 4))
    ;;:hook (haskell-mode . lsp)
    :commands lsp)


  (use-package lsp-ui
    :commands lsp-ui-mode))


;; Vastly simpler version of lsp, in some cases easier to get working. However,
;; not quite the same level of functionality
(when (eq *lsp-server* 'eglot) 
  (use-package eglot
    :bind
    ;;("C-c f" . eglot-code-actions)
    :init
    (setq project-current-inhibit-prompt nil)))

;; (setq eglot-workspace-configuration
;;       '((haskell
;;          (formattingProvider . "stylish-haskell"))))

;; (defun eglot-code-action-import (beg &optional end)
;;   ;;(format "Execute '%s' code actions between BEG and END." kind)
;;   (interactive (eglot--region-bounds))
;;   (eglot-code-actions beg end "quickfix.import.extend.list.topLevel"))


;; Debbuger protocol interop
;; (use-package dap-mode
;;   :after lsp-mode
;;   :config
;;   (dap-mode t)
;;   (dap-ui-mode t))


;; Provides a great alternative to standard emacs menus for searching for files, commands, etc
(use-package helm
  :preface (require 'helm-config)
  :bind (("M-x" . 'helm-M-x)
         ("C-;" . 'helm-M-x)
         ("C-f d" . 'helm-buffers-list)
         ("C-s" . 'helm-occur)
         ("f" . 'helm-next-source))
  :init (helm-mode 1)
  :custom ((helm-semantic-fuzzy-match t)
           (helm-bookmark-show-location t)))


(defun helm-imenu-candidates-override (&optional buffer)
  (with-current-buffer (or buffer helm-current-buffer)
    (let ((tick (buffer-modified-tick)))
      (if (eq helm-cached-imenu-tick tick)
          helm-cached-imenu-candidates
        (setq imenu--index-alist nil)
        (prog1 (setq helm-cached-imenu-candidates
                     (let ((index (cl-remove-if
                                   (lambda (list)
                                     (message "%s" (car list))
                                     (or (cl-search "alias" (car list))
                                         (cl-search "const" (car list))
                                         (cl-search "alias" (car list))))
                                   (imenu--make-index-alist t))))
                       (helm-imenu--candidates-1
                        (delete (assoc "*Rescan*" index) index))))
          (setq helm-cached-imenu-tick tick))))))


(advice-add 'helm-imenu-candidates :override #'helm-imenu-candidates-override)

(global-set-key (kbd "C-s") 'helm-occur)

(define-key mark-key-map (kbd "M-r") 'helm-mark-ring)
(define-key mark-key-map (kbd "r") 'helm-mark-ring)

(define-key find-key-map (kbd "M-r") 'helm-register)
(define-key find-key-map (kbd "r") 'helm-register)

(define-key helm-map (kbd "M-s") 'helm-previous-line)
(define-key helm-map (kbd "M-d") 'helm-next-line)
(define-key helm-map (kbd "M-a") 'helm-find-files-up-one-level)
(define-key helm-map (kbd "M-f") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-r") 'helm-find-files-rename)
(define-key helm-map (kbd "M-/") 'helm-find-files-down-last-level); 

;; Modify helm-bookmark to avoid prompting
(require 'helm-bookmark)

(defun helm-bookmark-run-delete-no-prompt ()
  "Delete bookmark from keyboard."
  (interactive)
  (with-helm-alive-p
      (helm-exit-and-execute-action 'helm-delete-marked-bookmarks)))

(define-key helm-bookmark-map (kbd "C-d") 'helm-bookmark-run-delete-no-prompt)

;; Faster alternative for text searching
(use-package helm-ag
  :bind (("C-p s" . 'helm-projectile-ag)))

(define-key find-key-map (kbd "M-g") 'helm-ag)
(define-key find-key-map (kbd "g") 'helm-ag)


(use-package helm-projectile
  :init (helm-projectile-on))


(when (eq *lsp-server* 'lsp)
  (use-package helm-lsp
    :commands helm-lsp-workspace-symbol))


;; compilation-mode
(setq compilation-scroll-output t)
(setq compilation-scroll-output 'first-error)

;; Window resizing
(use-package windsize
  :bind (("C-k a" . 'windsize-left)
         ("C-k f" . 'windsize-right)
         ("C-k s" . 'windsize-up)
         ("C-k d" . 'windsize-down)))


(use-package multiple-cursors
  :bind (:map cursor-key-map
              ("d" . mmc/mark-all-like-this-in-defun)
              ("M-d" . mc/mark-all-like-this-in-defun)
              ("s" . mc/mark-all-symbols-like-this-in-defun)
              ("M-s" . mc/mark-all-symbols-like-this-in-defun)
              ("n" . mc/insert-numbers)
              ("M-n" . mc/insert-numbers)
              ("l" . mc/edit-lines)
              ("M-l" . mc/edit-lines)
              ("m" . mc/edit-beginnings-of-lines)
              ("M-m" . mc/edit-beginnings-of-lines)
              ("b" . mc/mark-all-symbols-like-this)
              ("M-b" . mc/mark-all-symbols-like-this)
              ("i" . mc/mark-more-like-this-extended)
              ("M-i" . mc/mark-more-like-this-extended))
  :custom ((mc/always-repeat-command t)
           (mc/always-run-for-all t)))


;; Iteratively expand the selected region
(use-package expand-region
  :bind (:map mark-key-map
              ("k" . er/expand-region)
              ("M-k" . er/expand-region))
  :custom
  (expand-region-contract-fast-key "M-l")
  (expand-region-smart-cursor t))


;; Override function to allow also using the meta+key to repeat expansion
(defun prepare-for-more-expansions-with-meta (repeat-key-str)
  "Return bindings and a message to inform user about them"
  (let ((msg (format "Type %s or M-%s to expand again" repeat-key-str repeat-key-str))
        (bindings (list (cons repeat-key-str '(er/expand-region 1))
                        (cons (concat "M-" repeat-key-str) '(er/expand-region 1)))))
    ;; If contract and expand are on the same binding, ignore contract
    (unless (string-equal repeat-key-str expand-region-contract-fast-key)
      (setq msg (concat msg (format ", %s to contract" expand-region-contract-fast-key)))
      (push (cons expand-region-contract-fast-key '(er/contract-region 1)) bindings))
    ;; If reset and either expand or contract are on the same binding, ignore reset
    (unless (or (string-equal repeat-key-str expand-region-reset-fast-key)
                (string-equal expand-region-contract-fast-key expand-region-reset-fast-key))
      (setq msg (concat msg (format ", %s to reset" expand-region-reset-fast-key)))
      (push (cons expand-region-reset-fast-key '(er/expand-region 0)) bindings))
    (cons msg bindings)))


(advice-add 'er/prepare-for-more-expansions-internal :override #'prepare-for-more-expansions-with-meta)

;;------------------------------------------------------------------------------------
;; Language - Config/Nginx - Editing nginx configuration files
;;------------------------------------------------------------------------------------

(use-package nginx-mode)

(use-package company-nginx)

;;------------------------------------------------------------------------------------
;; Language - Markdown
;;------------------------------------------------------------------------------------

;; (use-package markdown-mode
;; :bind (("C-l l" . 'markdown-insert-link)
;;    ("C-l p". 'markdown-live-preview-mode)))

;;------------------------------------------------------------------------------------
;; Language - Elisp
;;------------------------------------------------------------------------------------

(defun elisp-bindings ()
  (define-key emacs-lisp-mode-map (kbd "M-.") 'xref-find-definitions)
  (define-key emacs-lisp-mode-map (kbd "M-,") 'xref-pop-marker-stack))

(add-hook 'emacs-lisp-mode-hook 'elisp-bindings)

;;------------------------------------------------------------------------------------
;; Language - Rust
;;------------------------------------------------------------------------------------

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
  :hook (rust-mode . lsp)
  :bind (("C-c k" . rust-compile)
         ("C-c t" . rust-test)
         ("C-c f" . helm-lsp-code-actions))
  :custom ((lsp-rust-analyzer-diagnostics-enable nil))
  :init (progn
          ;; Warning! This seems fairly buggy 2020-08-10 is the last version that seems to work for me
          ;;(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
          ;;(add-to-list 'projectile-project-root-files-bottom-up "Cargo.toml")
          (setq lsp-rust-server 'rust-analyzer)
          (setq lsp-restart 'ignore)))

;;------------------------------------------------------------------------------------
;; Language - Haskell
;;------------------------------------------------------------------------------------
 
(use-package haskell-mode
  :mode (("\\.hs\\'" . haskell-mode)
         ("\\.cabal\\'" . haskell-mode))
  :bind (("C-c t" . haskell-insert-type)
         ("C-c l" . haskell-process-load-file)
         ("M-z" . haskell-interactive-mode-clear)
         ("<return>" . align-newline))
  ;;:hook (;;(haskell-mode-hook . interactive-haskell-mode)
         ;;(haskell-mode-hook . eglot-ensure)
    ;;     )
  :custom ((haskell-process-log t)
           (haskell-process-args-stack-ghci ;; +RTS -M128m
            '("--ghci-options=-interactive-print=Text.Pretty.Simple.pPrint" "--no-build" "--no-load")

            )
           ))


(when (eq *lsp-server* 'eglot)
  (add-hook 'haskell-mode-hook 'eglot-ensure))


(when (eq *lsp-server* 'lsp)
  (use-package lsp-haskell
    :hook ((haskell-mode . lsp)
           (haskell-literate . lsp))))


;; Abreviations to save my wrists
(define-abbrev-table 'haskell-mode-abbrev-table
  '(("imq"  "import qualified")
    ("fm" "<$>")
    ("ff"  "$")
    ("tis"  "::")
    ;;("is"  "=")
    ("eql"  "==")
    ("la"  "<-")
    ("ra"  "->")
    ) "Haskell shortcuts")


(defun haskell-insert-type ()
  (interactive)
  (haskell-process-do-type t))


(add-hook 'project-find-functions #'cabal-root)

(defun cabal-root (dir)
  (message default-directory)
  (when-let ((root (find-cabal-directory default-directory)))
    (message root)
    (cons 'cabal root)))

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


;; This works, but we really need to have a linter to do this 
;; (require 'align)

;; (add-to-list 'align-rules-list
;;              '(haskell-left-arrows
;;                (regexp . "\\(\\s-+\\)\\(<-\\|←\\)\\s-+") ;; Can also be a function
;;                (modes . '(haskell-mode))))

;; (add-to-list 'align-rules-list
;;              '(haskell-assignment
;;                (regexp . "\\(\\s-+\\)=\\s-+")
;;                (modes . '(haskell-mode))))

;; (add-to-list 'align-rules-list
;;              '(haskell-arrows
;;                (regexp . "\\(\\s-+\\)\\(->\\|→\\)\\s-+")
;;                (modes . '(haskell-mode))))


;; (defun align-newline ()
;;   "A replacement function for `newline-and-indent', aligning as it goes.
;; The alignment is done by calling `align' on the region that was
;; indented."
;;   (interactive)
;;   (let ((separate (or (if (and (symbolp align-region-separate)
;; 			       (boundp align-region-separate))
;; 			  (symbol-value align-region-separate)
;; 			align-region-separate)
;; 		      'entire))
;; 	(end (point)))
;;     (call-interactively 'newline)
;;     (save-excursion
;;       (forward-line -1)
;;       (while (not (or (bobp)
;; 		      (align-new-section-p (point) end separate)))
;; 	(forward-line -1))
;;       (align (point) end))))


;;------------------------------------------------------------------------------------
;; Language - Web
;;------------------------------------------------------------------------------------

(use-package web-mode
  :mode (("\\.tsx\\'" . web-mode)
         ("\\.html\\'" . web-mode)
         ("\\.json\\'" . web-mode))
  :config ((add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
           (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode)))
  :custom ((web-mode-markup-indent-offset 2)
           (web-mode-css-indent-offset 2)
           (web-mode-code-indent-offset 2))
  :hook ((web-mode . setup-tsx-tide-hook)
         (web-mode . (lambda ()
                       (define-key web-mode-map (kbd "M-;") nil)
                       (define-key web-mode-map (kbd "M-:") nil)))))


(defun setup-tsx-tide-hook ()
  (when (string-equal "tsx" (file-name-extension buffer-file-name))
    (tide-mode)
    (tide-setup)))

;;------------------------------------------------------------------------------------
;; Language - Typescript
;;------------------------------------------------------------------------------------

(use-package typescript-mode
  :mode (("\\.ts\\'" . typescript-mode))
  ;;:init
  ;;(add-hook 'after-save-hook #'eslint-fix) -- too slow
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . flycheck-mode)))


;; TsServer interop
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


(defun tslint-fix ()
  "Apply linter."
  (interactive)
  (shell-command (concat "npx tslint --fix " (buffer-file-name)))
  ;;(shell-command (concat "npx prettier --write " (buffer-file-name)))
  (revert-buffer t t))

;;------------------------------------------------------------------------------------
;; Language - GLSL Shaders
;;------------------------------------------------------------------------------------

(defun glsl-define-checker ()
  (flycheck-define-checker glsl-lang-validator
    "A GLSL checker using glslangValidator.
   See URL https://www.khronos.org/opengles/sdk/tools/Reference-Compiler/"
    :command ("glslangValidator" source)
    :error-patterns
    ((error line-start "ERROR: " column ":" line ": " (message) line-end))
    :modes glsl-mode)

  (add-to-list 'flycheck-checkers 'glsl-lang-validator))


(use-package glsl-mode
  :init (glsl-define-checker))


;;------------------------------------------------------------------------------------
;; Language - Common Lisp
;;------------------------------------------------------------------------------------

;; (add-to-list 'load-path "~/.emacs.d/lisp/slime")
;; (add-to-list 'load-path "~/.emacs.d/lisp/sly")
;; (add-to-list 'load-path "~/.emacs.d/lisp/sly-stepper")
;; (add-to-list 'load-path "~/.emacs.d/lisp/sly-asdf")

;;(require 'sly-autoloads)
;;(require 'sly-stepper-autoloads)

(use-package sly)
(use-package sly-asdf)

(add-to-list 'sly-contribs 'sly-asdf 'append)

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
  ;;(sly-mrepl--eval-for-repl `(asdf:load-system ,(find-current-system))))
  (sly-asdf-load-system (find-current-system)))


(defun sly-repl-bindings ()
  (define-key sly-mrepl-mode-map (kbd "C-p c") 'sly-reload-project)
  (define-key sly-mrepl-mode-map (kbd "C-c l") 'sly-mrepl-sync)
  (define-key sly-mrepl-mode-map (kbd "M-z") 'sly-mrepl-clear-repl))


(defun sly-bindings ()
  (define-key sly-mode-map (kbd "C-p c") 'sly-reload-project)
  (define-key sly-mode-map (kbd "C-c l") 'sly-mrepl-sync)
  (define-key sly-mode-map (kbd "M-z") 'sly-mrepl-clear-repl))
