
;;(setq *clang-args '("-includeautoconf.h"))
(setq path-pref "~/.pref/")


(defun load-packages ()
  (require 'package)
  (setq package-user-dir (concat path-pref "elpa24"))
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
  (package-initialize)
  
  (define-key dired-mode-map (kbd "w") 'dired-previous-line)
  (define-key dired-mode-map (kbd "s") 'dired-next-line)

  ;; flycheck
  (global-flycheck-mode)

  (add-hook 'after-init-hook 'global-company-mode)
  (global-set-key (kbd "C-e") 'company-complete)

  ;; helm -> emacs qol auto completion features
  (require 'helm-config)
  (helm-mode 1)
  (semantic-mode 1)
  (global-semanticdb-minor-mode)
  (setq helm-semantic-fuzzy-match t)
  
  ;;(global-set-key (kbd "C-h") 'helm-command-prefix)
  (global-set-key (kbd "M-x") 'helm-M-x)
  ;;(global-set-key (kbd "M-1") 'helm-find-files)
  (global-set-key (kbd "M-2") 'helm-buffers-list)
  
  ;; projectile -> project management & helm integration
  ;;  commands (with helm-prjectile): 
  ;;   helm-projectile-switch-project
  ;;   helm-projectile-find-file
  ;;   helm-projectile-find-file-in-known-projects
  ;;   helm-projectile-find-file-dwim
  ;;   helm-projectile-find-dir
  ;;   helm-projectile-recentf
  ;;   helm-projectile-switch-to-buffer
  ;;   helm-projectile-grep (can be used for both grep or ack)
  ;;   helm-projectile-ag
  (projectile-mode)
  (require 'helm-projectile)
  (helm-projectile-on)
  ;;(global-set-key (kbd "C-c p g") 'helm-projectile-grep)
  (global-unset-key (kbd "C-p"))
  
  
  (define-key projectile-mode-map (kbd "C-p g") 'helm-projectile-grep)
  (define-key projectile-mode-map (kbd "C-p f") 'helm-projectile-find-file)
  
  ;; markdown-mode
  (add-hook 'markdown-mode-hook
            (lambda ()
              (local-set-key (kbd "C-l l") 'markdown-insert-link) 
              (local-set-key (kbd "C-l p") 'markdown-live-preview-mode)))
  
  

  ;; slime
  (if (file-exists-p "/usr/bin/sbcl" )
      (progn
        (load (expand-file-name "/cloud/.pref/quicklisp/slime-helper.el"))
        (setq inferior-lisp-program "/usr/bin/sbcl")
        (global-set-key (kbd "M-z") 'slime-repl-clear-buffer)))
  
  ;; js2-mode
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-to-list 'interpreter-mode-alist '("node" . js2-mode))


  ;; Haskell
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)

  ;; C/C++
  
  ;; HideShow
  ;;(yafolding-mode)
  
  ;;(global-set-key (kbd "M-k") 'yafolding-toggle-element)
  ;;(global-set-key (kbd "M-l") 'yafolding-toggle-all)

  (setq HS-MODE-HIDDEN nil)
  (defun toggle-hide-all ()
    (interactive)
    (if HS-MODE-HIDDEN
        (progn (hs-show-all)
               (setq HS-MODE-HIDDEN nil))
        (progn (hs-hide-all)
               (setq HS-MODE-HIDDEN t))))

  (setq hs-hide-comments-when-hiding-all nil)
  (add-hook 'c-mode-hook 'hs-minor-mode)
  (add-hook 'c++-mode-hook 'hs-minor-mode)
  (global-set-key (kbd "M-k") 'hs-toggle-hiding)
  (global-set-key (kbd "M-l") 'toggle-hide-all)

  (defvar hs-special-modes-alist
    (mapcar 'purecopy
            '((c-mode "{" "}" "/[*/]" "#ifdef" "#endif" nil nil)
              (c++-mode "{" "}" "/[*/]" nil nil)
              (bibtex-mode ("@\\S(*\\(\\s(\\)" 1))
              (js-mode "{" "}" "/[*/]" nil)))")")

  ;; Enable helm-gtags-mode
  (add-hook 'c-mode-hook 'helm-gtags-mode)
  (add-hook 'c++-mode-hook 'helm-gtags-mode)
  (add-hook 'asm-mode-hook 'helm-gtags-mode)

  (global-set-key (kbd "M-.") 'helm-gtags-dwim)
  (global-set-key (kbd "M-,") 'helm-gtags-find-tag)
  
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
  
  ;; autopair
  (run-with-idle-timer 0 nil (lambda () (require 'autopair)))
  (run-with-idle-timer 0 nil (lambda () (autopair-global-mode))))


(defun load-packages-v23 ()
  (load (concat path-pref "package.el"))
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
  (setq package-user-dir (concat path-pref "elpa23"))
  (package-initialize))


;; (if (version< emacs-version "24")
;;     (load-packages-v23)
;;   (load-packages))

(load-packages)

;;; emacs-packages ends here
