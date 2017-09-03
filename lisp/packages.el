
(setq package-list
      '(helm
        helm-projectile
        flycheck
        company
        autopair
        markdown-mode
        
        ;; lisp
        slime

        ;; typescript
        tide
        typescript-mode))


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



;; slime
(if (file-exists-p "/usr/bin/sbcl" )
    (progn
      (load (expand-file-name "/cloud/.pref/quicklisp/slime-helper.el"))
      (setq inferior-lisp-program "/usr/bin/sbcl")
      (global-set-key (kbd "M-z") 'slime-repl-clear-buffer)))

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

;; autopair
(run-with-idle-timer 0 nil (lambda () (require 'autopair)))
(run-with-idle-timer 0 nil (lambda () (autopair-global-mode)))


;;; emacs-packages ends here
