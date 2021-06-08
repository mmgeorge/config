;;(add-to-list 'load-path "~/.emacs.d/lisp/slime")
;;(add-to-list 'load-path "~/.emacs.d/lisp/sly")
;;(add-to-list 'load-path "~/.emacs.d/lisp/sly-stepper")
(add-to-list 'load-path "~/.emacs.d/lisp/sly-asdf")

;;;; Common Lisp

;;;;;; SLY ;;;;;;;;

;; START LOCAL SLY
;;(require 'sly-autoloads)
(require 'sly-asdf)
;;(require 'sly-stepper-autoloads)
(add-to-list 'sly-contribs 'sly-asdf 'append)
;; END LOCAL SLY

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


;;;;;; SLIME ;;;;;;;;

;; (defun slime-reload-project ()
;;   (interactive)
;;   (message "hello project")
;;   (slime-reload-system (find-current-system)))

;; (defun slime-bindings ()
;;   (define-key slime-repl-mode-map (kbd "C-p c") 'slime-reload-project))

;; ;; (add-hook 'slime-repl-mode-hook 'slime-bindings)


;; (require 'slime-autoloads)
;; (setq slime-contribs '(slime-fancy slime-asdf slime-cl-indent))
;; (setq slime-contribs '(slime-fancy ;;slime-asdf slime-cl-indent

;;(setq slime-kill-without-query-p t)

;; (defun slime-repl-hook ()
;;   (define-key slime-repl-mode-map (kbd "M-s") nil)
;;   (define-key slime-repl-mode-map (kbd "M-z") 'slime-repl-clear-buffer))

;; (add-hook 'slime-repl-mode-hook 'slime-repl-hook)
;;(add-hook 'lisp-mode-hook 'slime-hook)

;;(slime-setup '(slime-company))


;;(global-set-key (kbd "C-p c") 'reload-project)
;; (global-set-key (kbd "M-r") 'slime-compile-region)

;;(defun slime-hook ()
;;(run-with-idle-timer 0.25 nil (lambda ()
;;(slime)
;;(other-window 1)
;;(slime-load-system (project-system-name))
;;)))


(setq display-buffer-alist
      '(("\\*inferior-lisp\\*" display-buffer-below-selected (window-height . 15)   
         (nil))))
