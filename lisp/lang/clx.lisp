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
      ;; Should probably refactor this...
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
  ;;(substitute-key-definition 'slime-compile-defun 'clx-compile-defun clx-mode-map)
  ;;(use-local-map clx-mode-map)
  (define-key slime-mode-indirect-map (kbd "C-c C-c") 'clx-compile-defun)
  
  (font-lock-add-keywords lisp-mode clx-locks))
