(defun load-config (path)
  (setq loads
        '("theme.el"
          "half-page-scroll.el"
          "commands.el"
          "default.el"
          "templates.el"
          "lsp-ui-project-errors.el"
          "packages.el"
          ;; "lsp-wgsl.el"
          "wgslx.el"
          ))
  (dolist (file loads)
    (load-file (concat path file))))


(load-config EMACS-LOAD-PATH)
