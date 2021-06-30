(defun load-config (path)
  (setq loads
        '("theme.el"
          "half-page-scroll.el"
          "commands.el"
          "default.el"
          "templates.el"
          "packages.el"
          ))
  (dolist (file loads)
    (load-file (concat path file))))


(load-config EMACS-LOAD-PATH)
