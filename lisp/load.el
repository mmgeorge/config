(defun load-config (path)
  (setq loads
        '("visual.el"
          "remove-unwanted-buffers.el"
          "half-page-scroll.el"
          "commands.el"
          "default.el"
          "templates.el"
          "packages.el"))
  (dolist (file loads)
    (load-file (concat path file))))


(load-config EMACS-LOAD-PATH)
