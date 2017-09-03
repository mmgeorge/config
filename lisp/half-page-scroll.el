
;;  Quarter-Page Scroll
;;  Adapated from view.el
;; ------------------------------------------------------------------------
;; Determines how much of the page to scroll down.
;; Recommended: 2 or 4, 1 sets to default emacs scroll behavior
(defvar scroll-amount 4)

(defvar view-half-page-size nil
  "Default number of lines to scroll by View half page commands.
If nil that means use half the window size.")
(make-variable-buffer-local 'view-half-page-size)

(defun view-set-half-page-size-default (lines)
  ;; Get and maybe set half page size.
  (if (not lines) (or view-half-page-size
                      (/ (view-window-size) scroll-amount))   ;; Modify this line to 
    (setq view-half-page-size
          (if (zerop (setq lines (prefix-numeric-value lines)))
              (/ (view-window-size) scroll-amount)
            (view-page-size-default lines)))))

(defun view-window-size ()
  (let ((h (window-line-height -1)))
    (if h
        (1+ (nth 1 h))
      (1- (window-height)))))

(defun view-scroll-lines (lines backward default maxdefault)
  (if (or (null lines) (zerop (setq lines (prefix-numeric-value lines))))
      (setq lines default))
  (when (and lines (< lines 0))
    (setq backward (not backward) lines (- lines)))
  (when (and maxdefault lines (> lines (view-window-size)))
    (setq lines nil))
  (cond (backward (scroll-down lines))
        ((view-really-at-end)
         (if view-scroll-auto-exit
             (View-quit)
           (ding)
           (view-end-message)))
        (t (scroll-up lines)
           (if (view-really-at-end) (view-end-message)))))

(defun view-really-at-end ()
  ;; Return true if buffer end visible.  Maybe revert buffer and test.
  (and (pos-visible-in-window-p (point-max))
       (let ((buf (current-buffer))
             (bufname (buffer-name))
             (file (buffer-file-name)))
         (or (not view-try-extend-at-buffer-end)
             (null file)
             (verify-visited-file-modtime buf)
             (not (file-exists-p file))
             (when (buffer-modified-p buf)
               (setq file (file-name-nondirectory file))
               (not (yes-or-no-p
                     (format
                      "File %s changed on disk.  Discard your edits%s? "
                      file
                      (if (string= bufname file) ""
                        (concat " in " bufname))))))
             (progn
               (revert-buffer t t t)
               (pos-visible-in-window-p (point-max)))))))

(defun view-end-message ()
  (goto-char (point-max))
  (if (window-parameter nil 'quit-restore)
      (message "End of buffer.  Type %s to quit viewing."
               (substitute-command-keys
                (if view-scroll-auto-exit "\\[View-scroll-page-forward]"
                  "\\[View-quit]")))
    (message "End of buffer")))

(defun View-scroll-half-page-forward (&optional lines)
  "Scroll forward a \"half page\" (or prefix LINES) lines in View mode.
If LINES is not omitted, the \"half page size\" is set to the minimum of
window height and the absolute value of LINES.6
LINES=0 resets \"half page size\" to half window height."
  (interactive)
  (view-scroll-lines lines nil (view-set-half-page-size-default lines) t))

(defun View-scroll-half-page-backward (&optional lines)
  "Scroll backward a \"half page\" (or prefix LINES) lines in View mode.
See also `View-scroll-half-page-forward'."
  (interactive)
  (view-scroll-lines lines t (view-set-half-page-size-default lines) t))
