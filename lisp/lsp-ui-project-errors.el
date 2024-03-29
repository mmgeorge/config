;;; lsp-ui-project-errors.el --- Flycheck project errors support for lsp-mode -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023  Matt George
;; Copyright (C) 2017  fmdkdd
;; URL: https://github.com/emacs-lsp/lsp-ui
;; Keywords: languagues, tools
;; Version: 6.2

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'flycheck nil 'noerror)  ; Temporary solution, see #514
(require 'pcase)
(require 'dash)

(require 'lsp-protocol)
(require 'lsp-mode)

(defgroup lsp-ui-project-errors nil
  "The LSP extension to display syntax checking."
  :group 'tools
  :group 'convenience
  :group 'lsp-ui
  :link '(custom-manual "(lsp-ui-project-errors) Top")
  :link '(info-link "(lsp-ui-project-errors) Customizing"))

(defcustom lsp-ui-project-errors-list-position 'bottom
  "Position where `lsp-ui-project-errors-list' will show diagnostics for the
whole workspace."
  :type '(choice (const :tag "Bottom" bottom)
                 (const :tag "Right" right))
  :group 'lsp-ui-project-errors)

(defcustom lsp-ui-project-errors-list-severity 1
  "Position where `lsp-ui-project-errors-list' will show diagnostics for the
whole workspace."
  :type '(choice (const :tag "Errors" 1)
                 (const :tag "Warnings" 2))
  :group 'lsp-ui-project-errors)

(defvar-local lsp-ui-project-errors-list--buffer nil)
(defvar-local lsp-ui-project-errors--save-mode nil)
(defvar-local lsp-ui-project-errors--target-window 1)

(defun lsp-ui-project-errors-list--toggle-severity ()
  (interactive)
  (setq lsp-ui-project-errors-list-severity
        (if (eq lsp-ui-project-errors-list-severity 1) 2 1))
  (lsp-ui-project-errors-list--refresh)
  )

(defun lsp-ui-project-errors-list--post-command ()
  (when (eobp)
    (forward-line -1)))

(defun sorted-entries (hash-table)
  (let ((entries ()))
    (maphash (lambda (k v) (push (list k v) entries)) hash-table)
    (sort entries (lambda (a b)
                    (message (car a) (car b))
                    (string-lessp (car a) (car b))))
    entries))

(defun lsp-ui-project-errors-list--update (window workspace)
  "Update project-errors buffer in WINDOW belonging to WORKSPACE.
Use `lsp-diagnostics' to receive diagnostics from your LSP server."
  (let ((buffer-read-only nil)
        (lsp--cur-workspace workspace))
    (erase-buffer)
    (remove-overlays)
    (let ((entries (sorted-entries (lsp-diagnostics))))
      (dolist (entry entries)
        (let ((file (car entry))
              (diagnostic (cadr entry)))
          ;; Only include files with at least one error
          (when (seq-some (lambda (diag)
                                     (-let* (((&Diagnostic :message :severity? :source?
                                                           :range (&Range :start (&Position :line start-line))) diag))
                                       ;; Filter out clippy warnings (comes back as warnings instead of lint)
                                       (and (eq severity? lsp-ui-project-errors-list-severity)
                                            (not (cl-search "clippy" source?))
                                            )))
                         diagnostic)
            (overlay-put
             (make-overlay (point) (point))
             'after-string
             (concat (propertize (lsp-ui--workspace-path file)
                                 'face 'dired-directory)
                     (propertize "\n" 'face '(:height 0.2)))))
          (dolist (diag diagnostic)
            (-let* (((&Diagnostic :message :severity? :source?
                                  :range (&Range :start (&Position :line start-line))) diag)
                    (formatted-message (or (if source? (format "%s: %s" source? message) message) "???"))
                    (severity (or severity? 1))
                    (line (1+ start-line))
                    (face (cond ((= severity 1) 'error)
                                ((= severity 2) 'warning)
                                (t 'success)))
                    (text (concat (propertize (number-to-string line) 'face face)
                                  ": "
                                  (car (split-string formatted-message "\n")))))
              ;; Only output errors
              (when (and (eq severity lsp-ui-project-errors-list-severity)
                         ;; Filter out clippy warnings (comes back as warnings instead of lint errors)
                         ;; (not (cl-search "clippy" source?))
                         )
                (add-text-properties 0 (length text) `(diag ,diag file ,file window ,window) text)
                (insert (concat text "\n")))))))))
  (if (= (point) 1)
      (overlay-put (make-overlay 1 1)
                   'after-string "No diagnostic available\n")
    (goto-char 1))
  (lsp-ui-project-errors-list-mode))

(defun lsp-ui-project-errors-list ()
  "List all the diagnostics in the whole workspace."
  (interactive)
  (let ((buffer (get-buffer-create "*lsp-diagnostics*"))
        (workspace lsp--cur-workspace)
        (window (selected-window)))
    (setq lsp-ui-project-errors--target-window window)
    (with-current-buffer buffer
      (lsp-ui-project-errors-list--update window workspace))
    (add-hook 'lsp-diagnostics-updated-hook 'lsp-ui-project-errors-list--refresh nil t)
    (setq lsp-ui-project-errors-list--buffer buffer)
    (let ((win (display-buffer-in-side-window
                buffer `((side . ,lsp-ui-project-errors-list-position) (slot . 5) (window-width . 0.20)))))
      (set-window-dedicated-p win t)
      (select-window win)
      ;; (fit-window-to-buffer nil nil 10)
      )))

(defun lsp-ui-project-errors-list--refresh ()
  (let ((workspace lsp--cur-workspace)
        (buffer (get-buffer-create "*lsp-diagnostics*"))
        (current-window (selected-window)))
    (with-current-buffer buffer
      (lsp-ui-project-errors-list--update current-window workspace))))
    ;; (when (get-buffer-window lsp-ui-project-errors-list--buffer))
    ;; ;; (when (and (buffer-live-p lsp-ui-project-errors-list--buffer)
    ;; ;;            (get-buffer-window lsp-ui-project-errors-list--buffer)
    ;; ;;            workspace)
    ;;     (lsp-ui-project-errors-list--update current-window workspace)
    ;;     ))

(defun lsp-ui-project-errors-list--open ()
  (-when-let* ((diag (get-text-property (point) 'diag))
               ((&Diagnostic :range (&Range :start (&Position :line start-line
                                                              :character start-column))) diag)
               (file (get-text-property (point) 'file))
               (window (get-text-property (point) 'window))
               (marker (with-current-buffer
                           (or (get-file-buffer file)
                               (find-file-noselect file))
                         (save-restriction
                           (widen)
                           (save-excursion
                             (goto-char 1)
                             (forward-line start-line)
                             (forward-char start-column)
                             (point-marker))))))
    ;; Select the other window
    (other-window 1)
    (set-window-buffer (selected-window) (marker-buffer marker) t)
    (with-selected-window (selected-window)
      (goto-char marker)
      (recenter)
      (pulse-momentary-highlight-one-line (marker-position marker) 'next-error))
    window))

(defun lsp-ui-project-errors-list--view ()
  (interactive)
    (lsp-ui-project-errors-list--open))

(defun lsp-ui-project-errors-list--visit ()
  (interactive)
  (select-window (lsp-ui-project-errors-list--open)))

(defun lsp-ui-project-errors-list--quit ()
  (interactive)
  (kill-buffer))

(defvar lsp-ui-project-errors-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'lsp-ui-project-errors-list--quit)
    (define-key map (kbd "<return>") 'lsp-ui-project-errors-list--view)
    (define-key map (kbd "<M-return>") 'lsp-ui-project-errors-list--visit)
    map)
  "Keymap for ‘lsp-ui-project-errors-list-mode’.")

(define-derived-mode lsp-ui-project-errors-list-mode special-mode "lsp-ui-project-errors-list"
  "Mode showing project-errors diagnostics for the whole workspace."
  (setq truncate-lines t)
  (setq mode-line-format nil)
  (add-hook 'post-command-hook 'lsp-ui-project-errors-list--post-command nil t))

(declare-function lsp-ui--workspace-path "lsp-ui" (path))

(provide 'lsp-ui-project-errors)
;;; lsp-ui-project-errors.el ends here
