(define-key input-decode-map "\e\eOA" [(meta up)])
(define-key input-decode-map "\e\eOB" [(meta down)])


(defun scroll-up-line ()
  "Scroll text of selected window down ARG lines; or one line if no ARG.
If ARG is omitted or nil, scroll down by one line.
This is different from `scroll-down-command' that scrolls a full screen."
  (interactive)
  (scroll-up 4))

(defun scroll-down-line ()
  "Scroll text of selected window down ARG lines; or one line if no ARG.
If ARG is omitted or nil, scroll down by one line.
This is different from `scroll-down-command' that scrolls a full screen."
  (interactive)
  (scroll-down 4))

(defun cua-copy-deselect (arg)
  (interactive "P")
  (cua-copy-region arg)
  (cua--deactivate))


(defun select-current-line-and-forward-line (arg)
  "Select the current line and move the cursor by ARG lines IF
no region is selected.

If a region is already selected when calling this command, only move
the cursor by ARG lines."
  (interactive "p")
  (when (not (use-region-p))
    (forward-line 0)
    (set-mark-command nil))
  (forward-line arg))

(defun jump-to-top ()
  (interactive)
  (goto-line 0))

(require 'hi-lock)

(defun highlight-symbol-toggle ()
  (interactive)
  (let* ((regexp (find-tag-default-as-symbol-regexp)))
    (if (null regexp)
        (dolist (element hi-lock-interactive-patterns)
          (unhighlight-regexp (car element))
          )
         (if (or (assoc (hi-lock--hashcons regexp) hi-lock-interactive-patterns)
                 (assoc (hi-lock--hashcons regexp) hi-lock-interactive-lighters))
             (unhighlight-regexp (hi-lock--hashcons regexp))
           (message "%s" (hi-lock--hashcons regexp))
           (highlight-symbol-at-point)))))

  ;;(global-set-key (kbd "s-.") 'jpt-toggle-mark-word-at-point)


(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-p"))
(global-unset-key (kbd "M-i"))
(global-unset-key (kbd "M-q"))
(global-unset-key (kbd "M-r"))
(global-unset-key (kbd "M-v"))
(global-unset-key (kbd "M-z"))
(global-unset-key (kbd "C-s"))
;;(global-unset-key (kbd "M-,"))

;; Find commands
(define-prefix-command 'find-key-map)
(global-set-key (kbd "M-o") 'find-key-map)
(define-key find-key-map (kbd "a") 'helm-find-files)
(define-key find-key-map (kbd "s") 'helm-buffers-list)
(define-key find-key-map (kbd "M-a") 'helm-find-files)
(define-key find-key-map (kbd "M-s") 'helm-buffers-list)

(define-key find-key-map (kbd "o") 'other-window)
(define-key find-key-map (kbd "M-o") 'other-window)

(define-key find-key-map (kbd "M-l") 'goto-line)
(define-key find-key-map (kbd "l") 'goto-line)

(define-key find-key-map (kbd "j") 'jump-to-top)
(define-key find-key-map (kbd "M-j") 'jump-to-top)

;; Find - Bookmarks
(define-key find-key-map (kbd "d") 'helm-bookmarks)
(define-key find-key-map (kbd "M-d") 'helm-bookmarks)
(define-key find-key-map (kbd "b a") 'bookmark-insert)
(define-key find-key-map (kbd "b e") 'bookmark-delete-all)

(define-key global-map (kbd "M-g g") 'keyboard-quit)
(define-key global-map (kbd "M-g M-g") 'keyboard-quit)

;; Mark commands
(define-prefix-command 'mark-key-map)
(global-set-key (kbd "M-i") 'mark-key-map)

(define-key mark-key-map (kbd "M-s") 'mark-sexp)
(define-key mark-key-map (kbd "s") 'mark-sexp)

(define-key mark-key-map (kbd "M-b") 'mark-page)
(define-key mark-key-map (kbd "b") 'mark-page)

(define-key mark-key-map (kbd "M-b") 'mark-paragraph)
(define-key mark-key-map (kbd "b") 'mark-paragraph)

(define-key mark-key-map (kbd "M-e") 'mark-end-of-sentence)
(define-key mark-key-map (kbd "e") 'mark-end-of-sentence)

(define-key mark-key-map (kbd "M-d") 'mark-defun)
(define-key mark-key-map (kbd "d") 'mark-defun)

(define-key mark-key-map (kbd "M-i") 'cua-set-mark)
(define-key mark-key-map (kbd "i") 'cua-set-mark)

(define-key mark-key-map (kbd "M-w") 'mark-word)
(define-key mark-key-map (kbd "w") 'mark-word)

(define-key mark-key-map (kbd "M-h") 'highlight-symbol-toggle)
(define-key mark-key-map (kbd "h") 'highlight-symbol-toggle)

(define-key mark-key-map (kbd "M-/") 'comment-line)
(define-key mark-key-map (kbd "/") 'comment-line)
(global-set-key (kbd "M-'") 'comment-line)

(define-key mark-key-map (kbd "M-l") 'select-current-line-and-forward-line)
(define-key mark-key-map (kbd "l") 'select-current-line-and-forward-line)

;; Cursor commands
(define-prefix-command 'cursor-key-map)
(define-key mark-key-map (kbd "j") 'cursor-key-map)
(define-key mark-key-map (kbd "M-j") 'cursor-key-map)

(define-prefix-command 'window-key-map)
(global-set-key (kbd "M-w") 'window-key-map)
(define-key window-key-map (kbd "l") 'split-window-below)
(define-key window-key-map (kbd ";") 'split-window-right)


;; Kill commands
(define-prefix-command 'kill-key-map)
(global-set-key (kbd "C-k") 'kill-key-map)
(define-key kill-key-map (kbd "b") 'kill-current-buffer)
(define-key kill-key-map (kbd "r s") 'replace-string)
;;(define-key basic-key-map (kbd "a") 'kill-buffer-and-window)
(define-key kill-key-map (kbd "w") 'delete-window)
(define-key kill-key-map (kbd "o") 'delete-other-windows)
(define-key kill-key-map (kbd "c") 'comment-line)

(define-key kill-key-map (kbd "k") 'kill-buffer)

;; Window Split commands
(global-set-key (kbd "C-x l") 'split-window-below)
(global-set-key (kbd "C-x ;") 'split-window-right)

;;(define-key basic-key-map (kbd "k") 'switch-to-buffer)

;;; Code:
;; Hotkey rebinds
;;(global-set-key (kbd "M-1") 'helm-find-files)
;;(global-set-key (kbd "M-2") 'switch-to-buffer)

(global-set-key (kbd "C-<tab>") 'other-window)
;;(global-set-key (kbd "M-o") 'other-window)
    
(global-set-key (kbd "C-t") 'next-error)

;;(global-set-key (kbd "C-/") 'yank)
;;(global-set-key (kbd "M-q") 'back-to-indentation)
;;(global-set-key (kbd "M-r") 'move-end-of-line)

(global-unset-key (kbd "M-m"))
(global-unset-key (kbd "M-/"))
(global-unset-key (kbd "M-j"))

(global-set-key (kbd "M-i c") 'comment-or-uncomment-region)
(global-set-key (kbd "M-i M-c") 'comment-or-uncomment-region)

(global-set-key (kbd "C-h") 'kill-whole-line)

(global-set-key (kbd "M-m") 'back-to-indentation)
(global-set-key (kbd "M-/") 'move-end-of-line)
(global-set-key (kbd "M-_") 'move-end-of-line)
;;(global-set-key (kbd "M-z") 'back-to-indentation)
(global-set-key (kbd "M-v") 'move-end-of-line)

;;(global-set-key (kbd "M-u") 'back-to-indentation)
;;(global-set-key (kbd "M-p") 'move-end-of-line)
;;(global-set-key (kbd "C-h") 'kill-whole-line)
(global-set-key (kbd "M-h") 'kill-word)
(global-set-key (kbd "M-u") 'cua-copy-deselect)
(global-set-key (kbd "M-y") 'cua-paste)
;;(global-set-key (kbd "C-u") 'kill-region)
;;(global-set-key (kbd "M-z") 'undo)
;; (global-set-key (kbd "M-c") 'company-complete)
  
;;(global-set-key (kbd "M-l") 'View-scroll-half-page-forward) 
;;(global-set-key (kbd "M-k") 'View-scroll-half-page-backward)   

(global-set-key (kbd "M-l") 'scroll-up-line) 
(global-set-key (kbd "M-k") 'scroll-down-line)   
;(global-set-key (kbd "M-k") 'previous-line)
;(global-set-key (kbd "M-l") 'next-line)

(global-set-key (kbd "C-;") 'forward-word)
(global-set-key (kbd "C-:") 'forward-word)
;(global-set-key (kbd "C-s") 'backward-word)
;(global-set-key (kbd "C-k") 'View-scroll-half-page-backward)
;(global-set-key (kbd "C-l") 'View-scroll-half-page-forward)
;;(global-set-key (kbd "M-4") 'delete-other-windows)

(global-set-key (kbd "C-x )") 'delete-window)

(global-set-key (kbd "M-3") 'kill-buffer)
(global-set-key (kbd "M-s") 'previous-line)
(global-set-key (kbd "M-d") 'next-line)
(global-set-key (kbd "M-f") 'forward-word)
;;(global-set-key (kbd "C-d") 'forward-char)
(global-set-key (kbd "M-a") 'backward-word)
(global-set-key (kbd "C-c v") 'erase-buffer)
(global-set-key (kbd "C-x f") 'set-mark-command)

(global-set-key (kbd "C-w") 'enlarge-window)
(global-set-key (kbd "C-e") 'shrink-window)

(global-set-key (kbd "M-;") 'forward-char)
(global-set-key (kbd "M-:") 'forward-char)
(global-set-key (kbd "M-j") 'backward-char)

;(define-key dired-mode-map (kbd "w") 'dired-previous-line)
;(define-key dired-mode-map (kbd "s") 'dired-next-line)



(provide 'emacs-commands)
;;; emacs-commands ends here

