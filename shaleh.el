;; shaleh- is public. shaleh/ is private

;; I like that load-file loads exactly the file I request when a path is used
;; but I want to use compiled files if they exist.
(defun shaleh-load-file (filename)
  "Load a file, prefer to load the compiled version."
  (let ((elc (concat (file-name-sans-extension filename) ".elc")))
    (if (file-exists-p elc)
        (load-file elc)
      (load-file filename)
     )
   )
)

(defun shaleh-kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer
    (delq (current-buffer)
          (remove-if-not 'buffer-file-name (buffer-list))
     )
   )
 )

(defun shaleh-move-line (n)
  "Move the current line up or down by N lines."
  (interactive "p")
  (let ((col (current-column))
        start
        end)
    (beginning-of-line)
    (setq start (point))
    (end-of-line)
    (forward-char)
    (setq end (point))
    (let ((line-text (delete-and-extract-region start end)))
      (forward-line n)
      (insert line-text)
      ;; restore point to original column in moved line
      (forward-line -1)
      (forward-char col)
     )
   )
 )

(defun shaleh-move-line-up (n)
  "Move the current line up by N lines."
  (interactive "p")
  (shaleh-move-line (if (null n) -1 (- n)))
 )

(defun shaleh-move-line-down (n)
  "Move the current line down by N lines."
  (interactive "p")
  (shaleh-move-line (if (null n) 1 n))
 )

(defun shaleh-camelscore-word-at-point ()
  (interactive)
  (if (use-region-p)
      (save-restriction
         (narrow-to-region (region-beginning) (region-end))
         (shaleh/camelscore-word-at-point)
         (widen)
       )
    (shaleh/camelscore-word-at-point)
   )
)

(defun shaleh-eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))
   )
 )

;;; Internals

(defun shaleh/split-name (s)
  (split-string
    (let ((case-fold-search nil))
      (downcase (replace-regexp-in-string "\\([a-z]\\)\\([A-Z]\\)" "\\1 \\2" s))
     )
    "[^A-Za-z0-9]+")
 )

(defun shaleh/mapcar-head (fn-head fn-rest list)
  "Like MAPCAR, but applies a different function to the first element."
  (if list
      (cons (funcall fn-head (car list)) (mapcar fn-rest (cdr list)))
   )
 )

(defun shaleh/shell-variable-style (s) (mapconcat 'upcase (shaleh/split-name s) "_"))

(defun shaleh/camelcase  (s) (mapconcat 'capitalize (shaleh/split-name s) ""))

(defun shaleh/underscore (s) (mapconcat 'downcase (shaleh/split-name s) "_"))

(defun shaleh/dasherize  (s) (mapconcat 'downcase (shaleh/split-name s) "-"))

(defun shaleh/colonize   (s) (mapconcat 'capitalize (shaleh/split-name s) "::"))

(defun shaleh/camelscore (s)
  (cond ((string-match-p "\:"  s) (shaleh/underscore s))
        ((string-match-p "_" s)   (shaleh/camelcase s))
        (t                        (shaleh/colonize s))
   )
 )

;; Reminder: Internal
(defun shaleh/camelscore-word-at-point ()
  (let* ((case-fold-search nil)
         (beg (and (skip-chars-backward "[:alnum:]:_-") (point)))
         (end (and (skip-chars-forward  "[:alnum:]:_-") (point)))
         (txt (buffer-substring beg end))
         (cml (shaleh/camelscore txt))
         )
    (if cml
        (progn (delete-region beg end)
               (insert cml)
         )
     )
   )
)

(defun shaleh/make-shell-variable-work-at-point ()
  (let* ((case-fold-search nil)
         (beg (and (skip-chars-backward "[:alnum:]:-") (point)))
         (end (and (skip-chars-forward  "[:alnum:]:-") (point)))
         (txt (buffer-substring beg end))
         (cml (shaleh/shell-variable-style txt))
         )
    (if cml
      (progn (delete-region beg end)
             (insert cml)
       )
     )
   )
)

(defun shaleh-reverse-region (beg end)
  "Reverse characters between BEG and END."
  (interactive "r")
  (insert (nreverse (delete-and-extract-region beg end))))

(provide 'shaleh)
