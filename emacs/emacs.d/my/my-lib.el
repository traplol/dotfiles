(require 'cl)

(defun my-no-op (&rest rest)
  "An interactive function that accepts any arguments, does nothing, and returns nothing."
  (interactive)
  (values))

(defun* get-closest-pathname (file &optional (max-level 3))
  (let* ((root (expand-file-name "/"))
         (level 0)
         (dir (loop
               for d = default-directory then (expand-file-name ".." d)
               do (setq level (+ level 1))
               if (file-exists-p (expand-file-name file d))
               return d
               if (> level max-level)
               return nil
               if (equal d root)
               return nil)))
    (if dir
        (expand-file-name file dir)
      nil)))

(defun my--split-name (s)
  (my--split-string
   (let ((case-fold-search nil))
     (downcase
      (replace-regexp-in-string "\\([a-z]\\)\\([A-Z]\\)" "\\1 \\2" s)))
   "[^A-Za-z0-9]+"))

(defun my-snake-case-string (s)
  (mapconcat 'downcase   (my--split-name s) "_"))

(defun my-snake-case-region (begin end)
  "Converts 'normal' identifiers within the region from CamelCase into snake_case"
  (interactive "r")
  (let* ((word (buffer-substring begin end))
         (underscored (my-snake-case-string word)))
    (save-excursion
      (widen) ; break out of the subregion so we can fix every usage of the function
      (replace-string word underscored nil (point-min) (point-max)))))

(defun my-replace-last-sexp ()
  "Evaluate the preceding S-Expression and replace it with the result."
  (interactive)
  (let ((value (eval (preceding-sexp))))
    (kill-sexp -1)
    (insert (format "%S" value))))

(defun my-toggle-window-dedicated ()
  "Control whether or not Emacs is allowed to display another buffer in current window."
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window (not (window-dedicated-p window))))
       "%s: Can't touch this!"
     "%s is up for grabs.")
   (current-buffer)))

(defun my-yank-sexp (&optional arg)
  (interactive "p")
  (let ((opoint (point)))
    (forward-sexp (or arg 1))
    (copy-region-as-kill opoint (point))))

(defun my-layout-save (&optional register)
  (interactive)
  (frameset-to-register (or register ?1)))

(defun my-layout-load (&optional register)
  (interactive)
  (jump-to-register (or register ?1) t))

(provide 'my-lib)
