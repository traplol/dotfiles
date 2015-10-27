(require 'cl)

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

(provide 'my-lib)
