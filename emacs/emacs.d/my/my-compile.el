(require 'my-lib)

(progn
  (define-key (current-global-map) (kbd "<f5>") 'compile)
  (add-hook 'c-mode-common-hook 'my-make-cmd))

(defun my-make-cmd ()
  (interactive)
  (unless (file-exists-p "Makefile")
	(set (make-local-variable 'compile-command)
		 (let ((file (file-name-nondirectory buffer-file-name))
			   (mkfile (get-closest-pathname "Makefile")))
		   (if mkfile
			   (progn (format "cd %s; make -f %s"
							  (file-name-directory mkfile) mkfile))
			 (format "%s -c -o %s.o %s %s %s"
					 (or (getenv "CC") "gcc")
					 (file-name-sans-extension file)
					 (or (getenv "CPPFLAGS") "-DDEBUG=9")
					 (or (getenv "CFLAGS") "-ansi -pedantic -Wall -g")
					 file))))))

(provide 'my-compile)
