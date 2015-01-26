(setq my:packages
      '(
		auto-complete
		autopair
		evil
		magit
		paredit
		rainbow-delimiters
        yasnippet
        ))




(setq my:packman-packages-list-file
	  (expand-file-name "~/.emacs.d/my/packman/packages.el"))


(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))


(defun my:packman-package-exists (pkg-sym)
  "Checks if a package exists.
Returns t if the package exists, otherwise nil."
  (interactive "SPackage name: ")
  (when (not package-archive-contents)
	(package-refresh-contents))
  (if (assoc pkg-sym package-archive-contents)
	  (let ()
		(message (format "Package '%s' exists." pkg-sym))
		t)
    (let ()
	  (message (format "Package '%s' does not exist." pkg-sym))
	  nil)))

(defun my:packman-install-package (pkg-sym)
  "Installs a package."
  (interactive "SPackage name: ")
  (when (my:packman-package-exists pkg-sym)
	(if (or t
			(not (package-installed-p pkg-sym)))
		(let ()
		  (message (format "Installing package: %s" pkg-sym))
		  (package-install pkg-sym)
		  (load my:packman-packages-list-file)
  		  (add-to-list 'my:packman-packages-list pkg-sym)
		  (print my:packman-packages-list)
		  (print pkg-sym)		  
		  (my:save-packman-list my:packman-packages-list-file))
	  (let ()
		(message (format "Package '%s' already installed." pkg-sym))))))

(defun my:packman-install-packages-from-list (package-list)
  "Installs the packages in the PACKAGE-LIST."
  (when package-list
	(dolist (pkg package-list)
	  ;; if the package is not in the package archive, display a warning.
	  (if (not (my:packman-package-exists pkg))
		  (display-warning 'my:packman-package-not-found (format "%s" pkg))
		(my:packman-install-package pkg)))))

(defun my:packman-install-my-packages ()
  "Installs the packages from `my:packman-packages-list'."
  (interactive)
  (load my:packman-packages-list-file)
  (my:packman-install-packages-from-list my:packman-packages-list))

(defun my:packman-list-unaccounted-packages ()
  "Like `package-list-packages', but shows only the packages that
  are installed and are not in `my:packages'.  Useful for
  cleaning out unwanted packages."
  (interactive)
  (require 'cl)
  (load my:packman-packages-list-file)
  (package-show-package-list
   (remove-if-not (lambda (x) (and (not (memq x my:packman-packages-list))
								   (not (package-built-in-p x))
								   (package-installed-p x)))
                  (mapcar 'car package-archive-contents))))

(defun my:save-packman-list (filename)
  (save-excursion
	(let ((buf (find-file-noselect filename)))
	  (set-buffer buf)
	  (erase-buffer)
	  (when (not (boundp 'my:packman-packages-list))
		  (setq my:packman-packages-list 'nil))
	  (print (list 'setq 'my:packman-packages-list (list 'quote my:packman-packages-list)) buf)
	  (save-buffer)
	  )))
