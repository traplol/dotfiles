(setq my:packages
      '(
	auto-complete
	autopair
	evil
	paredit
	rainbow-delimiters
        yasnippet
        ))


(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(dolist (pkg my:packages)
  ;; if the package is not in the package archive, display a warning.
  (if (not (assoc pkg package-archive-contents))
      (display-warning 'package-not-found (format "%s" pkg))
    ;; otherwise if it's not installed, install it.
    (when (not (package-installed-p pkg))
      (package-install pkg))))


(defun package-list-unaccounted-packages ()
  "Like `package-list-packages', but shows only the packages that
  are installed and are not in `my:packages'.  Useful for
  cleaning out unwanted packages."
  (interactive)
  (require 'cl)
  (package-show-package-list
   (remove-if-not (lambda (x) (and (not (memq x my:packages))
                            (not (package-built-in-p x))
                            (package-installed-p x)))
                  (mapcar 'car package-archive-contents))))
