(require 'paredit)
(require 'rainbow-delimiters)

(load (expand-file-name "~/.emacs.d/my/highlight-sexp.el"))
(require 'highlight-sexp)

(defun my--sexp-modes ()
  (paredit-mode 1)
  (show-paren-mode 1)
  (rainbow-delimiters-mode 1)
  (highlight-sexp-mode 1))

(add-hook 'lisp-mode-hook 'my--sexp-modes)
(add-hook 'emacs-lisp-mode-hook 'my--sexp-modes)
(add-hook 'scheme-mode-hook 'my--sexp-modes)
(add-hook 'racket-mode-hook 'my--sexp-modes)
