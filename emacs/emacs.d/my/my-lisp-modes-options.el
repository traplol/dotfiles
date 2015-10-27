(require 'paredit)
(require 'rainbow-delimiters)

(defun my-lisp-modes ()
  (paredit-mode)
  (show-paren-mode)
  (rainbow-delimiters-mode))

(add-hook 'lisp-mode-hook 'my-lisp-modes)
(add-hook 'emacs-lisp-mode-hook 'my-lisp-modes)
