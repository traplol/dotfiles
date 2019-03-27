(require 'paredit)
(require 'rainbow-delimiters)

(when (file-exists-p (expand-file-name "~/quicklisp/clhs-use-local.el"))
  (load (expand-file-name "~/quicklisp/clhs-use-local.el") t))

(setq inferior-lisp-program "sbcl --dynamic-space-size 2048")
(load (expand-file-name "~/quicklisp/slime-helper.el"))

(load (expand-file-name "~/.emacs.d/my/highlight-sexp.el"))
(require 'highlight-sexp)

(defun my--sexp-modes ()
  (local-set-key (kbd "C-M-i") 'hl-sexp-copy-highlighted)
  (local-set-key (kbd "C-M-j") 'hl-sexp-kill-highlighted)
  (aggressive-indent-mode 1)
  (paredit-mode 1)
  (show-paren-mode 1)
  (rainbow-delimiters-mode 1)
  (highlight-sexp-mode 1))

(add-hook 'lisp-mode-hook 'my--sexp-modes)
(add-hook 'emacs-lisp-mode-hook 'my--sexp-modes)
(add-hook 'scheme-mode-hook 'my--sexp-modes)
(add-hook 'racket-mode-hook 'my--sexp-modes)

(defun my--lisp-local-keys ()
  (local-set-key (kbd "<f1>") 'slime-hyperspec-lookup)
  (local-set-key (kbd "<f2>") 'slime-edit-definition)
  (local-set-key (kbd "<f3>") 'slime-disassemble-symbol)
  )
(add-hook 'lisp-mode-hook 'my--lisp-local-keys)
