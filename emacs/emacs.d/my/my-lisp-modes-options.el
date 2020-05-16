(require 'paredit)
(require 'rainbow-delimiters)
(require 'my-lib)

(when (file-exists-p (expand-file-name "~/quicklisp/clhs-use-local.el"))
  (load (expand-file-name "~/quicklisp/clhs-use-local.el") t))

(setq inferior-lisp-program "sbcl --dynamic-space-size 12288")
(load (expand-file-name "~/quicklisp/slime-helper.el"))


(load (my-dot-emacs "my/highlight-sexp.el"))
(require 'highlight-sexp)

(defun my--find-symbol ()
  (interactive)
  (case major-mode
    (emacs-lisp-mode
     (let ((sym (symbol-at-point)))
       (xref-push-marker-stack)
       (unless (or (ignore-errors (find-function sym) t)
                   (ignore-errors (find-variable sym) t)
                   (ignore-errors (find-face-definition sym) t))
         (xref-pop-marker-stack))))
    (lisp-mode (call-interactively 'slime-edit-definition))))


(defun my--sexp-modes ()
  (local-set-key (kbd "TAB") (key-lambda (completion-at-point)))
  (local-set-key (kbd "C-M-i") 'hl-sexp-copy-highlighted)
  (local-set-key (kbd "C-M-j") 'hl-sexp-kill-highlighted)
  (local-set-key (kbd "<f2>") 'my--find-symbol)
  (local-set-key (kbd "M-.") 'my--find-symbol)
  (aggressive-indent-mode 1)
  (paredit-mode 1)
  (show-paren-mode 1)
  (rainbow-delimiters-mode 1)
  (highlight-sexp-mode 1))

(add-hook 'lisp-mode-hook 'my--sexp-modes)
(add-hook 'emacs-lisp-mode-hook 'my--sexp-modes)
(add-hook 'scheme-mode-hook 'my--sexp-modes)
(add-hook 'racket-mode-hook 'my--sexp-modes)

(defun my--elisp-local-keys ()
  (local-set-key (kbd "C-c C-c") 'eval-defun))

(add-hook 'emacs-lisp-mode-hook 'my--elisp-local-keys)

(defun my--lisp-local-keys ()
  (local-set-key (kbd "<f1>") 'slime-hyperspec-lookup)
  (local-set-key (kbd "<f3>") 'slime-disassemble-symbol)
  (local-set-key (kbd "<f4>") 'slime-macroexpand-1)
  (local-set-key (kbd "C-<f9>") 'slime-restart-inferior-lisp))

(add-hook 'lisp-mode-hook 'my--lisp-local-keys)
