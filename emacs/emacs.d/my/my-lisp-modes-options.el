(require 'paredit)
(require 'rainbow-delimiters)

(when (file-exists-p (expand-file-name "~/quicklisp/clhs-use-local.el"))
  (load (expand-file-name "~/quicklisp/clhs-use-local.el") t))

(setq inferior-lisp-program "sbcl --dynamic-space-size 2048")
(load (expand-file-name "~/quicklisp/slime-helper.el"))

(load (expand-file-name "~/.emacs.d/my/highlight-sexp.el"))
(require 'highlight-sexp)

(defun my--find-symbol ()
  (interactive)
  (case major-mode
    (emacs-lisp-mode
     (let ((func (symbol-at-point))
           (var (variable-at-point))
           (face (face-at-point t)))
       (cond ((functionp func)
              (xref-push-marker-stack)
              (find-function func))
             ((/= 0 var)
              (xref-push-marker-stack)
              (find-variable var))
             (face
              (xref-push-marker-stack)
              (find-face-definition face)))))
    (lisp-mode (call-interactively 'slime-edit-definition))))

(defun my--sexp-modes ()
  (local-set-key (kbd "TAB") (lambda () (interactive) (completion-at-point)))
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



(defun my--lisp-local-keys ()
  (local-set-key (kbd "<f1>") 'slime-hyperspec-lookup)
  (local-set-key (kbd "<f3>") 'slime-disassemble-symbol)
  )
(add-hook 'lisp-mode-hook 'my--lisp-local-keys)
