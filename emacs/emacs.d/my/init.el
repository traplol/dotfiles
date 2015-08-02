;; Disable the toolbar in gui-mode
(tool-bar-mode -1)
;; Disable the menubar
(menu-bar-mode -99)

;; auto-update any new packages.
(load (expand-file-name "~/.emacs.d/my/packman/packman.el"))
(my:packman-install-my-packages)

;; f5 to compile.
(load (expand-file-name "~/.emacs.d/my/my:compile"))

;; auto-complete
(require 'auto-complete)

;; default config for auto-complete
(require 'auto-complete-config)
(ac-config-default)

;; yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;; evil-mode
(add-to-list 'load-path (expand-file-name "~/.emacs.d/evil"))
(require 'evil)
(evil-mode 1)
;; Use Emacs keybindings when in insert mode.
(setcdr evil-insert-state-map nil)
(define-key evil-insert-state-map [escape] 'evil-normal-state)

;; autopair
(require 'autopair)
(autopair-global-mode 1)
(setq autopair-autowrap t)

;; My c and cpp mode options!
(load (expand-file-name "~/.emacs.d/my/my:c-and-c++-mode-options"))
;; My lisp mode options
(load (expand-file-name "~/.emacs.d/my/my:lisp-modes-options"))

;; Inline eval-replace sexp.
(defun my:replace-last-sexp ()
  (interactive)
  (let ((value (eval (preceding-sexp))))
    (kill-sexp -1)
    (insert (format "%S" value))))

;; irony-mode
(defun my:c-mode-hook ()
  (company-mode 1)
  (company-irony 1)
  (flycheck-mode 1)
  (irony-mode 1))

(add-hook 'c++-mode-hook 'my:c-mode-hook)
(add-hook 'c-mode-hook 'my:c-mode-hook)
(add-hook 'objc-mode-hook 'my:c-mode-hook)

(defun my:irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
	'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
	'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'my:irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(add-hook 'html-mode-hook 'web-mode)
