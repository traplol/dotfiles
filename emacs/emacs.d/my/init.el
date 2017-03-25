;; Disable the toolbar in gui-mode
(tool-bar-mode -1)
;; Disable the menubar
(menu-bar-mode -99)
;; Line/column mode
(setq column-number-mode t)
;; Disable backup files (*~ files)
(setq make-backup-files nil)
;; Emacs server
(server-start)

;; Font stuff
(set-default-font "DejaVu Sans Mono 15")

;; auto-refresh buffers that change on disk
(global-auto-revert-mode t)

;; auto-update any new packages.
(load (expand-file-name "~/.emacs.d/my/packman/packman.el"))
(my-packman-install-my-packages)

;; f5 to compile.
(load (expand-file-name "~/.emacs.d/my/my-compile"))

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
(require 'evil-numbers)
(global-set-key (kbd "C-c a") 'evil-numbers/inc-at-pt)
(global-set-key (kbd "C-c x") 'evil-numbers/dec-at-pt)

;; autopair
(require 'autopair)
(autopair-global-mode 1)
(setq autopair-autowrap t)

;; My c and cpp mode options!
(load (expand-file-name "~/.emacs.d/my/my-c-and-c++-mode-options"))
;; My lisp mode options
(load (expand-file-name "~/.emacs.d/my/my-lisp-modes-options"))

;; Inline eval-replace sexp.
(defun my-replace-last-sexp ()
  (interactive)
  (let ((value (eval (preceding-sexp))))
    (kill-sexp -1)
    (insert (format "%S" value))))

;; irony-mode
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(defun my-c-mode-hook ()
  (c-set-offset 'case-label '+)
  (company-mode 1)
  (company-irony 1)
  (flycheck-mode 1)
  (irony-mode 1))

(add-hook 'c++-mode-hook (lambda ()
                           (setq flycheck-clang-language-standard "c++11")))
(add-hook 'c++-mode-hook 'my-c-mode-hook)
(add-hook 'c-mode-hook 'my-c-mode-hook)
(add-hook 'c-mode-hook (lambda ()
                         (setq flycheck-clang-language-standard "c99")))
(add-hook 'objc-mode-hook 'my-c-mode-hook)

(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
	'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
	'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(defun my-go-mode-hook ()
  (setq tab-width 4)
  (setq indent-tabs-mode t)
  (add-hook 'before-save-hook 'gofmt-before-save))

(add-hook 'go-mode-hook 'my-go-mode-hook)


(add-hook 'html-mode-hook 'web-mode)

;; Auto complete the `end' keyword for ruby control structures
(require 'ruby-end)
(add-hook 'ruby-mode-hook 'ruby-end-mode)
(setq ruby-end-insert-newline nil)

;; Add '.scss' file extension to open in css-mode
(add-to-list 'auto-mode-alist '("\\.scss\\'" . css-mode))

(defun my-js-erb-modes ()
  (javascript-mode)
  (web-mode))
;; Add '.js.erb' file extension to open in javascript-mode and web-mode
(add-to-list 'auto-mode-alist '("\\.js.erb\\'" . my-js-erb-modes))

;; GNU Assembler
(setq asm-comment-char ?\#)

;; sudo edit the current buffer
(defun sudo-edit (&optional arg)
  "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root):")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;;
(defadvice ido-find-file (after sudo-find-file activate)
  "Find file as root if necessary."
  (unless (and buffer-file-name
               (file-writable-p buffer-file-name))
    (find-alternate-file (concat "/sudo::" buffer-file-name))))

(require 'ido)
(ido-mode 't)
