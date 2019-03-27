;; my-lib
(load (expand-file-name "~/.emacs.d/my/my-lib"))
(require 'my-lib)
;; Sets environment variables from shell
(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

;; store backup and autosave files in /tmp directory
(setq backup-directory-alist         `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      create-lockfiles               nil)

;; Disable the toolbar in gui-mode
(tool-bar-mode -1)
;; Disable the menubar
(menu-bar-mode -1)
;; Line/column mode
(setq column-number-mode t)
;; Disable backup files (*~ files)
(setq make-backup-files nil)
;; Emacs server
(require 'server)
(unless (server-running-p)
  (server-start))

(require 'evil)
(evil-mode 1)


(require 'company)

;; IDO
(require 'ido)
(setq ido-enable-flex-matching  t
      ido-everywhere            t
      ido-use-filename-at-point 'guess
      ido-create-new-buffer     'always)
(ido-mode 1)

;; Flymake Cursor
(eval-after-load 'flymake '(require 'flymake-cursor))
(custom-set-variables
 '(help-at-pt-timer-delay 0.9)
 '(help-at-pt-display-when-idle '(flymake-overlay)))
;; Font stuff
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
;(set-default-font "DejaVu Sans Mono 10")
;(require 'unicode-fonts)
;(unicode-fonts-setup)

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; auto-refresh buffers that change on disk
(global-auto-revert-mode t)

;; auto-update any new packages.
(load (expand-file-name "~/.emacs.d/my/packman/packman.el"))
(my-packman-install-my-packages)

;; f5 to compile.
;;(load (expand-file-name "~/.emacs.d/my/my-compile"))

;; auto-complete
(require 'auto-complete)

;; default config for auto-complete
(require 'auto-complete-config)
(ac-config-default)

;; yasnippet
;(require 'yasnippet)
;(yas-global-mode 1)

;; autopair
;(require 'autopair)
;(autopair-global-mode 1)
;(setq autopair-autowrap t)

;; My c and cpp mode options!
(load (expand-file-name "~/.emacs.d/my/my-c-and-c++-mode-options"))
;; My lisp mode options
(load (expand-file-name "~/.emacs.d/my/my-lisp-modes-options"))
;; My Tupfile mode
(load (expand-file-name "~/.emacs.d/my/my-tup-mode"))
(require 'tup-mode)
(add-to-list 'auto-mode-alist '("\\.tup\\'" . tup-mode))
(add-to-list 'auto-mode-alist '("\\Tupfile\\'" . tup-mode))

;; My Malang mode
(load (expand-file-name "~/.emacs.d/my/malang-mode"))
(add-to-list 'auto-mode-alist '("\\.ma\\'" . mal-mode))

;; OCaml config
(load (expand-file-name "~/.emacs.d/my/my-ocaml-mode-config"))

;; irony-mode
;;(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
	'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
	'irony-completion-at-point-async))
;;(add-hook 'irony-mode-hook 'my-irony-mode-hook) ;
;;(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

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
(setq asm-comment-char ?\#) ; set the comment char to # or ;

;; Rust mode
(require 'rust-mode)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
(setq rust-format-on-save nil)
(add-hook 'rust-mode-hook 'racer-mode)
(add-hook 'racer-mode-hook 'eldoc-mode)
(add-hook 'racer-mode-hook 'company-mode)
(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
(setq company-tooltip-align-annotations t)

;; Haskell mode
(require 'haskell-mode)
;;(add-to-list 'auto-mode-alist '("\\.hs\\'" . haskell-mode))
;;(eval-after-load 'flycheck '(add-hook 'flycheck-mode-hook 'flycheck-haskell-setup))
;;(add-hook 'haskell-mode-hook 'flycheck-mode-hook)
(require 'flymake-haskell-multi)
(add-hook 'haskell-mode-hook 'flymake-haskell-multi-load)

;; (require 'omnisharp)
;; (require 'csharp-mode)
;; (eval-after-load 'company '(add-to-list 'company-backends 'company-omnisharp))
;; (defun my-csharp-mode-setup ()
;;   (omnisharp-mode)
;;   (company-mode)
;;   (flycheck-mode)

;;   (setq indent-tabs-mode nil)
;;   (setq c-syntactic-indentation t)
;;   (c-set-style "ellemtel")
;;   (setq c-basic-offset 4)
;;   (setq truncate-lines t)
;;   (setq tab-width 4)
;;   (setq evil-shift-width 4)
;;   (electric-pair-local-mode 1)
;;   (set (make-local-variable 'compile-command) "dotnet build")
;;   (local-set-key (kbd "C-c C-c") 'recompile)
;;   (local-set-key (kbd "<f5>") 'recompile))
;; (add-hook 'csharp-mode-hook 'my-csharp-mode-setup t)


(require 'eww)
(defun my-eww-browse-url (url &optional new-window)
  (when new-window
    (pop-to-buffer
     (generate-new-buffer (format "*eww-%s*" (url-host (url-generic-parse-url
                                                        (eww--dwim-expand-url url))))))
    (eww-mode))
  (eww url)
  (let ((title (plist-get eww-data :title)))
    (when title
      (rename-buffer (format "*eww : %s *" title) t))))
(setq browse-url-browser-function 'my-eww-browse-url
      browse-url-new-window-flag t)


;; Experimenting with transparency
(set-frame-parameter (selected-frame) 'alpha '(98 98))

;; My Keyboard shortcut overrides
(load (expand-file-name "~/.emacs.d/my/my-keyboard-overrides"))

(load (expand-file-name "~/.emacs.d/my/project-time"))
(project-time-mode 1)

(find-file (expand-file-name "~/.notes/general.org"))
