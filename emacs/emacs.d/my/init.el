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

;; IDO
(require 'ido)
(let ()
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t)
  (setq ido-use-filename-at-point 'guess)
  (setq ido-create-new-buffer 'always)
  (ido-mode 1))

;; Flymake Cursor
(eval-after-load 'flymake '(require 'flymake-cursor))
(custom-set-variables
 '(help-at-pt-timer-delay 0.9)
 '(help-at-pt-display-when-idle '(flymake-overlay)))
;; Font stuff
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-default-font "DejaVu Sans Mono 10")
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

(add-hook 'c++-mode-hook
          (lambda ()
            (c-set-offset 'case-label '+)
            (company-mode 1)
            (company-irony 1)
            (flycheck-mode 1)
            (irony-mode 1)
            (setq flycheck-clang-language-standard "c++1z")
            (setq flycheck-clang-include-path
                  (list (expand-file-name "~/workspace/c++/header-libs/")))))
;(add-hook 'c++-mode-hook 'my-c-mode-hook)
;(add-hook 'c-mode-hook 'my-c-mode-hook)
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
(setq asm-comment-char ?\#) ; set the comment char to # or ;

;; Rust mode
(require 'rust-mode)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
(setq rust-format-on-save nil)
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)
(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
(setq company-tooltip-align-annotations t)

;; Haskell mode
(require 'haskell-mode)
;(add-to-list 'auto-mode-alist '("\\.hs\\'" . haskell-mode))
;(eval-after-load 'flycheck '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))
;(add-hook 'haskell-mode-hook #'flycheck-mode-hook)
(require 'flymake-haskell-multi)
(add-hook 'haskell-mode-hook 'flymake-haskell-multi-load)

;; camelCase to snake_case
(defun split-name (s)
  (split-string
   (let ((case-fold-search nil))
     (downcase
      (replace-regexp-in-string "\\([a-z]\\)\\([A-Z]\\)" "\\1 \\2" s)))
   "[^A-Za-z0-9]+"))
(defun snake-case-string (s) (mapconcat 'downcase   (split-name s) "_"))
(defun snake-case-region (begin end) (interactive "r")
  (let* ((word (buffer-substring begin end))
         (underscored (snake-case-string word)))
    (save-excursion
      (widen) ; break out of the subregion so we can fix every usage of the function
      (replace-string word underscored nil (point-min) (point-max)))))

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")

;; Experimenting with transparency
(set-frame-parameter (selected-frame) 'alpha '(98 98))

(find-file (expand-file-name "~/.notes/general.org"))

(global-set-key (kbd "C-x C-p") #'find-file-at-point)
