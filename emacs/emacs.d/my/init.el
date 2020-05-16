;; my-lib
(require 'my-lib)
;; Sets environment variables from shell
(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

;; store backup and autosave files in /tmp directory
(setq backup-directory-alist         `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      ;; Disable backup files (*~ files) and .lock files
      create-lockfiles               nil
      make-backup-files              nil)

;; Disable the toolbar in gui-mode
(tool-bar-mode -1)
;; Disable the menubar
(menu-bar-mode -1)
;; Line/column mode
(setq column-number-mode t)

(when nil
  ;; Emacs server
  (require 'server)
  (unless (server-running-p)
    (server-start)))

(require 'evil)
(evil-mode 1)

;; set default major mode for the *scratch* buffer
(setq initial-major-mode 'emacs-lisp-mode)

;;(require 'company)

(progn
  ;; IDO
  (require 'ido)
  (setq ffap-machine-p-known      'reject
        ffap-machine-p-unknown    'reject
        ido-enable-flex-matching  t
        ido-everywhere            t
        ido-use-filename-at-point 'nil
        ido-create-new-buffer     'always)
  (ido-mode 1))

;; Flymake Cursor
(when nil
  (eval-after-load 'flymake '(require 'flymake-cursor))
  (custom-set-variables
   '(help-at-pt-timer-delay 0.9)
   '(help-at-pt-display-when-idle '(flymake-overlay))))

;; Font stuff
(progn
  (set-language-environment "UTF-8")
  (set-default-coding-systems 'utf-8)
  (let ((font-to-use "Hack 16"))
    (add-to-list 'default-frame-alist (cons 'font font-to-use))
    (set-face-attribute 'default t :font font-to-use))
  (require 'unicode-fonts)
  ;; comment this line out if startup times start taking too long
  ;;(unicode-fonts-setup)
  )

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; auto-refresh buffers that change on disk
(global-auto-revert-mode t)

;; auto-update any new packages.
;;(load (my-dot-emacs "my/packman/packman"))
;;(my-packman-install-my-packages)

;; f5 to compile.
;;(require 'my-compile)

;; auto-complete
(require 'auto-complete)

;; default config for auto-complete
(require 'auto-complete-config)
(ac-config-default)

;; My c and cpp mode options!
(load (my-dot-emacs "my/my-c-and-c++-mode-options"))
;; My lisp mode options
(load (my-dot-emacs "my/my-lisp-modes-options"))
;; My Tupfile mode
(load (my-dot-emacs "my/my-tup-mode"))
(require 'tup-mode)
(add-to-list 'auto-mode-alist '("\\.tup\\'" . tup-mode))
(add-to-list 'auto-mode-alist '("\\Tupfile\\'" . tup-mode))

;; My Malang mode
(load (my-dot-emacs "my/malang-mode"))
(add-to-list 'auto-mode-alist '("\\.ma\\'" . mal-mode))

;; OCaml config
;;(load (my-dot-emacs "my/my-ocaml-mode-config"))

;; irony-mode
;;(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
	'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
	'irony-completion-at-point-async))
;;(add-hook 'irony-mode-hook 'my-irony-mode-hook) ;
;;(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; Auto complete the `end' keyword for ruby control structures
;;(require 'ruby-end)
;;(add-hook 'ruby-mode-hook 'ruby-end-mode)
;;(setq ruby-end-insert-newline nil)

;; GNU Assembler
(setq asm-comment-char ?\#) ; set the comment char to # or ;

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
;;(set-frame-parameter (selected-frame) 'alpha '(100 100))

;; My Keyboard shortcut overrides
(load (my-dot-emacs "my/my-keyboard-overrides"))

;; My global minor-mode that keeps track of time spent editing files and aggregates
;; that time into tables
(load (my-dot-emacs "my/project-time"))
(project-time-mode 1)

(require 'i3-ipc)
(require 'my-media)
(require 'my-layout)
(progn ;; theme stuffs
  (load-theme 'tango-dark t)
  (let ((powerline-path (my-dot-emacs "powerline/")))
    (when (file-directory-p powerline-path)
      (add-to-list 'load-path )
      (require 'powerline)
      (powerline-default-theme))))
(find-file (expand-file-name "~/.notes/general.org"))
