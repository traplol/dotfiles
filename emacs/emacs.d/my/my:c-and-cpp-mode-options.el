(defun my:load-c-and-cpp-mode-options ()
  (require 'auto-complete-c-headers)
  (require 'cc-mode)
  (require 'irony)
  (require 'company)
  (require 'flycheck)
  
  (eval-after-load 'company
	'(add-to-list 'company-backends 'company-irony))
  (eval-after-load 'flycheck
	'(add-to-list 'flycheck-checkers 'irony))

  ;; Cc Mode style
  (add-to-list 'load-path (expand-file-name "~/.emacs.d/cc-mode"))
  (setq-default c-basic-offset 4 c-default-style "linux")
  (setq-default tab-width 4 indent-tabs-mode t)

  ;; Keybinds
  (define-key c-mode-base-map (kbd "RET") 'newline-and-indent)
  ;; Opens the company-irony auto-complete suggestion menu.
  (define-key c-mode-base-map (kbd "C-<return>") 'company-irony)
  
  ;; Hooks
  (add-hook 'c-mode-common-hook 'iedit-mode)
  (add-hook 'c-mode-common-hook 'irony-mode)
  (add-hook 'c-mode-common-hook 'flycheck-mode)
  (add-hook 'c-mode-hook
			'(lambda ()
			   ;; init auto-complete-c-headers for c-mode
			   (add-to-list 'ac-sources 'ac-source-c-headers)
			   (add-to-list 'achead:include-directories '"/usr/lib/clang/3.5.0/include")))
  (add-hook 'c++-mode-hook
			'(lambda ()
			   ;; init auto-complete-c-headers for c++-mode
			   (add-to-list 'ac-sources 'ac-source-c-headers)
			   (add-to-list 'achead:include-directories '"/usr/include/c++/4.9.2")
			   (add-to-list 'achead:include-directories '"/usr/include/c++/4.9.2/x86_64-unknown-linux-gnu")
			   (add-to-list 'achead:include-directories '"/usr/include/c++/4.9.2/backward")
			   (add-to-list 'achead:include-directories '"/usr/lib/clang/3.5.0/include")))
  (add-hook 'irony-mode-hook
			'(lambda ()
			   ;; replace symbols with irony-mode's versions
			   (define-key irony-mode-map [remap completion-at-point]
				 'irony-completion-at-point-async)
			   (define-key irony-mode-map [remap complete-symbol]
				 'irony-completion-at-point-async)))
  (add-hook 'irony-mode-hook 'company-mode)
  (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)

  'my:load-c-and-cpp-mode-options
  ) ;; my:load-c-and-cpp-mode-options

(my:load-c-and-cpp-mode-options)
(provide 'my:c-and-cpp-mode-options)
