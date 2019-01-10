(require 'my-lib)

(when (boundp 'evil-insert-state-map)
  ;; Use Emacs keybindings when in insert mode.
  (setcdr evil-insert-state-map nil)
  (define-key evil-insert-state-map [escape] 'evil-normal-state))

;; Key shortcuts
(global-set-key (kbd "C-x C-p") 'find-file-at-point)
(global-set-key (kbd "C-x C-m") 'my-replace-last-sexp)
(require 'evil-numbers)
(global-set-key (kbd "C-c a") 'evil-numbers/inc-at-pt)
(global-set-key (kbd "C-c x") 'evil-numbers/dec-at-pt)
(global-set-key (kbd "C-x C-r") (lambda ()
                                  (interactive)
                                  (call-interactively 'eval-region)
                                  ;; eval-region doesn't write anything to the minibuffer so this
                                  ;; is just feedback for something happening.
                                  (message "evaluated region.")))
(global-set-key (kbd "S-C-h") (lambda () (interactive) (shrink-window-horizontally 4)))
(global-set-key (kbd "S-C-l") (lambda () (interactive) (enlarge-window-horizontally 4)))
(global-set-key (kbd "S-C-j") (lambda () (interactive) (shrink-window 2)))
(global-set-key (kbd "S-C-k") (lambda () (interactive) (enlarge-window 2)))

(global-set-key (kbd "M-b") 'other-window)

(defun gud-keybinds ()
  (local-set-key (kbd "<f6>") 'gdb)
  (local-set-key (kbd "<f2>") 'gud-break)
  (local-set-key (kbd "<f7>") 'gud-run)
  (local-set-key (kbd "<f8>") 'gud-cont)
  (local-set-key (kbd "<f10>") 'gud-next)
  (local-set-key (kbd "<f11>") 'gud-step))
(add-hook 'c-mode-hook 'gud-keybinds)
(add-hook 'c++-mode-hook 'gud-keybinds)
(add-hook 'gud-mode-hook 'gud-keybinds)

(global-set-key (kbd "C-c d") 'my-toggle-window-dedicated)
