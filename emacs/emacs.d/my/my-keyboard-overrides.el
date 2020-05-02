(require 'my-lib)

(global-set-key (kbd "M-ESC ESC") nil)

(when (boundp 'evil-insert-state-map)
  ;; Use Emacs keybindings when in insert mode.
  (setcdr evil-insert-state-map nil)
  (define-key evil-insert-state-map [escape] #'evil-normal-state))
(when (boundp 'evil-normal-state-map)
  (define-key evil-normal-state-map (kbd "M-.") nil))

(global-set-key (kbd "C-x C-g") #'my-cd)
(global-set-key (kbd "C-x C-p") #'find-file-at-point)
(global-set-key (kbd "C-x C-m") #'my-replace-last-sexp)
(require 'evil-numbers)
(global-set-key (kbd "C-c a") #'evil-numbers/inc-at-pt)
(global-set-key (kbd "C-c x") #'evil-numbers/dec-at-pt)
(global-set-key (kbd "C-x C-r") (key-lambda 
                                  (call-interactively 'eval-region)
                                  ;; eval-region doesn't write anything to the minibuffer so this
                                  ;; is just feedback for something happening.
                                  (message "evaluated region.")))
(global-set-key (kbd "S-C-h") (key-lambda (shrink-window-horizontally 4)))
(global-set-key (kbd "S-C-l") (key-lambda (enlarge-window-horizontally 4)))
(global-set-key (kbd "S-C-j") (key-lambda (shrink-window 2)))
(global-set-key (kbd "S-C-k") (key-lambda (enlarge-window 2)))

(global-set-key (kbd "M-v") (key-lambda (select-window (previous-window))))
(global-set-key (kbd "M-b") #'other-window)
(global-set-key (kbd "M-n") #'other-window)

(global-set-key (kbd "<f4>") #'comment-region)
(global-set-key (kbd "S-<f4>") #'uncomment-region)
(global-set-key (kbd "<f5>") #'my-toggle-window-dedicated)

(global-set-key (kbd "<f8>") (key-lambda
                               (when (yes-or-no-p "Load frameset? ")
                                 (jump-to-register ?1 t))))
(global-set-key (kbd "<f9>") (key-lambda
                               (when (yes-or-no-p "Save frameset? ")
                                 (frameset-to-register ?1))))


