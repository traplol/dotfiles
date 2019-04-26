;;; Load my packman.
(load (expand-file-name "~/.emacs.d/my/packman/packman.el"))

;;; Load my init.
(load (expand-file-name "~/.emacs.d/my/init.el"))



;; Custom menu stuff beyond this point.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-enabled-themes '(tango-dark))
 '(inhibit-startup-screen t)
 '(org-babel-load-languages '((python . t) (C . t) (emacs-lisp . t)))
 '(package-selected-packages
   '(pdf-tools aggressive-indent centered-cursor-mode rainbow-mode company-irony-c-headers irony-eldoc neotree exec-path-from-shell rmsbolt racket-mode kotlin-mode vue-mode ensime scala-mode php-mode d-mode flymake-cursor flymake-haskell-multi markdown-mode flycheck-haskell haskell-mode unicode-fonts ruby-end rainbow-delimiters racer paredit magit go-mode flycheck-rust flycheck-irony evil-numbers evil company-irony bison-mode autopair auto-complete)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(rainbow-delimiters-depth-1-face ((t (:foreground "green"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "cyan"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "yellow"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "salmon"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "dark green"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "deep pink"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "turquoise4"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "medium blue"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "chocolate")))))
;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line
