(setq-default c-basic-offset 4 c-default-style "linux")
(setq-default tab-width 4 indent-tabs-mode t)
;; Bind f5 to complile
(add-hook 'c-mode-common-hook
		  (lambda ()
			(define-key c-mode-map (kbd "<f5>") 'compile)
			(define-key c++-mode-map (kbd "<f5>") 'compile)))
