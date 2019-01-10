(setq-default c-basic-offset 4 c-default-style "linux")
(setq-default tab-width 4 indent-tabs-mode nil)

(require 'company)
(require 'irony)

(add-hook 'c++-mode-hook
         (lambda ()
            (c-set-offset 'case-label '+)
            (setq company-clang-arguments '("-stdlib=libc++" "-std=c++17"))
            (company-mode 1)
            (company-irony 1)
            (irony-mode 1)
            ))
(add-hook 'c-mode-hook
          (lambda ()
            (c-set-offset 'case-label '+)
            (company-mode 1)
            (company-irony 1)
            (irony-mode 1)
            ))


