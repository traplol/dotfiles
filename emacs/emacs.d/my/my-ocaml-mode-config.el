;;;
;;; Copied and adapted from the realworldocaml.org 2nd edition book in development
;;;   http://dev.realworldocaml.org/install.html
;;;

(require 'cl)

(defun setup-ocaml-mode-config (tuareg-dir)
  (add-to-list 'load-path tuareg-dir)
  (require 'tuareg)
  (require 'tuareg-menhir)
  
  (add-to-list 'auto-mode-alist '("\\.ml[i]?$" . tuareg-mode))

  ;; -- Tweaks for OS X -------------------------------------
  ;; Tweak for problem on OS X where Emacs.app doesn't run the right
  ;; init scripts when invoking a sub-shell
  (cond
   ((eq window-system 'ns) ; macosx
    ;; Invoke login shells, so that .profile or .bash_profile is read
    (setq shell-command-switch "-lc")))

  ;; -- opam and utop setup --------------------------------
  ;; Setup environment variables using opam
  (dolist
      (var (car (read-from-string
                 (shell-command-to-string "opam config env --sexp"))))
    (setenv (car var) (cadr var)))
  ;; Update the emacs path
  (setq exec-path (split-string (getenv "PATH") path-separator))
  ;; Update the emacs load path
  (push (concat (getenv "OCAML_TOPLEVEL_PATH")
                "/../../share/emacs/site-lisp") load-path)
  ;; Automatically load utop.el
  (require 'utop)
  (autoload 'utop "utop" "Toplevel for OCaml" t)
  (autoload 'utop-setup-ocaml-buffer "utop" "Toplevel for OCaml" t)
  (add-hook 'tuareg-mode-hook 'utop-minor-mode)

  (define-key utop-mode-map (kbd "C-c C-e") 'utop-eval-phrase)
  (define-key tuareg-mode-map (kbd "C-c C-e") 'utop-eval-phrase)
  (message "tuareg-mode setup")

  ;; -- merlin setup ---------------------------------------

  (setq opam-share (substring (shell-command-to-string "opam config var share") 0 -1))
  (add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))
  (require 'merlin)

  ;; Enable Merlin for ML buffers
  (add-hook 'tuareg-mode-hook 'merlin-mode)

  ;; So you can do it on a mac, where `C-<up>` and `C-<down>` are used
  ;; by spaces.
  (define-key merlin-mode-map
    (kbd "C-c <up>") 'merlin-type-enclosing-go-up)
  (define-key merlin-mode-map
    (kbd "C-c <down>") 'merlin-type-enclosing-go-down)
  (set-face-background 'merlin-type-face "#88FF44")

  ;; -- enable auto-complete -------------------------------
  ;; Not required, but useful along with merlin-mode
  (require 'auto-complete)
  (add-hook 'tuareg-mode-hook 'auto-complete-mode)
  (message "merlin setup")
  (message "setup-ocaml-mode-config finished"))

(let* ((path (expand-file-name "~/workspace/elisp/tuareg/tuareg.el"))
       (exists? (file-exists-p path)))
  (if (not exists?)
      (warn (format "tuareg: file doesn't exist!: %s" path))
    (setup-ocaml-mode-config (file-name-directory path))))
