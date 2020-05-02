
(defun my-layout-stream-watching ()
  (interactive)
  (delete-other-windows)
  (evil-window-vsplit)
  (evil-window-vsplit)
  (evil-window-move-far-right)
  (evil-window-split)
  (switch-to-buffer "*scratch*")
  (let (window (get-buffer-window (current-buffer)))
    (set-window-dedicated-p window t))
  )

(defun my-layout-save (&optional register)
  (interactive)
  (frameset-to-register (or register ?1)))

(defun my-layout-load (&optional register)
  (interactive)
  (jump-to-register (or register ?1) t))

(provide 'my-layout)
