
(require 'i3-ipc)


(defmacro my-media-with-vlc-top-right (timeout &rest body)
  (declare (indent defun))
  (let ((result-sym (make-symbol "--result--"))
        (wnd-sym (make-symbol "--window--"))
        (active-ws-sym (make-symbol "--active-ws--")))
    `(i3-with-saved-focus
      (let ((,result-sym (progn ,@body)))
        (i3-serial-commands
         (let ((,wnd-sym (i3-wait-for "^VLC" ,timeout))
               (,active-ws-sym (i3-active-workspace)))
           (when ,wnd-sym
             (command 'move-to-workspace ,wnd-sym ,active-ws-sym)
             (command 'floating ,wnd-sym t)
             (command 'set-pos ,wnd-sym 1706 900)
             (command 'set-size ,wnd-sym 855 496))))))))

(defun my-run-cvlc (path &rest extra-args)
  (interactive "fPlay movie: ")
  (let ((orig-buf (current-buffer)))
    (unwind-protect
        (save-selected-window
          (let* ((localname (format "CVLC : %s" path))
                 (buf (switch-to-buffer localname))
                 (process (make-process :name localname
                                        :buffer buf
                                        :command `("cvlc" ,path ,@extra-args)))
                 (inhibit-read-only t))
            (with-current-buffer buf
              (read-only-mode)
              (ansi-color-for-comint-mode-on)
              (comint-mode))
            (set-process-filter process 'comint-output-filter)))
      (switch-to-buffer orig-buf)))
  (my-media-with-vlc-top-right 3
    (sleep-for 2)))

(defun my-run-streamlink (url &optional quality &rest extra-args)
  (interactive (let ((u (read-string "Url: "))
                     (q (read-string "Quality (720p): " nil nil "720p")))
                 (list u q)))
  (unless (string-match "^[a-zA-Z0-9_]+://" url)
    (setf url (concat "https://twitch.tv/" url)))
  ;;(interactive "sUrl: ")
  (unless quality (setf quality "720p"))
  
  (let ((orig-buf (current-buffer)))
    (unwind-protect
        (save-selected-window 
          (let* ((localname (format "Streamlink : %s" url))
                 (buf (switch-to-buffer localname))
                 (process (make-process :name localname
                                        :buffer buf
                                        :command `("streamlink"
                                                   "-p" "cvlc"
                                                   ,url ,quality ,@extra-args)))
                 (inhibit-read-only t))
            (with-current-buffer buf
              (read-only-mode)
              (ansi-color-for-comint-mode-on)
              (comint-mode))
            (set-process-filter process 'comint-output-filter)))
      (switch-to-buffer orig-buf)))
  (my-media-with-vlc-top-right 3
    (sleep-for 2)))


(when nil
  (defvar --startrek-n-- 0)
  (defvar --startrek-eps-- (directory-files-recursively "~/videos" "Star Trek TNG S.+\\.mkv"))
  (defun star-trek-next ()
    (prog1 (elt --startrek-eps-- --startrek-n--)
      (incf --startrek-n--)))
  (defun star-trek-prev ()
    (progn (decf --startrek-n--)
           (elt --startrek-eps-- --startrek-n--)))


  (elt --startrek-eps-- (setq --startrek-n-- 17))
  (my-run-cvlc (star-trek-next))

  (my-run-streamlink "asmongold")
  (my-run-cvlc "https://www.youtube.com/watch?v=QLZlc4npBfM"))

(provide 'my-media)
