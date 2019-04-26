
(require 'my-i3-manip)

(defun run-cvlc (path &rest extra-args)
  (i3-with-saved-focus
   (let ((orig-buf (current-buffer)))
     (unwind-protect
         (let* ((localname (format "CVLC : %s" path))
                (buf (switch-to-buffer localname))
                (process (make-process :name localname
                                       :buffer buf
                                       :command `("cvlc" ,path ,@extra-args)))
                (inhibit-read-only t))
           (read-only-mode)
           (ansi-color-for-comint-mode-on)
           (comint-mode)
           (set-process-filter process 'comint-output-filter)
           (with-timeout (5)
             (while (= 0 (point-max)) (sleep-for 0.1))))
       (switch-to-buffer orig-buf)))
   (i3-batch-commands
    (let ((wnd (i3-wait-for "^VLC")))
      (when wnd
        (command 'floating wnd t)
        (command 'set-pos wnd 1706 900)
        (command 'set-size wnd 855 496))))))

(when nil
  (run-cvlc "https://www.youtube.com/watch?v=QLZlc4npBfM"))

(defun my-run-cvlc (movie-path &rest extra-args)
  (interactive "fPlay movie: ")
  (let* ((filename (file-name-nondirectory movie-path))
         (localname (format "CVLC : %s" filename))
         (buf (switch-to-buffer-other-window localname))
         (process (make-process :name localname
                                :buffer buf
                                :command `("cvlc" ,movie-path ,@extra-args)))
         (inhibit-read-only t))
    (with-current-buffer buf
      (read-only-mode)
      (ansi-color-for-comint-mode-on)
      (comint-mode))
    (set-process-filter process 'comint-output-filter))
  (i3-batch-commands
   (let ((id (i3-wait-for "^VLC")))
     (when id
       (sleep-for 2)
       (command 'floating id t)
       (command 'set-pos id 1706 900)
       (command 'set-size id 855 496)))))

(defun my-run-streamlink (url &optional quality &rest extra-args)
  (interactive (let ((u (read-string "Url: "))
                     (q (read-string "Quality (720p): " nil nil "720p")))
                 (list u q)))
  (unless (string-match "^[a-zA-Z0-9_]+://" url)
    (setf url (concat "https://twitch.tv/" url)))
  ;;(interactive "sUrl: ")
  (unless quality (setf quality "720p"))
  (let* ((localname (format "Streamlink : %s" url))
         (buf (switch-to-buffer-other-window localname))
         (process (make-process :name localname
                                :buffer buf
                                :command `("streamlink"
                                           "-p" "cvlc"
                                           ,url
                                           ,quality
                                           ,@extra-args)))
         (inhibit-read-only t))
    (with-current-buffer buf
      (read-only-mode)
      (ansi-color-for-comint-mode-on)
      (comint-mode))
    (set-process-filter process 'comint-output-filter)))


(provide 'my-media)

