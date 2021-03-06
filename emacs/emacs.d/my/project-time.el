(defvar project-time-max-idle-time 120
  "Maximum amount of time before the buffer is considered idle.
The time up to and while idle is not tracked.
Default value is 120 seconds (2 minutes)")

(defvar project-time-save-interval 30
  "Time between auto-saves of the PROJECT-TIME-SAVE-FILE.
Default value is 30 seconds")

(defvar project-time-save-file (concat (expand-file-name user-emacs-directory) ".project-time.save.el")
  "The path of the file to save to.
Default value is $EMACS_DIR/.project-time.save.el")

(defvar pt--buffer-times '()
  "alist of (BUFFER-FILE-NAME . (LAST-UPDATE-TIME TIME-TOTAL))")

(defvar pt--time-of-last-save 0
  "When the PT--BUFFER-TIMES list was last saved")

(defun pt--format-timespan (seconds)
  "Helper function to format SECONDS as a human-readable timespan format."
  (format-seconds "%H, %M and %z%S" seconds))

(defun pt--format-date (date-seconds)
  "Helper function to format DATE-SECONDS as a human-readable date format."
  (format-time-string "%F %T" date-seconds))

(defun pt--format-name (path)
  "Helper function to format PATH without the directory, just the filename"
  (file-name-nondirectory path))

(defun pt--parent-directory (path)
  "Helper function to walk a PATH's parent directories.
You can call this with the output of itself to follow parent directories.
Returns NIL when called with /"
  (let ((realpath (file-truename path)))
    (unless (equal "/" realpath)
      (file-name-directory (directory-file-name realpath)))))

(defun int-time () (floor (float-time)))

(defun pt--consolidate-all-times-to-dirs ()
  "Consoldates the time totals for every parent directory recursively up to and including /
Returns an alist of (DIRNAME . TOTAL-TIME)"
  (progn
    (let ((directory-times)
          (add-to-dirtimes
           (lambda (key value)
             (let ((kvp (assoc key directory-times)))
               (if (null kvp)
                   (map-put directory-times key value)
                 (setcdr kvp (+ (cdr kvp) value)))))))
      (dolist (entry pt--buffer-times)
        (letrec ((name (first entry))
                 (last-update-time (cadr entry))
                 (time-total (caddr entry))
                 (directory (pt--parent-directory name)))
          (while directory
            (funcall add-to-dirtimes directory time-total)
            (setq directory (pt--parent-directory directory)))))
      directory-times)))

(defun pt--pretty-format-entry (entry)
  (let ((name (car entry))
        (last-update-time (cadr entry))
        (time-total (caddr entry)))
    (format "Buffer: %-25s Last-Edit: %-25s Total: %s"
            (pt--format-name name)
            (pt--format-date last-update-time)
            (pt--format-timespan time-total))))

(defun pt--prettify-table ()
  (let ((msgs)
        (sorted (stable-sort pt--buffer-times
                             (lambda (a b)
                               (> (caddr a) (caddr b))))))
    (push (format "%-25s%-25s%s" "Buffer" "Last-Edit" "Total Time") msgs)
    (dolist (entry sorted)
      (let ((name (car entry))
            (last-update-time (cadr entry))
            (time-total (caddr entry)))
        (push (format "%-25s%-25s%s"
                      (pt--format-name name)
                      (pt--format-date last-update-time)
                      (pt--format-timespan time-total))
              msgs)))
    (mapconcat 'identity (reverse msgs) "\n")))

(defun pt--pretty-print-table ()
  (message (pt--prettify-table)))

(defun pt--pretty-print-consolidate ()
  (let ((sorted (stable-sort (pt--consolidate-all-times-to-dirs)
                             (lambda (a b) (and (< (cdr a) (cdr b)))))))
    (dolist (entry sorted)
      (let ((dir (car entry))
            (time-total (cdr entry)))
        (message "%s %s" dir (pt--format-timespan time-total))))))

(defun pt--pretty-print-times ()
  (dolist (pretty (mapcar 'pt--pretty-format-entry pt--buffer-times)) 
    (message pretty)))

(defun pt--track ()
  "Tracks the current buffer's filename.
If the current buffer has no filename then this does nothing."
  (let ((name (buffer-file-name)))
    (unless (null name)
      (set-text-properties 0 (length name) nil name)
      (message "%s" name)
      (let ((lst (assoc name pt--buffer-times)))
        (cond
         ((null lst)
          (setq pt--buffer-times (cons (cons name (list (int-time) 0))
                                       pt--buffer-times)))
         ((listp lst)
          (letrec ((now-time (int-time))
                   (last-update-time (cadr lst))
                   (delta-time (- now-time last-update-time))
                   (prev-time (caddr lst))
                   (time-total (+ delta-time prev-time)))
            (if (> delta-time project-time-max-idle-time)
                (setcdr lst (list now-time prev-time))
              (setcdr lst (list now-time time-total)))))))
      'ok)))

(defun pt--save-to-file ()
  "Saves the current PT--BUFFER-TIMES alist to PROJECT-TIME-SAVE-FILE and sets
PT--TIME-OF-LAST-SAVE to the current time."
  (setq pt--time-of-last-save (int-time))
  (save-excursion
    (with-temp-buffer
      (let ((buf (current-buffer)))
        (princ ";;; This is an autogenerated file.\n;;; Any modifications to this file will probably be lost." buf)
        (princ "\n(" buf)
        (dolist (entry pt--buffer-times) (princ (format "\n%S" entry) buf))
        (princ "\n)" buf)
        (write-region nil nil project-time-save-file nil 0)))))


(defun pt--load-from-file ()
  "Attempts to read the contents of PROJECT-TIME-SAVE-FILE and sets PT--BUFFER-TIMES."
  (when (file-exists-p project-time-save-file)
    (with-temp-buffer (insert-file-contents project-time-save-file)
                      (let ((code (buffer-string)))
                        (when (= 0 (length code))
                          (setq code "()"))
                        (let ((times (first (read-from-string code))))
                          (setq pt--buffer-times (if (consp times) times nil)))))))



(defun pt--on-after-change (a b c)
  "Function called by AFTER-CHANGE-FUNCTIONS."
  (progn
    (pt--track)
    (let ((save-time-delta (- (int-time) pt--time-of-last-save)))
      (when (> save-time-delta project-time-save-interval)
        (pt--save-to-file)))))

;;;###autoload
(define-minor-mode project-time-mode
  "minor mode for keeping track of time spent editing buffers."
  :global t
  :lighter " PT"
  nil
  (progn
    (when (null pt--buffer-times)
      (pt--load-from-file))
    (unless (memq 'pt--on-after-change after-change-functions)
      (setq after-change-functions
            (cons 'pt--on-after-change after-change-functions)))))

(provide 'project-time-mode)
