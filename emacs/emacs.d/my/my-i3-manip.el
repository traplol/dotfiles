
(require 'cl)
(require 'json)
(require 'bindat)

(defvar *i3-bin* "/usr/bin/i3")

(defvar *i3-ipc-socket-path* (string-trim (shell-command-to-string (format "%s --get-socketpath" *i3-bin*))))

(defvar *i3-thing* nil
  "Dynamic scope variable for using an explicit pre-parsed json object in commands. See `I3-WITH-JSON'.
This is useful for using a specific json object with a series of commands without reparsing.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; i3 IPC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst +i3-ipc-run-command+       0)
(defconst +i3-ipc-get-workspaces+    1)
(defconst +i3-ipc-subscribe+         2)
(defconst +i3-ipc-get-outputs+       3)
(defconst +i3-ipc-get-tree+          4)
(defconst +i3-ipc-get-marks+         5)
(defconst +i3-ipc-get-bar-config+    6)
(defconst +i3-ipc-get-version+       7)
(defconst +i3-ipc-get-binding-modes+ 8)
(defconst +i3-ipc-get-config+        9)
(defconst +i3-ipc-send-tick+        10)
(defconst +i3-ipc-sync+             11)

(defun i3--pack-u32 (u32)
  (bindat-pack '((n u32r)) (list (cons 'n u32))))

(defun i3--format-ipc (type &optional message)
  (format "i3-ipc%s%s%s"
          (i3--pack-u32 (length message))
          (i3--pack-u32 type)
          (or message "")))

(defun i3--bindump (binary-string)
  (values (mapconcat (lambda (byte) (format "%02x" byte)) binary-string " ") binary-string))

(defun i3--ipc-raw (binary-string)
  (with-temp-buffer
    (let* ((buf (current-buffer))
           (proc (make-network-process :name "i3-ipc"
                                       :buffer buf
                                       :family 'local
                                       :service *i3-ipc-socket-path*)))
      (process-send-string proc binary-string)
      (accept-process-output proc 1 nil t)
      (prog1 (buffer-string)
        (delete-process proc)))))

(defun i3--ipc-parse (response)
  "Splits RESPONSE into a json-string or NIL on failure"
  (when (> (length response) 14)
    (substring response 14)))

(defun i3--ipc-command (message)
  (i3--ipc-parse (i3--ipc-raw (i3--format-ipc +i3-ipc-run-command+ message))))

(defun i3--ipc-get-tree ()
  (i3--ipc-parse (i3--ipc-raw (i3--format-ipc +i3-ipc-get-tree+))))

(defun i3--ipc-get-workspaces ()
  (i3--ipc-parse (i3--ipc-raw (i3--format-ipc +i3-ipc-get-workspaces+))))

(defun i3--ipc-get-outputs ()
  (i3--ipc-parse (i3--ipc-raw (i3--format-ipc +i3-ipc-get-outputs+))))

(defun i3--ipc-get-marks ()
  (i3--ipc-parse (i3--ipc-raw (i3--format-ipc +i3-ipc-get-marks+))))

(defun i3--ipc-get-bar-config ()
  (i3--ipc-parse (i3--ipc-raw (i3--format-ipc +i3-ipc-get-bar-config+))))

(defun i3--ipc-get-version ()
  (i3--ipc-parse (i3--ipc-raw (i3--format-ipc +i3-ipc-get-version+))))

(defun i3--ipc-get-binding-modes ()
  (i3--ipc-parse (i3--ipc-raw (i3--format-ipc +i3-ipc-get-binding-modes+))))

(defun i3--ipc-get-config ()
  (i3--ipc-parse (i3--ipc-raw (i3--format-ipc +i3-ipc-get-config+))))

(defun i3--ipc-send-tick ()
  (i3--ipc-parse (i3--ipc-raw (i3--format-ipc +i3-ipc-send-tick+))))

(defun i3--ipc-sync ()
  (i3--ipc-parse (i3--ipc-raw (i3--format-ipc +i3-ipc-sync+))))

(defun i3--find-key-value (search-key search-value thing)
  "Recursively searches THING for a HASH-TABLE containing a SEARCH-KEY:SEARCH-VALUE pair."
  (typecase thing
    (list
     (cl-loop for e in thing
              do (let ((found (i3--find-key-value search-key search-value e)))
                   (when found (return found)))))
    (hash-table
     (if (typecase search-value
           (string (string-match search-value (or (gethash search-key thing) "")))
           (otherwise (equal search-value (gethash search-key thing))))
         thing
       (cl-loop for key being the hash-key of thing
                using (hash-value value)
                do (let ((found (i3--find-key-value search-key search-value value)))
                     (when found (return found))))))))

(defun i3-filter (hash-predicate thing)
  "Recursively traverses THING for HASH-TABLEs that HASH-PREDICATE returns non-nil for."
  (let ((results))
    (typecase thing
      (list
       (cl-loop for e in thing
                for found = (i3-filter hash-predicate e)
                do (when found (setq results (append results found)))))
      (hash-table
       (if (funcall hash-predicate thing)
           (push thing results)
         (cl-loop for key being the hash-key of thing
                  using (hash-value value)
                  for found = (i3-filter hash-predicate value)
                  do (when found (setq results (append results found)))))))
    results))

(defun i3--string-json (string)
  "Parses a json STRING into lisp objects.

Objects are converted to HASH-TABLEs where the keys are STRINGs.
Arrays are converted to LISTs."
  (let* ((json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string)
         (json (json-read-from-string string)))
    json))

(defun i3-get-tree ()
  "Gets the i3 data tree and parses it into lisp objects."
  (i3--string-json (i3--ipc-get-tree)))

(defun i3-find-window (window-title)
  "Searches the dynamicly scopped `*I3-THING*' for a window with a title matching WINDOW-TITLE.
WINDOW-TITLE may be a regular expression.

If `*I3-THING*' is nil then the result of calling `I3-GET-TREE' will be used instead."
  (let ((json-thing (or *i3-thing* (i3-get-tree))))
    (i3--find-key-value "name" window-title json-thing)))

(defun i3-wait-for (window-title &optional timeout)
  "Continuously searches for a window with a title matching WINDOW-TITLE or until the TIMEOUT 
has been exceeded. WINDOW-TITLE may be a regular expression. 

The default value for TIMEOUT is 10 (seconds)"
  (or timeout (setq timeout 10))
  (let ((timedout nil))
    (run-at-time (+ timeout (float-time)) nil (lambda (&rest args) (setq timedout t)))
    ;; if *i3-thing* is non-nil before calling this function then i3-find-window would
    ;; always timeout unless they window was already opened. So here we explicitly set
    ;; it to nil so i3-get-tree is always called.
    (let ((*i3-thing* nil))
      (loop for thing = (i3-find-window window-title)
            until (or timedout thing)
            do (sleep-for 0.1)
            finally (return thing)))))

(defun i3-window-p (table)
  (and (hash-table-p thing) (gethash "window" table) t))

(defun i3-get-windows (&optional thing)
  (unless thing
    (setf thing (or *i3-thing* (i3-get-tree))))
  (i3-filter #'i3-window-p thing))

(defun i3-format-command (type id command-string &rest args)
  ;; 'class', 'instance', 'window_role', 'con_id', 'id', 'window_type',
  ;; 'con_mark', 'title', 'urgent', 'workspace', 'tiling', 'floating',
  ;; ']'
  (let ((command (apply #'format command-string args))
        (type-str (or
                   (and (stringp type) type)
                   (case type
                     (class "class")
                     (instance "instance")
                     (window-role "window_role")
                     (con-id "con_id")
                     (id "id")
                     (window-type "window_type")
                     (con-mark "con_mark")
                     (title "title")
                     (urgent "urgent")
                     (workspace "workspace")
                     (tiling "tiling")
                     (floating "floating")
                     (otherwise (error "Unrecognized type %s" type))))))
    (format "[%s=\"%s\"] %s" type-str id command)))

(defun i3-cmd-focus (id &optional type)
  (i3-format-command (or type 'con-id) id "focus"))

(defun i3-cmd-set-pos (id x y &optional type)
  (i3-format-command (or type 'con-id) id "move position %dpx %dpx" x y))

(defun i3-cmd-floating (id mode &optional type)
  (i3-format-command (or type 'con-id) id
                      "floating %s"
                      (cond ((eq 'toggle mode) "toggle")
                            (mode "enable")
                            (t "disable"))))

(defun i3-cmd-set-size (id x y &optional type)
  (i3-format-command (or type 'con-id) id "resize set %dpx %dpx" x y))


(defun i3-focused-window ()
  (let ((json-thing (or *i3-thing* (i3-get-tree))))
    (i3--find-key-value "focused" t json-thing)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Getter helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun i3-get-dimensions (table)
  (let ((rect (gethash "rect" table)))
    (when rect
      (list (gethash "x" rect) (gethash "y" rect)
            (gethash "width" rect) (gethash "height" rect)))))

(defun i3-get-pos (table)
  (let ((rect (gethash "rect" table)))
    (when rect
      (list (gethash "x" rect) (gethash "y" rect)))))

(defun i3-get-size (table)
  (let ((rect (gethash "rect" table)))
    (when rect
      (list (gethash "width" rect) (gethash "height" rect)))))

(defun i3-get-window-title (table) (gethash "name" table))

(defun i3-get-con-id (table) (gethash "id" thing))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro i3-with-saved-focus (&rest body)
  (let ((focused-sym (gensym))
        (result-sym (gensym)))
    `(let ((,focused-sym (i3-focused-window))
           (,result-sym (progn ,@body)))
       (when ,focused-sym
         (i3--ipc-command (i3-cmd-focus (gethash "id" ,focused-sym) 'con-id)))
       ,result-sym)))

(defmacro i3-batch-commands (&rest body)
  "Batches a group of commands into one `I3--IPC-COMMAND' call.

This macro provides a COMMAND function that accepts either a STRING or a SYMBOL and ARGS.

When called with a STRING, the string is literally inserted into the command buffer followed
by a \";\\n\".

When called with a SYMBOL and ARGS `I3-CMD-' is prepended to the symbol and if that function
is FBOUNDP then it is called with ARGS.
"
  `(cl-flet ((command
              (cmd &rest args)
              (insert (typecase cmd
                        (string cmd)
                        (symbol (let ((sym (intern (concat "i3-cmd-" (symbol-name cmd)))))
                                  (unless (fboundp sym) (error "Unbound function %s" sym))
                                  (apply sym args))))
                      ";\n")))
     (i3--ipc-command (with-temp-buffer ,@body (buffer-string)))))

(defmacro i3-serial-commands (&rest body)
  "This macro provides a COMMAND function that accepts either a STRING or a SYMBOL and ARGS
and makes one `I3--IPC-COMMAND' call per COMMAND call.

When called with a STRING, the string is literally sent to i3.

When called with a SYMBOL and ARGS `I3-CMD-' is prepended to the symbol and if that function
is FBOUNDP then it is called with ARGS.
"
  `(cl-flet ((command
              (cmd &rest args)
              (i3--ipc-command (typecase cmd
                                  (string cmd)
                                  (symbol (let ((sym (intern (concat "i3-cmd-" (symbol-name cmd)))))
                                            (unless (fboundp sym) (error "Unbound function %s" sym))
                                            (apply sym args)))))))
     ,@body))

(defmacro i3-with-json (json &rest body)
  "Accepts either a json STRING to be parsed with `I3--STRING-JSON' or a HASHTABLE and
locally binds it to the dynamic scope variable `*I3-THING*'."
  (let ((json-sym (gensym)))
    `(let* ((,json-sym ,json)
            (*i3-thing* (typecase ,json-sym
                          (string (i3--string-json ,json-sym))
                          (hash-table ,json-sym)
                          (otherwise (error "Unrecognized type %s" ,json-sym)))))
       ,@body)))

(when nil

  (i3-with-json (i3-get-tree)
                (i3-with-saved-focus
                 (i3-batch-commands
                  (let ((id (i3-con-id (i3-find-window "^VLC"))))
                    (command 'floating id t)
                    (command 'set-pos id 1706 900)
                    (command 'set-size id 855 496)))))

  )

(provide 'my-i3-manip)
