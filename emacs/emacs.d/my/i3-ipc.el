
(require 'cl)
(require 'json)
(require 'bindat)

(defun i3-get-socket-path ()
  "Attempts to get the i3 socket path by first reading the 'I3SOCK' 
environment variable, then trying to shell out 'i3 --get-socketpath'"
  (or (getenv "I3SOCK" (window-frame))
      (string-trim (shell-command-to-string "i3 --get-socketpath"))))

(defconst *i3-ipc-socket-path* (i3-get-socket-path))

(defvar *i3-thing* nil
  "Dynamic scope variable for using an explicit pre-parsed json object in commands. See `i3-with-json'.
This is useful for using a specific json object with a series of commands without reparsing.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; i3 IPC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(lexical-let ((i3-ipc-message-bindat-spec '((magic str 6)
                                            (length u32r)
                                            (type u32r)
                                            (payload str (length))))
              (coding-system 'emacs-internal))
  
  (defun i3--pack-message (type &optional message)
    (unless (and (integerp type) (<= 0 type 11))
      (error "i3-ipc: TYPE expected to be integer: 0 <= TYPE <= 11 got: %S" type))
    (unless message (setq message ""))
    (let ((bytes (encode-coding-string message coding-system t)))
      (bindat-pack i3-ipc-message-bindat-spec
                   (list (cons 'magic "i3-ipc")
                         (cons 'length (length bytes))
                         (cons 'type type)
                         (cons 'payload bytes)))))

  (defun i3--unpack-message (message)
    "Occasionally event messages will be received immediately after 
one-another,this handles that case and parses MESSAGE into a list 
of event messages represented as an alist with these symbols as keys:

magic: \"i3-ipc\"
length: length in bytes of the payload
type: message type
payload: a string containing the response payload"
    (let* ((start 0)
           (bytes (encode-coding-string message coding-system t))
           (n (length bytes))
           (messages))
      (while (< start n)
        (let* ((unpacked (bindat-unpack i3-ipc-message-bindat-spec
                                        (substring bytes start)))
               (payload (assoc 'payload unpacked)))
          (setf (cdr payload) (decode-coding-string (cdr payload) coding-system t))
          (incf start (+ 6 4 4 (alist-get 'length unpacked)))
          (push unpacked messages)))
      (reverse messages))))

(defun i3--bindump (binary-string)
  (values (mapconcat (lambda (byte) (format "%02x" byte)) binary-string " ") binary-string))

(defun i3--make-ipc-proc (process-name process-buffer)
  "Tries, and retries up to 5 times, to open an IPC socket using `*i3-ipc-socket-path'"
  (let ((proc (loop repeat 5
                    do (condition-case err
                           (return (make-network-process :name process-name
                                                         :buffer process-buffer
                                                         :family 'local
                                                         :service *i3-ipc-socket-path*))
                         ((file-error)
                          (message "Warning: (i3-ipc) %S" err)
                          (setq *i3-ipc-socket-path* (i3-get-socket-path)))))))
    (unless proc (error "i3-ipc: Unable to create IPC process: %S" *i3-ipc-socket-path*))
    proc))

(defun i3--ipc-raw (binary-string)
  (with-temp-buffer
    (let* ((buf (current-buffer))
           (proc (i3--make-ipc-proc "i3-ipc" buf)))
      (process-send-string proc binary-string)
      (accept-process-output proc 1 nil t)
      (prog1 (buffer-string)
        (delete-process proc)))))

(defun i3--event-parse-and-dispatch (proc msg handler)
  (dolist (unpacked (i3--unpack-message msg))
    (let ((type (alist-get 'type unpacked))
          (payload (alist-get 'payload unpacked)))
      (when (= 1 (ash type -31)) ;; bit[31] == 1 == event
        (let ((friendly-event-type (alist-get (logand type #x7f)
                                              '((0 . workspace)
                                                (1 . output)
                                                (2 . mode)
                                                (3 . window)
                                                (4 . barconfig-update)
                                                (5 . binding)
                                                (6 . shutdown)
                                                (7 . tick)))))
          (if friendly-event-type
              (funcall handler proc friendly-event-type payload)
            (message "Warning: (i3-ipc) unrecognized event type: %s" (logand type #x7f))))))))

(defun i3--ipc-subscribe (event-handler events)
  "Subscribe to i3 IPC events.
EVENT-HANDLER should be a function that accept 3 arguments: the i3-ipc network-process,
the event type in the form of a symbol, and the raw message payload from i3 (this
should be a regular json object).

EVENTS should be either the symbol 'all which means the subscribe to _all_ events or
a list of symbols: 'workspace, 'output, 'mode, 'window, 'barconfig-update, 'binding,
'shutdown, and/or 'tick"
  (setq events
        (if (eq 'all events)
            "[\"workspace\",\"output\",\"mode\",\"window\",\"barconfig_update\",\"binding\",\"shutdown\",\"tick\"]"
          (json-encode-array
           (loop for e in (remove-duplicates events)
                 collect (case e
                           (workspace "workspace")
                           (output "output")
                           (mode "mode")
                           (window "window")
                           (barconfig-update "barconfig_update")
                           (binding "binding")
                           (shutdown "shutdown")
                           (tick "tick")
                           (otherwise (error "i3-ipc: Unknown event type: %S" e)))))))
  (save-excursion
    (let* ((buf (generate-new-buffer (generate-new-buffer-name "*i3-ipc-events*")))
           (proc (i3--make-ipc-proc "i3-ipc-subscriber" buf)))
      (switch-to-buffer-other-window buf)
      (set-process-filter proc (lexical-let ((the-event-handler event-handler))
                                 (lambda (p m)
                                   (i3--event-parse-and-dispatch p m the-event-handler))))
      (set-process-sentinel proc (lambda (proc msg)
                                   (let ((buffer (process-buffer proc)))
                                     (cond ((or (equal "finished\n" msg)
                                                (search "exited" msg)
                                                (search "failed" msg)
                                                (search "(core dumped)" msg))
                                            (delete-process proc)
                                            (and buffer (buffer-name buffer)
                                                 (kill-buffer buffer)))
                                           ((equal "deleted\n" msg)
                                            (and buffer (buffer-name buffer)
                                                 (kill-buffer buffer)))))))
      (process-send-string proc (i3--pack-message +i3-ipc-subscribe+ events))
      (list proc buf))))

(defun i3--ipc-naive-parse (response)
  "Splits RESPONSE into a json-string or nil on failure"
  (when (> (length response) 14)
    (substring response 14)))

(defun i3--ipc-command (message)
  (when (/= 0 (length message))
    (i3--ipc-naive-parse (i3--ipc-raw (i3--pack-message +i3-ipc-run-command+ message)))))

(defun i3--ipc-get-tree ()
  (i3--ipc-naive-parse (i3--ipc-raw (i3--pack-message +i3-ipc-get-tree+))))

(defun i3--ipc-get-workspaces ()
  (i3--ipc-naive-parse (i3--ipc-raw (i3--pack-message +i3-ipc-get-workspaces+))))

(defun i3--ipc-get-outputs ()
  (i3--ipc-naive-parse (i3--ipc-raw (i3--pack-message +i3-ipc-get-outputs+))))

(defun i3--ipc-get-marks ()
  (i3--ipc-naive-parse (i3--ipc-raw (i3--pack-message +i3-ipc-get-marks+))))

(defun i3--ipc-get-bar-config (&optional bar-id)
  (i3--ipc-naive-parse (i3--ipc-raw (i3--pack-message +i3-ipc-get-bar-config+ bar-id))))

(defun i3--ipc-get-version ()
  (i3--ipc-naive-parse (i3--ipc-raw (i3--pack-message +i3-ipc-get-version+))))

(defun i3--ipc-get-binding-modes ()
  (i3--ipc-naive-parse (i3--ipc-raw (i3--pack-message +i3-ipc-get-binding-modes+))))

(defun i3--ipc-get-config ()
  (i3--ipc-naive-parse (i3--ipc-raw (i3--pack-message +i3-ipc-get-config+))))

(defun i3--ipc-send-tick (&optional message)
  (i3--ipc-naive-parse (i3--ipc-raw (i3--pack-message +i3-ipc-send-tick+ message))))

(defun i3--ipc-sync (window-id random-id)
  (let ((json (format "{\"window\":%d, \"rnd\":%d}"
                      window-id
                      random-id)))
    (i3--ipc-naive-parse (i3--ipc-raw (i3--pack-message +i3-ipc-sync+ json)))))

(defalias 'i3-ipc-subscribe #'i3--ipc-subscribe)
(defalias 'i3-send-tick #'i3--ipc-send-tick)

(defun i3--find-key-value (search-key search-value thing)
  "Recursively searches THING for a hash-table containing a SEARCH-KEY:SEARCH-VALUE pair."
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


(defun i3-find (hash-predicate thing)
  "Recursively searches THING for a hash-table that HASH-PREDICATE returns non-nil for."
  (typecase thing
    (list
     (cl-loop for e in thing
              do (let ((found (i3-find hash-predicate e)))
                   (when found (return found)))))
    (hash-table
     (if (funcall hash-predicate thing)
         thing
       (cl-loop for key being the hash-key of thing
                using (hash-value value)
                do (let ((found (i3-find hash-predicate value)))
                     (when found (return found))))))))

(defun i3-filter (hash-predicate thing)
  "Recursively traverses and collects THING for hash-tables that HASH-PREDICATE returns non-nil for."
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

+ objects are converted to hash-tables where the keys are strings
+ arrays are converted to lists
+ null is nil
+ false is :json-false
+ true is t"
  (when string
    (let* ((json-object-type 'hash-table)
           (json-array-type 'list)
           (json-key-type 'string)
           (json (json-read-from-string string)))
      json)))

(defun i3-version ()
  "Gets i3 version info and returns it as a list (MAJOR MINOR PATCH HUMAN-READABLE)"
  (let ((version (i3--string-json (i3--ipc-get-version))))
    (when version
      (list (gethash "major" version)
            (gethash "minor" version)
            (gethash "patch" version)
            (gethash "human_readable" version)))))

(defun i3-loaded-config-file ()
  "Gets the currently loaded i3 config file path"
  (let ((version (i3--string-json (i3--ipc-get-version))))
    (when version
      (gethash "loaded_config_file_name" version))))

(defun i3-binding-modes ()
  "Gets the i3 binding modes as a list"
  (i3--string-json (i3--ipc-get-binding-modes)))

(defun i3-tree ()
  "Gets the i3 data tree and parses it into lisp objects."
  (i3--string-json (i3--ipc-get-tree)))

(defun i3-find-window (window-title)
  "Searches the dynamicly scopped `*i3-thing*' for a window with a title matching WINDOW-TITLE.
WINDOW-TITLE may be a regular expression.

If `*i3-thing*' is nil then the result of calling `i3-tree' will be used instead."
  (let ((json-thing (or *i3-thing* (i3-tree))))
    (i3--find-key-value "name" window-title json-thing)))

(defun i3-wait-for (window-title &optional timeout)
  "Continuously searches for a window with a title matching WINDOW-TITLE or until the TIMEOUT 
has been exceeded. WINDOW-TITLE may be a regular expression. 

The default value for TIMEOUT is 10 (seconds)"
  (unless timeout
    (setq timeout 10))
  ;; if *i3-thing* is non-nil before calling this function then i3-find-window would
  ;; always timeout unless they window was already opened. So here we explicitly set
  ;; it to nil so i3-tree is always called.
  (let ((*i3-thing* nil))
    (with-timeout (timeout nil)
      (loop for thing = (i3-find-window window-title)
            until thing
            do (sleep-for 0.1)
            finally (return thing)))))

(defun i3-window-p (thing)
  "Tries to determine if THING is an i3 window"
  (and (hash-table-p thing)
       (gethash "window" thing)
       t))

(defun i3-windows (&optional thing)
  "Returns the windows of THING where THING is a hash-table representing an i3 tree 
response.

If THING is nil then try dynamic scoped `*i3-thing*' and if that is nil, make a fresh
call to `i3-tree'"
  (i3-filter #'i3-window-p (or thing *i3-thing* (i3-tree))))

(defun i3-focused-window (&optional thing)
  "Returns the focused window of THING where THING is a hash-table representing an i3 
tree response.

If THING is nil then try dynamic scoped `*i3-thing*' and if that is nil, make a fresh
call to `i3-tree'"
  (let ((json-thing (or *i3-thing* (i3-tree))))
    (i3--find-key-value "focused" t json-thing)))

(defun i3-workspace-p (thing)
  "Tries to determine if THING is an i3 workspace"
  (and (hash-table-p thing)
       (equal "workspace" (gethash "type" thing))
       t))

(defun i3-workspaces (&optional thing)
  "Returns the workspaces of THING where THING is a hash-table representing an i3 tree 
response.

If THING is nil then try dynamic scoped `*i3-thing*' and if that is nil, make a fresh
call to `i3-tree'"
  (i3-filter #'i3-workspace-p (or thing *i3-thing* (i3-tree))))

(defun i3-active-workspace (&optional thing)
  "Returns the active workspace of THING where THING is a hash-table representing an i3
tree response. Note that the returned object should return nil when passed to
`i3-get-focused-p'. 

If THING is nil then try dynamic scoped `*i3-thing*' and if that is nil, make a fresh
call to `i3-tree'"
  (let* ((workspaces (i3--string-json (i3--ipc-get-workspaces)))
         (active-ws (i3--find-key-value "focused" t workspaces))
         (active-ws-name (i3-get-name active-ws)))
    (i3-find (lambda (table)
               (and (i3-workspace-p table)
                    (equal active-ws-name (i3-get-name table))))
             (or thing *i3-thing* (i3-tree)))))

(defun i3-bar-ids ()
  "Asks i3 for a list of bar-ids that can be used with `i3-bar-config'"
  (i3--string-json (i3--ipc-get-bar-config)))

(defun i3-bar-config (bar-id)
  "Asks i3 for a hash-table containing the bar configuration for the given BAR-ID."
  (i3--string-json (i3--ipc-get-bar-config bar-id)))



(defvar i3--sync-events nil)
(defvar x-client-message-handlers (make-hash-table))

(defun x-register-client-message-handler (message-type handler)
  (or (functionp handler) (error "HANDLER is not a function. %S" handler))
  (let ((atom-value (etypecase message-type
                      (integer message-type)
                      (string (x-get-name-atom message-type))
                      (symbol (x-get-name-atom message-type)))))
    (push handler (gethash atom-value x-client-message-handlers))))

(defun x-handle-one-client-message (event)
  (let ((message-type (alist-get 'message-type event)))
    (dolist (handler (gethash message-type x-client-message-handlers))
      (funcall handler event))))

(lexical-let ((registered-handler-p nil))
  (when (eq 'x (window-system))
    (x-register-client-message-handler
     "I3_SYNC"
     (lambda (e)
       (let* ((data (alist-get 'data e))
              (window (aref data 0))
              (rnd (aref data 1)))
         (push (cons window rnd) i3--sync-events))))
    (setq registered-handler-p t))

  (defun i3-sync-wait (&optional timeout)
    "Uses i3 sync-protocol and waits for the response event from X.
Default TIMEOUT is 1 second."
    (when registered-handler-p)
    (unless timeout (setq timeout 1))
    (let ((id (frame-parameter (window-frame) 'window-id)))
      (cond (id
             (let* ((my-id (string-to-number id))
                    (rnd (random (ash 1 31)))
                    (cell (cons my-id rnd))
                    (deadline (+ timeout (float-time)))
                    (foundp nil))
               (i3--ipc-sync my-id rnd)
               (while (and (not foundp) (< (float-time) deadline))
                 (if (member cell i3--sync-events)
                     (progn (setq i3--sync-events (remove cell i3--sync-events))
                            (setq foundp t))
                   (sleep-for 0.1)))
               (message (if foundp "(i3-ipc) synchronized" "(i3-ipc) sync timeout"))
               foundp))
            (t
             (message "Warning: (i3-ipc): unable to determine window id.")
             nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun i3-format-command (type thing command-string &rest args)
  "Formats a command string.
TYPE should be either a string or a symbol representing the identifier type.

THING represents the identifier to use, 
if THING is a hash-table then the window's id will be used and TYPE will be ignored,
if THING is nil then this is equivalent to calling just calling (format COMMAND-STRING ARGS...)

COMMAND-STRING and ARGS is a format specifier to be applied with `format'"
  ;; 'class', 'instance', 'window_role', 'con_id', 'id', 'window_type',
  ;; 'con_mark', 'title', 'urgent', 'workspace', 'tiling', 'floating',
  ;; ']'
  (when (hash-table-p thing)
    (let ((id (i3-get-id thing)))
      (unless id (error "i3-ipc: Unable to extract id from THING: %S" thing))
      (setq thing id)
      (setq type "id")))
  (let ((command (apply #'format command-string args)))
    (if (null thing)
        command
      (let ((type-str (or
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
                         (floating "floating"))
                       (error "i3-ipc: Unrecognized type %S" type))))
        (format "[%s=\"%s\"] %s" type-str thing command)))))


(defmacro i3-defcmd (name arglist &rest body)
  "Defines two functions. Use like DEFUN.

NAME is a symbol without any 'i3-' prefix.

Defuns i3-cmd-NAME with the aglist of ARGLIST and body of BODY, this 
function is expected to return a string representing the *actual* i3 
command.

Defuns i3-NAME with the optional docstring in the CAR position of BODY 
and forwards any arguments to i3-cmd-NAME then passing the result to 
`i3--ipc-command' to be dispatched to i3.

\(fn NAME ARGLIST &optional DOCSTRING &rest BODY)"
  (declare (indent defun))
  (unless (symbolp name) (error "i3-ipc: Cannot define command with non-symbol name %S" name))
  (let* ((uses-rest)
         (argsyms (remove-if
                   (lambda (e)
                     (when (and (symbolp e)
                                (eq ?& (aref (symbol-name e) 0)))
                       (if (eq '&rest e)
                           (setq uses-rest t))
                       t))
                   arglist))
         (cmd-sym (intern (concat "i3-cmd-" (symbol-name name))))
         (exec-sym (intern (concat "i3-" (symbol-name name))))
         (docstring (when (stringp (car-safe body))
                      (concat (car body)
                              "\n\nForwards arguments to `i3-cmd-" (symbol-name name) "'and opens a new "
                              "\nIPC socket to dispatch the command to i3."))))

    `(progn (defun ,cmd-sym ,arglist ,@body)
            (defun ,exec-sym ,arglist
              ,@(when docstring (list docstring))
              (i3--ipc-command
               ,(if uses-rest
                    (cons 'apply (cons (list 'function cmd-sym) argsyms))
                  (cons cmd-sym argsyms)))))))




(i3-defcmd focus (thing &optional type-hint)
  "Tells i3 to focus THING

TYPE-HINT may be a string or one of the symbols: class instance window-role con-id 
id window-type con-mark title urgent workspace tiling floating"

  (i3-format-command (or type-hint 'id) thing "focus"))

(i3-defcmd floating (thing mode &optional type-hint)
  "Tells i3 to set the floating attribute for THING. MODE may be the symbol 
'toggle to toggle fullscreen,nil to disable fullscreen, or otherwise to to 
enable fullscreen.

TYPE-HINT may be a string or one of the symbols: class instance window-role con-id 
id window-type con-mark title urgent workspace tiling floating"
  (i3-format-command (or type-hint 'id) thing
                     "floating %s"
                     (cond ((eq 'toggle mode) "toggle")
                           (mode "enable")
                           (t "disable"))))

(i3-defcmd set-pos (thing x y &optional type-hint)
  "Tells i3 to set the poisition of THING to absolute position X,Y

TYPE-HINT may be a string or one of the symbols: class instance window-role con-id 
id window-type con-mark title urgent workspace tiling floating"
  (i3-format-command (or type-hint 'id) thing "move position %dpx %dpx" x y))

(i3-defcmd set-size (thing width height &optional type-hint)
  "Tells i3 to set the size of THING to WIDTH x HEIGHT

TYPE-HINT may be a string or one of the symbols: class instance window-role con-id 
id window-type con-mark title urgent workspace tiling floating"
  (i3-format-command (or type-hint 'id) thing "resize set %dpx %dpx" width height))

(i3-defcmd reload ()
  "Tells i3 to reload, this also reloads i3's config."
  "reload")

(i3-defcmd move-to-workspace (thing workspace &optional type-hint)
  "Tells i3 to move THING to WORKSPACE.

TYPE-HINT may be a string or one of the symbols: class instance window-role con-id 
id window-type con-mark title urgent workspace tiling floating"
  (let ((ws-name (etypecase workspace
                   (string workspace)
                   (hash-table (i3-get-name workspace)))))
    (i3-format-command (or type-hint 'id)
                       thing
                       "move container to workspace %s"
                       ws-name)))


(i3-defcmd move-to-scratcpad (thing &optional type-hint)
  "Tells i3 to move THING to the scratchpad.

TYPE-HINT may be a string or one of the symbols: class instance window-role con-id 
id window-type con-mark title urgent workspace tiling floating"
  (i3-format-command (or type-hint 'id)
                     thing
                     "move scratchpad"))

(i3-defcmd show-scratchpad ()
  "Tells i3 to show the scratchpad"
  "scratchpad show")

(i3-defcmd kill (thing &optional type-hint)
  "Tells i3 to kill THING

TYPE-HINT may be a string or one of the symbols: class instance window-role con-id 
id window-type con-mark title urgent workspace tiling floating"
  (i3-format-command (or type-hint 'id)
                     thing
                     "kill"))

(i3-defcmd fullscreen (thing mode &optional type-hint)
  "Tells i3 to set the fullscreen attribute for THING. MODE may be 
symbol 'toggle to toggle fullscreen,nil to disable fullscreen, or 
otherwise to to enable fullscreen.

TYPE-HINT may be a string or one of the symbols: class instance window-role con-id 
id window-type con-mark title urgent workspace tiling floating"
  (i3-format-command (or type-hint 'id) thing
                     "fullscreen %s"
                     (cond ((eq 'toggle mode) "toggle")
                           (mode "enable")
                           (t "disable"))))

(i3-defcmd exit ()
  "Tells i3 to exit.
There's a reasonable chance this will succeed at exiting 
i3 and error in elisp"
  "exit")

(i3-defcmd restart ()
  "Tells i3 to restart.
There's a reasonable chance this will succeed at restarting
i3 and error in elisp"
  "restart")

(i3-defcmd switch-mode (mode-name)
  "Tells i3 to switch to mode MODE-NAME."
  (format "mode \"%s\"" mode-name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Getter helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun i3-get-dimensions (table)
  "Returns a list of (X Y WIDTH HEIGHT)"
  (let ((rect (gethash "rect" table)))
    (when rect
      (list (gethash "x" rect)
            (gethash "y" rect)
            (gethash "width" rect)
            (gethash "height" rect)))))

(defun i3-get-pos (table)
  "Returns a list of (X Y)"
  (let ((rect (gethash "rect" table)))
    (when rect
      (list (gethash "x" rect)
            (gethash "y" rect)))))

(defun i3-get-size (table)
  "Returns a list of (WIDTH HEIGHT)"
  (let ((rect (gethash "rect" table)))
    (when rect
      (list (gethash "width" rect)
            (gethash "height" rect)))))

(defun i3-get-children (table)
  "Returns a list of child nodes from TABLE

For a workspace this should correspond to all windows in that workspace"
  (gethash "nodes" table))

(defun i3-get-name (table)
  "Returns the name property of TABLE

This should correspond to the name of a window or workspace"
  (gethash "name" table))

(defun i3-get-num (table)
  "Returns the num property of TABLE

This should correspond to the internal number of a workspace"
  (gethash "num" table))

(defun i3-get-urgent-p (table)
  "Returns the urgent property of TABLE

This should correspond to whether or not the window or workspace is tagged as urgent"
  (not (eq :json-false (or (gethash "urgent" table)
                           :json-false))))

(defun i3-get-focused-p (table)
  "Returns the focused property of TABLE

This should correspond to whether or not the window is in focus.

Note that i3 appears to set the current workspace as focused=false in the `i3-tree'
structure. If the focused workspace is desired use `i3-active-workspace' instead."
  (not (eq :json-false (or (gethash "focused" table)
                           :json-false))))

(defun i3-get-output (table)
  "Returns the output property of TABLE

This should correspond to the name of the screen/display"
  (gethash "output" table))

(defun i3-get-con-id (table)
  "Returns the container id property of TABLE

This should correspond to the internal container id of a window or workspace"
  (gethash "id" table))

(defun i3-get-id (table)
  "Returns the window id property of TABLE

This should correspond to the X window-id"
  (gethash "window" table))

(defun i3-get-window-properties (table)
  "Returns the window properties of TABLE"
  (gethash "window_properties" table))

(defun i3-get-window-class (table)
  "Retruns the window class of TABLE

This may be used as an identifier in other i3- functions when TYPE-HINT is the 
symbol 'class"
  (let ((props (i3-get-window-properties table)))
    (when props (gethash "class" props))))

(defun i3-get-window-instance (table)
  "Retruns the window instance of TABLE

This may be used as an identifier in other i3- functions when TYPE-HINT is the 
symbol 'instance"
  (let ((props (i3-get-window-properties table)))
    (when props (gethash "instance" props))))

(defun i3-get-window-title (table)
  "Retruns the window title of TABLE

This may be used as an identifier in other i3- functions when TYPE-HINT is the 
symbol 'title"
  (let ((props (i3-get-window-properties table)))
    (when props (gethash "title" props))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro i3-with-saved-focus (&rest body)
  "Saves the currently focused window (if any), executes BODY, and then restores the
originally focused window. The result of BODY is returned normally."
  (let ((focused-sym (gensym))
        (result-sym (gensym)))
    `(let* ((,focused-sym (i3-focused-window))
            (,result-sym (unwind-protect (progn ,@body)
                           (when ,focused-sym
                             (i3--ipc-command (i3-cmd-focus ,focused-sym))))))
       ,result-sym)))

(defmacro i3-batch-commands (&rest body)
  "Batches a group of commands into one `i3--ipc-command' call.

This macro provides a COMMAND function that accepts either a string or a symbol and args.

Note that while the body of this macro executes serially, the i3 effects of COMMAND will not
happen until _after_ this macro completes. See `i3-serial-commands'.

When called with a string, the string is literally inserted into the command buffer followed
by a \";\\n\".

When called with a symbol and args `i3-cmd-*' is prepended to the symbol and if that function
is `fboundp' then it is called with ARGS.
"
  `(cl-flet ((command
              (cmd &rest args)
              (insert (typecase cmd
                        (string cmd)
                        (symbol (let ((sym (intern (concat "i3-cmd-" (symbol-name cmd)))))
                                  (unless (fboundp sym) (error "i3-ipc: Unbound function %s" sym))
                                  (apply sym args))))
                      ";\n")))
     (i3--ipc-command (with-temp-buffer ,@body (buffer-string)))
     (i3-sync-wait)))

(defmacro i3-serial-commands (&rest body)
  "This macro provides a COMMAND function that accepts either a string or a symbol and args
and makes one `i3--ipc-command' call per COMMAND call.

Note that unlike `i3-batch-commands', the effects of calling COMMAND happen immediately.

When called with a string, the string is literally sent to i3.

When called with a symbol and args `i3-cmd-*' is prepended to the symbol and if that function
is `fboundp' then it is called with args.
"
  `(cl-flet ((command (cmd &rest args)
                      (i3--ipc-command (etypecase cmd
                                         (string cmd)
                                         (symbol (let ((sym (intern (concat "i3-cmd-" (symbol-name cmd)))))
                                                   (unless (fboundp sym) (error "i3-ipc: Unbound function %s" sym))
                                                   (apply sym args)))))
                      (i3-sync-wait)))
     ,@body))

(defmacro i3-with-json (json &rest body)
  "Accepts either a json string to be parsed with `i3--string-json' or a hashtable and
locally binds it to the dynamic scope variable `*i3-thing*'."
  (declare (indent defun))
  (let ((json-sym (gensym)))
    `(let* ((,json-sym ,json)
            (*i3-thing* (etypecase ,json-sym
                          (string (i3--string-json ,json-sym))
                          (hash-table ,json-sym))))
       ,@body)))

(provide 'i3-ipc)
