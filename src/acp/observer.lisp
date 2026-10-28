(in-package #:autolith)

;;;; -- ACP Presentation --

(defvar *acp-current-tool-call-id* nil
  "The provider call identifier bound within one ACP tool execution.")

(defparameter *acp-permission-timeout-seconds* 120
  "Maximum wait for an editor permission response.")

(defclass acp-observer (agent-observer)
  ((session :initarg :session :reader acp-observer-session :type acp-session
            :documentation "The session receiving this turn's updates.")
   (turn-sequence :initarg :turn-sequence :accessor acp-observer-turn-sequence
                  :documentation "The durable user-message sequence qualifying tool identifiers.")
   (streamed-text :initform (text-buffer-create) :accessor acp-observer-streamed-text :type string
                  :documentation "Text already emitted for the current provider response.")
   (update-buffer :reader acp-observer-update-buffer :type agentcomms:acp-update-buffer
                  :documentation "The ordered buffer coalescing visible thought fragments.")
   (thought-state :initform nil :accessor acp-observer-thought-state
                  :type          (member nil :streaming :boundary)
                  :documentation "Whether contiguous thoughts continue or cross a provider response boundary.")
   (response-counter :initform 0 :accessor acp-observer-response-counter :type (integer 0)
                     :documentation "The number of provider responses presented in this turn.")
   (agent-message-id :initform nil :accessor acp-observer-agent-message-id
                     :type          (or null string)
                     :documentation "The version 2 message id of the current agent response.")
   (thought-message-id :initform nil :accessor acp-observer-thought-message-id
                       :type          (or null string)
                       :documentation "The version 2 message id of the current reasoning stream.")
   (open-tool-calls :initform (make-hash-table :test #'equal) :reader acp-observer-open-tool-calls
                    :documentation "Version 2 tool calls awaiting a terminal update.")
   (open-terminals :initform (make-hash-table :test #'equal) :reader acp-observer-open-terminals
                   :documentation "Version 2 shell tool calls and their display terminal ids.")
   (compaction-counter :initform 0 :accessor acp-observer-compaction-counter :type (integer 0)
                       :documentation "The number of compactions announced in this turn.")
   (compaction-id :initform nil :accessor acp-observer-compaction-id
                  :type          (or null string)
                  :documentation "The current version 2 compaction id.")
   (lock :initform (make-lock "Autolith ACP updates") :reader acp-observer-lock
         :documentation "The lock serializing short presentation updates, never permission waits."))
  (:documentation "An incremental ACP presentation sink with editor authorization."))

(defmethod initialize-instance :after ((observer acp-observer) &key)
  "Bind each observer's update buffer to its session's wire envelope."
  (let ((session (acp-observer-session observer)))
    (setf (slot-value observer 'update-buffer)
          (agentcomms:make-agent-update-buffer
           (acp-session-service session) (acp-session-identifier session)))))

(-> acp-observer-create (acp-session) acp-observer)
(defun acp-observer-create (session)
  "Create a fresh presentation sink for SESSION's next user turn."
  (make-instance 'acp-observer :session session
                 :turn-sequence         (conversation-next-sequence
                                         (application-conversation (acp-session-application session)))))

(-> acp-observer-v2-p (acp-observer) boolean)
(defun acp-observer-v2-p (observer)
  "Return true when OBSERVER presents an ACP version 2 session."
  (acp-session-v2-p (acp-observer-session observer)))

(-> acp-observer--next-message-id (acp-observer string) string)
(defun acp-observer--next-message-id (observer prefix)
  "Return a fresh version 2 message id for OBSERVER with PREFIX."
  (format nil "~A~D-~D" prefix
          (acp-observer-turn-sequence observer)
          (acp-observer-response-counter observer)))

(-> acp-observer--send (acp-observer hash-table) null)
(defun acp-observer--send (observer update)
  "Send one update, including terminal tool outcomes during cancellation."
  (let ((session (acp-observer-session observer)))
    (unless (acp-service-closed-p (acp-session-service session))
      (agentcomms:update-buffer-send (acp-observer-update-buffer observer) update)
      (setf (acp-observer-thought-state observer)
            (when (eq ':agent-thought-chunk (agentcomms:acp-update-kind update)) ':streaming))))
  nil)

(-> acp-observer-flush (acp-observer) null)
(defun acp-observer-flush (observer)
  "Deliver pending thoughts before a permission request or terminal prompt response."
  (unless (acp-service-closed-p (acp-session-service (acp-observer-session observer)))
    (agentcomms:update-buffer-flush (acp-observer-update-buffer observer)))
  nil)

(-> acp-tool-identifier (integer string) string)
(defun acp-tool-identifier (turn-sequence identifier)
  "Qualify IDENTIFIER by its durable turn, including during session replay."
  (format nil "~D:~A" turn-sequence identifier))

(-> acp-tool-kind (string) keyword)
(defun acp-tool-kind (name)
  "Map a native tool's operation to an ACP presentation category."
  (cond
    ((string= name "resource.read")
     ':read)
    ((string= name "resource.edit")
     ':edit)
    ((string= name "shell.run")
     ':execute)
    ((uiop:string-prefix-p "search." name)
     ':search)
    ((string= name "web.run")
     ':search)
    ((string= name "web_extra.gist")
     ':fetch)
    ((string= name "plan.update")
     ':think)
    (t
     ':other)))

(-> acp-observer--arguments (acp-observer string) hash-table)
(defun acp-observer--arguments (observer identifier)
  "Return the persisted provider arguments of IDENTIFIER as a JSON object."
  (let* ((application (acp-session-application (acp-observer-session observer)))
         (call        (find identifier (reverse (conversation-input-items
                                                 (application-conversation application)))
                            :key (lambda (item) (json-get item "call_id")) :test #'equal)))
    (handler-case (json-decode (or (and call (json-get call "arguments")) "{}"))
      (error ()
        (json-object)))))

(-> acp-tool--argument-preview (t) string)
(defun acp-tool--argument-preview (value)
  "Render a bounded argument summary before sanitizing or measuring display cells."
  (let ((text (cond
                ((stringp value)
                 (subseq value 0 (min (length value) 192)))
                ((hash-table-p value)
                 (format nil "{~D fields}" (hash-table-count value)))
                ((and (vectorp value) (plusp (length value)) (every #'stringp value))
                 (let ((joined (format nil "~{~A~^ ~}"
                                       (map 'list
                                            (lambda (item)
                                              (subseq item 0 (min (length item) 192)))
                                            (subseq value 0 (min 4 (length value)))))))
                   (subseq joined 0 (min (length joined) 192))))
                ((vectorp value)
                 (format nil "[~D items]" (length value)))
                ((and (integerp value) (> (integer-length value) 64))
                 "<integer>")
                (t
                 (json-encode value)))))
    (text-cell-prefix (sanitize-text text :single-line-p t) 48)))

(-> acp-tool-title (string t) string)
(defun acp-tool-title (name arguments)
  "Return NAME with a bounded, single-line preview of its principal argument."
  (let* ((key     (when (json-object-p arguments)
                    (or (find-if (lambda (key) (nth-value 1 (gethash key arguments)))
                                 '("forms" "form" "command" "uri" "query" "patterns" "path"
                                   "url" "system" "name" "task" "text"))
                        (first (sort (loop for key being the hash-keys of arguments
                                          collect key)
                                     #'string<)))))
         (preview (when key
                    (acp-tool--argument-preview (gethash key arguments)))))
    (if (non-empty-string-p preview)
        (format nil "~A · ~A" name preview)
        name)))

(-> acp-observer--tool-report
    (acp-observer &key (:identifier string) (:title string) (:status keyword)
                  (:arguments t) (:output (option string)) (:content (option list)))
    hash-table)
(defun acp-observer--tool-report
    (observer &key identifier title status arguments output content)
  "Construct a tool report using the same identity for progress and authorization."
  (agentcomms:acp-tool-call
   (acp-tool-identifier (acp-observer-turn-sequence observer) identifier)
   (acp-tool-title title arguments) :name title :kind (acp-tool-kind title) :status status
   :raw-input arguments :raw-output output
   :content (or content
                (when output
                  (list (agentcomms:acp-tool-call-content
                         (agentcomms:acp-text-content output)))))))

(defmethod agent-observer-text ((observer acp-observer) text)
  "Stream assistant text and retain its length for nonstreamed response reconciliation.

Empty text sends nothing: an empty message chunk would split the thought
fragments that providers interleave with empty content deltas."
  (acp-session-check-cancelled (acp-observer-session observer))
  (when (plusp (length text))
    (with-lock-held ((acp-observer-lock observer))
      (text-buffer-append (acp-observer-streamed-text observer) text)
      (acp-observer--send
       observer
       (if (acp-observer-v2-p observer)
           (agentcomms:acp-v2-update-agent-message-chunk
            (agentcomms:acp-text-content text)
            (or (acp-observer-agent-message-id observer) "agent"))
           (agentcomms:acp-update-agent-message (agentcomms:acp-text-content text))))))
  nil)

(defmethod agent-observer-reasoning ((observer acp-observer) text)
  "Stream visible reasoning, separating contiguous summaries from distinct responses."
  (acp-session-check-cancelled (acp-observer-session observer))
  (when (plusp (length text))
    (with-lock-held ((acp-observer-lock observer))
      (acp-observer--send
       observer
       (if (acp-observer-v2-p observer)
           (agentcomms:acp-v2-update-agent-thought-chunk
            (agentcomms:acp-text-content
             (if (eq ':boundary (acp-observer-thought-state observer))
                 (format nil "~2%~A" text)
                 text))
            (or (acp-observer-thought-message-id observer) "thought"))
           (agentcomms:acp-update-agent-thought
            (agentcomms:acp-text-content
             (if (eq ':boundary (acp-observer-thought-state observer))
                 (format nil "~2%~A" text)
                 text)))))))
  nil)

(-> acp-observer--plan (acp-observer) null)
(defun acp-observer--plan (observer)
  "Project the durable workspace plan after its native publication."
  (let* ((session (acp-observer-session observer))
         (plan    (plan-load (application-configuration (acp-session-application session))))
         (entries (when plan
                    (mapcar (lambda (step)
                              (agentcomms:acp-plan-entry
                               (plan-step-text step)
                               :status (ecase (plan-step-status step)
                                         (:pending
                                          ':pending)
                                         (:doing
                                          ':in-progress)
                                         (:done
                                          ':completed))))
                            (workspace-plan-steps plan)))))
    (acp-observer--send
     observer
     (if (acp-observer-v2-p observer)
         (agentcomms:acp-v2-update-plan "workspace" entries)
         (agentcomms:acp-update-plan entries))))
  nil)

(defmethod agent-observer-status ((observer acp-observer) status details)
  "Translate provider and durable tool lifecycle events to ACP updates."
  ;; Completion events must be sent before the cancelled prompt response.
  (unless (eq status ':tool-call-completed)
    (acp-session-check-cancelled (acp-observer-session observer)))
  (with-lock-held ((acp-observer-lock observer))
    (case status
      (:user-message-persisted
       (let ((sequence (getf details :sequence)))
         (setf (acp-observer-turn-sequence observer) sequence)
         (when (and (acp-observer-v2-p observer)
                    (not (getf details :automatic-p)))
           (acp-observer--send
            observer
            (agentcomms:acp-v2-update-user-message
             (list (agentcomms:acp-text-content (getf details :text)))
             (format nil "u~D" sequence))))))
      (:provider-request-started
       (application-idle-compaction-note-started
        (acp-session-application (acp-observer-session observer)))
       (incf (acp-observer-response-counter observer))
       (setf (acp-observer-agent-message-id observer)
             (acp-observer--next-message-id observer "a")
             (acp-observer-thought-message-id observer)
             (acp-observer--next-message-id observer "t"))
       (text-buffer-clear (acp-observer-streamed-text observer))
       (when (acp-observer-thought-state observer)
         (setf (acp-observer-thought-state observer) ':boundary)))
      (:provider-request-completed
       (let* ((application   (acp-session-application (acp-observer-session observer)))
              (configuration (application-configuration application))
              (usage         (getf details :usage)))
         (application-idle-compaction-note-completed application usage)
         (when (and (acp-observer-v2-p observer) (json-object-p usage))
           (let* ((input  (or (json-get usage "input_tokens") 0))
                  (output (or (json-get usage "output_tokens") 0))
                  (used   (or (json-get usage "total_tokens") (+ input output)))
                  (size   (config :context-window configuration)))
             (when (and (integerp used) (plusp used) (integerp size) (plusp size))
               (acp-observer--send
                observer (agentcomms:acp-update-usage used size)))))))
      (:assistant-response-persisted
       (let ((text (getf details :text)) (streamed (acp-observer-streamed-text observer)))
         (when (and (stringp text) (uiop:string-prefix-p streamed text)
                    (< (length streamed) (length text)))
           (let ((tail (subseq text (length streamed))))
             (acp-observer--send
              observer
              (if (acp-observer-v2-p observer)
                  (agentcomms:acp-v2-update-agent-message-chunk
                   (agentcomms:acp-text-content tail)
                   (or (acp-observer-agent-message-id observer) "agent"))
                  (agentcomms:acp-update-agent-message
                   (agentcomms:acp-text-content tail))))))))
      ((:compaction-started :compaction-completed)
       (when (acp-observer-v2-p observer)
         (if (eq status ':compaction-started)
             (progn
               (incf (acp-observer-compaction-counter observer))
               (setf (acp-observer-compaction-id observer)
                     (format nil "compaction-~D-~D"
                             (acp-observer-turn-sequence observer)
                             (acp-observer-compaction-counter observer)))
               (acp-observer--send
                observer
                (agentcomms:acp-v2-update-compaction
                 (acp-observer-compaction-id observer) ':in-progress)))
             (when (acp-observer-compaction-id observer)
               (acp-observer--send
                observer
                (agentcomms:acp-v2-update-compaction
                 (acp-observer-compaction-id observer) ':completed))
               (setf (acp-observer-compaction-id observer) nil))))
       (setf (application-idle-compaction-episode
              (acp-session-application (acp-observer-session observer))) nil))
      (:tool-call-started
       (let* ((arguments   (acp-observer--arguments observer (getf details :call-id)))
              (identifier  (acp-tool-identifier (acp-observer-turn-sequence observer)
                                                (getf details :call-id)))
              (terminal-id (when (and (acp-observer-v2-p observer)
                                      (string= (getf details :tool) "shell.run"))
                             (format nil "term:~A" identifier)))
              (report      (acp-observer--tool-report
                            observer :identifier (getf details :call-id)
                                     :title      (getf details :tool)
                                     :status     ':in-progress
                                     :arguments  arguments
                                     :content    (when terminal-id
                                                   (list (agentcomms:acp-terminal-content
                                                          terminal-id))))))
         (when terminal-id
           (setf (gethash identifier (acp-observer-open-terminals observer)) terminal-id)
           (acp-observer--send
            observer
            (agentcomms:acp-v2-update-terminal
             terminal-id
             :command (json-get arguments "command")
             :cwd     (let ((directory (json-get arguments "directory")))
                        (and (stringp directory)
                             (namestring (uiop:ensure-directory-pathname directory)))))))
         (when (acp-observer-v2-p observer)
           (setf (gethash identifier (acp-observer-open-tool-calls observer)) report))
         (acp-observer--send
          observer
          (if (acp-observer-v2-p observer)
              (agentcomms:acp-v2-update-tool-call report)
              (agentcomms:acp-update-tool-call report)))))
      (:tool-call-completed
       (let* ((identifier  (acp-tool-identifier (acp-observer-turn-sequence observer)
                                                (getf details :call-id)))
              (output      (getf details :output))
              (terminal-id (gethash identifier (acp-observer-open-terminals observer)))
              (content     (if terminal-id
                               (list (agentcomms:acp-terminal-content terminal-id))
                               (list (agentcomms:acp-tool-call-content
                                      (agentcomms:acp-text-content output)))))
              (update      (agentcomms:acp-tool-call-update
                            identifier
                            :status     (if (getf details :success-p) ':completed ':failed)
                            :raw-output output
                            :content    content)))
         (when terminal-id
           (acp-observer--send
            observer
            (agentcomms:acp-v2-update-terminal
             terminal-id
             :output      (agentcomms:acp-v2-terminal-output
                           (usb8-array-to-base64-string
                            (utf8-string-to-octets (or output ""))))
             :exit-status (agentcomms:acp-v2-terminal-exit-status
                           (if (getf details :success-p) 0 1) nil)))
           (remhash identifier (acp-observer-open-terminals observer)))
         (when (acp-observer-v2-p observer)
           (remhash identifier (acp-observer-open-tool-calls observer)))
         (acp-observer--send
          observer
          (if (acp-observer-v2-p observer)
              (agentcomms:acp-v2-update-tool-call update)
              (agentcomms:acp-update-tool-call-progress update)))
         (when (string= (getf details :tool) "plan.update")
           (acp-observer--plan observer))))))
  nil)

(-> acp-observer--session-info (acp-observer) null)
(defun acp-observer--session-info (observer)
  "Send the durable conversation title and last activity as a version 2 update."
  (when (acp-observer-v2-p observer)
    (let* ((conversation (application-conversation
                          (acp-session-application
                           (acp-observer-session observer))))
           (title        (conversation-title conversation))
           (updated      (conversation-last-activity-at conversation)))
      (when title
        (acp-observer--send
         observer
         (agentcomms:acp-update-session-info
          :title      title
          :updated-at (and (integerp updated) (universal-time->rfc3339 updated)))))))
  nil)

(-> acp-observer-cancel-open-tools (acp-observer) null)
(defun acp-observer-cancel-open-tools (observer)
  "Mark every version 2 tool call still open as cancelled."
  (when (acp-observer-v2-p observer)
    (let ((open (acp-observer-open-tool-calls observer)))
      (maphash (lambda (identifier report)
                 (declare (ignore report))
                 (acp-observer--send
                  observer
                  (agentcomms:acp-v2-update-tool-call
                   (agentcomms:acp-tool-call-update identifier :status ':cancelled))))
               open)
      (clrhash open)))
  nil)

(-> acp-observer--permission
    (acp-observer hash-table list &key (:title (or null string)) (:subject t))
    (option string))
(defun acp-observer--permission (observer report options &key title subject)
  "Request a bounded editor decision and accept only an offered option."
  (let ((session (acp-observer-session observer)))
    (acp-session-check-cancelled session)
    (acp-observer-flush observer)
    (let ((choice
           (handler-case
               (multiple-value-bind (outcome identifier)
                   (if (acp-observer-v2-p observer)
                       (agentcomms:agent-request-permission-v2
                        (acp-session-service session) (acp-session-identifier session)
                        :title   (or title "Allow this operation?")
                        :subject (or subject
                                     (agentcomms:acp-v2-tool-call-permission-subject report))
                        :options options
                        :timeout *acp-permission-timeout-seconds*)
                       (agentcomms:agent-request-permission
                        (acp-session-service session) (acp-session-identifier session)
                        report :options options :timeout *acp-permission-timeout-seconds*))
                 (when (and (eq outcome ':selected)
                            (find identifier options :test #'equal
                                  :key                     (lambda (option)
                                                             (agentcomms:json-get option "optionId"))))
                   identifier))
             (agentcomms:acp-error ()
               nil))))
      (acp-session-check-cancelled session)
      choice)))

(-> acp-observer--approval
    (acp-observer string &key (:arguments hash-table) (:key t) (:subject t))
    boolean)
(defun acp-observer--approval (observer title &key arguments key subject)
  "Ask for exact operation approval, retaining allow-always only for this session."
  (let* ((session (acp-observer-session observer))
         (saved   (with-lock-held ((acp-session-lock session))
                    (gethash key (acp-session-permissions session))))
         (report  (acp-observer--tool-report
                   observer :identifier (or *acp-current-tool-call-id* (make-identifier))
                   :title title :status ':pending :arguments arguments)))
    (acp-session-check-cancelled session)
    (or saved
        (let ((choice
               (acp-observer--permission
                observer report
                (list (agentcomms:acp-permission-option "once" "Allow once" ':allow-once)
                      (agentcomms:acp-permission-option "session" "Allow exact operation this session" ':allow-always)
                      (agentcomms:acp-permission-option "deny" "Reject" ':reject-once))
                :title   title
                :subject subject)))
          (when (equal choice "session")
            (with-lock-held ((acp-session-lock session))
              (setf (gethash key (acp-session-permissions session)) t)))
          (and (member choice '("once" "session") :test #'equal) t)))))

(defmethod agent-observer-authorize-command ((observer acp-observer) command directory)
  "Apply native process policy with editor approval in ask mode."
  (let* ((session     (acp-observer-session observer))
         (application (acp-session-application session)))
    (acp-session-check-cancelled session)
    (case (acp-session-mode session)
      (:full-access
       ':full-access)
      (:sandboxed
       (if (application--command-sandbox-available-p) ':sandboxed ':deny))
      (:auto
       (application--automatic-command-decision
        (nth-value 0
                   (permissions-model-classify-command
                    command directory :provider (application-provider application)
                    :configuration              (application-configuration application)
                    :sandbox-available-p        (application--command-sandbox-available-p)))))
      (otherwise
       (if (acp-observer--approval
            observer "shell.run"
            :arguments (json-object "command" command
                                    "directory" (namestring directory))
            :subject   (and (acp-observer-v2-p observer)
                            (agentcomms:acp-v2-command-permission-subject
                             command (namestring directory)
                             :tool-call-id *acp-current-tool-call-id*))
            :key       (list ':command command (namestring directory)))
           ':full-access
           ':deny)))))

(defmethod agent-observer-authorize-tool ((observer acp-observer) tool arguments)
  "Apply the editor session's policy without granting local sandboxing to MCP."
  (let* ((session     (acp-observer-session observer))
         (application (acp-session-application session)))
    (acp-session-check-cancelled session)
    (case (acp-session-mode session)
      (:full-access
       ':allow)
      (:auto
       (if (eq (nth-value
                0
                (application--model-command-permission
                 application
                 (application--tool-authorization-request tool arguments)
                 (config :working-directory (application-configuration application))
                 :sandbox-available-p nil))
               ':full-access)
           ':allow
           ':deny))
      (otherwise
       (if (acp-observer--approval
            observer (tool-display-name tool)
            :arguments arguments
            :key       (list ':tool (application--tool-permission-key tool arguments)))
           ':allow
           ':deny)))))

(defmethod agent-observer-call-with-tool-execution ((observer acp-observer) identifier function)
  "Own tool workers and propagate ACP session bindings and permission identity."
  (let ((*acp-current-tool-call-id* identifier))
    (acp-session--call-with-tool (acp-observer-session observer) function)))
