(in-package #:autolith)

;;;; -- ACP Agent Methods --

(defmethod agentcomms:agent-implementation ((service acp-service))
  "Identify the loaded Autolith source version."
  (declare (ignore service))
  (agentcomms:acp-implementation "autolith" *autolith-version*))

(defparameter *acp-permission-modes*
  '(("ask" "Ask" "Approve commands and external tools in the editor.")
    ("auto" "Auto" "Classify commands using the configured permission policy.")
    ("sandboxed" "Sandboxed" "Run commands in the configured workspace sandbox.")
    ("full-access" "Full access" "Allow commands and external tools without approval."))
  "The permission mode identifiers, names, and descriptions.")

(defmethod agentcomms:agent-capabilities ((service acp-service))
  "Advertise durable loading, text context, HTTP MCP, and session cleanup."
  (declare (ignore service))
  (agentcomms:acp-agent-capabilities :load-session t :embedded-context t :mcp-http t :close t))

(defmethod agentcomms:agent-v2-capabilities ((service acp-service))
  "Advertise the ACP v2 baseline, deletion, and text context."
  (declare (ignore service))
  (agentcomms:acp-v2-agent-capabilities
   :embedded-context t :mcp-stdio t :mcp-http t :delete t))

(-> acp-service--mode-value (acp-session) string)
(defun acp-service--mode-value (session)
  "Return SESSION's permission mode as its wire value."
  (string-downcase (symbol-name (acp-session-mode session))))

(-> acp-service--modes (acp-session) hash-table)
(defun acp-service--modes (session)
  "Return the version 1 permission modes and current selection for SESSION."
  (agentcomms:json-object
   "modes"
   (agentcomms:acp-session-mode-state
    (acp-service--mode-value session)
    (loop for (id name description) in *acp-permission-modes*
          collect (agentcomms:acp-session-mode id name :description description)))))

(-> acp-service--mode-config-option (acp-session) hash-table)
(defun acp-service--mode-config-option (session)
  "Return SESSION's permission mode as a version 2 config option."
  (agentcomms:acp-v2-config-option-select
   "mode" "Session mode" (acp-service--mode-value session)
   :category ':mode
   :options  (loop for (id name description) in *acp-permission-modes*
                   collect (agentcomms:acp-v2-config-select-option
                            id name :description description))))

(-> acp-service--model-config-option (acp-session) (option hash-table))
(defun acp-service--model-config-option (session)
  "Return SESSION's selected model as a version 2 config option, when known."
  (let* ((configuration (application-configuration (acp-session-application session)))
         (current       (config :model configuration))
         (options       nil))
    (dolist (registration (provider-registrations))
      (dolist (model (provider-registration-models registration))
        (push (agentcomms:acp-v2-config-select-option
               (provider-model-name model)
               (provider-model-name model)
               :description (provider-registration-name registration))
              options)))
    (when options
      (agentcomms:acp-v2-config-option-select
       "model" "Model" current
       :category ':model
       :options  (nreverse options)))))

(-> acp-service--reasoning-config-option (acp-session) (option hash-table))
(defun acp-service--reasoning-config-option (session)
  "Return SESSION's reasoning effort as a version 2 config option, when known."
  (let* ((configuration (application-configuration (acp-session-application session)))
         (model         (config :model configuration))
         (provider      (config :model-provider configuration))
         (efforts       (configuration--reasoning-efforts-for model provider)))
    (when efforts
      (agentcomms:acp-v2-config-option-select
       "thought_level" "Reasoning effort" (config :reasoning-effort configuration)
       :category ':thought-level
       :options  (loop for effort in efforts
                       collect (agentcomms:acp-v2-config-select-option
                                effort
                                (string-capitalize effort)))))))

(-> acp-service--config-options (acp-session) list)
(defun acp-service--config-options (session)
  "Return every version 2 config option for SESSION."
  (remove nil
          (list (acp-service--mode-config-option session)
                (acp-service--model-config-option session)
                (acp-service--reasoning-config-option session))))

(-> acp-service--available-commands (acp-session) list)
(defun acp-service--available-commands (session)
  "Return native commands and skills as version 2 available commands."
  (let* ((application   (acp-session-application session))
         (configuration (application-configuration application))
         (commands
           (loop for command in (application-command-list)
                 for name = (application-command-name command)
                 collect (agentcomms:acp-v2-available-command
                          (if (and (plusp (length name)) (char= (char name 0) #\/))
                              (subseq name 1)
                              name)
                          (application-command-description command)
                          :hint (application-command-argument command))))
         (skills
           (loop for skill in (skill-catalog-skills
                               (skill-catalog-for-configuration configuration))
                 collect (agentcomms:acp-v2-available-command
                          (format nil "$~A" (skill-metadata-name skill))
                          (or (skill-metadata-description skill)
                              (skill-metadata-name skill))))))
    (append commands skills)))

(-> acp-service--open-session
    (acp-service &key (:cwd string) (:identifier (option string)) (:mcp-servers list)) acp-session)
(defun acp-service--open-session (service &key cwd identifier mcp-servers)
  "Construct a leased headless application, publishing it only after successful setup."
  (acp-service--call-with-operation
   service (gensym "SESSION-SETUP")
   (lambda ()
     (when (>= (hash-table-count (acp-service-sessions service)) *acp-session-maximum-count*)
       (error 'agentcomms:acp-state-error :message "The live session limit is reached."))
     (when (and identifier (gethash identifier (acp-service-sessions service)))
       (agentcomms:acp-invalid-params "Session ~A is already open." identifier))
     (agentcomms:acp-validate-absolute-path cwd "cwd")
     (unless (uiop:directory-exists-p cwd)
       (agentcomms:acp-invalid-params "The working directory does not exist: ~A." cwd))
     (let* ((configuration (configuration-copy
                            (acp-service-configuration service)
                            :working-directory (platform-truename *platform* (uiop:ensure-directory-pathname cwd))))
            (conversation nil) (lease nil) (registry nil) (worker nil)
            (completed-p   nil))
       (unwind-protect
            (progn
              (if identifier
                  (progn
                    (setf lease (conversation-lease-acquire configuration identifier)
                          conversation (conversation-load-by-id configuration identifier))
                    (unless (equal identifier (conversation-identifier conversation))
                      (agentcomms:acp-invalid-params "Use the canonical conversation identifier.")))
                  (setf conversation (conversation-create configuration)
                        lease (conversation-lease-acquire configuration
                                                          (conversation-identifier conversation))))
              (setf configuration (application--configuration-for-conversation configuration conversation))
              (let ((provider (provider-create configuration
                                               :reasoning-summaries-p (config :reasoning-traces-p configuration))))
                (setf registry (acp-mcp-create-tool-registry
                                configuration mcp-servers
                                :version (or (agentcomms:acp-agent-protocol-version service) 1))
                      worker (lisp-worker-pool-create configuration))
                (let* ((application
                        (make-instance 'application
                                       :configuration configuration :conversation conversation
                                       :conversation-lease lease :provider provider
                                       :tool-registry registry :worker worker
                                       :agent            (agent-create :configuration configuration :conversation conversation
                                                                       :provider provider :tool-registry registry :worker worker)
                                       :ui nil :permission-mode (acp-service-permission-mode service)
                                       :permission-state (permissions-load configuration)))
                       (session (make-instance 'acp-session
                                               :service          service
                                               :identifier       (conversation-identifier conversation)
                                               :application      application
                                               :mode             (acp-service-permission-mode service)
                                               :protocol-version (or (agentcomms:acp-agent-protocol-version service) 1))))
                  (acp-input-initialize session)
                  (with-lock-held ((acp-service-lock service))
                    (when (acp-service-closed-p service)
                      (error 'agentcomms:acp-connection-closed))
                    (setf (gethash (acp-session-identifier session) (acp-service-sessions service)) session
                          completed-p t))
                  session)))
         (unless completed-p
           (let ((failures (application--discard-connection-resources nil registry worker)))
             (unless failures
               (when registry
                 (acp-mcp-release-credentials (mcp-tool-registry-manager registry)))))
           (when lease (conversation-lease-release lease))))))))

(defmethod agentcomms:agent-new-session
    ((service acp-service) &key cwd mcp-servers additional-directories params)
  "Create a durable session with session-local client MCP servers."
  (declare (ignore params))
  (when additional-directories
    (agentcomms:acp-invalid-params "Additional directories are not supported."))
  (let ((session (acp-service--open-session service :cwd cwd :mcp-servers mcp-servers)))
    (handler-case
        (progn
          (acp-completion-start session)
          (values (acp-session-identifier session) (acp-service--modes session)))
      (serious-condition (condition)
        (acp-session-close session)
        (with-lock-held ((acp-service-lock service))
          (remhash (acp-session-identifier session) (acp-service-sessions service)))
        (error condition)))))

(-> acp-replay--record-update (list integer boolean) (option hash-table))
(defun acp-replay--record-update (record turn-sequence v2-p)
  "Project one durable record into the canonical ACP transcript update."
  (let ((fields (rest record)))
    (case (first record)
      (:message
       (unless (getf fields :automatic-p)
         (let ((sequence (or (getf fields :seq) 0))
               (text     (getf fields :content)))
           (if v2-p
               (if (eq (getf fields :role) ':user)
                   (agentcomms:acp-v2-update-user-message
                    (list (agentcomms:acp-text-content text))
                    (format nil "u~D" sequence))
                   (agentcomms:acp-v2-update-agent-message
                    (list (agentcomms:acp-text-content text))
                    (format nil "a~D" sequence)))
               (funcall (if (eq (getf fields :role) ':user)
                            #'agentcomms:acp-update-user-message
                            #'agentcomms:acp-update-agent-message)
                        (agentcomms:acp-text-content text))))))
      (:provider-item
       (let* ((item     (json-decode (getf fields :wire-json)))
              (type     (json-get item "type"))
              (sequence (or (getf fields :seq) 0)))
         (cond
           ((equal type "message")
            (let ((text (response-item-assistant-text item)))
              (when text
                (if v2-p
                    (agentcomms:acp-v2-update-agent-message
                     (list (agentcomms:acp-text-content text))
                     (format nil "a~D" sequence))
                    (agentcomms:acp-update-agent-message
                     (agentcomms:acp-text-content text))))))
           ((equal type "reasoning")
            (let ((text (response-item-reasoning-summary item)))
              (when text
                (if v2-p
                    (agentcomms:acp-v2-update-agent-thought
                     (list (agentcomms:acp-text-content text))
                     (format nil "t~D" sequence))
                    (agentcomms:acp-update-agent-thought
                     (agentcomms:acp-text-content text))))))
           ((equal type "function_call")
            (let ((name       (function-call-canonical-name item))
                  (arguments  (json-decode (json-get item "arguments")))
                  (identifier (acp-tool-identifier turn-sequence (json-get item "call_id"))))
              (if v2-p
                  (agentcomms:acp-v2-update-tool-call
                   (agentcomms:acp-tool-call-update
                    identifier
                    :title     (acp-tool-title name arguments)
                    :name      name
                    :kind      (acp-tool-kind name)
                    :status    ':pending
                    :raw-input arguments))
                  (agentcomms:acp-update-tool-call
                   (agentcomms:acp-tool-call
                    identifier
                    (acp-tool-title name arguments)
                    :name name :kind (acp-tool-kind name) :status ':pending
                    :raw-input arguments))))))))
      (:tool-result
       (let* ((output (getf fields :output))
              (update (agentcomms:acp-tool-call-update
                      (acp-tool-identifier turn-sequence (getf fields :call-id))
                      :status     (if (member (getf fields :status) '(:ok :success))
                                      ':completed
                                      ':failed)
                      :raw-output output
                      :content    (list (agentcomms:acp-tool-call-content
                                         (agentcomms:acp-text-content output))))))
         (if v2-p
             (agentcomms:acp-v2-update-tool-call update)
             (agentcomms:acp-update-tool-call-progress update)))))))

(-> acp-replay-map-updates (conversation function &key (:v2-p boolean)) null)
(defun acp-replay-map-updates (conversation function &key v2-p)
  "Call FUNCTION on chronological updates with durable turn-qualified tool identities."
  (let ((turn-sequence 0))
    (conversation-replay--map-records
     conversation
     (lambda (record)
       (when (and (eq (first record) ':message)
                  (eq (getf (rest record) :role) ':user))
         (setf turn-sequence (getf (rest record) :seq)))
       (let ((update (acp-replay--record-update record turn-sequence v2-p)))
         (when update (funcall function update))))))
  nil)

(-> acp-session-replay (acp-session) null)
(defun acp-session-replay (session)
  "Replay chronological durable messages, reasoning, calls, and results before load returns."
  (let ((observer (acp-observer-create session)))
    (acp-replay-map-updates
     (application-conversation (acp-session-application session))
     (lambda (update) (acp-observer--send observer update))
     :v2-p (acp-session-v2-p session))
    (acp-observer--plan observer))
  nil)

(defmethod agentcomms:agent-load-session
    ((service acp-service) &key session-id cwd mcp-servers additional-directories params)
  "Load a leased conversation and replay its history before answering."
  (declare (ignore params))
  (when additional-directories
    (agentcomms:acp-invalid-params "Additional directories are not supported."))
  (let ((session (acp-service--open-session service :cwd cwd :identifier session-id
                                            :mcp-servers mcp-servers)))
    (handler-case
        (progn
          (acp-session-replay session)
          (acp-completion-start session)
          (acp-service--modes session))
      (serious-condition (condition)
        (acp-session-close session)
        (with-lock-held ((acp-service-lock service))
          (remhash session-id (acp-service-sessions service)))
        (error condition)))))

(-> acp-service--session-extras (acp-session) hash-table)
(defun acp-service--session-extras (session)
  "Return the version 2 setup response extras for SESSION."
  (agentcomms:json-object
   "configOptions"     (coerce (acp-service--config-options session) 'vector)
   "availableCommands" (coerce (acp-service--available-commands session) 'vector)))

(-> acp-service--send-available-commands (acp-session) null)
(defun acp-service--send-available-commands (session)
  "Publish SESSION's commands as a version 2 available command update."
  (when (acp-session-v2-p session)
    (agentcomms:agent-send-update
     (acp-session-service session)
     (acp-session-identifier session)
     (agentcomms:acp-v2-update-available-commands
      (acp-service--available-commands session))))
  nil)

(-> acp-service--discard-failed-session (acp-service acp-session string) null)
(defun acp-service--discard-failed-session (service session identifier)
  "Close SESSION after failed setup and remove IDENTIFIER from SERVICE."
  (acp-session-close session)
  (with-lock-held ((acp-service-lock service))
    (remhash identifier (acp-service-sessions service)))
  nil)

(defmethod agentcomms:agent-v2-new-session
    ((service acp-service) &key cwd mcp-servers additional-directories params)
  "Create a durable version 2 session with configuration options."
  (declare (ignore params))
  (when additional-directories
    (agentcomms:acp-invalid-params "Additional directories are not supported."))
  (let ((session (acp-service--open-session service :cwd cwd :mcp-servers mcp-servers)))
    (handler-case
        (progn
          (acp-completion-start session)
          (acp-service--send-available-commands session)
          (values (acp-session-identifier session)
                  (acp-service--session-extras session)))
      (serious-condition (condition)
        (acp-service--discard-failed-session
         service session (acp-session-identifier session))
        (error condition)))))

(defmethod agentcomms:agent-v2-resume-session
    ((service acp-service)
     &key session-id cwd mcp-servers additional-directories replay-from params)
  "Restore a version 2 session and replay history when REPLAY-FROM asks for it."
  (declare (ignore params))
  (when additional-directories
    (agentcomms:acp-invalid-params "Additional directories are not supported."))
  (let ((session (acp-service--open-session service :cwd cwd :identifier session-id
                                            :mcp-servers mcp-servers)))
    (handler-case
        (progn
          (when replay-from
            (acp-session-replay session))
          (acp-completion-start session)
          (acp-service--send-available-commands session)
          (acp-service--session-extras session))
      (serious-condition (condition)
        (acp-service--discard-failed-session service session session-id)
        (error condition)))))

(defmethod agentcomms:agent-v2-prompt
    ((service acp-service) session-id prompt &key params accept)
  "Run one version 2 prompt and accept it after durable insertion."
  (declare (ignore params))
  (acp-session-run-turn (acp-service--session service session-id)
                        (acp-prompt->text prompt)
                        :accept accept))

(defmethod agentcomms:agent-v2-set-config-option
    ((service acp-service) session-id config-id &key value params)
  "Apply one version 2 configuration option and return all current options."
  (declare (ignore params))
  (let* ((session     (acp-service--session service session-id))
         (application (acp-session-application session))
         (mode        (find value *acp-permission-modes* :key #'first :test #'equal)))
    (cond
      ((string= config-id "mode")
       (unless mode
         (agentcomms:acp-invalid-params "Unknown permission mode ~A." value))
       (with-lock-held ((acp-session-lock session))
         (when (acp-session-prompt-thread session)
           (error 'agentcomms:acp-state-error
                  :message "Change mode between prompt turns."))
         (setf (acp-session-mode session) (intern (string-upcase value) :keyword)
               (application-permission-mode application) (acp-session-mode session))
         (clrhash (acp-session-permissions session))))
      ((string= config-id "model")
       (unless (stringp value)
         (agentcomms:acp-invalid-params "A model option takes a string."))
       (application-set-model application value))
      ((string= config-id "thought_level")
       (unless (stringp value)
         (agentcomms:acp-invalid-params "A reasoning option takes a string."))
       (application-set-reasoning-effort application value))
      (t
       (agentcomms:acp-invalid-params "Unknown configuration option ~A." config-id)))
    (agentcomms:agent-send-update
     service session-id
     (agentcomms:acp-v2-update-config-options
      (acp-service--config-options session)))
    (acp-service--config-options session)))

(-> acp-service--conversation-timestamp (t) (option string))
(defun acp-service--conversation-timestamp (value)
  "Return universal time VALUE as RFC 3339, or NIL."
  (and (integerp value) (universal-time->rfc3339 value)))

(defmethod agentcomms:agent-v2-list-sessions
    ((service acp-service) &key cwd cursor params)
  "List durable conversations as version 2 sessions."
  (declare (ignore params))
  (let* ((configuration (acp-service-configuration service))
         (offset        (if (or (null cursor) (string= cursor ""))
                            0
                            (handler-case (parse-integer cursor :junk-allowed nil)
                              (error ()
                                (agentcomms:acp-invalid-params
                                 "cursor must be an integer")))))
         (requested-cwd (and cwd (namestring (uiop:ensure-directory-pathname cwd))))
         (paths         (remove-if-not
                         (lambda (pathname)
                           (or (null requested-cwd)
                               (let* ((conversation (conversation-load pathname))
                                      (origin       (conversation-origin-directory conversation)))
                                 (and origin
                                      (string= requested-cwd
                                               (namestring
                                                (uiop:ensure-directory-pathname origin)))))))
                         (conversation-list configuration)))
         (page          (subseq paths (min offset (length paths))
                                (min (+ offset 50) (length paths))))
         (next          (+ offset (length page))))
    (values
     (loop for pathname in page
           for conversation = (conversation-load pathname)
           collect (agentcomms:acp-session-info
                    (conversation-identifier conversation)
                    (namestring
                     (uiop:ensure-directory-pathname
                      (or (conversation-origin-directory conversation)
                          cwd
                          (config :working-directory configuration))))
                    :title      (conversation-title conversation)
                    :updated-at (acp-service--conversation-timestamp
                                 (conversation-last-activity-at conversation))))
     (and (< next (length paths)) (princ-to-string next)))))

(defmethod agentcomms:agent-v2-delete-session
    ((service acp-service) session-id params)
  "Delete a closed version 2 session's durable conversation."
  (declare (ignore params))
  (when (agentcomms:acp-agent-session service session-id)
    (agentcomms:acp-invalid-params "Close session ~A before deleting it." session-id))
  (conversation-delete (acp-service-configuration service) session-id)
  nil)

(-> acp-prompt->text (list) string)
(defun acp-prompt->text (blocks)
  "Preserve ordered text and embedded text context, rejecting unsupported content."
  (with-output-to-string (stream)
    (loop for block in blocks for first-p = t then nil
          for type = (agentcomms:acp-content-type block)
          do (unless first-p (terpri stream))
          (case type
            (:text
             (write-string (agentcomms:acp-content-text block) stream))
            (:resource-link
             (format stream "Resource ~A: ~A" (agentcomms:json-get block "name")
                     (agentcomms:json-get block "uri")))
            (:resource
             (let* ((resource (agentcomms:json-get block "resource"))
                    (text     (agentcomms:json-get resource "text")))
               (unless (stringp text)
                 (agentcomms:acp-invalid-params "Binary embedded context is not supported."))
               (format stream "Resource ~A:~%~A" (agentcomms:json-get resource "uri") text)))
            (otherwise
             (agentcomms:acp-invalid-params "Unsupported prompt content: ~A." type))))))

(defmethod agentcomms:peer-handle-notification :before
    ((service acp-service) connection method &key params)
  "Apply cancellation to headless continuations and explicit tool invocations too."
  (declare (ignore connection))
  (when (and (string= method "session/cancel") (hash-table-p params))
    (let* ((identifier (agentcomms:json-get params "sessionId"))
           (session    (with-lock-held ((acp-service-lock service))
                         (gethash identifier (acp-service-sessions service))))
           (epoch      (and session
                            (with-lock-held ((acp-session-lock session))
                              (when (acp-session-prompt-thread session)
                                (acp-session-epoch session))))))
      (when epoch (acp-session-cancel session :expected-epoch epoch))))
  nil)

(-> acp-session-run-turn
    (acp-session string &key (:automatic-p boolean) (:queued-p boolean)
                 (:prepare (or null function)) (:accept (or null function)))
    keyword)
(defun acp-session-run-turn
    (session text &key automatic-p queued-p prepare accept)
  "Run one admitted turn, preserving cancellation and durable repair for either input source."
  (let* ((service                      (acp-session-service session))
         (application                  (acp-session-application session))
         (conversation                 (application-conversation application))
         (controller                   (acp-input-controller session))
         (*acp-extension-invocation-p* (or automatic-p queued-p))
         (managed-foreground-p         (and (acp-session-v2-p session)
                                            (or automatic-p queued-p)))
         (start-sequence               nil)
         (observer                     nil)
         (failure                      nil)
         (source                       (cond (queued-p
                                              "queue")
                                             (automatic-p
                                              "completion")
                                             (t
                                              "prompt")))
         (outcome                      ':condition)
         (claimed-p                    nil))
    (acp-service--call-with-operation
     service session
     (lambda ()
       (unwind-protect
            (handler-case
                (progn
                  (with-lock-held ((acp-session-lock session))
                    (when (or (acp-session-closed-p session) (acp-session-closing-p session))
                      (agentcomms:acp-invalid-params "The session is closing or closed."))
                    (when (and (or automatic-p queued-p) (acp-session-cancelled-p session))
                      (error 'application-turn-cancelled))
                    (unless (or automatic-p queued-p)
                      (setf (acp-session-cancelled-p session) nil
                            (acp-session-completion-report session) nil))
                    (incf (acp-session-epoch session))
                    (setf (acp-session-prompt-thread session) (current-thread)
                          (acp-session-prompt-interruptible-p session) t
                          claimed-p t))
                  (let ((*active-application* application)
                        (*configuration*      (application-configuration application))
                        (*default-pathname-defaults*
                          (config :working-directory (application-configuration application))))
                    (acp-extension-turn-notify session "running" :source source)
                    (when managed-foreground-p
                      (agentcomms:agent-send-state service (acp-session-identifier session)
                                                   ':running))
                    (acp-session-check-cancelled session)
                    (let ((prepared (if prepare (funcall prepare) t)))
                      (setf outcome
                            (if (not prepared)
                                ':end-turn
                                (progn
                                  (when controller
                                    (application-input-controller-begin-external-work controller))
                                  (when (and controller (not (or automatic-p queued-p)))
                                    (with-lock-held ((application-input-controller-lock controller))
                                      (setf (application-input-controller-queued-work-paused-p controller) nil)))
                                  (setf start-sequence (conversation-next-sequence conversation)
                                        observer (acp-observer-create session))
                                  (acp-extension-notify session "state")
                                  (agent-run-user-turn
                                   (application-agent application) (if queued-p prepared text)
                                   :observer observer :automatic-p automatic-p
                                   :accepted-function accept
                                   :pending-input-identifier
                                   (and queued-p controller
                                        (application-input-controller-active-work-identifier controller)))
                                  (acp-session--call-with-finalization
                                   session
                                   (lambda ()
                                     (acp-session-check-cancelled session)
                                     (acp-observer-flush observer)
                                     (acp-observer--session-info observer)
                                     ':end-turn))))))))
              (application-turn-cancelled (condition)
                (setf outcome ':cancelled)
                (acp-session--call-with-finalization
                 session
                 (lambda ()
                   (when start-sequence
                     (application--record-turn-aborted application condition
                                                       :turn-start-sequence start-sequence
                                                       :reason              ':cancelled))
                   (when observer
                     (acp-observer-cancel-open-tools observer)
                     (handler-case (acp-observer-flush observer)
                       (serious-condition () nil))
                     (handler-case (acp-observer--session-info observer)
                       (serious-condition () nil)))
                   ':cancelled)))
              (serious-condition (condition)
                (setf failure condition)
                (acp-session--call-with-finalization
                 session
                 (lambda ()
                   (when start-sequence
                     (application--record-turn-aborted application condition
                                                       :turn-start-sequence start-sequence
                                                       :reason              ':application-error))
                   (when observer
                     (acp-observer-cancel-open-tools observer)
                     (handler-case (acp-observer-flush observer)
                       (serious-condition () nil))
                     (handler-case (acp-observer--session-info observer)
                       (serious-condition () nil)))
                   (with-lock-held ((acp-session-lock session))
                     (let ((report (princ-to-string condition)))
                       (setf (acp-session-completion-report session)
                             (subseq report 0 (min 2000 (length report))))))
                   (error condition)))))
         (when claimed-p
           (sb-sys:without-interrupts
             (unwind-protect
                  (when controller
                    (application-input-controller-finish-external-work
                     controller :pause-p (not (eq outcome ':end-turn))))
               (with-lock-held ((acp-session-lock session))
                 (setf (acp-session-prompt-interruptible-p session) nil
                       (acp-session-prompt-thread session) nil))
               (acp-extension-turn-notify session "idle" :source source :outcome outcome)
               (when managed-foreground-p
                 (agentcomms:agent-send-state
                  service (acp-session-identifier session) ':idle
                  :stop-reason (case outcome
                                 (:end-turn
                                  ':end-turn)
                                 (:cancelled
                                  ':cancelled)
                                 (otherwise
                                  ':error))
                  :error       (when (eq outcome ':condition)
                                 (agentcomms:json-object
                                  "code"    -32603
                                  "message" (if failure
                                                (let ((report (princ-to-string failure)))
                                                  (subseq report 0 (min 2000 (length report))))
                                                "The foreground turn failed.")))))
               (acp-extension-notify session "state")
               (acp-completion-wake session)))))))))

(defmethod agentcomms:agent-prompt ((service acp-service) session-id prompt &key params)
  "Run one explicit user prompt through the shared primary turn boundary."
  (declare (ignore params))
  (acp-session-run-turn (acp-service--session service session-id) (acp-prompt->text prompt)))

(defmethod agentcomms:agent-cancel ((service acp-service) session-id)
  "Interrupt the prompt and its concurrent tools without waiting on the reader."
  (acp-session-cancel (acp-service--session service session-id)))

(defmethod agentcomms:agent-set-mode ((service acp-service) session-id mode-id &key params)
  "Validate and publish a permission mode change between turns."
  (declare (ignore params))
  (let* ((session (acp-service--session service session-id))
         (mode    (find mode-id '(:ask :auto :sandboxed :full-access)
                        :key (lambda (value) (string-downcase (symbol-name value))) :test #'equal)))
    (unless mode (agentcomms:acp-invalid-params "Unknown mode ~A." mode-id))
    (with-lock-held ((acp-session-lock session))
      (when (acp-session-prompt-thread session)
        (error 'agentcomms:acp-state-error :message "Change mode between prompt turns."))
      (setf (acp-session-mode session) mode
            (application-permission-mode (acp-session-application session)) mode)
      (clrhash (acp-session-permissions session)))
    (agentcomms:agent-send-update service session-id (agentcomms:acp-update-current-mode mode-id)))
  nil)

(defmethod agentcomms:agent-close-session ((service acp-service) session-id params)
  "Cancel work and release this session's runtime resources and lease."
  (declare (ignore params))
  (acp-session-close (acp-service--session service session-id))
  (with-lock-held ((acp-service-lock service))
    (remhash session-id (acp-service-sessions service)))
  nil)

(defmethod agentcomms:peer-connection-closed :before ((service acp-service) connection reason)
  "Stop admission and cancel work before the library joins request handlers."
  (declare (ignore connection reason))
  (let ((sessions nil))
    (with-lock-held ((acp-service-lock service))
      (setf (acp-service-closed-p service) t
            sessions (loop for session being the hash-values of (acp-service-sessions service)
                           collect session)))
    (mapc #'acp-session-cancel sessions)))

(defmethod agentcomms:peer-handle-request :around ((service acp-service) connection method &key params)
  "Keep diagnostics off the protocol stream in every inbound handler thread."
  (declare (ignore params))
  (let* ((*standard-output* *error-output*) (*trace-output* *error-output*)
         (*standard-input* (make-string-input-stream ""))
         (*terminal-io*    (make-two-way-stream *standard-input* *error-output*))
         (*query-io* *terminal-io*) (*debug-io* *terminal-io*))
    (handler-case (call-next-method)
      (authentication-error ()
        (error 'agentcomms:acp-method-error :code -32000
               :message                           "Provider authentication required. Configure credentials before starting ACP.")))))

(-> acp-service-close (acp-service) null)
(defun acp-service-close (service)
  "Stop admission and release all sessions, retaining failed ownership for a retry."
  (let ((sessions nil)
        (failed-p nil))
    (with-lock-held ((acp-service-lock service))
      (setf (acp-service-closed-p service) t
            sessions (loop for session being the hash-values of (acp-service-sessions service)
                           collect session)))
    (mapc #'acp-session-cancel sessions)
    (dolist (session sessions)
      (handler-case
          (progn
            (acp-session-close session)
            (with-lock-held ((acp-service-lock service))
              (remhash (acp-session-identifier session) (acp-service-sessions service))))
        (serious-condition ()
          (setf failed-p t))))
    (telemetry--call-safely #'telemetry-shutdown)
    (when failed-p
      (error 'agentcomms:acp-state-error :message "ACP session cleanup failed.")))
  nil)

(-> acp-service-serve (acp-service agentcomms:acp-channel) null)
(defun acp-service-serve (service channel)
  "Own SERVICE and its channel through disconnect and resource cleanup."
  (unwind-protect
       (agentcomms:acp-agent-serve service channel :name "Autolith ACP"
                                   :log-function         #'agentcomms:acp-standard-error-log)
    (unwind-protect (acp-service-close service)
      (agentcomms:channel-close channel)))
  nil)

(-> acp-run (configuration &key (:permission-mode keyword) (:pristine-p boolean)) null)
(defun acp-run (configuration &key (permission-mode ':ask) pristine-p)
  "Bootstrap a headless ACP endpoint and serve UTF-8 standard I/O until disconnect."
  (let* ((channel           (agentcomms:acp-standard-io-channel))
         (*standard-output* *error-output*)
         (*trace-output*    *error-output*)
         (*standard-input*  (make-string-input-stream ""))
         (*terminal-io*     (make-two-way-stream *standard-input* *error-output*))
         (*query-io*        *terminal-io*)
         (*debug-io*        *terminal-io*))
    (unwind-protect
         ;; Client declarations can contain credentials for the service lifetime.
         (call-with-secret-use
          (lambda ()
            (configuration-ensure-directories configuration)
            (conversation-identifier-migrate configuration)
            (context-runtime-reset)
            (unless pristine-p (durable-mutations-load configuration))
            (image-state-load configuration :pristine-p pristine-p)
            (application--load-extension-configuration configuration :pristine-p pristine-p)
            (setf configuration (provider-bootstrap-configuration configuration))
            (with-slynk-endpoint (configuration)
              (acp-service-serve (make-instance 'acp-service :configuration configuration
                                                :permission-mode            permission-mode)
                                 channel))))
      (agentcomms:channel-close channel)))
  nil)
