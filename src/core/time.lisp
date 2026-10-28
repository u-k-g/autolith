(in-package #:autolith)

;;;; -- Epoch Conversion --

;;; cl-rfc8628 owns the epoch offset and the Unix-to-universal conversion.

(-> universal-time->unix-time (integer) integer)
(defun universal-time->unix-time (universal-time)
  "Convert Common Lisp UNIVERSAL-TIME to integer Unix-time seconds."
  (- universal-time *unix-epoch-universal-time*))

(-> universal-time->rfc3339 (integer) string)
(defun universal-time->rfc3339 (universal-time)
  "Return UNIVERSAL-TIME as an RFC 3339 UTC timestamp."
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time universal-time 0)
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0DZ"
            year month day hour minute second)))


;;;; -- RFC 3339 Parsing --

(-> rfc3339--field (string integer integer) integer)
(defun rfc3339--field (text start end)
  "Parse the decimal RFC 3339 field of TEXT between START and END."
  (parse-integer text :start start :end end))

(-> rfc3339--offset-seconds (string integer) integer)
(defun rfc3339--offset-seconds (text start)
  "Parse TEXT's timezone suffix at START into seconds east of UTC."
  (let ((designator (char text start)))
    (cond
      ((member designator '(#\Z #\z))
       (unless (= (1+ start) (length text))
         (error 'rfc3339-syntax-error
                :text    text
                :message "Trailing content follows the UTC designator."))
       0)
      ((member designator '(#\+ #\-))
       (unless (and (= (+ start 6) (length text))
                    (char= (char text (+ start 3)) #\:))
         (error 'rfc3339-syntax-error
                :text    text
                :message "The RFC 3339 numeric offset is malformed."))
       (let ((hours   (rfc3339--field text (+ start 1) (+ start 3)))
             (minutes (rfc3339--field text (+ start 4) (+ start 6)))
             (sign    (if (char= designator #\-) -1 1)))
         (* sign (+ (* hours 3600) (* minutes 60)))))
      (t
       (error 'rfc3339-syntax-error
              :text    text
              :message "An RFC 3339 timestamp requires a timezone suffix.")))))

(-> rfc3339->universal-time (string) (option timestamp))
(defun rfc3339->universal-time (text)
  "Return TEXT parsed as an RFC 3339 timestamp, or NIL when malformed.

Fractional seconds are truncated toward zero."
  (handler-case
      (progn
        (unless (and (>= (length text) 20)
                     (char= (char text 4) #\-)
                     (char= (char text 7) #\-)
                     (member (char text 10) '(#\T #\t #\Space))
                     (char= (char text 13) #\:)
                     (char= (char text 16) #\:))
          (error 'rfc3339-syntax-error
                 :text    text
                 :message "The RFC 3339 timestamp layout is malformed."))
        (let* ((year           (rfc3339--field text 0 4))
               (month          (rfc3339--field text 5 7))
               (day            (rfc3339--field text 8 10))
               (hour           (rfc3339--field text 11 13))
               (minute         (rfc3339--field text 14 16))
               (second         (rfc3339--field text 17 19))
               (suffix-start
                 (if (char= (char text 19) #\.)
                     (let ((position (position-if-not #'digit-char-p text
                                                      :start 20)))
                       (unless (and position (> position 20))
                         (error 'rfc3339-syntax-error
                                :text    text
                                :message "The RFC 3339 fraction is malformed."))
                       position)
                     19))
               (offset-seconds (rfc3339--offset-seconds text suffix-start)))
          (- (encode-universal-time second minute hour day month year 0)
             offset-seconds)))
    (error ()
      nil)))


;;;; -- Conditions --

(define-condition rfc3339-syntax-error (autolith-error)
  ((text
    :initarg       :text
    :reader        rfc3339-syntax-error-text
    :type          string
    :documentation "The text that failed to parse as an RFC 3339 timestamp."))
  (:documentation "Text does not have the RFC 3339 timestamp shape Autolith accepts."))
