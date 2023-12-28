;;;====================================================
;;;
;;;   Code for unparsing validation code in model files
;;;
;;;====================================================
 
(in-package #:rails-unparser)

(defun unparse-validation-method (method-name context test error-msg)
  (let* ((model-attributes (remove nil (flatten (extract-attributes-from-expression test context))))
         (nullable-atts (remove-if-not #'nullable? model-attributes))
         ;; due to the usage of incf and decf below (probably a wrong-headed idea)
         ;; we can use this let (*nesting-level* *nesting-level*) to protect against
         ;; those destructive operations in the case of errors
         (*nesting-level* *nesting-level*))
    (with-output-to-string (code)
      (format code "def ~a" method-name)
;; this <unless> nesting should only happen when a vulnerable operation will occur (or put the onus on callers?
      (when (vulnerable-to-null? test)
        (incf *nesting-level*)
	    (format code "~%~aunless ~{~a.blank?~^ || ~}" (make-indent)
                (mapcar #'(lambda (att) (unparse att :ruby)) (remove-duplicates nullable-atts))))
	  (with-nesting
          (format code "~%~aif ~a" (make-indent)
                  (unparse-expression
                   (unparse-attribute-references test context) :ruby))
        (with-nesting
            (format code "~%~aerrors.add(:~a, ~s)"
                    (make-indent) (if (typep context 'attribute)
                                      (unparse context :ruby)
                                      (unparse (primary-key context) :ruby))
                    (or error-msg
                        (format nil "The check to ensure that ~a has failed"
                                (unparse-expression (negate-expression test) :english)))))
        (format code "~%~aend" (make-indent)))
      (when (vulnerable-to-null? test)
        (format code "~%~aend" (make-indent))
	    (decf *nesting-level*))
      (format code "~%~aend" (make-indent)))))

(defmethod rewrite-expression ((operator t) expression)
  (negate-expression expression))

(defmethod rewrite-expression ((operator (eql :when)) expression)
  (list (get-operator :and)
        (second expression)
        (negate-expression (third expression))))

(defmethod rewrite-expression ((operator (eql :max-rows)) expression)
  (list (get-operator :gt)
        (list (get-operator :rows) (second expression) (fourth expression))
        (third expression)))

(defun make-event-option(event)
  (when event (list "on" (strcat ":" (string-downcase (symbol-name event))))))

(defun make-name-from-expression (exp)
  (let ((name
         (cond ((typep exp 'named-object) (or (name exp) ""))
               ;;this below won't make it through the above, just here til I'm sure...
               ((typep exp 'operator) (name exp))
               ((null exp) "")
               ((symbolp exp) (format nil "~:(~a~)" (symbol-name exp)))
               ((stringp exp) (format nil "_~a_" (subseq exp 0 (min (length exp) 5))))
               ((atom exp) (format nil "~a" exp))
               (t (strcat (make-name-from-expression (car exp))
                          (make-name-from-expression (cdr exp)))))))
    (if (string= name "")
        ""
        (ruby:make-legal-name name))))

(defun unparse-validation-constraint (constraint)
  (let* ((exp (expression constraint))
         (context (context (formula constraint)))
         (event (make-event-option (event constraint)))
         (method-name (snake-case (or (name constraint) (make-name-from-expression exp)))))
    (setf exp (rewrite-expression (operator-key (car exp)) exp))
    (with-output-to-string (code)
      (format code "~%~avalidate :~a~a~%~a" (make-indent) method-name
              (if event
                  (format nil ", ~a: ~a" (car event) (cadr event))
                  "")
              (make-indent))
      (format code "~a"
              (unparse-validation-method
               method-name context exp (message constraint)))
      (format code "~%"))))

(defmethod unparse-model-validation ((op operator) args &key event message condition)
  (unparse-model-validation (operator-key op) args :event event :message message :condition condition))

(defmethod unparse-model-validation ((op symbol) args &key event message condition)
  (declare (ignorable event message condition))
  (comment-with-warning nil "no unparse-model-validation method written for ~a (~a)" op args))

#|
These variables are available to message strings:
 - %{value} -> submitted value being validated
 - %{model} -> model (class) being validated
 - %{attribute} -> name of the attribute we are validating
 - object.<any attribute from the model>
 - message can be a procedure. EG:
    validates :username,
       uniqueness: {
          # object = person object being validated
          # data = { model: "Person", attribute: "Username", value: <username> }
                    message: ->(object, data) do
                               "Hey #{object.name}, #{data[:value]} is already taken."
|#

(defun unparse-conditional-validation (op args &key event message)
  (let* ((predicate (first args))
         (consequent (second args)))
    (unparse-model-validation
     (car consequent) (cdr consequent)
     :event event :message message
     :condition (list op predicate))))

(defun make-conditional-option (conditional)
  (when conditional
    (let* ((operator (if (symbolp (car conditional))
                         (car conditional)
                         (operator-key (car conditional))))
           (arg (second conditional))
           (option (if (member operator '($if $when)) "if" "unless"))
           (expr (if (symbolp arg)
                     (strcat ":" (unparse-expression arg :ruby))
                     (ruby:unparse-lambda nil arg))))
      (list option expr))))

(defmethod unparse-model-validation ((op (eql :when)) args &key event message condition)
  (when condition
    (error "it does not make sense to have a condition here (~a, ~a)"
           op condition))
  (unparse-conditional-validation op args :event event :message message))
(defmethod unparse-model-validation ((op (eql :unless)) args &key event message condition)
  (when condition
    (error "it does not make sense to have a condition here (~a, ~a)"
           op condition))
  (unparse-conditional-validation op args :event event :message message))

(defmethod unparse-model-validation ((op (eql :call)) args &key event message condition)
  (when message
    (warn "we can not do anything with a message in a custom validates_with validation ($call ~a)"
          args))
  (format
   nil "validates_with ~a~a~a" (car args)
   (if event
       (format nil ", on: :~a" (string-downcase (symbol-name event)))
       "")
   (if condition
       (let ((option (make-conditional-option condition)))
         (format nil ", ~a: ~a" (car option) (cadr option)))
       "")))

(defparameter *numericality-options-that-take-procs*
  '("greater_than" "greater_than_or_equal_to" "equal_to" "less_than"
    "less_than_or_equal_to" "only_integer" "other_than"))

(defun unparse-comparison-arg (exp option entity)
  (if (or (numberp exp)
          (and (stringp exp)
               (or (ruby:is-range? exp)
                   (member exp ruby:*ruby-constants* :test #'string-equal))))
      exp
      (progn
        (unless (member option *numericality-options-that-take-procs* :test #'string-equal)
          (error "the numericality option ~s does not accept proc expressions or method names. (~a)"
                 option exp))
        (let ((obj-ref (instance-name entity)))
          (ruby:unparse-lambda (list obj-ref)
                                (unparse-attribute-references exp entity obj-ref))))))

(defmethod unparse-model-validation ((op (eql :not-null)) args &key event message condition)
  (let ((att (car args)))
    (if (eql (data-type att) :boolean)
        ""
        (apply #'unparse-field-validation
         (car args) "presence"
         (remove nil
           (list
            (when message (list "message" (format nil "~s" message)))
            (make-conditional-option condition)
            (make-event-option event)))))))

(defmethod unparse-model-validation ((op (eql :regex)) args &key event message condition)
  (apply #'unparse-field-validation
         (car args) "format"
         (remove nil
                 (list (when message (list "message" (format nil "~s" message)))
                       (list "with" (format nil "/\\A~a\\z/" (cadr args)))
                       (make-conditional-option condition)
                       (make-event-option event)))))

(defmethod unparse-model-validation ((op (eql :in)) args &key event message condition)
  (let* ((att (car args))
         (legal-values (if (eql (data-type att) :boolean)
                           (if (implement-as-string? att)
                               (legal-values (domain att))
                               (list t nil))
                           (cdr args))))
    (unparse-field-validation
     att "inclusion"
     (list "in" (unparse-array (append legal-values (when (nullable? att) (list :null))) :ruby))
     (list "message"
           (format nil "~s"
                   (or message (format nil "~a value must be one of ~a. \"%{value}\" is not in there"
                                       (long-name att)
                                       (format-acceptable-value-list (cdr args))))))
     (make-conditional-option condition)
     (make-event-option event))))

(defmethod unparse-model-validation ((op (eql :length)) args &key event message condition)
  (unparse-field-validation
   (car args) "length"
   (list "is" (cadr args))
   (list "too_long"
         (format nil "~s"
                 (or message (format nil "~a length must be ~r character~:p. \"%{value}\" is too long"
                                     (long-name (car args)) (cadr args)))))
   (list "too_short"
         (format nil "~s"
                 (or message (format nil "~a length must be ~r character~:p. \"%{value}\" is too short"
                                     (long-name (car args)) (cadr args)))))
   (make-conditional-option condition)
   (make-event-option event)))

(defmethod unparse-model-validation ((op (eql :length-lt)) args &key event message condition)
  (unparse-field-validation
   (car args) "length"
   (list "maximum" (1- (cadr args)))
   (list "message"
         (format nil "~s"
                 (or message (format nil "~a length must be shorter than ~r characters. \"%{value}\" is too long"
                                     (long-name (car args)) (cadr args)))))
   (make-conditional-option condition)
   (make-event-option event)))

(defmethod unparse-model-validation ((op (eql :length-between)) args &key event message condition)
  (unparse-field-validation
   (car args) "length"
   (list "in" (ruby:unparse-range (cadr args) (caddr args)))
   (list "too_long"
         (format nil "~s"
                 (or message (format nil "~a length must be no more than ~r character~:p. \"%{value}\" is too long"
                                     (long-name (car args)) (caddr args)))))
   (list "too_short"
         (format nil "~s"
                 (or message (format nil "~a length must be at least ~r character~:p. \"%{value}\" is too short"
                                     (long-name (car args)) (cadr args)))))
   (make-conditional-option condition)
   (make-event-option event)))

(defmethod unparse-model-validation ((op (eql :length-gt)) args &key event message condition)
  (unparse-field-validation
   (car args) "length"
   (list "minimum" (1+ (cadr args)))
   (list "message"
         (format nil "~s"
                 (or message (format nil "~a length must be longer than ~r character~:p. \"%{value}\" is too short"
                                     (long-name (car args)) (cadr args)))))
   (make-conditional-option condition)
   (make-event-option event)))

(defmethod unparse-model-validation ((op (eql :unique)) args &key event message condition)
  (if (tenant-scoped-entity? (my-entity (car args)))
      (unparse-model-validation :unique-within
           (append args (list (tenant-key (my-entity (car args)))))
           :event event :condition condition
           :message (or message (format nil "all %{model} records must have a unique ~a value. \"%{value}\" is taken"
                                 (long-name (car args)))))
      (unparse-field-validation
       (car args) "uniqueness"
       (list "message"
             (format nil "~s"
                     (or message
                         (format nil "all %{model} records must have a unique ~a value. \"%{value}\" is taken"
                                 (long-name (car args))))))
       (make-conditional-option condition)
       (make-event-option event))))

(defmethod unparse-model-validation ((op (eql :unique-within)) args &key event message condition)
  (let ((field (car args))
        (context (cadr args)))
    (unparse-field-validation
     field "uniqueness"
     (list "scope" (strcat ":" (schema-name context)))
     (list "message"
           (format nil "~s"
                   (or message (format nil "all %{model} records must have a ~
                                          unique ~a within their ~a. \"%{value}\" is taken"
                                       (long-name field) (short-name context)))))
     (make-conditional-option condition)
     (make-event-option event))))

(defun unparse-comparison-validation (field option arg message &key event condition)
    (if (member (data-type field) '(:date :time :datetime))
        (progn
          (warn "work to do: dates and times are not sorted yet!")
          (comment-out nil "field: ~a, option: ~a, arg: ~a" field option arg))
        (unparse-field-validation
         field "numericality"
         (list option (unparse-comparison-arg arg option (my-entity field)))
         (list "message" message)
         (when (eql :integer (data-type field)) (list "only_integer" "true"))
         (make-conditional-option condition)
         (make-event-option event))))

(defmethod unparse-model-validation ((op (eql :>)) args &key event message condition)
  (unparse-model-validation :gt args :event event :condition condition :message message))
(defmethod unparse-model-validation ((op (eql :gt)) args &key event message condition)
  (let ((field (car args)))
    (unparse-comparison-validation
     field "greater_than" (cadr args)
     (format nil "~s"
             (or message (format nil "the value of ~a must be greater than ~a. \"%{value}\" is too small"
                                 (long-name field) (unparse-expression (cadr args) :english))))
     :event event :condition condition)))

(defmethod unparse-model-validation ((op (eql :<)) args &key event message condition)
  (unparse-model-validation :lt args :event event :condition condition :message message))
(defmethod unparse-model-validation ((op (eql :lt)) args &key event message condition)
  (let ((field (car args)))
    (unparse-comparison-validation
     field "less_than" (cadr args)
     (format nil "~s"
             (or message (format nil  "the value of ~a must be less ~
                                             than ~a. \"%{value}\" is too large"
                                 (long-name field) (unparse-expression (cadr args) :english))))
     :event event :condition condition)))

(defmethod unparse-model-validation ((op (eql :<=)) args &key event message condition)
  (let ((field (car args)))
    (unparse-comparison-validation
     field "less_than_or_equal_to" (cadr args)
     (format nil "~s"
                   (or message (format nil "the value of ~a must be less than or equal ~
                                            to ~a. \"%{value}\" is too large"
                                       (long-name field) (unparse-expression (cadr args) :english))))
     :event event :condition condition)))

(defmethod unparse-model-validation ((op (eql :>=)) args &key event message condition)
  (let ((field (car args)))
    (unparse-comparison-validation
     field "greater_than_or_equal_to" (cadr args)
     (format nil "~s"
                   (or message (format nil "the value of ~a must be greater than or equal ~
                                            to ~a. \"%{value}\" is too small"
                                       (long-name field) (unparse-expression (cadr args) :english))))
     :event event :condition condition)))

(defmethod unparse-model-validation ((op (eql :=)) args &key event message condition)
  (let ((field (car args)))
    (unparse-comparison-validation
     field "equal_to" (cadr args)
     (format nil "~s"
                   (or message (format nil "the value of ~a must be ~a."
                                       (long-name field) (unparse-expression (cadr args) :english))))
     :event event :condition condition)))

(defmethod unparse-model-validation ((op (eql :!=)) args &key event message condition)
  (let ((field (car args)))
    (unparse-comparison-validation
     field "other_than" (cadr args)
     (format nil "~s"
                   (or message (format nil "the value of ~a must not be ~a."
                                       (long-name field) (unparse-expression (cadr args) :english))))
     :event event :condition condition)))

(defmethod unparse-model-validation ((op (eql :between)) args &key event message condition)
  (let ((field (car args)))
    (unparse-comparison-validation
     field "in" (ruby:unparse-range (cadr args) (caddr args))
     (format nil "~s"
                   (or message (format nil "the value of ~a must be between ~a and ~a."
                                       (long-name field) (cadr args) (caddr args))))
     :event event :condition condition)))

(defmethod unparse-model-validation ((op (eql :odd)) args &key event message condition)
  (let ((field (car args)))
    (unparse-comparison-validation
     field "odd" "true"
     (format nil "~s"
                   (or message (format nil "the value of ~a must be odd."
                                       (long-name field))))
     :event event :condition condition)))

(defmethod unparse-model-validation ((op (eql :even)) args &key event message condition)
  (let ((field (car args)))
    (unparse-comparison-validation
     field "even" "true"
     (format nil "~s"
                   (or message (format nil "the value of ~a must be even."
                                       (long-name field))))
     :event event :condition condition)))

;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
