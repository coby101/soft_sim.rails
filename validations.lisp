;;;===========================================================================
;;; file: validations.lisp auth: Coby Beck date: 2021-08-03
;;; ---------------------------------------------------------------------------
;;; code associated with generating the model validations
;;; ---------------------------------------------------------------------------
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :ror)

(defparameter *handled-validation-helpers*
  '($when $unless $call $not-null $regex $in $length $length-lt $length-between
    $length-gt $unique-within $unique $> $gt $< $lt $<= $>= $= $!= $between $odd $even))

(defun simple-validation-helper? (exp)
  (cond
    ((comparison-operator? (car exp))
     (notany #'returns-date? (cdr exp)))
    ((member (operator-key (car exp)) '($when $unless))
     (and (simple-validation-helper? (second exp))
           (simple-validation-helper? (third exp))))
    ((eql (operator-key (car exp)) '$unique-within)
     (and (typep (caddr exp) 'attribute)
          (eq (my-entity (cadr exp)) (my-entity (caddr exp)))))
    (t (and (member (operator-key (car exp)) *handled-validation-helpers*)
            (typep (cadr exp) 'attribute)
            (contains-constants-only (cddr exp))
            (not (recursive-find (get-operator '$current-date) exp))))))

(defun simple-validation-method? (exp context)
  (cond ((member (operator-key (car exp)) '($when $unless))
         (and (simple-validation-method? (second exp) context)
              (simple-validation-method? (third exp) context)))
        ((member (operator-key (car exp)) '($rows-eql $min-rows $max-rows))
         (simple-validation-method? (fourth exp) context))
        (t (contains-reachable-values? (cdr exp) context))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  validation methods
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun write-validator-method (constraint)
  (declare (ignorable constraint))
  nil)
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

(defmethod rewrite-expression ((operator t) expression)
  (ruby:negate-expression expression))

(defmethod rewrite-expression ((operator (eql '$when)) expression)
  (list (get-operator '$and)
        (second expression)
        (ruby:negate-expression (third expression))))

(defmethod rewrite-expression ((operator (eql '$max-rows)) expression)
  (list (get-operator '$gt)
        (list (get-operator '$rows) (second expression) (fourth expression))
        (third expression)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; simple rails validation helpers
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod format-model-validation ((exp t))
  (comment-with-warning
   nil "no format-model-validation method written for ~s of type ~a"
   exp (type-of exp)))

(defmethod format-model-validation ((exp list))
  (format-model-validation
   (make-instance 'constraint
       :formula exp
       :message nil :event nil)))

(defmethod format-model-validation ((con constraint))
  (let ((exp (expression con))
        (event (when (not (eql (event con) :all)) (event con)))
        (msg (message con)))
    (cond ((simple-validation-helper? exp)
           (unparse-model-validation (car exp) (cdr exp) :event event :message msg))
          ((simple-validation-method? exp (context (formula con)))
           (unparse-validation-constraint con))
          (t (comment-with-warning
              nil "can't do this one: ~a" (english:unparse-expression exp))))))

(defun unparse-field-validation (field type &rest options)
  (let* ((args
          (if (and (< (length options) 2)
                   (or (null options)
                       (stringp (car options))))
              ;; eg "true" is the only option
              (car options)
              (mapcar #'(lambda(opt)
                          (format nil "~a: ~a" (car opt) (cadr opt)))
                      (remove nil
                              (append options
                                      (when (nullable? field)
                                        (list (list "allow_blank" "true"))))))))
         (formatted-args (cond ((null args) "{ }")
                               ((atom args) (format nil "~a" args))
                               ((= (length args) 1)
                                (format nil "{ ~a }" (car args)))
                               (t
                                (format nil "~%    { ~{~a~^,~%      ~}}" args)))))
    (format nil "validates :~a, ~a: ~a" (schema-name field) type formatted-args)))

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



(defmethod unparse-model-validation ((op (eql '$when)) args &key event message condition)
  (when condition
    (error "it does not make sense to have a condition here (~a, ~a)"
           op condition))
  (unparse-conditional-validation op args :event event :message message))
(defmethod unparse-model-validation ((op (eql '$unless)) args &key event message condition)
  (when condition
    (error "it does not make sense to have a condition here (~a, ~a)"
           op condition))
  (unparse-conditional-validation op args :event event :message message))

(defun unparse-conditional-validation (op args &key event message)
  (let* ((predicate (first args))
         (consequent (second args)))
    (unparse-model-validation
     (car consequent) (cdr consequent)
     :event event :message message
     :condition (list op predicate))))

(defmethod unparse-model-validation ((op (eql '$call)) args &key event message condition)
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

(defmethod unparse-model-validation ((op (eql '$not-null)) args &key event message condition)
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

(defmethod unparse-model-validation ((op (eql '$regex)) args &key event message condition)
  (apply #'unparse-field-validation
         (car args) "format"
         (remove nil
                 (list (when message (list "message" (format nil "~s" message)))
                       (list "with" (format nil "/\\A~a\\z/" (cadr args)))
                       (make-conditional-option condition)
                       (make-event-option event)))))

(defmethod unparse-model-validation ((op (eql '$in)) args &key event message condition)
  (let* ((att (car args))
         (legal-values (if (eql (data-type att) :boolean)
                           (if (implement-as-string? att)
                               (legal-values (domain att))
                               (list t nil))
                           (cdr args))))
    (unparse-field-validation
     att "inclusion"
     (list "in" (ruby:unparse-array (append legal-values (when (nullable? att) (list :null)))))
     (list "message"
           (format nil "~s"
                   (or message (format nil "~a value must be one of ~a. \"%{value}\" is not in there"
                                       (long-name att)
                                       (format-acceptable-value-list (cdr args))))))
     (make-conditional-option condition)
     (make-event-option event))))

(defmethod unparse-model-validation ((op (eql '$length)) args &key event message condition)
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

(defmethod unparse-model-validation ((op (eql '$length-lt)) args &key event message condition)
  (unparse-field-validation
   (car args) "length"
   (list "maximum" (1- (cadr args)))
   (list "message"
         (format nil "~s"
                 (or message (format nil "~a length must be shorter than ~r characters. \"%{value}\" is too long"
                                     (long-name (car args)) (cadr args)))))
   (make-conditional-option condition)
   (make-event-option event)))

(defmethod unparse-model-validation ((op (eql '$length-between)) args &key event message condition)
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

(defmethod unparse-model-validation ((op (eql '$length-gt)) args &key event message condition)
  (unparse-field-validation
   (car args) "length"
   (list "minimum" (1+ (cadr args)))
   (list "message"
         (format nil "~s"
                 (or message (format nil "~a length must be longer than ~r character~:p. \"%{value}\" is too short"
                                     (long-name (car args)) (cadr args)))))
   (make-conditional-option condition)
   (make-event-option event)))

(defmethod unparse-model-validation ((op (eql '$unique)) args &key event message condition)
  (if (tenant-scoped-entity? (my-entity (car args)))
      (unparse-model-validation '$unique-within
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

(defmethod unparse-model-validation ((op (eql '$unique-within)) args &key event message condition)
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

(defmethod unparse-model-validation ((op (eql '$>)) args &key event message condition)
  (unparse-model-validation '$gt args :event event :condition condition :message message))
(defmethod unparse-model-validation ((op (eql '$gt)) args &key event message condition)
  (let ((field (car args)))
    (unparse-comparison-validation
     field "greater_than" (cadr args)
     (format nil "~s"
             (or message (format nil "the value of ~a must be greater than ~a. \"%{value}\" is too small"
                                 (long-name field) (english:unparse-expression (cadr args)))))
     :event event :condition condition)))

(defmethod unparse-model-validation ((op (eql '$<)) args &key event message condition)
  (unparse-model-validation '$lt args :event event :condition condition :message message))
(defmethod unparse-model-validation ((op (eql '$lt)) args &key event message condition)
  (let ((field (car args)))
    (unparse-comparison-validation
     field "less_than" (cadr args)
     (format nil "~s"
             (or message (format nil  "the value of ~a must be less ~
                                             than ~a. \"%{value}\" is too large"
                                 (long-name field) (english:unparse-expression (cadr args)))))
     :event event :condition condition)))

(defmethod unparse-model-validation ((op (eql '$<=)) args &key event message condition)
  (let ((field (car args)))
    (unparse-comparison-validation
     field "less_than_or_equal_to" (cadr args)
     (format nil "~s"
                   (or message (format nil "the value of ~a must be less than or equal ~
                                            to ~a. \"%{value}\" is too large"
                                       (long-name field) (english:unparse-expression (cadr args)))))
     :event event :condition condition)))

(defmethod unparse-model-validation ((op (eql '$>=)) args &key event message condition)
  (let ((field (car args)))
    (unparse-comparison-validation
     field "greater_than_or_equal_to" (cadr args)
     (format nil "~s"
                   (or message (format nil "the value of ~a must be greater than or equal ~
                                            to ~a. \"%{value}\" is too small"
                                       (long-name field) (english:unparse-expression (cadr args)))))
     :event event :condition condition)))

(defmethod unparse-model-validation ((op (eql '$=)) args &key event message condition)
  (let ((field (car args)))
    (unparse-comparison-validation
     field "equal_to" (cadr args)
     (format nil "~s"
                   (or message (format nil "the value of ~a must be ~a."
                                       (long-name field) (english:unparse-expression (cadr args)))))
     :event event :condition condition)))

(defmethod unparse-model-validation ((op (eql '$!=)) args &key event message condition)
  (let ((field (car args)))
    (unparse-comparison-validation
     field "other_than" (cadr args)
     (format nil "~s"
                   (or message (format nil "the value of ~a must not be ~a."
                                       (long-name field) (english:unparse-expression (cadr args)))))
     :event event :condition condition)))

(defmethod unparse-model-validation ((op (eql '$between)) args &key event message condition)
  (let ((field (car args)))
    (unparse-comparison-validation
     field "in" (ruby:unparse-range (cadr args) (caddr args))
     (format nil "~s"
                   (or message (format nil "the value of ~a must be between ~a and ~a."
                                       (long-name field) (cadr args) (caddr args))))
     :event event :condition condition)))

(defmethod unparse-model-validation ((op (eql '$odd)) args &key event message condition)
  (let ((field (car args)))
    (unparse-comparison-validation
     field "odd" "true"
     (format nil "~s"
                   (or message (format nil "the value of ~a must be odd."
                                       (long-name field))))
     :event event :condition condition)))

(defmethod unparse-model-validation ((op (eql '$even)) args &key event message condition)
  (let ((field (car args)))
    (unparse-comparison-validation
     field "even" "true"
     (format nil "~s"
                   (or message (format nil "the value of ~a must be even."
                                       (long-name field))))
     :event event :condition condition)))

(defun make-event-option(event)
  (when event (list "on" (strcat ":" (string-downcase (symbol-name event))))))

(defun make-conditional-option (conditional)
  (when conditional
    (let* ((operator (if (symbolp (car conditional))
                         (car conditional)
                         (operator-key (car conditional))))
           (arg (second conditional))
           (option (if (member operator '($if $when)) "if" "unless"))
           (expr (if (symbolp arg)
                     (strcat ":" (ruby:unparse-expression arg))
                     (ruby:unparse-lambda nil arg))))
      (list option expr))))

;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
