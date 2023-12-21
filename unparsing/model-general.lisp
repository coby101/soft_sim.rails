;;;====================================================
;;;
;;;   Code for unparsing general code in model files
;;;
;;;====================================================
 
(in-package :rails-unparser)

;(defvar conditions (cddr expr))

;ModelName.find_by(attribute-name: value)
;PhoneType.find_by name: n
;PhoneType.find_by("name = '#{n}'").id
;(defvar contexts (list (find-entity :parent) (find-entity :other)))
;(setf expr (find-field :index :other))
;; this is kind of a mirror to resolve-symbol-references in formulas.lisp
'(defun unparse-where-clause-refs (expr &key obj-var context other-contexts)
    "go through an expression and determine if attributue objects need extra
     work to reference them in unparsing code given possibly multiple contexts"
  (declare (ignorable obj-var))
  (cond
    ((null expr) nil)
    ((typep expr 'operator) expr)
    ((keywordp expr) expr)
    ((typep expr 'attribute)
     (let ((possible-context (remove-if-not
                  #'(lambda (e) (eq e (my-entity expr)))
                  (mapcar #'(lambda(c)
                      (if (typep c 'entity) c (entity c)))
                      (list* context other-contexts)))))
       (if (= 1 (length possible-context))
           (unparse-attribute-references expr (car possible-context))
           (let ((fmtstr (if (null possible-context)
                             "unable to reach attribute ~a in any of the given contexts: ~a"
                             "more than one possible route to ~a in these contexts: ~a")))
             (error fmtstr expr (list* context other-contexts))))))
    ((atom expr) expr) ;; numbers, strings
    ((field-reference-expression? expr) (unparse-attribute-references expr context))
    ((listp expr) (mapcar #'(lambda (item)
                  (apply #'unparse-where-clause-refs item (list* context other-contexts)))
              expr))
    (t (error "how did we get here?? ~a ~a" expr (list* context other-contexts)))))

(defmethod unparse-summary-attribute ((att summary-attribute))
  (ruby:unparse-method (schema-name att) nil
    (as-literal
     (format nil "self.~a.~a" (schema-name (car (path att))) (unparse-summary-method att)))))

(defmethod unparse-derived-attribute ((att calculated-attribute))
  (ruby:unparse-method (schema-name att) nil
       (unparse-attribute-references (expression att) (my-entity att))))

(defun extract-attributes-from-expression (exp &optional  context)
  (cond
    ((typep exp 'persistent-attribute)
     (let* ((model (or (and (typep context 'attribute) (my-entity context))
                       (and (typep context 'entity) context)
                       (and (typep context 'relation) (entity context)))))
       (when (or (null model) (eq model (my-entity exp)))
         exp)))
    ((listp exp) (mapcar #'(lambda (i) (extract-attributes-from-expression i context))
                         exp))
    (t nil)))

(defun vulnerable-to-null? (exp)
  (cond
    ((atom exp) nil)
    ((and (or (typep (car exp) 'operator) (operator-symbol? (car exp))) 
          (not (can-handle-null-args? (car exp))))
     (some #'(lambda(arg)
               (or (and (typep arg 'attribute) (nullable? arg))
                   (vulnerable-to-null? arg)))
           (cdr exp)))
    (t (some #'vulnerable-to-null? exp))))

(defmethod unparse-path ((att attribute) (context entity) &key bad-start context-var)
  (let ((path (path-to-attribute context att (my-entity att) bad-start)))
    (format nil "~a~{.~a~}" (or context-var (instance-name (car path)))
            (mapcar #'instance-name (cdr path)))))

(defun unparse-model-association (type relation &rest options)
  (format nil "~a :~a~{, ~a~}" type relation
          (mapcar #'(lambda(opt)
                      (format nil "~a: ~a" (car opt) (cadr opt)))
                  (remove nil options))))

(defun unparse-callback-registration (event timing method &rest options)
  (let* ((args (mapcar #'(lambda(opt)
                           (format nil "~a: :~a" (car opt) (unparse (if (eq (cadr opt) t) (cadr opt) (cadr opt)) :ruby)))
                       options)))
    (format nil "~a_~a :~a~{, ~a~}" timing event method args)))
#|
(unparse-callback-registration "validation" "after" "normalize_name")
(unparse-callback-registration "validation" "after" "normalize_name" (list "on" "create"))
|#

(defun unparse-default-meta-data(att)
  (let ((default (default-value att)))
    (cond
      ((null default) nil)
      ((eql (data-type att) :boolean)
       (if (member (default-value att)
                   (list "yes" "on" "true") :test #'string-equal)
           'true 'false))
      ((or (stringp default)
           (numberp default))
       default)
      (t (format nil "-> (obj) { \"~a\" }" (unparse-expression default :ruby))))))

(defun unparse-find_calendar_entities (calendar-relationships)
  (apply #'ruby:unparse-method "find_calendar_entities" nil
         (loop for rel in calendar-relationships
               collect
               (let* ((cal-side (lhs rel))
                      (cal-ent (entity cal-side))
                      (link-field (cadadr (expression (matchmaker rel))))
                      (find-by (if (eq cal-ent (find-entity :day))
                                   (format nil "date: ~a" (schema-name link-field))
                                   (format nil "start: ~a.at_beginning_of_~(~a~)"
                                           (schema-name link-field) (name cal-ent)))))
                 (as-literal (format nil "self.~a = ~a.find_by(~a)"
                                     (instance-name cal-side) (model-name cal-ent) find-by))))))

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

(defmethod unparse-default-value-expression ((att attribute) (context entity))
  (let ((default (default-value att)))
    (unless (typep default 'attribute)
      (error "unhandled default value ~a" (type-of default)))
    (if (eq (my-entity default) context)
      (unparse-expression (unparse-attribute-references default context) :ruby)
      (unparse-expression
       (list '$unless :ruby
             (as-literal (format nil "~{~a~^.~}.nil?"
                                 (mapcar #'instance-name (butlast (path-to-attribute context default)))))
             (unparse-attribute-references default context))
       :ruby))))

(defmethod unparse-summary-expression ((att attribute))
  (error "this method is not appropriate for non-summary attributes"))

(defmethod unparse-summary-expression ((att summary-attribute))
  (format nil "~a.~a" (snake-case (plural (my-entity (source att))))
		  (unparse-summary-method att)))

;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
