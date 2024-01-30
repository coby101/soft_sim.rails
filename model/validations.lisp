;;;===================================================
;;;
;;;   code for generating model validations
;;;
;;;===================================================

(in-package #:model)

(defparameter *handled-validation-helpers*
  '(:when :unless :call :not-null :regex :in :length :length-lt :length-between
    :length-gt :unique-within :unique :> :gt :< :lt :<= :>= := :!= :between :odd :even))

(defun contains-constants-only (exp)
  (every #'(lambda (elt)
             (cond ((atom elt) (not (typep elt 'attribute)))
                   (t (contains-constants-only elt))))
         exp))

(defun contains-reachable-values? (exp context)
  (or (contains-constants-only exp)
      (let ((entity (etypecase context
                      (entity context)
                      (relation (entity context))
                      (attribute (my-entity context)))))
        (every #'(lambda (elt)
                   (cond ((typep elt 'attribute)
                          (or (eq (my-entity elt) entity)
                              (path-to entity (my-entity elt))))
                         ((atom elt) (not (typep elt 'attribute)))
                         (t (contains-reachable-values? elt context))))
               exp))))

(defun simple-validation-helper? (exp)
  (cond
    ((comparison-operator? (car exp))
     (notany #'returns-date? (cdr exp)))
    ((member (operator-key (car exp)) '(:when :unless))
     (and (simple-validation-helper? (second exp))
           (simple-validation-helper? (third exp))))
    ((eql (operator-key (car exp)) :unique-within)
     (and (typep (caddr exp) 'attribute)
          (eq (my-entity (cadr exp)) (my-entity (caddr exp)))))
    (t (and (member (operator-key (car exp)) *handled-validation-helpers*)
            (typep (cadr exp) 'attribute)
            (contains-constants-only (cddr exp))
            (not (recursive-find (get-operator :current-date) exp))))))

(defun simple-validation-method? (exp context)
  (cond ((member (operator-key (car exp)) '(:when :unless))
         (and (simple-validation-method? (second exp) context)
              (simple-validation-method? (third exp) context)))
        ((member (operator-key (car exp)) '(:rows-eql :min-rows :max-rows))
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
              nil "can't do this one: ~a" (unparse-expression exp :english))))))

(defmethod record-model-validations ((ent entity))
  (when (not (read-only? ent))
    (remove nil (mapcar #'format-model-validation (constraints ent)))))

;; simian:audit-attribute may be deprecated, can do without this anyway for now.
;(defmethod attribute-model-validations ((att audit-attribute)) nil)
(defmethod attribute-model-validations ((att primary-key)) nil)
(defmethod attribute-model-validations ((att foreign-key))
  (when (not (read-only? att))
    (let ((constraints (remove-if #'null-constraint? (constraints att))))
      (remove nil (mapcar #'format-model-validation constraints)))))

(defmethod attribute-model-validations ((att attribute))
  (when (not (read-only? att))
    (remove nil (mapcar #'format-model-validation (constraints att)))))
  
(defmethod model-validations ((ent entity))
  (append
   (apply #'append (mapcar #'attribute-model-validations (persistent-attributes ent)))
   (record-model-validations ent)
   ))

;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
