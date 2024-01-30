;;;============================================================================================
;;;
;;;   unparse-attribute-references will be called by functions trying to write methods and
;;;   expressions for model class definitions and erb code (derived attributes, inheritied
;;;   fields and state predicates). As a kind of preprocessor, it finds attribute objects
;;;   in an expression (formula, constraint, default value etc) that may require some kind
;;;   of special reference code based on a context argument. It does the necessary work to
;;;   provide this as appropriate. The end result is the original expression with all the
;;;   attribute references replaced with a $literal expression to become a part of the 
;;;   original unparsing operation. Callers can supply an optional object variable name
;;;
;;;============================================================================================
 
(in-package #:rails-unparser)

(defmethod unparse-attribute-references ((exp t) (context t) &optional obj-var)
  "go through an expression and using the given context replace all occurrences of an 
   attribute object or attribute reference dotted lists with $literal expressions of code
   that will correctly return the attribute value. eg. in the simplest case an attribute
   referred to in the context of its own entity object needs only its schema_name."
  (declare (ignorable obj-var))
  (error "this one fell through the cracks: ~a - ~a" exp context))

(defmethod unparse-attribute-reference ((exp t) (context t) &optional obj-var)
  "given an attribute object from a fully resolved expression, return a literal code
   snippet that can be used within the given context to reference the attribute value.
   The most simple case is where context is the attribute's own entity and the code is
   just schema-name. If the attribute is the end of a dotted-list field reference
   then resolve the relationship chain Rails will need to retrive the value. eg. given
   Employee belongs to Employee as my_manager and belongs to Division as employer, and
   Division belongs to Company then, when context is an Employee model, the implementation
   code will need to use my_manager&.employer&.company_type to get company_type value"
  (declare (ignorable obj-var))
  (error "this one fell through the cracks: ~a - ~a" exp context))

(defmethod unparse-attribute-references ((expr list) (context t) &optional obj-var)
  (mapcar #'(lambda (ex) (unparse-attribute-reference ex context obj-var)) expr))

(defmethod unparse-attribute-reference ((expr list) (context t) &optional obj-var)
  (if (field-reference-expression? expr)
      (labels ((chain (dotted-list)
                 (if (atom dotted-list)
                     (format nil "~a~a" (if obj-var (strcat obj-var ".") "") (schema-name dotted-list))
                     (format nil "~a&.~a" (instance-name (car dotted-list)) (chain (cdr dotted-list))))))
        (as-literal (chain expr)))
      (unparse-attribute-references expr context obj-var)))

(defmethod unparse-attribute-references ((obj attribute)    (context t)  &optional obj-var) (unparse-attribute-reference  obj context obj-var))
(defmethod unparse-attribute-references ((obj entity-state) (context t)  &optional obj-var) (unparse-attribute-references (expression (predicate obj)) context obj-var))
(defmethod unparse-attribute-references ((obj formula)      (context t)  &optional obj-var) (unparse-attribute-references (expression obj) context obj-var))
(defmethod unparse-attribute-references ((expr list) (context attribute) &optional obj-var) (unparse-attribute-references expr (my-entity context) obj-var))
;; I don't recalll why this was added, seems deletable..
(defmethod unparse-attribute-references :before ((exp t) (context t) &optional obj-var) (declare (ignorable obj-var)))

(defmethod unparse-attribute-reference ((exp operator) (context t) &optional obj-var) (declare (ignorable obj-var)) exp)
(defmethod unparse-attribute-reference ((exp number)   (context t) &optional obj-var) (declare (ignorable obj-var)) exp)
(defmethod unparse-attribute-reference ((exp string)   (context t) &optional obj-var) (declare (ignorable obj-var)) exp)
(defmethod unparse-attribute-reference ((exp entity)   (context t) &optional obj-var) (declare (ignorable obj-var)) exp)
(defmethod unparse-attribute-reference ((att attribute) (context attribute) &optional obj-var) (unparse-attribute-references att (my-entity context) obj-var))

;; not specializing on calculated-attribute as there will be a method with the attribute's
;; name and it can be directly referenced the same as a stored attribute
(defmethod unparse-attribute-reference ((att attribute) (context entity) &optional obj-var)
  (if (eq (my-entity att) context)
      (as-literal (strcat (if obj-var (strcat obj-var ".") "") (schema-name att)))
      (error "unable to resolve attribute reference (~a, ~a) without a condition component" att context)))

(defmethod unparse-attribute-reference ((att attribute) (context relation) &optional obj-var)
  (if (find att (attributes context))
      (as-literal
       (format nil "~a~a.~a"
               (if obj-var (strcat obj-var ".") "")
               (instance-name context) (schema-name att)))
      (error "unable to resolve attribute reference (~a, ~a)" att context)))

;; this method should not be wanted as the object model post-processing will not leave
;; unresolved attribute references. it was overriding the same specialization above, 
;; so let's just disable it for now and see (2024.01.22)
;; (defmethod unparse-attribute-reference ((att attribute) (context entity) &optional obj-var)
;;   (let ((path (path-to-attribute context att)))
;;     (as-literal
;;      (format nil "~a~{~a~^.~}" (if obj-var (strcat obj-var ".") "")
;;              (mapcar #'instance-name path)))))

;; expecting only literal strings or standard operator expression (albeit simple ones)
(defmethod unparse-find-condition ((con string) &optional args)
  (when args (error "we should not have any args here. ~a ~a" con args))
  (format nil "'~a'" con))

(defmethod unparse-find-condition ((obj list) &optional args)
  (if (or (typep (car obj) 'operator) (operator-symbol? (car obj)))
      (progn
        (when args (error "we shouldn't have any args here...? (~a)" args))
        (unparse-expression (car obj) :ruby (cdr obj)))
      (unparse obj :ruby))) ;; not sure this UNPARSE will ever be appropriate...

(defmethod unparse-find-condition ((operator (eql '$eql)) &optional args)
  (format nil "~a: ~a" (unparse-expression (car args) :ruby) (unparse-expression (cadr args) :ruby)))

(defun unparse-find-record (entity conditions)
  (format nil "~a.find_by(~{~a~^, ~})" (model-name entity)
      (mapcar #'unparse-find-condition conditions)))

;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
