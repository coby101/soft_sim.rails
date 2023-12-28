;;;============================================================
;;;
;;; unparse-attribute-references will be called by functions trying to write methods and
;;; expressions for model class definitions and erb code (derived attributes, inheritied
;;; fields and state predicates). It finds attribute objects that may require some kind of
;;; special reference code and does the necessary work to provide this as appropriate.
;;;
;;;============================================================
 
(in-package #:rails-unparser)

(defmethod unparse-attribute-references :before ((exp t) (context t) &optional obj-var)
  (declare (ignorable obj-var)))

(defmethod unparse-attribute-references ((exp t) (context t) &optional obj-var)
  (declare (ignorable obj-var))
  (error "this one fell through the cracks: ~a - ~a" exp context))
(defmethod unparse-attribute-references ((exp operator) (context t) &optional obj-var)
  (declare (ignorable obj-var))
  exp)
(defmethod unparse-attribute-references ((exp number) (context t) &optional obj-var)
  (declare (ignorable obj-var))
  exp)
(defmethod unparse-attribute-references ((exp string) (context t) &optional obj-var)
  (declare (ignorable obj-var))
  exp)
(defmethod unparse-attribute-references ((exp entity) (context t) &optional obj-var)
  (declare (ignorable obj-var))
  exp)
(defmethod unparse-attribute-references ((obj entity-state) (context t) &optional obj-var)
  (unparse-attribute-references (expression (predicate obj)) context obj-var))
(defmethod unparse-attribute-references ((att attribute) (context attribute) &optional obj-var)
  (unparse-attribute-references att (my-entity context) obj-var))

;; not specializing on calculated-attribute as there will be a method with the attribute's
;; name and it can be directly referenced the same as a stored attribute
(defmethod unparse-attribute-references ((att attribute) (context entity) &optional obj-var)
  (if (eq (my-entity att) context)
      (as-literal (strcat (if obj-var (strcat obj-var ".") "") (schema-name att)))
      (error "unable to resolve attribute reference (~a, ~a) without a condition component" att context)))

(defmethod unparse-attribute-references ((att attribute) (context relation) &optional obj-var)
  (if (find att (attributes context))
      (as-literal
       (format nil "~a~a.~a"
               (if obj-var (strcat obj-var ".") "")
               (instance-name context) (schema-name att)))
      (error "unable to resolve attribute reference (~a, ~a)" att context)))

(defmethod unparse-attribute-references ((att attribute) (context entity) &optional obj-var)
  (let ((path (path-to-attribute context att)))
    (as-literal
     (format nil "~a~{~a~^.~}" (if obj-var (strcat obj-var ".") "")
             (mapcar #'instance-name path)))))

(defun reachable-field? (field context)
  (format t "~a ~a" field context)
  (or (eq context (my-entity field))
      (typep context 'relation)
      (and (typep context 'entity)
           (path-to-attribute context field))))

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

(defmethod unparse-attribute-references ((expr list) (context attribute) &optional obj-var)
  (unparse-attribute-references expr (my-entity context) obj-var))
(defmethod unparse-attribute-references ((expr list) (context t) &optional obj-var)
  (if (field-reference-expression? expr)
      ;; stuck here: if expr is 3 long there is a where expression with two contexts for attribute var names
      (if (= 2 (length expr))
          (progn
            (unless (reachable-field? (cadr expr) context)
              (error "unless an attribute is specified in the context of a relationship or is ~
                  otherwise reachable, there needs to be a conditional component for a ~
                  proper reference (~a - context: ~a)" expr context))
            (unparse-attribute-references (cadr expr) (car expr) obj-var))
          ;; we are here because this is a reference to an attribute in an unrelated model
          (as-literal (strcat (unparse-find-record (first expr) (cddr expr)) "."
                              (schema-name (cadr expr)))))
      (mapcar #'(lambda (ex)
                  (unparse-attribute-references ex context obj-var))
              expr)))

;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
