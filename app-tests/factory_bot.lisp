;;;===========================================================================
;;;
;;;   code associated with generating Factory_Bot factory definitions
;;;
;;;===========================================================================

(in-package :app-tests)

(defmethod test-datum ((type logical-type) (att attribute)) (test-datum (id type) att))
(defmethod test-datum ((type symbol) (att attribute))
  (ruby:unparse-data :string (format nil "~a not implemented" type)))

(defmethod test-datum ((type (eql :id-name)) (att attribute))
  (format nil "| i | \"~a #{i}\"" (short-name (my-entity att))))
(defmethod test-datum ((type (eql :user-key)) (att attribute))
  (format nil "| i | \"~a#{i}\"" (short-name att)))

(defmethod test-datum ((type (eql :date)) (att attribute))
  (unparse-expression :current-date :ruby))

(defmethod test-datum ((type (eql :name)) (att attribute))
  "Faker::Name.first_name")

(defmethod test-datum ((type (eql :birthdate)) (att attribute))
  "Faker::Date.between(from: 60.years.ago, to: 20.years.ago)")

(defmethod test-datum ((type (eql :label)) (att attribute))
  "Faker::Alphanumeric.alphanumeric(number: 10)")

(defmethod test-datum ((type (eql :email)) (att attribute))
  (let* ((model (my-entity att))
         (based-on (or (find-field :name model)
                       (find-field :givenname model)
                       (find-field :code model)))
         (prefix (if based-on (format nil "#{~a}" (schema-name based-on)) "email"))
         (unique? (unique? att)))
    (strcat (if unique? "| i | " "")
            (ruby:unparse-data :string
      (format nil "~a~a@email.com"
              prefix
              (if unique? "#{i}" ""))))))

(defun testable-entities (&optional (app *application*))
 (application-entities app))

(defun factory-attribute-name (attribute)
  (if (unique? attribute)
      (format nil "sequence(:~a)" (schema-name attribute))
      (schema-name attribute)))

(defun unparse-factory-association (parent)
  (if (string-equal (name parent) (name (entity parent)))
      (snake-case (name parent))
      (format nil "association :~a, factory: :~a" (snake-case (name parent)) (snake-case (name (entity parent)))))) 

(defmethod attribute-with-datum ((attribute attribute))
  (format nil "~a { ~a }" (factory-attribute-name attribute)
          (test-datum (logical-type attribute) attribute)))

(defun unparse-factory (model &optional stream)
  (let ((parents (mapcar #'lhs (required-parental-relationships model)))
        (attributes (remove-if #'(lambda (att)
                                   (or (default-value att) (nullable? att)))
                               (persistent-attributes model))))
    (format stream "~afactory :~a do" (make-indent) (snake-case (name model)))
    (with-nesting
      (let ((fmt-str (format nil "~~{~%~a~~a~~}" (make-indent))))
        (format stream fmt-str (mapcar #'attribute-with-datum attributes))
        (format stream fmt-str (mapcar #'unparse-factory-association parents))))
    (format stream "~%~aend" (make-indent))))

(defun write-factory-file (model &optional (stream t))
  (format-file-notice stream "write-factory-file")
  (unparse-factory model stream))

(defun write-factories (&optional (app *application*))
  (let ((testable-models (testable-entities app)))
    (dolist (model testable-models)
      (with-open-file (ffile (factories-file-path (schema-name model)) :direction :output :if-exists :supersede)
        (write-factory-file model ffile)))))

;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:

