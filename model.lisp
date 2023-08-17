;;;===========================================================================
;;; file:   model.lisp
;;; auth:   Coby Beck
;;; date:   2020-12-04
;;;
;;;---------------------------------------------------------------------------
;;;   code associated with generating the model aspect of Rails MVC framework 
;;;---------------------------------------------------------------------------  
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :ror)

(defun model-entities (&optional (app *application*))
  (remove-if #'(lambda(ent)
                 (typep ent 'specialized-entity))
             (schema-entities app)))
;; still working out best rails approach to defining STI models
;;  this method is for each model in its own file
(defun model-entities (&optional (app *application*))
  (schema-entities app))

(defun application_record.rb ()
  (let ((file (merge-pathnames
               (make-pathname :name "application_record"
                              :type "rb")
               (model-directory)))
        (*nesting-level* 0))
    (with-open-file (mod-file file :direction :output :if-exists :supersede)
      (format-file-notice mod-file "application_record.rb")
      ;; db_import is used in seeds.rb (not really necessary)
      (format mod-file "~&class ApplicationRecord < ActiveRecord::Base~%
  self.abstract_class = true

  def self.db_import!(data)
    return true unless data.present?
    data.each do |atts|
      self.find_or_create_by!(atts) unless atts[:id].present? && self.where(id: atts[:id]).present?
    end
  end
  def check_deleteable?
    true
  end
end~%"))))

(defun generate-model-files (&optional (app *application*))
  (application_record.rb)
  (dolist (entity (model-entities app))
    (let ((file (model-file-path entity))
          (*nesting-level* 0))
      (with-open-file (mod-file file :direction :output :if-exists :supersede)
        (format-file-notice mod-file "write-model-class")
        (write-model-class entity mod-file)))))

(defmethod write-model-class ((entity symbol) &optional stream)
  (when (find-entity entity)
    (write-model-class (find-entity entity) (or stream t))))

(defmethod write-model-class ((entity entity) &optional stream)
;;(if validates_with goes in then write the validator class first
  (when nil ;(requires-active-model-code? entity)
    (format stream "~%~{~a~%~}"
            (mapcar #'write-validator-method
                    (remove-if-not #'ror::calls-custom-method?
                                   (apply #'append
                                          (mapcar #'constraints
                                                  (persistent-attributes entity)))))))
  (format stream "~%~a~%" (model-class-declaration entity))
  (let ((includes (required-include-statements entity)))
    (when includes
      (format stream "~{  ~a~%~}" includes)))
  (write-model-class-body entity stream)
  (format stream "~%end~%"))

(defmethod write-model-class ((entity generalized-entity) &optional stream)
  (call-next-method))
;  (dolist (sub (subclasses entity))
;    (write-model-class sub stream)))

(defmethod write-model-class-body ((ent entity) &optional stream)
  (with-nesting
    (let ((format-string (format nil "~~{~a~~a~~%~~}" (make-indent))))
      (when (and *authenticated-application?* (authenticating-model? ent))
        (insert-authentication-model-code ent stream))
      (register-callbacks ent stream)
      (format stream "~%  #  relationships~%")
      (dolist (role (my-roles ent))
        (let ((code (declare-model-association role)))
          (when code (format stream "~a~a~%" (make-indent) code))))
      (let ((validations (model-validations ent)))
        (when validations
          (format stream "~%  #  validations~%")
          (format stream format-string validations)))
      (when (calculated-attributes ent)
        (format stream "~%  #  derived attributes ~%")
        (format stream format-string (derived-model-attributes ent)))
      (when (states ent)
        (format stream "~%  #  model state methods ~%")
        (format stream format-string (model-state-methods ent))
        (format stream "~%  #  model state scopes ~%")
        (format stream format-string (model-state-scopes ent)))
      (when (dependent-children ent)
        (format stream "~%~a~a" (make-indent) (check_deleteable? ent)))
      (write-public-callback-methods ent stream)
      (write-callback-methods ent stream)
      (meta_data ent stream))))


(defun check_deleteable?(entity)
  (ruby::unparse-method "check_deleteable?" nil
    (as-literal
     (format nil "~{self.~a.empty?~^ && ~}" (mapcar #'schema-name (dependent-children entity))))))

(defmethod model-class-declaration ((ent specialized-entity))
  (format nil "class ~a < ~a" (model-name ent) (model-name (super ent))))
(defmethod model-class-declaration ((ent schema-entity))
  (format nil "class ~a < ApplicationRecord" (model-name ent)))

(defun requires-active-model-code? (entity)
  (some #'calls-custom-method?
        (apply #'append
               (mapcar #'constraints
                       (persistent-attributes entity)))))

(defun required-include-statements (entity)
  (let (statements)
    (when (requires-active-model-code? entity)
      (push "include ActiveModel::Validations" statements))
    statements))

(defun calls-custom-method? (constraint)
  (recursive-find (get-operator '$call) (expression constraint)))

(defmethod model-validations ((ent entity))
  (append
   (apply #'append (mapcar #'attribute-model-validations (persistent-attributes ent)))
   (record-model-validations ent)
   ))

(defmethod record-model-validations ((ent entity))
  (when (not (read-only? ent))
    (remove nil (mapcar #'format-model-validation (constraints ent)))))

(defmethod attribute-model-validations ((att audit-attribute)) nil)
(defmethod attribute-model-validations ((att primary-key)) nil)
(defmethod attribute-model-validations ((att foreign-key))
  (when (not (read-only? att))
    (let ((constraints (remove-if #'null-constraint? (constraints att))))
      (remove nil (mapcar #'format-model-validation constraints)))))

(defmethod attribute-model-validations ((att attribute))
  (when (not (read-only? att))
    (remove nil (mapcar #'format-model-validation (constraints att)))))
  
(defmethod derived-model-attributes ((ent entity))
  (append
   (mapcar #'unparse-summary-attribute
           (remove-if-not #'(lambda(att) (typep att 'cached-summary))
                          (summary-attributes ent)))
   (mapcar #'unparse-derived-attribute
           (remove-if #'(lambda(att) (typep att 'cached-calculation))
                      (calculated-attributes ent)))))

(defmethod model-state-methods ((ent entity))
  (mapcar #'(lambda(state)
              (ruby::unparse-method (strcat "is_" (snake-case (name state)) "?") nil
                                    (unparse-attribute-references
                                     (expression (predicate state)) ent)))
          (states ent)))

(defmethod model-state-scopes ((ent entity))
  (mapcar #'format-model-scope (states ent)))

(defmethod format-model-scope ((state entity-state))
  (let ((lambda-body
         (as-literal (format nil "where(~s)"
                             (sql::unparse-expression
                              (sql::unparse-attribute-references (expression (predicate state))
                                                                 (my-entity state)))))))
    (format nil "scope :~a, ~a" (snake-case (name state))
            (ruby::unparse-lambda nil lambda-body))))

(defun meta_data(entity &optional stream)
  (format stream "~%~adef self.meta_data" (make-indent))
    (with-nesting
        (format stream (ruby::unparse-hash (entity-meta-data (find-entity entity)))))
  (format stream "~%~aend~%" (make-indent)))

(defun entity-meta-data (entity)
  (let* ((attributes (append (foreign-keys entity) (user-attributes entity) (summary-attributes entity) (derived-attributes entity))))
	`((:attributes  ,(loop for att in (sort (remove-duplicates attributes) #'string-lessp :key #'name)
                            collect `(,(keywordify (schema-name att))
	                                  ((:logical_type ,(keywordify (snake-case (name (logical-type att)))))
                                       (:domain ,(domain-data (domain att)))
	                                   (:default ,(ignore-errors (unparse-default-meta-data att)))
	                                   (:nullable?  ,(or (nullable? att) 'false))))))
      (:read_only? ,(or (read-only? entity) 'false))
      (:keep_history? ,(or (keep-history? entity) 'false))
      (:select_display_method ,nil) ;; use look up table properties, find userselectdata field or code?
      (:select_data ,nil) ;; use look up table properties, find attribute field or use id
      (:children ,(mapcar #'(lambda (c) (make-symbol (model-name c))) (children entity)))
      (:parents ,(mapcar #'(lambda (c) (make-symbol (model-name c))) (parents entity))))))

(defmethod domain-data((domain t))
  (remove nil
          (list (list :domain_type (if (eql (type-of domain) 'attribute-domain) :user_controlled (keywordify (string-downcase (substitute #\_ #\- (symbol-name (type-of domain)))))))
	            (list :data_type (data-type domain))
                (cond
                  ((typep domain 'static-enumeration)
                   (list :source (legal-values domain)))
                  ((typep domain 'referential-enumeration)
                   (let ((data-source (data-source domain)))
                     (list :source (list (make-symbol (or (ignore-errors (model-name (my-entity data-source))) (name (simian::my-attribute domain))))
                                         (keywordify (or (ignore-errors (schema-name data-source)) (name (simian::my-attribute domain))))))))
                  (t nil))
                (when nil ;; (constraints domain)
                  (list :constraints (mapcar #'formula (constraints domain)))))))

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
      (t (format nil "-> (obj) { \"~a\" }" (ruby::unparse-expression default))))))


;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
