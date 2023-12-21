;;;===========================================================================
;;;
;;;   Code for generating miscellaneous parts of controllers code
;;;
;;;===========================================================================

(in-package :controller)

(defmethod controller-class-declaration ((aspect aspect))
  (format nil "class ~a < ApplicationController~%" (controller-name aspect)))
(defmethod controller-class-declaration ((str string))
  (format nil "class ~a < ApplicationController~%" str))

(defun view-access-check (aspect)
  (format nil "before_action  -> { check_view_access (~s) }" (name (view aspect))))

;; staff_phones_attributes: {}, staff_notes_attributes: {}
(defun actions (aspect)
  (append (when (or (creatable? aspect) (updatable? aspect))
            (list :edit :update :patch))
          (when (creatable? aspect)
            (list :new :create :delete))
          (when (or (creatable? aspect) (updatable? aspect) (showable? aspect))
            (list :detail))
          (when (or (listable? aspect) (creatable? aspect)
                    (updatable? aspect) (showable? aspect))
            (list :list))))

(defmethod model_params-method ((aspect aspect) &optional (stream t))
  (let* ((entity (entity aspect))
         (instance-name (instance-name entity))
         (mvas (multi-valued-attributes entity))
         (modifiable-fields
           (if (typep (entity aspect) 'attribute-table)
               (extract-attributes (list-panel aspect))
               (remove-duplicates
                (flatten (append (extract-attributes (edit-panel aspect))
                                 (extract-attributes (add-panel aspect))))))))
    (format stream
            (ruby:unparse-method
             (format nil "~a_params" instance-name) nil
             (as-literal
              (format nil "params.require(:~a).permit(~{~a~^, ~})"
                      instance-name
                      (append (mapcar #'(lambda (f) (strcat ":" (schema-name f)))
                                        ; have to put the mva stuff at the end
                                      (remove-if #'(lambda (f)
                                                     (typep f 'multi-valued-attribute))
                                                 modifiable-fields))
                              (mapcar #'(lambda(mva)
                                            (format nil "~a_attributes: {}" (schema-name (child-entity mva))))
                                        mvas))))))))

(defmethod format-private-controller-methods ((aspect aspect) &optional (stream t))
  (format stream "~%~aprivate~%~%" (make-indent))
  (when (showable? aspect)
    (when (use-set_method? aspect)
      (format stream "~a~a~%~%" (make-indent) (set_model-method aspect nil))))
  (when (updatable? aspect)
    (comment-out stream "Only allow a list of trusted parameters through.")
    (format stream "~a~a" (make-indent) (model_params-method aspect nil))))

;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
