;;;===========================================================================
;;;
;;;   code associated with generating the controller aspect of Rails MVC framework 
;;;
;;;===========================================================================

(in-package :controller)

(defun access-denied-path () "/no_access")

(defun check-access-method ()
  (format nil "~adef check_view_access (view_name)
    current_view = View.find_by(name: view_name)
    if current_user.user_roles.pluck(:role_id).intersection(current_view.view_roles.pluck(:role_id)).empty?
      redirect_to '~a'~%~aend~%~aend~%" (make-indent) (access-denied-path)
      (with-nesting (make-indent)) (make-indent)))

(defun tenant-scoping-method ()
  (format nil "~adef tenant_scoping
    @tenant ||= current_user.try(:tenant)~%~aend~%"
          (make-indent) (make-indent)))

(defun application_controller.rb ()
  (let ((file (merge-pathnames
               (make-pathname :name "application_controller"
                              :type "rb")
               (controller-directory)))
        (*nesting-level* 0))
    (with-open-file (cont-file file :direction :output :if-exists :supersede)
      (format-file-notice cont-file "application_controller.rb")
      (format cont-file "~&class ApplicationController < ActionController:Base")
      (with-nesting
          (when *authenticated-application?*
            (format cont-file "~%~a~a" (make-indent) (app-controller-authentication-code)))
        (format cont-file "~%~abefore_action :db_application" (make-indent))
        (when *notified-application?*
          (format cont-file "~%~a~a" (make-indent) (app-controller-notification-code)))
        (format cont-file "~%~%~aprivate~%" (make-indent))
        (format cont-file "~%~a" (check-access-method))
        (format cont-file "~%~a" (tenant-scoping-method))
        (when *notified-application?*
          (format cont-file "~%~a"  (send_notification-method)))
        (format cont-file "~%end~%")))))

(defmethod index-method ((aspect aspect) &optional (stream t))
  (format stream "~a~a"
          (make-indent)
          (ruby:unparse-method
           "index" nil (as-literal (with-nesting (instance-variable-assignment aspect :list))))))

(defmethod show-method ((aspect aspect) &optional (stream t))
  (format stream "~a~a" (make-indent)
          (apply #'ruby:unparse-method
                 "show" nil (unless (use-set_method? aspect)
                           (list (as-literal (with-nesting (instance-variable-assignment aspect :show))))))))

(defmethod new-method ((aspect aspect) &optional (stream t))
  (format stream "~a~a" (make-indent) ;; instance-variable-assignment will account for defaults
          (ruby:unparse-method "new" nil (as-literal (with-nesting (instance-variable-assignment aspect :new))))))

(defmethod edit-method ((aspect aspect) &optional (stream t))
  (format stream "~a~a" (make-indent)
          (apply #'ruby:unparse-method
                 "edit" nil (unless (use-set_method? aspect)
                          (list (as-literal (with-nesting (instance-variable-assignment aspect :edit))))))))

(defmethod redirect_to ((aspect aspect) (from (eql :create)) (to (eql :show)) &optional var)
  (let ((instance-var (or var (strcat "@" (instance-name (entity aspect))))))
    (format nil "redirect_to ~a" (unparse-path-method-call aspect :detail instance-var))))

(defmethod redirect_to ((aspect aspect) (from (eql :update)) (to (eql :show)) &optional var)
  (redirect_to aspect :create :show var))

(defmethod redirect_to ((aspect aspect) (from (eql :delete)) (to (eql :list)) &optional var)
  (declare (ignorable var))
  (format nil "redirect_to ~a" (unparse-path-method-call aspect :list)))

(defmethod redirect_to ((aspect aspect) (from (eql :delete)) (to (eql :show)) &optional var)
  (redirect_to aspect :create :show var))

(defmethod respond_to ((aspect aspect) (action (eql :update)))
  (let* ((entity (entity aspect))
         (instance-name (instance-name entity)))
    (format nil "respond_to do |format|~%  ~a~a" (make-indent)
            (indent-block nil
              (with-nesting
                  (ruby:unparse-if-statement
                   (as-literal (format nil "!update_conflict?(@~a) && @~:*~a.update(~:*~a_params)" instance-name))
                   (as-literal
                    (format nil "format.html { ~a, notice: action_success_message }~%~
                                 format.json { render json: { status: :ok, location: @~a }}"
                            (redirect_to aspect :update :show (strcat "@" instance-name)) instance-name))
                   (as-literal
                    (format nil "format.html { render :edit, status: :unprocessable_entity }~%~
                                 format.json { render json: { errors: @~a.errors, status: :unprocessable_entity }}"
                            instance-name))))))))

(defmethod respond_to ((aspect aspect) (action (eql :create)))
  (let ((entity (entity aspect)))
    (format nil "respond_to do |format|~%  ~a~a" (make-indent)
            (indent-block nil
             (with-nesting
                 (ruby:unparse-if-statement
                  (as-literal (strcat "@" (instance-name entity) ".save"))
                  (as-literal
                   (format nil "format.html { ~a, notice: action_success_message }~%~
                                format.json { render :show, status: :created, location: @~a }"
                           (redirect_to aspect :create :show) (instance-name entity)))
                  (as-literal
                   (format nil "format.html { render :new, status: :unprocessable_entity }~%~
                                format.json { render json: @~a.errors, status: :unprocessable_entity }"
                           (instance-name entity)))))))))

(defmethod respond_to ((aspect aspect) (action (eql :delete)))
  (let* ((entity (entity aspect))
         (instance-name (instance-name entity)))
    (format nil "respond_to do |format|~%  ~a~a" (make-indent)
            (indent-block
             nil
             (with-nesting
                 (if (typep (entity aspect) 'attribute-table)
                     (error "we shouldn't be here")
                     (ruby:unparse-if-statement
                      (as-literal (format nil "@~a.destroy" instance-name))
                      (as-literal
                       (format nil "format.html { ~a, notice: action_success_message }~%format.json { head :no_content }"
                              (redirect_to aspect :delete :list)))
                      (as-literal
                       (format nil "format.html { ~a, alert: @~a.errors }~%~
                         format.json { render json: {errors: @~a.errors, status: :unprocessable_entity} }"
                               (redirect_to aspect :delete :show (strcat "@" instance-name))
                               instance-name instance-name))
                      )))))))

(defmethod create-method ((aspect aspect) &optional (stream t))
  (format stream "~a~a" (make-indent)
          (ruby:unparse-method
           "create" nil
           (as-literal (with-nesting (instance-variable-assignment aspect :create)))
           (as-literal (respond_to aspect :create))
           (as-literal "end"))))

(defmethod delete-method ((aspect aspect) &optional (stream t))
  (format stream "~a~a" (make-indent)
          (ruby:unparse-method
           "destroy" nil
           (as-literal (if (use-set_method? aspect) ""
                           (with-nesting (instance-variable-assignment aspect :delete))))
           (if (typep (entity aspect) 'attribute-table)
               (as-literal (format nil "@~a.destroy~%    respond_to do |format|~%      format.js"
                                   (instance-name (entity aspect))))
               (as-literal (respond_to aspect :delete)))
           (as-literal "end"))))

(defmethod update-method ((aspect aspect) &optional (stream t))
  (format stream "~a~a" (make-indent)
          (ruby:unparse-method
           "update" nil
           (as-literal
            (if (use-set_method? aspect) ""
                (with-nesting (instance-variable-assignment aspect :edit))))
           (as-literal (respond_to aspect :update))
           (as-literal "end"))))

(defmethod controller-class-definition ((aspect aspect) &optional (stream t))
  (let ((actions (implemented-actions aspect)))
    (format stream (controller-class-declaration aspect))
    (with-nesting
        (when (name (view aspect))
          (format stream "~%~a~a~%~%" (make-indent) (view-access-check aspect)))
      (dolist (filter (write-action-filters aspect))
          (format stream "~%~a~a~%~%" (make-indent) filter))
      (when (member :list actions)
        (index-method aspect stream) (terpri stream))
      (when (member :detail actions)
        (show-method aspect stream) (terpri stream))
      (when (member :new actions)
        (new-method aspect stream) (terpri stream))
      (when (member :edit actions)
        (edit-method aspect stream) (terpri stream))
      (when (member :create actions)
        (create-method aspect stream) (terpri stream))
      (when (member :update actions)
        (update-method aspect stream) (terpri stream))
      (when (member :delete actions)
        (delete-method aspect stream) (terpri stream))
      (format-private-controller-methods aspect stream))
    (format stream "~%end~%")))

(defun write-controller-files (view)
  (dolist (aspect (aspects view))
    (let ((file (controller-file-path aspect)))
      (with-open-file (cont-file file :direction :output :if-exists :supersede)
        (format-file-notice cont-file "write-controller-files")
        (controller-class-definition aspect cont-file)))))

(defun generate-controllers (&optional (app *application*))
  (application_controller.rb)
  (framework_controller.rb)
  ;; (when *dev-mode*
  ;;   (dolist (view (mapcar #'create-default-view (schema-entities app)))
  ;;     (write-controller-files view)))
  (dolist (view (views app))
    (write-controller-files view)))

;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
