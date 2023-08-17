;;;===========================================================================
;;; file:   controller.lisp
;;; auth:   Coby Beck
;;; date:   2020-12-04
;;; update: 
;;;---------------------------------------------------------------------------
;;;   code associated with generating the controller aspect of Rails MVC framework 
;;;---------------------------------------------------------------------------  
;;;
;;; 2020
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :ror)

(defun application_controller.rb ()
  (let ((file (merge-pathnames
               (make-pathname :name "application_controller"
                              :type "rb")
               (controller-directory)))
        (*nesting-level* 0))
    (with-open-file (cont-file file :direction :output :if-exists :supersede)
      (format-file-notice cont-file "application_controller.rb")
      (format cont-file "~&class ApplicationController < ActionController::Base")
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

(defun tenant-scoping-method ()
  (format nil "~adef tenant_scoping
    @tenant ||= current_user.try(:tenant)~%~aend~%"
          (make-indent) (make-indent)))

(defun check-access-method ()
  (format nil "~adef check_view_access (view_name)
    current_view = View.find_by(name: view_name)
    if current_user.user_roles.pluck(:role_id).intersection(current_view.view_roles.pluck(:role_id)).empty?
      redirect_to '~a'~%~aend~%~aend~%" (make-indent) (access-denied-path)
      (with-nesting (make-indent)) (make-indent)))

(defun access-denied-path ()
  "/no_access")

(defun generate-controllers (&optional (app *application*))
  (application_controller.rb)
  (framework_controller.rb)
  ;; (when *dev-mode*
  ;;   (dolist (view (mapcar #'create-default-view (schema-entities app)))
  ;;     (write-controller-files view)))
  (dolist (view (views app))
    (write-controller-files view)))

;; will need to write an ApplicationContoller as well, the generated one is currently empty

(defun write-controller-files (view)
  (dolist (aspect (aspects view))
    (let ((file (controller-file-path aspect)))
      (with-open-file (cont-file file :direction :output :if-exists :supersede)
        (format-file-notice cont-file "write-controller-files")
        (controller-class-definition aspect cont-file)))))

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

(defmethod controller-class-declaration ((aspect aspect))
  (format nil "class ~a < ApplicationController~%" (controller-name aspect)))
(defmethod controller-class-declaration ((str string))
  (format nil "class ~a < ApplicationController~%" str))

(defun write-action-filters (aspect)
  ;; door is open here for other filter actions
  (remove nil (list (set_model-filter aspect))))

(defun use-set_method? (aspect)
  (and (not (multi-valued-attributes (entity aspect)))
       (root? aspect)))

(defun view-access-check (aspect)
  (format nil "before_action  -> { check_view_access (~s) }" (name (view aspect))))

(defun set_model-filter (aspect)
  (when (use-set_method? aspect)
    (format nil "before_action :set_~a, only: ~a"
            (instance-name (entity aspect))
            (applicable-set_method-actions aspect))))

(defun applicable-set_method-actions (aspect)
  (declare (ignorable aspect))
  ;; seems we should be selective about this list based on allowed operations
  (let ((methods (list :show :edit :update :destroy)))
    (ruby::unparse-array methods)))

(defmethod set_model-method ((aspect aspect) &optional (stream t))
  (let ((entity (entity aspect)))
    (format stream (ruby::unparse-method
                    (format nil "set_~a" (instance-name entity)) nil
                    (as-literal
                     (instance-variable-assignment aspect :show))))))

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
            (ruby::unparse-method
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

(defmethod format-private-controller-methods ((aspect aspect) &optional (stream t))
  (format stream "~%~aprivate~%~%" (make-indent))
  (when (showable? aspect)
    (when (use-set_method? aspect)
      (format stream "~a~a~%~%" (make-indent) (set_model-method aspect nil))))
  (when (updatable? aspect)
    (comment-out stream "Only allow a list of trusted parameters through.")
    (format stream "~a~a" (make-indent) (model_params-method aspect nil))))

(defmethod index-method ((aspect aspect) &optional (stream t))
  (format stream "~a~a"
          (make-indent)
          (ruby::unparse-method
           "index" nil (as-literal (with-nesting (instance-variable-assignment aspect :list))))))

(defmethod show-method ((aspect aspect) &optional (stream t))
  (format stream "~a~a" (make-indent)
          (apply #'ruby::unparse-method
                 "show" nil (unless (use-set_method? aspect)
                           (list (as-literal (with-nesting (instance-variable-assignment aspect :show))))))))

(defmethod new-method ((aspect aspect) &optional (stream t))
  (format stream "~a~a" (make-indent) ;; instance-variable-assignment will account for defaults
          (ruby::unparse-method "new" nil (as-literal (with-nesting (instance-variable-assignment aspect :new))))))

(defmethod edit-method ((aspect aspect) &optional (stream t))
  (format stream "~a~a" (make-indent)
          (apply #'ruby::unparse-method
                 "edit" nil (unless (use-set_method? aspect)
                          (list (as-literal (with-nesting (instance-variable-assignment aspect :edit))))))))

(defmethod create-method ((aspect aspect) &optional (stream t))
  (let ((entity (entity aspect)))
    (format stream "~a~a" (make-indent)
            (ruby::unparse-method
             "create" nil
             (as-literal (with-nesting (instance-variable-assignment aspect :create)))
             (as-literal (respond_to aspect :create))
             (as-literal "end")))))

(defmethod delete-method ((aspect aspect) &optional (stream t))
  (format stream "~a~a" (make-indent)
          (ruby::unparse-method
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
          (ruby::unparse-method
           "update" nil
           (as-literal
            (if (use-set_method? aspect) ""
                (with-nesting (instance-variable-assignment aspect :edit))))
           (as-literal (respond_to aspect :update))
           (as-literal "end"))))

(defmethod respond_to ((aspect aspect) (action (eql :update)))
  (let* ((entity (entity aspect))
         (instance-name (instance-name entity)))
    (format nil "respond_to do |format|~%  ~a~a" (make-indent)
            (indent-block nil
              (with-nesting
                  (ruby::unparse-if-statement
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
                 (ruby::unparse-if-statement
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
                     (ruby::unparse-if-statement
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
#|
def destroy
    @project = Project.find(params[:id])
    respond_to do |format|
      if @project.destroy
          format.html { redirect_to prj_admin_projects_path, notice: 'Your Project record was successfully deleted.' }
          format.json { head :no_content }
      else
        format.html { redirect_to prj_admin_project_path(@project), alert: @project.errors }
        format.json { render json: {errors: @project.errors, status: :unprocessable_entity} }  
      end
    end
end
|#

(defmethod redirect_to ((aspect aspect) (from (eql :create)) (to (eql :show)) &optional var)
  (let ((instance-var (or var (strcat "@" (instance-name (entity aspect))))))
    (format nil "redirect_to ~a" (path-method-call aspect :detail instance-var))))

(defmethod redirect_to ((aspect aspect) (from (eql :update)) (to (eql :show)) &optional var)
  (redirect_to aspect :create :show var))

(defmethod redirect_to ((aspect aspect) (from (eql :delete)) (to (eql :list)) &optional var)
  (declare (ignorable var))
  (format nil "redirect_to ~a" (path-method-call aspect :list)))

(defmethod redirect_to ((aspect aspect) (from (eql :delete)) (to (eql :show)) &optional var)
  (redirect_to aspect :create :show var))

(defmethod instance-variable-assignment ((aspect aspect) (action (eql :edit)) &key (key-param "id") (recursive? t))
  (instance-variable-assignment aspect :show :recursive? recursive?))

(defmethod instance-variable-assignment ((aspect aspect) (action (eql :delete)) &key (key-param "id") (recursive? t))
  (instance-variable-assignment aspect :show :recursive? recursive?))

(defmethod mva-includes ((aspect aspect))
  (let ((mvas (multi-valued-attributes (entity aspect))))
    (if mvas
        (format nil ".includes(~{:~a~^, ~})"
                (mapcar #'(lambda(att) (schema-name (child-entity att))) mvas))
        "")))

(defmethod unparse-controller-model-reference ((aspect aspect))
  (let* ((entity (entity aspect))
         (closest-rel (get-closest-relative entity (view aspect)))
         (relationship (when closest-rel (find-relationship closest-rel entity))))
    (if (root? aspect)
        (if (tenant-scoped-entity? entity)
            (format nil "@~a.~a" (instance-name (path (tenant-key entity))) (schema-name entity))
            (model-name entity))
        (format nil "@~a.~a" (instance-name closest-rel) (schema-name (rhs relationship))))))

(defmethod instance-variable-assignment ((aspect aspect) (action (eql :show)) &key (key-param "id") (recursive? t))
  (let* ((entity (entity aspect)))
    (format nil "~a~a" (if recursive? (set-parents aspect) "")
            (format nil "~a = ~a~a.find(params[:~a])" (strcat "@" (instance-name entity))
                    (unparse-controller-model-reference aspect) (mva-includes aspect)
                    key-param))))

;    @company = Company.find(params[:company_id])
;    @division = @company.divisions.build
(defmethod instance-variable-assignment ((aspect aspect) (action (eql :new)) &key (key-param "id") (recursive? t))
  (let* ((entity (entity aspect)))
    (format nil "~a~a" (if recursive? (set-parents aspect) "")
            (format nil "~a = ~a.~a" (strcat "@" (instance-name entity))
                    (unparse-controller-model-reference aspect)
                    (build-method aspect)))))

;    @company = Company.find(params[:company_id])
;    @division = @company.divisions.build(division_params)
(defmethod instance-variable-assignment ((aspect aspect) (action (eql :create)) &key (key-param "id") (recursive? t))
  (let* ((entity (entity aspect)))
    (format nil "~a~a" (if recursive? (set-parents aspect) "")
            (format nil "@~a = ~a.~a(~a_params)" (instance-name entity)
                    (unparse-controller-model-reference aspect)
                    (build-method aspect) (instance-name entity)))))

(defmethod build-method ((aspect aspect))
  (if (or (not (root? aspect))
          (tenant-scoped-entity? (entity aspect)))
      "build"
      "new"))

(defmethod instance-variable-assignment ((aspect aspect) (action (eql :list)) &key (key-param "id") (recursive? t))
  (let* ((entity (entity aspect))
         (closest-rel (get-closest-relative entity (view aspect)))
         (relationship (when closest-rel (find-relationship closest-rel entity))))
    (format nil "~a~a" (if recursive? (set-parents aspect) "")
            (format nil "@~a = ~a~{.~a~}"
                    (snake-case (plural (entity aspect)))
                    (unparse-controller-model-reference aspect)
                    (or (format-controller-filters aspect)
                        (if (root? aspect) (list "all")))))))

(defmethod set-parents ((aspect aspect))
  (if (root? aspect)
       ""
      (let* ((entity (entity aspect))
             (closest-rel (get-closest-relative entity (view aspect)))
             (rel (when closest-rel (find-relationship closest-rel entity))))
        (format nil "~a~%~a"
                (instance-variable-assignment
                 (find-aspect (view aspect) closest-rel) :show :key-param (strcat (instance-name (lhs rel)) "_id"))
                (make-indent)))))

(defmethod format-controller-filters ((aspect aspect))
  (let ((filters (filters aspect)))
    (when filters
      (loop for f in filters
         collect (unparse-filter-application f)))))

(defmethod unparse-filter-application ((filter entity-state))
  (snake-case (name filter)))

(defmethod unparse-filter-application ((filter formula))
  (format nil "where(~s)" (sql::unparse-expression (expression filter))))


;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
