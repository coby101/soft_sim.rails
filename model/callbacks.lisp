;;;===========================================================================
;;;
;;;   Code for registering and writing model callbacks
;;;
;;;===========================================================================

(in-package #:model)

(defun trigger-dependent-summary-fields (entity)
  (remove-if-not #'(lambda (att) (typep att 'cached-summary))
                 (dependent-summary-fields entity)))

(defun destroyed-by-parent-predicate-name (parent)
  (format nil "destroyed_by_parent_~a" (instance-name parent)))
(defun require-update-predicate-name (parent)
  (format nil "~a_updates_required" (instance-name parent)))
(defun summary-update-method-name (parent)
  (format nil "trigger_~a_summary_updates"
          (instance-name parent)))

;; these three functions should be methods in classes/entities.lisp
;;BEGIN
(defun calendar-links (ent)
  (remove-if-not
   #'(lambda(r)
       (and (typep r 'one-to-many)
            (eq ent (entity (rhs r)))
            (member (entity (lhs r)) (calendar-entities))
            (not (member (entity (rhs r)) (calendar-entities)))))
   (relationships ent)))

(defun interdependent-defaulting-fields (entity)
  (remove-if-not
   #'(lambda (f)
       (let ((default (default-value f)))
         (and default (typep default 'attribute))))
   (attributes entity)))

(defun find-calendar-entities (entity)
  (let ((calendar-links (calendar-links entity)))                     ;; don't have time to sort this
    (when (and calendar-links (not (typep entity 'reporting-entity)) (not (string-equal "Budget" (name entity)))) 
      (unparse-find_calendar_entities calendar-links))))
;; END

(defun set-interdependent-defaults (entity)
  (let* ((attributes (interdependent-defaulting-fields entity))
         (expressions (mapcar #'(lambda (att)
                                  (if (or (typep att 'foreign-key)
                                          (and (eql (data-type att) :string)
                                               (not (nullable? att))))
                                      (format nil "self.~a = self.~:*~a.presence || ~a" (schema-name att)
                                              (unparse-default-value-expression att entity))
                                      (format nil "self.~a ||= ~a" (schema-name att)
                                              (unparse-default-value-expression att entity))))
                              attributes)))
    (when attributes
      (apply #'ruby:unparse-method
            "set_interdependent_defaults" nil
            (mapcar #'as-literal expressions)))))

(defun set-cached-calculations (entity)
  (let* ((attributes (cached-calculations entity))
         (expressions (mapcar #'(lambda (att)
                                  (format nil "self.~a = ~a" (schema-name att)
                                          (unparse-expression (unparse-attribute-references (expression att) entity) :ruby)))
                              attributes)))
    (when attributes
      (format nil "def set_cached_calculations ~{~%    ~a~}~%  end~%" expressions))))

#|
  def order_updates_required
    !saved_changes.keys.intersection(["order_id", "pend_amt", "exp_amt", "inv_amt"]).blank?
  end
  def destroyed_by_parent_order
    !destroyed_by_association.blank?
  end
  def trigger_order_summary_updates
    self.order.set_cached_order_line_summaries
    self.order.save!
    if order_id_previously_changed? && ((order_id_previously_was || 0) != 0)
      old = Order.find(order_id_previously_was)
      old.set_cached_order_line_summaries
      old.save!
    end
  end
|#

(defun cascade-deletes? (rel)
  (typep (my-relationship rel) 'aggregation))

(defun destroyed_by_parent (parent-relation child-entity)
  (declare (ignorable child-entity))
  (apply
   #'ruby:unparse-method (destroyed-by-parent-predicate-name parent-relation)
   nil
   (if (cascade-deletes? parent-relation)
       (list (as-literal
              "# not checking which association was involved poses a small potential")
             (as-literal
              "# risk but this can only happen with cascade deletes which are uncommon")
             (as-literal
              "# and apply to aggregations only. Can a child ever be in more than one?")
             (as-literal
              "!destroyed_by_association.blank?"))
       (list (as-literal
              "# there is no cascade delete in this relationship")
             (as-literal "false")))))

(defun parent_updates_required (relation child-entity)
  (let ((atts (remove-if-not #'(lambda(a) (eq (my-entity a) (entity relation)))
                             (trigger-dependent-summary-fields child-entity))))
    (ruby:unparse-method (require-update-predicate-name relation)
       nil
       (as-literal (format nil "!saved_changes.keys.intersection(~a).blank?~a"
                           (unparse-array
                            (list* (strcat (instance-name relation) "_id")
                                   (mapcar #'schema-name (mapcar #'source atts))) :ruby)
                            (if (required-relation? relation)
                                ""
                                (format nil " && !~a.nil?" (instance-name relation))))))))

(defun trigger_parent_summary_update (relation child-entity)
  (let* ((rel-name (instance-name relation)))
    (ruby:unparse-method
     (summary-update-method-name relation)
     nil
     (as-literal (format nil "self.~a.set_cached_~a_summaries"
                         rel-name (instance-name child-entity)))
     (as-literal (format nil "self.~a.save!" rel-name))
     (as-literal (format nil "if ~a_id_previously_changed? && ((~:*~a_id_previously_was || 0) != 0)" rel-name))
     (as-literal (format nil "~aold = ~a.find(~a_id_previously_was)"
                          (make-indent) (model-name (entity relation)) rel-name))
     (as-literal (format nil "~aold.set_cached_~a_summaries"
                         (make-indent) (instance-name child-entity)))
     (as-literal (format nil "~aold.save!" (make-indent)))
     (as-literal (format nil "end")))))

(defmethod trigger-summary-updates ((entity symbol)) (trigger-summary-updates (find-entity entity)))
(defmethod trigger-summary-updates ((entity entity))
  (let ((attributes (trigger-dependent-summary-fields entity)))
    (when attributes
      (let ((relationships (relationships entity))
            (parent-models (remove-duplicates (mapcar #'my-entity attributes))))
        (loop for model in parent-models
            nconcing
            (let* ((rel (find-if #'(lambda(rel)
                                     (and (eq entity (entity (rhs rel)))
                                          (eq model (entity (lhs rel)))))
                                 relationships)))
              (remove nil
                      (list (parent_updates_required (lhs rel) entity)
                            (destroyed_by_parent (lhs rel) entity)
                            (trigger_parent_summary_update (lhs rel) entity)))))))))

(defmethod ensure-summary-method-body ((entity entity) context)
  (let* ((instance (instance-name entity))
         (model (model-name entity))
         (left-key (car (associates entity)))
         (left-key-name (strcat (instance-name left-key) "_id"))
         (left-key-ref (unparse-expression (unparse-attribute-references (cons left-key (primary-key left-key)) entity) :ruby))
         (right-key (cadr (associates entity)))
         (right-key-name (strcat (instance-name right-key) "_id"))
         (right-key-ref (unparse-path (primary-key right-key) context :bad-start nil)))
    (with-nesting
        (list (as-literal
               (format nil "if ~a && ~a~%~a~a~%~a~a~%~a~a~%~aend"
                       ;; if <test>
                       (instance-name (car (path-to-attribute context (primary-key left-key))))
                       (instance-name (car (path-to-attribute context (primary-key right-key))))
                       (with-nesting (make-indent))
                       ;; object assignment
                       (format nil "object = ~a.find_or_create_by(~a: ~a, ~a: ~a)"
                               model left-key-name left-key-ref right-key-name right-key-ref)
                       ;; save if new record
                       (with-nesting (make-indent))
                       (with-nesting
                               (ruby:unparse-if-statement
                                (as-literal "object.new_record?")
                                (as-literal (format nil "errors.add(:~a_id, \"Unable to create a ~a record\") ~
                                                    unless object.save" instance (long-name entity)))))
                       ;; set reference key
                       (with-nesting (make-indent))
                       (format nil "self.~a_id = object.id" instance)
                       (make-indent)))))))

(defmethod ensure-summary-method-body ((entity period-report) context)
  (let* ((instance (instance-name entity))
         (model (model-name entity))
         (assoc1 (car (associates entity)))
         (assoc2 (cadr (associates entity)))
         (warning (if (and (calendar-entity? assoc1) (calendar-entity? assoc2))
                    (format nil "# I haven't figured out this situation yet...~%") "")))
    (let* ((period-entity (or (and (calendar-entity? assoc1) assoc1) assoc2))
           (other-key (car (remove period-entity (associates entity)))))
      (list
       (as-literal
        (format nil "~aself.~a_id = ~a.period_report(~a, ~a).id" warning instance model
                (unparse-path (primary-key (calendar-entity period-entity)) context :bad-start nil ) ;entity)
                (unparse-path (primary-key other-key) context :bad-start nil)))))));entity)))))

(defmethod ensure-summary-record ((entity entity) context)
  (let* ((instance (instance-name entity))
         (method (format nil "ensure_~a" instance)))
    (format nil
            (apply #'ruby:unparse-method method nil (ensure-summary-method-body entity context)))))

;; every one of these empty methods should be looking for custom code to insert
(defmethod on-create-declarations   ((entity entity)))
(defmethod on-update-declarations   ((entity entity)))
(defmethod on-commit-declarations   ((entity entity)))
(defmethod on-rollback-declarations ((entity entity)))
(defmethod on-create-methods        ((entity entity)))
(defmethod on-update-methods        ((entity entity)))
(defmethod on-commit-methods        ((entity entity)))
(defmethod on-rollback-methods      ((entity entity)))
(defmethod on-destroy-declarations  ((entity entity)))
(defmethod on-destroy-declarations  ((entity symbol)) (on-destroy-declarations (find-entity entity)))
(defmethod on-destroy-declarations  ((entity entity))
  (remove nil
          (append
           (let ((fields (trigger-dependent-summary-fields entity)))
             (when fields 
               (loop for parent in (remove-duplicates (mapcar #'my-entity fields))
                     collect (unparse-callback-registration
                              "destroy" "after" (summary-update-method-name parent)
                              (list "unless" (destroyed-by-parent-predicate-name parent))))))
           (append
            (when (dependent-children entity)
              (list (unparse-callback-registration
                     "destroy" "before" "prevent_orphaned_records" (list "prepend" t))))))))

(defmethod on-save-declarations ((entity symbol)) (on-save-declarations (find-entity entity)))
(defmethod on-save-declarations ((entity entity))
  (remove nil
          (append
           (let ((fields (trigger-dependent-summary-fields entity)))
             (when fields 
               (loop for parent in (remove-duplicates (mapcar #'my-entity fields))
                     collect (unparse-callback-registration
                              "save" "after" (summary-update-method-name parent)
                             (list "if" (require-update-predicate-name parent)))))))))

(defmethod on-validation-declarations ((entity entity))
  (remove nil
          (append
           (let ((calendars (calendar-links entity)))
             (when (and calendars (not (typep entity 'reporting-entity)))
               (list (unparse-callback-registration
                      "validation" "before" "find_calendar_entities"))))
           (let ((defaults (interdependent-defaulting-fields entity)))
             (when defaults
               ;; BUG must check if this is a subclass and if so add model name
               (list (unparse-callback-registration
                      "validation" "before" "set_interdependent_defaults"))))
           (let ((cached (cached-calculations entity)))
               ;; BUG must check if this is a subclass and if so add model name
             (when cached
               (list (unparse-callback-registration
                      "validation" "before" "set_cached_calculations"))))
           (unless (or (typep entity 'period-report) (typep entity 'attribute-table))
             (let ((xsums (remove-if-not #'(lambda (e) (typep e 'reporting-entity))
                                         (parents entity))))
               (loop for xsum in xsums
                     collect (unparse-callback-registration
                              "validation" "after" (strcat "ensure_" (instance-name xsum)))))))))

(defmethod on-save-methods ((entity entity))
  (remove nil
          (append (trigger-summary-updates entity))))

(defmethod on-validation-methods ((entity entity))
  (remove nil
          (append
           (list (find-calendar-entities entity))
           (list (set-interdependent-defaults entity))
           (list (set-cached-calculations entity))
           (unless (or (typep entity 'period-report) (typep entity 'attribute-table))
             (let ((xsums (remove-if-not #'(lambda (e) (typep e 'reporting-entity)) (parents entity))))
               (loop for xsum in xsums
                     collect (ensure-summary-record xsum entity)))))))


(defmethod on-destroy-methods ((entity entity))
  (let* ((children (dependent-children entity)))
    (when children
      (list (apply #'ruby:unparse-method "prevent_orphaned_records" nil
             (append
              (loop for child in children
                    collect
                    (as-literal
                     (format nil "errors.add(:base, \"You can not delete a ~a when there ~
                            are ~a attached\") unless ~a.blank?"
                             (long-name entity) (long-plural child) (schema-name child))))
              (list (as-literal (format nil "throw :abort if ~{!~a.blank?~^ || ~}" (mapcar #'schema-name children))))))))))

(defun calendar-link-field (rel)
  (cadadr (expression (matchmaker rel))))

(defun set-cached-summaries (entity)
  (let* ((attributes (cached-summaries entity)))
    (when attributes
      (let ((child-models (remove-duplicates (mapcar #'(lambda (a) (my-entity (source a))) attributes))))
        (loop for model in child-models
              collect
              (let* ((child-atts (remove-if-not #'(lambda(a) (eq (my-entity (source a)) model))
                                                attributes))
                     (expressions (mapcar #'(lambda (att)
                                              (format nil "self.~a = ~a" (schema-name att)
                                                      (unparse-summary-expression att)))
                                          child-atts)))
                (format nil "def set_cached_~a_summaries~{~%    ~a~}~%  end~%"
                        (instance-name model) expressions)))))))

(defmethod find-calendar-object-expression ((calendar-entity calendar-entity) other-entity)
  (declare (ignorable other-entity))
  (format nil "~a.find(period_id)" (model-name calendar-entity)))

;; passed ProjectReport and ProjectSupplier
(defmethod find-calendar-object-expression ((period-entity period-report) other-entity)
  (let ((model (model-name period-entity))
        (assoc1 (car (associates period-entity)))
        (assoc2 (cadr (associates period-entity))))
    (when (and (calendar-entity? assoc1) (calendar-entity? assoc2))
      (error "I haven't figured out this situation yet..."))
    (let* ((calendar-assoc (or (and (calendar-entity? assoc1) assoc1) assoc2))
           (other-assoc (car (remove calendar-assoc (associates period-entity)))))
      (format nil "~a.period_report(period_id, ~a.find(~a_id).~a_id)" model (model-name other-entity)
              (instance-name other-entity) (instance-name other-assoc)))))

(defmethod period-report-method ((entity period-report))
  (let* ((model (model-name entity))
         (assoc1 (car (associates entity)))
         (assoc2 (cadr (associates entity)))
         (warning (if (and (calendar-entity? assoc1) (calendar-entity? assoc2))
                      (format nil "# I haven't figured out this situation yet...~%") "")))
    (let* ((period-entity (or (and (calendar-entity? assoc1) assoc1) assoc2))
           (other-entity (car (remove period-entity (associates entity))))
           (find-report-args (format nil "~a_id: calendar_object.id, ~a_id: ~:*~a_id"
                                     (instance-name period-entity) (instance-name other-entity))))
      (ruby:unparse-method
       "self.period_report" (list "period_id" (strcat (instance-name other-entity) "_id"))
       (as-literal (format nil "~acalendar_object = ~a"
                           warning (find-calendar-object-expression period-entity other-entity)))
       (as-literal (format nil "~a.find_or_create_by!(~a)" model find-report-args))))))

(defmethod public-callback-methods ((entity entity))
  (remove nil
          (append
           (set-cached-summaries entity)
           (when (typep entity 'period-report)
             (list (period-report-method entity))))))

(defmethod write-public-callback-methods ((entity entity) &optional (stream t))
  (let ((fmt-str (format nil "~~&~apublic~%~%~~{~a~~a~%~~}" (make-indent) (make-indent)))
        (methods (public-callback-methods entity)))
    (when methods
      (format stream "~%  #  public methods used by callbacks ~%")
      (format stream fmt-str methods))))

(defmethod register-callbacks ((entity entity) &optional (stream t))
  (let ((fmt-str (format nil "~~{~a~~a~%~~}" (make-indent)))
        (callbacks (append (on-validation-declarations entity)
                    (on-save-declarations entity)
                    (on-create-declarations entity)
                    (on-update-declarations entity)
                    (on-destroy-declarations entity)
                    (on-commit-declarations entity)
                    (on-rollback-declarations entity))))
    (when callbacks
      (terpri stream)
      (comment-out stream " callbacks")
      (format stream fmt-str callbacks))))

(defmethod write-callback-methods ((entity entity) &optional (stream t))
  (let ((fmt-str (format nil "~~&~aprivate~%~%~~{~a~~a~%~~}" (make-indent) (make-indent)))
        (methods (append (on-validation-methods entity)
                    (on-save-methods entity)
                    (on-create-methods entity)
                    (on-update-methods entity)
                    (on-destroy-methods entity)
                    (on-commit-methods entity)
                    (on-rollback-methods entity))))
    (when methods
      (format stream "~%  #  private callback methods ~%")
      (format stream fmt-str methods))))

;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
