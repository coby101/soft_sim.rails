;;;===========================================================================
;;; file:   internationalization.lisp
;;; auth:   Coby Beck
;;; date:   2022-01-22
;;;
;;;---------------------------------------------------------------------------
;;;   code associated with localizations
;;;   this unparses the application specification's language version yml
;;;   file only, other languages will need to be done by hand
;;;   by translating everything in generated.*locale*.yml
;;;---------------------------------------------------------------------------  
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :ror)

(defvar *locale* "en")
(defvar *show-button-text* "View")
(defvar *delete-conf* "Are you sure? You are about to delete this record.")

(defun generated.yml ()
  (let ((file-path (merge-pathnames
		    (make-pathname :name (format nil "generated.~a" *locale*) :type "yml")
		    (implementation-subdirectory "ror" "config" "locales"))))
    (with-open-file (yml file-path :direction :output :if-exists :supersede)
      (format-file-notice yml "internationalization.lisp")
      (format yml "~%en:~a" (yml:indent))
      (terpri yml)
;      (with-nesting (format yml (global-translations)))
;      (dolist (view (views *application*))
;	    (with-nesting (format yml "~%~a" (unparse-translation view))))
      (terpri yml)
      (with-nesting (format yml (active-record-translations)))
      (terpri yml))))
 ;     (with-nesting (format yml (model-translations))))))

(defun global-translations()
  (yml:unparse-pair-tree
   (list :global
	 `((:actions "")
	   (:show ,*show-button-text*)
	   (:edit "Edit")
	   (:new "New")
	   (:add "Add")
	   (:error "error")
	   (:delete "Delete")
	   (:list "List")
	   (:prevented_saving "prevented your record from saving")
	   (:no_data "(no data)")
	   (:no_parent "(none)")
	   (:delete_confirmation "Are you sure? You are about to delete this record.")
	   (:confirmation "Are you sure?")
	   (:delete_warning "You are about to delete this record.")))))

(defun t.prevented-saving () "t('global.prevented_saving')")
(defun t.success () "t('global.show')")
(defun t.show () "t('global.show')")
(defun t.edit () "t('global.edit')")
(defun t.new  () "t('global.new')")
(defun t.error  () "t('global.error')")
(defun t.add  () "t('global.add')")
(defun t.delete () "t('global.delete')")
(defun t.list () "t('global.list')")
(defun t.no-data () "t('global.no_data')")
(defun t.no-parent () "t('global.no_parent')")
(defun t.delete-confirmation () "t('global.delete_confirmation')")
(defun t.confirmation () "t('global.confirmation')")
(defun t.delete-warning () "t('global.delete_warning')")

(defmethod unparse-translation ((entity entity))
  (yml:unparse-pair-tree (make-translation-tree entity)))

(defmethod make-translation-tree ((entity entity) &optional (key (instance-name entity)))
  (list key
	    `((:one ,(short-name entity))
          (:other ,(short-plural entity))
          (:long_name ,(long-name entity))
	      (:short_name ,(short-name entity))
	      (:long_plural ,(long-plural entity))
	      (:short_plural ,(short-plural entity)))))

(defmethod t.long-name ((entity entity))
  (format nil "t('schema.~a.long_name')" (schema-name entity)))

(defmethod t.short-name ((entity entity))
  (format nil "t('schema.~a.short_name')" (schema-name entity)))

(defmethod t.name ((att attribute))
  (format nil "t('.~a')" (schema-name att)))

(defmethod t.name ((item t))
  (format nil "t('.~a')" (schema-name item)))

(defmethod t.long-plural ((entity entity))
  (format nil "t('schema.~a.long_plural')" (schema-name entity)))

(defmethod t.short-plural ((entity entity))
  (format nil "t('schema.~a.short_plural')" (schema-name entity)))

(defmethod t.button-title ((action symbol) (entity entity))
  (multiple-value-bind (action-key name-form)
      (ecase action
        (:new (values "new" "name"))
        (:list (values "list" "plural"))
        (:edit (values "edit" "name"))
        (:detail (values "show" "name"))
        (:delete (values "delete" "name")))
    (format nil "t('global.~a') + ' ' + t('schema.~a.long_~a')" action-key (instance-name entity) name-form)))


(defmethod translation ((key t) (element t) (context t))
  (error "no translation written for ~a ~a ~a" key element context))

(defmethod translation ((key (eql :title)) (aspect aspect) (context (eql :show)))
  `(:title ,(format nil "~a - ~a Details" (long-name (view aspect)) (long-name (entity aspect)))))

(defmethod translation ((key (eql :title)) (aspect aspect) (context (eql :index)))
  `(:title ,(format nil "~a - ~a List" (long-name (view aspect)) (long-name (entity aspect)))))

(defmethod translation ((key (eql :title)) (aspect aspect) (context (eql :new)))
  `(:title ,(format nil "~a - Create New ~a" (long-name (view aspect)) (long-name (entity aspect)))))

(defmethod translation ((key (eql :title)) (aspect aspect) (context (eql :context)))
  `(:title ,(short-name (entity aspect))))

(defmethod translation ((key (eql :title)) (aspect aspect) (context (eql :edit)))
  `(:title ,(format nil "~a - Edit ~a" (long-name (view aspect)) (long-name (entity aspect)))))

(defmethod translation ((key (eql :show)) (aspect aspect) context) 
  (declare (ignorable context))
  `(:show ,(append
	        `(,(translation :title aspect key))
	        (apply #'append
		           (mapcar #'(lambda(item)
			                   (let ((item-name (schema-name item)))
				                 (list (list item-name (long-panel-item-name item)))))
			               (apply #'append (panel-items (detail-panel aspect))))))))

(defmethod translation ((key (eql :index)) (aspect aspect) context) 
  (declare (ignorable context))
  `(:index ,(append
	         `(,(translation :title aspect key))
	         (apply #'append
		            (mapcar #'(lambda(item)
			                    (let ((item-name (schema-name item)))
				                  (list (list item-name (short-panel-item-name item)))))
			                (panel-items (list-panel aspect)))))))

(defmethod translation ((key (eql :edit)) (aspect aspect) context) 
  (declare (ignorable context))
  `(:edit (,(translation :title aspect key))))

(defmethod translation ((key (eql :new)) (aspect aspect) context) 
  (declare (ignorable context))
  `(:new (,(translation :title aspect key))))

(defmethod translation ((key (eql :form)) (aspect aspect) context) 
  (declare (ignorable context))
  `(:form ,(apply #'append
		          (let ((items
                          (if (typep (entity aspect) 'attribute-table)
                              (panel-items (list-panel aspect))
                              (remove-duplicates
			                   (append (when (updatable? aspect)
                                         (apply #'append (panel-items (edit-panel aspect))))
                                       (when (creatable? aspect)
                                         (apply #'append (panel-items (add-panel aspect)))))))))
                    (mapcar #'(lambda(item)
			                    (let ((item-name (schema-name item)))
				                  (list (list item-name (long-panel-item-name item)))))
			                items)))))

(defmethod translation ((key (eql :context)) (aspect aspect) context) 
  (declare (ignorable context))
  `(:context ,(append
	           `(,(translation :title aspect key)
                 (:long_model_name ,(long-name (entity aspect)))
                 (:short_model_name ,(short-name (entity aspect)))
                 (:long_model_plural ,(long-plural (entity aspect)))
                 (:short_model_plural ,(short-plural (entity aspect))))
	           (apply #'append
		              (mapcar #'(lambda(item)
			                      (let ((item-name (schema-name item)))
				                    (list (list item-name (long-panel-item-name item)))))
			                  (apply #'append (panel-items (context-panel aspect))))))))

(defmethod make-translation-tree ((aspect aspect) &optional (key (snake-case (plural (entity aspect)))))
  (list key
        (remove nil
		(list
		 (when (showable? aspect) (translation :show aspect nil))
         (when (context-panel aspect) (translation :context aspect nil))
		 (when (listable? aspect) (translation :index aspect nil))
		 (when (updatable? aspect) (translation :form aspect nil))
		 (when (creatable? aspect) (translation :new aspect nil))
		 (when (updatable? aspect) (translation :edit aspect nil))
		 (when (updatable? aspect)
		   `(:update
		     ((:success
		       ,(format nil "Your ~a record was successfully updated." (long-name (entity aspect)))))))
		 (when (creatable? aspect)
		   `(:create
		     ((:success
		       ,(format nil "Your ~a record was successfully created" (long-name (entity aspect)))))))
		 (when (creatable? aspect)
		   `(:destroy
		     ((:success
		       ,(format nil "Your ~a record was successfully deleted" (long-name (entity aspect)))))))))))

(defmethod unparse-translation ((view view))
  (yml:unparse-pair-tree
   (list (snake-case (name view))
	 (append `((:name ,(long-name view))
		   (:long_name ,(long-name view))
		   (:short_name ,(short-name view)))
		 (mapcar #'make-translation-tree (aspects view))))))

; (with-nesting (format yml (model-translations)))
(defmethod active-record-translations ()
  (let ((entities (sort (append (framework-entities *application*) (application-entities *application*)) #'string-lessp :key #'name)))
    (yml:unparse-pair-tree
     (list :activerecord
           `((:models
              ,(mapcar #'make-translation-tree entities))
             (:attributes
              ,(loop for model in entities
                     collect
                     (list (instance-name model)
                           (loop for att in (sort (remove-duplicates (append
                                                                      (user-attributes model)
                                                                      (summary-attributes model)
                                                                      (derived-attributes model)
                                                                      (foreign-keys model)))  ;; this should not include generalizations!
                                                  #'string-lessp :key #'name)
                                 collect
                                 (make-translation-tree att))))))))))

(defmethod model-translations ()
  (yml:unparse-pair-tree
   `(:schema
     (,(mapcar #'make-translation-tree (append (application-entities *application*)
                                               (framework-entities *application*)))))))
(defun pcmd-translations ()
  (yml:unparse-pair-tree
   `(:entity
     (,(mapcar #'make-entity-translation (append (application-entities *application*)
                                                 (framework-entities *application*)))))))

(defmethod make-entity-translation ((entity entity) &optional (key (snake-case (name entity))))
  (list key
	`((:long_name ,(long-name entity))
	  (:short_name ,(short-name entity))
      (:description ,(description entity))
      (:attribute (,(mapcar #'make-translation-tree (append (user-attributes entity) (summary-attributes entity) (derived-attributes entity))))))))

(defmethod make-translation-tree ((att attribute) &optional (key (schema-name att)))
  (let ((user-summary (or (user-summary att) ""))
        (user-detail (or (user-detail att) ""))
        (tech-summary (or (tech-summary att) ""))
        (tech-detail (or (tech-detail att) "")))
    `(,key
      ((:one ,(long-name att))
       (:other ,(long-plural att))
       (:long_name ,(long-name att))
	   (:short_name ,(short-name att))
       (:description ,user-summary)
       (:detailed_description ,user-detail)
       (:technical_summary ,tech-summary)
       (:technical_detail ,tech-detail)))))


;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
