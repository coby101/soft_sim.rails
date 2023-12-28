;;;===========================================================================
;;;
;;;   Code for uparsing layout and form elements
;;;
;;;===========================================================================
 
(in-package #:rails-unparser)

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

;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
