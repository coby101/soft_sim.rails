;;;===========================================================================
;;;
;;;   code associated with localizations.
;;;
;;;   this only unparses the yml file in the language of the application
;;;   specification, which is expected to match *locale*. Any other
;;;   languages will need to be done by hand in <rails>/config/locales/
;;;
;;;===========================================================================

(in-package #:app-config)

(defvar *locale* "en")
(defvar *show-button-text* "View")
(defvar *delete-conf* "Are you sure? You are about to delete this record.")

(defmethod make-translation-tree ((entity entity) &optional (key (instance-name entity)))
  (list key
	    `((:one ,(short-name entity))
          (:other ,(short-plural entity))
          (:long_name ,(long-name entity))
	      (:short_name ,(short-name entity))
	      (:long_plural ,(long-plural entity))
	      (:short_plural ,(short-plural entity)))))

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

;; FIXME this is appropriate only for *locale* = :en
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

(defmethod unparse-translation ((entity entity))
  (yml:unparse-pair-tree (make-translation-tree entity)))

(defmethod unparse-translation ((view view))
  (yml:unparse-pair-tree
   (list (snake-case (name view))
	 (append `((:name ,(long-name view))
		   (:long_name ,(long-name view))
		   (:short_name ,(short-name view)))
		 (mapcar #'make-translation-tree (aspects view))))))

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

; (with-nesting (format yml (model-translations)))
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
                                                 
;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
