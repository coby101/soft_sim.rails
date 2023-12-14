;;;===========================================================================
;;;
;;;   Code for generating routes.rb
;;;
;;;===========================================================================

(in-package :ror)

(defun framework-routes ()
  (append
   (if *authenticated-application?*
       (list (format nil "devise_for :~a" (schema-name (find-entity *user-model*)))
             "root to: 'framework#home'")
       (list "get '/', to: 'framework#home', as: 'home'"))
   (list "get '/no_access', to: 'framework#denied'"
         "get '/about', to: 'framework#about', as: 'about'"
         "mount ActionCable.server => '/cable'")))

(defmethod path-method-name ((aspect aspect) action &optional in-full)
  (let* ((entity (entity aspect))
         (no-plural? (uncountable-name? entity))
         (path-suffix (if in-full "_path" ""))
         (namespace (unparse-namespace aspect :path-method))
         (hierarchy (format nil "~{~a_~}" (mapcar #'instance-name (reverse (path-to-root entity (view aspect)))))))
    (case action
      (:new (strcat "new_" namespace hierarchy (instance-name entity) path-suffix))
      (:update (strcat namespace hierarchy (instance-name entity) path-suffix))
      (:edit (strcat "edit_" namespace hierarchy (instance-name entity) path-suffix))
      (:list (strcat namespace hierarchy (model-plural entity) (if no-plural? "_index" "") path-suffix))
      (:detail (strcat namespace hierarchy (instance-name entity) path-suffix))
      (:delete (strcat namespace hierarchy (instance-name entity) "_path")))))

(defun route-controller (aspect)
  (format nil "~a~a" (unparse-namespace aspect :file-path)
          (model-plural (entity aspect))))

(defun http-method (action)
  (ecase action
    ((:list :detail :edit :new) "get")
    (:create "post")
    (:delete "delete")
    (:update "put")
    (:patch "patch"))) ;;; patch vs put, put is for backwards compatibility

(defun controller-method (action)
  (ecase action
    (:new "new")
    (:list "index")
    (:detail "show")
    (:edit "edit")
    (:create "create")
    (:delete "destroy")
    (:patch "update")
    (:update "update")))

(defmethod unparse-route ((aspect aspect) action)
  (apply #'format-route
   (http-method action)
   (route aspect action)
   (route-controller aspect)
   (controller-method action)
   (when (or (and (typep (entity aspect) 'attribute-table)
                  (eql action :update))
             (member action '(:new :edit :list :detail)))
     (list (list "as" (path-method-name aspect action))))))

(defmethod unparse-routes ((aspect aspect))
  (let ((actions (implemented-actions aspect)))
  (loop for action in (sort actions #'> :key #'route-rank)
     collect (unparse-route aspect action))))

(defun write-routes (stream &optional (app *application*))
  (format stream "~%~{  ~a~%~}~%~%" (framework-routes))
  ;; (when *dev-mode*
  ;;   (dolist (view (mapcar #'create-default-view (schema-entities app)))
  ;;     (comment-out stream "routes for the ~s view" (long-name view))
  ;;     (format stream "~{  ~a~%~}~%"
  ;;             (mapcan #'unparse-routes (aspects view)))))
  (dolist (view (views app))
    (comment-out stream "routes for the ~s view" (long-name view))
    (format stream "~{  ~a~%~}~%"
            (mapcan #'unparse-routes (aspects view)))))

(defun routes.rb (&optional (app *application*))
  (with-open-file (routes (routes-file-path) :direction :output :if-exists :supersede)
    (format-file-notice routes "routes.rb")
    (format routes "~%Rails.application.routes.draw do~%")
    (write-routes routes app)
    (format routes "~%end~%")))

;; unused code, may be wanted later
(defun unparse-route-namespace(path)
  (when path
    (let ((views (find-views path)))
      (format nil "~anamespace :~a do~%~a~a~aend~%" (make-indent) (car path)
              (if (cdr path)
                  (with-nesting
                      (unparse-route-namespace (cdr path)))
                  "")
              (if views
                  (with-nesting
                      (let ((fmt-str (format nil "~a~~{~~a~~^~%~a~~}~%" (make-indent) (make-indent))))
                    (format nil fmt-str (apply #'append (mapcar #'unparse-routes views)))))
                  "")
              (make-indent)))))

(defun implemented-actions (aspect)
  (let ((entity (entity aspect))
        (view (view aspect)))
    (if (and (name view)
             (typep entity 'attribute-table)
             (find-aspect view (owner entity)))
        (when (updatable? (find-aspect view (owner entity)))
          (list :update :delete))
        (actions aspect))))

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

(defun format-route (method path controller action &rest options)
  (format nil "~a '~a', to: '~a#~a'~{~a~}" method path controller action
          (mapcar #'(lambda(opt)
                      (format nil ", ~a: '~a'" (car opt) (cadr opt)))
                  options)))

(defmethod route ((aspect aspect) action)
  (let* ((entity (entity aspect))
         (path (unparse-namespace aspect :route))
         (hierarchy (unparse-url-heirarchy entity (view aspect))))
    (ecase action
      ((:create :list) (strcat path hierarchy "/" (model-plural entity)))
      ((:detail :update :delete :patch)
       (format nil "~a~a/~a/:~a" path hierarchy (model-plural entity) (schema-name (primary-key entity))))
      (:new (format nil "~a~a/~a/new" path hierarchy (model-plural entity)))
      (:edit (format nil "~a~a/~a/:~a/edit" path hierarchy
                     (model-plural entity) (schema-name (primary-key entity)))))))

(defun route-rank (action)
  "sort ranks in a way that rails routing will get the appropriate result. The main ~
   issue being GET '/entity/:id' steals GET '/entity/new' if it comes first ~
   and POST will steal GET"
  (ecase action
    (:list 8)
    (:new 7)
    (:edit 6)
    (:detail 5)
    (:create 4)
    (:update 3)
    (:patch 2)
    (:put 1)
    (:delete 0)))

;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
