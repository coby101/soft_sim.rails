;;;===========================================================================
;;;
;;;   Code for generating routes.rb. There are two types of unparsing avalable
;;;   and are chosen via the 'fully-resolved?' parameter passed to 'wirte-routes'
;;;   The fully resolved approach produces very detailed route specifications and
;;;   is not a very conventional approach but it provides for "reaching inside" 
;;;   where no conventional routes are wanted. With 'fully-resolved' nil, the 
;;;   rails magic provided by 'resources' is written instead.
;;;
;;;===========================================================================

(in-package #:app-config)

(defvar *unparse-fully-resolved-routes* nil)

(defun framework-routes ()
  (append
   (if *authenticated-application?*
       (list (format nil "devise_for :~a" (schema-name (find-entity *user-model*)))
             "root to: 'framework#home'")
       (list "get '/', to: 'framework#home', as: 'home'"))
   (list "get '/no_access', to: 'framework#denied'"
         "get '/about', to: 'framework#about', as: 'about'"
         "mount ActionCable.server => '/cable'")))

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

(defun implemented-actions (aspect)
  (let ((entity (entity aspect))
        (view (view aspect)))
    (if (and (name view)
             (typep entity 'attribute-table)
             (find-aspect view (owner entity)))
        (when (updatable? (find-aspect view (owner entity)))
          (list :update :delete))
        (actions aspect))))

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

(defmethod unparse-routes ((aspect aspect))
  (let ((actions (implemented-actions aspect)))
  (loop for action in (sort actions #'> :key #'route-rank)
     collect (unparse-route aspect action))))

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
;(setf space (car (spaces *application*)))

(defun unparse-aspect-resources (tree &optional (stream t))
  (if (null tree)
    (format stream "")
    (let* ((aspect (car tree))
           (actions (implemented-actions aspect)))
      (format stream "resources :~a, only: %i[~{~a~^ ~}]" (schema-name (entity aspect)) (remove-duplicates (mapcar #'controller-method actions) :test #'string-equal))
      (if (or (cdr tree) nil) ;;(member :list actions)) the :list action is PCMD specific
          (format stream " do~%"))
      (if nil ;;(member :list actions)) the :list action is PCMD specific
          (format stream "~a  collection do get :list end" (make-indent)))
      (if (cdr tree)
          (with-nesting
            (dolist (subtree (cdr tree))
              (format stream "~a" (make-indent))
              (unparse-aspect-resources subtree stream)
              (format stream "~%" ))))
      (if (or (cdr tree)  nil) ;;(member :list actions)) the :list action is PCMD specific
              (format stream "~aend" (make-indent))))))

(defun unparse-view-resources (view &optional (stream t))
  (let ((heirarchy (view-heirarchy view)))
    (if heirarchy
        (unparse-aspect-resources heirarchy stream)
        (format stream ""))))

#|
scope '/affiliates' do
  resources :clients, policy_for: 'clients' do
    collection do
      get :list
      post :import
      get :download_template_file
    end
    resources :client_notes, only: [:update, :destroy]
    resources :client_contacts, only: [:update, :destroy]
    resources :projects, only: [:show, :index, :destroy] do
      collection do get :list end
    end
  end
  resources :subcontracts, policy_for: 'subcontracts' do
    collection do get :list end
    end
  end
|#

(defun unparse-route-scope (space &optional (stream t))
  (format stream "scope '/~a' do" (snake-case (name space)))
  (with-nesting
     (dolist (view (views space))
       (format stream "~%~a" (make-indent))
       (unparse-view-resources view stream)))
  (format stream "~%end~%"))

(defun write-resources-file (space)
  (with-open-file (resource-file (route-resource-filepath (string-downcase (name space))) :direction :output :if-exists :supersede)
    (unparse-route-scope space resource-file)))

(defun write-routes (stream &key (app *application*) (fully-resolved? nil))
  (format stream "~%~{  ~a~%~}~%~%" (framework-routes))
  ;; (when *dev-mode*
  ;;   (dolist (view (mapcar #'create-default-view (schema-entities app)))
  ;;     (comment-out stream "routes for the ~s view" (long-name view))
  ;;     (format stream "~{  ~a~%~}~%"
  ;;             (mapcan #'unparse-routes (aspects view)))))
  (if fully-resolved?
   (dolist (view (views app))
     (format stream "  # routes for the ~s view~%" (long-name view))
     (format stream "~{  ~a~%~}~%" (mapcan #'unparse-routes (aspects view))))
   (dolist (space (spaces app))
     (format stream "  # routes for the ~s application space~%" (long-name space))
     (format stream "  draw :~a~%" (string-downcase (name space)))
     (write-resources-file space))))

(defun routes.rb (&optional (app *application*))
  (with-open-file (routes (routes-file-path) :direction :output :if-exists :supersede)
    (format-file-notice routes "routes.rb")
    (format routes "~%Rails.application.routes.draw do~%")
    (write-routes routes :app app :fully-resolved? *unparse-fully-resolved-routes*)
    (format routes "~%end~%")))

;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
