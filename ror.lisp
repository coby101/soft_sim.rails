;;;===========================================================================
;;; file:   ror.lisp
;;; auth:   Coby Beck
;;; date:   2020-12-04
;;;
;;;---------------------------------------------------------------------------
;;;   code associated with generating a Ruby on Rails application
;;;---------------------------------------------------------------------------  
;;;
;;; 2020
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
load soft_sim
load-project
bash - rails new pcmd
ip is http://172.105.180.23:3000

|#

(in-package :ror)

(defun language-locale () "en")

(defun generate-application (&optional (app *application*))
  (format t "*simple-table-layouts?* is ~a for everything except forms~%" *simple-table-layouts?*)
  (clean-up)
  (when *authenticated-application?*
    (insert-authentication-attributes))
  (conventionalize-app)
  (create-bash-scripts)
  (format t "~&writing ~a" (namestring (routes-file-path)))
  (routes.rb app) 
  (write-data-load-tasks) 
  (generated.yml)
  (format t "~&writing schema.rb at ~s" (namestring (schema-file-path)))
  (schema.rb app)
  (format t "~&writing controller files in ~s" (namestring (controller-directory)))
  (generate-controllers app)
  (format t "~&writing model files in ~s" (namestring (model-directory)))
  (generate-model-files app)
  (format t "~&writing view files in ~s" (namestring (implementation-subdirectory "ror" "app" "views")))
  (generate-views app)
  (format t "~&taking care of dependencies")
  (install-dependencies)
  (format t "~&generating code fragments for documentation")
  (generate-code-for-documentation app)
  (format t "~&writing seeds.rb at ~s" (namestring (seed-file-path)))
  (seeds.rb app)
  (when *dev-mode*
    (format t "~&writing load file at ~s" (namestring (load-file-path)))
    (generate-load-file app))
  (format t "~%~%done~%~%You will find some install scripts in ~a" *installation-directory*))


(defun clean-up()
  "this function will delete all entity specific files to ensure nothing is left ~
   behind due to renaming.  Files with application independent names will remain."
  ;; no need to check if these directories exist as implementation-subdirectory
  ;; will create them to be harmlessly deleted while still empty
  (uiop:delete-directory-tree (implementation-subdirectory "ror" "app") :validate t)
  (uiop:delete-directory-tree (implementation-subdirectory "ror" "config") :validate t)
  (uiop:delete-directory-tree (db-directory) :validate t)
  (uiop:delete-directory-tree (implementation-subdirectory "ror" "views") :validate t))

(defun generate-load-file (&optional (app *application*))
  (let ((file-path (load-file-path)))
    (with-open-file (load-file file-path :direction :output :if-exists :supersede)
      (format load-file "load '~a'~%" (namestring (schema-file-path)))
      (dolist (entity (model-entities app))
        (format load-file "load '~a'~%" (namestring (model-file-path entity)))
        (format load-file "puts 'loaded ~a'~%" (namestring (model-file-path entity)))))))

(defun generate-code-for-documentation (&optional (app *application*))
  (when *dev-mode* (setf simian::*code-examples* (make-hash-table :test #'equalp)))
  (dolist (entity (schema-entities app))
    (store-code-examples entity "Model Definition"
       (with-output-to-string (str)
         (write-model-class entity str))))
  (dolist (entity (database-tables app))
    (store-code-examples entity "Table Schema Definition"
       (with-output-to-string (str)
         (create_table entity str)
         (terpri str)
         (change_table entity str))))
  (dolist (view (views app))
    (dolist (aspect (aspects view))
      ;; (store-code-examples aspect "View Layouts"
      ;;     (with-output-to-string (str)
      ;;       (when (listable? aspect) (write-list-layout aspect str))
      ;;       (when (showable? aspect) (write-detail-layout aspect str))
      ;;       (when (updatable? aspect) (write-edit-layout aspect str))
      ;;       (when (creatable? aspect) (write-create-layout aspect str))
      ;;       (when (updatable? aspect) (write-update-form aspect str))))
      (store-code-examples aspect "Controller Definition"
          (with-output-to-string (str)
            (controller-class-definition aspect str))))))

(defparameter *reserved-words*
  '("ADDITIONAL_LOAD_PATHS"
    "ARGF"
    "ARGV"
    "ActionController"
    "ActionView"
    "ActiveRecord"
    "ArgumentError"
    "Array"
    "BasicSocket"
    "Benchmark"
    "Bignum"
    "Binding"
    "CGI"
    "CGIMethods"
    "CROSS_COMPILING"
    "Class"
    "ClassInheritableAttributes"
    "Comparable"
    "ConditionVariable"
    "Config"
    "Continuation"
    "DRb"
    "DRbIdConv"
    "DRbObject"
    "DRbUndumped"
    "Data"
    "Date"
    "DateTime"
    "Delegater"
    "Delegator"
    "Digest"
    "Dir"
    "ENV"
    "EOFError"
    "ERB"
    "Enumerable"
    "Errno"
    "Exception"
    "FALSE"
    "FalseClass"
    "Fcntl"
    "File"
    "FileList"
    "FileTask"
    "FileTest"
    "FileUtils"
    "Fixnum"
    "Float"
    "FloatDomainError"
    "GC"
    "Gem"
    "GetoptLong"
    "Hash"
    "IO"
    "IOError"
    "IPSocket"
    "IPsocket"
    "IndexError"
    "Inflector"
    "Integer"
    "Interrupt"
    "Kernel"
    "LN_SUPPORTED"
    "LoadError"
    "LocalJumpError"
    "Logger"
    "Marshal"
    "MatchData"
    "MatchingData"
    "Math"
    "Method"
    "Module"
    "Mutex"
    "Mysql"
    "MysqlError"
    "MysqlField"
    "MysqlRes"
    "NIL"
    "NameError"
    "NilClass"
    "NoMemoryError"
    "NoMethodError"
    "NoWrite"
    "NotImplementedError"
    "Numeric"
    "OPT_TABLE"
    "Object"
    "ObjectSpace"
    "Observable"
    "Observer"
    "PGError"
    "PGconn"
    "PGlarge"
    "PGresult"
    "PLATFORM"
    "PStore"
    "ParseDate"
    "Precision"
    "Proc"
    "Process"
    "Queue"
    "RAKEVERSION"
    "RELEASE_DATE"
    "RUBY"
    "RUBY_PLATFORM"
    "RUBY_RELEASE_DATE"
    "RUBY_VERSION"
    "Rack"
    "Rake"
    "RakeApp"
    "RakeFileUtils"
    "Range"
    "RangeError"
    "Rational"
    "Regexp"
    "RegexpError"
    "Request"
    "RuntimeError"
    "STDERR"
    "STDIN"
    "STDOUT"
    "ScanError"
    "ScriptError"
    "SecurityError"
    "Signal"
    "SignalException"
    "SimpleDelegater"
    "SimpleDelegator"
    "Singleton"
    "SizedQueue"
    "Socket"
    "SocketError"
    "StandardError"
    "String"
    "StringScanner"
    "Struct"
    "Symbol"
    "SyntaxError"
    "SystemCallError"
    "SystemExit"
    "SystemStackError"
    "TCPServer"
    "TCPSocket"
    "TCPserver"
    "TCPsocket"
    "TOPLEVEL_BINDING"
    "TRUE"
    "Task"
    "Text"
    "Thread"
    "ThreadError"
    "ThreadGroup"
    "Time"
    "Transaction"
    "TrueClass"
    "TypeError"
    "UDPSocket"
    "UDPsocket"
    "UNIXServer"
    "UNIXSocket"
    "UNIXserver"
    "UNIXsocket"
    "UnboundMethod"
    "Url"
    "VERSION"
    "Verbose"
    "YAML"
    "ZeroDivisionError"
    "@base_path"
    "accept"
    "Acces"
    "Axi"
    "action"
    "attributes"
    "application2"
    "callback"
    "category"
    "connection"
    "database"
    "dispatcher"
    "display1"
    "drive"
    "errors"
    "format"
    "host"
    "key"
    "layout"
    "load"
    "link"
    "new"
    "notify"
    "open"
    "public"
    "quote"
    "render"
    "request"
    "records"
    "responses"
    "save"
    "scope"
    "send"
    "session"
    "system"
    "template"
    "test"
    "timeout"
    "to_s"
    "type"
    "URI"
    "visits"
    "Observer"))

(defun blacklisted-attribute-names (ent)
  (append (list "created_at" "created_on" "updated_at" "updated_on" "deleted_at"
                "lock_version" "type" "id" "position" "lft" "rgt" "quote_value")
          *reserved-words*
          (mapcar #'(lambda (fk)
                      (strcat (snake-case (name fk)) "_id"))
                  (foreign-keys ent))
          (list (strcat (snake-case (name ent)) "_count"))))

(defmethod conventionalize-entity ((entity entity))
  (mapcar #'conventionalize-attribute-name (attributes entity)))

(defmethod conventionalize-entity ((entity generalized-entity))
  (call-next-method)
  (when (not (typep entity 'specialized-entity))
    (unless (string-equal "Type"
                          (ignore-errors (name (find-field :modeltype entity))))
      (let ((model-type (make-instance 'persistent-attribute
                :designation  (make-instance 'designation
                                 :id :modeltype
                                 :name "Type"
                                 :plural "Types"
                                 :short-name "Model Type"
                                 :short-plural "Model Types"
                                 :long-name "Model Specialization Type"
                                 :long-plural "Model Specialization Types")
                :indexed?     t
                :read-only?   t
                :domain (default-domain (get-logical-type :label))
                :logical-type (get-logical-type :label))))
        (add-entity-attribute entity model-type)
        (resolve-constraints model-type)))))

(defmethod conventionalize-attribute-name ((att composite-key))
  nil)
(defmethod conventionalize-attribute-name ((att inherited-attribute))
  nil)
(defmethod conventionalize-attribute-name ((att primary-key))
  (setf (name att) "id"))
(defmethod conventionalize-attribute-name ((att foreign-key))
  (unless (string-equal "_id" (subseq (name att) (- (length (name att)) 3)))
    (setf (name att) (strcat (name att) "_id"))))

(defmethod conventionalize-attribute-name ((att attribute))
  (when (and (member (snake-case (name att))
                     (blacklisted-attribute-names (my-entity att))
                     :test #'string-equal)
             (not (eql :modeltype (id att))))
    (setf (name att) (strcat (name (my-entity att)) (name att)))))

(defun conventionalize-app (&optional (app *application*))
  (configure-inflections app)
  (mapcar #'conventionalize-entity (schema app)))

(defmethod irregular-plurals ((obj named-object))
  (remove nil
          (list
           (when (not (string-equal (cl-inflector::pluralize 2 (name obj))
                                    (plural obj)))
             (list (name obj) (plural obj)))
           (when (not (string-equal (cl-inflector::pluralize 2 (short-name obj))
                                    (short-plural obj)))
             (list (short-name obj) (short-plural obj)))
           (when (not (string-equal (cl-inflector::pluralize 2 (long-name obj))
                                    (long-plural obj)))
             (list (long-name obj) (long-plural obj))))))

(defun application-acronyms (&optional (app *application*))
  (remove-duplicates
   (remove nil
          (apply #'append
                (mapcar #'find-acronyms
                        (append (mapcar #'name (schema app))
                                (mapcar #'name (find-all-relationships app))))))
   :test #'string-equal))

(defun configure-inflections (&optional (app *application*))
  (let ((plurals
         (append (apply #'append (mapcar #'irregular-plurals (schema app)))
                 (apply #'append (mapcar #'irregular-plurals (find-all-relationships app)))))
        (acronyms (application-acronyms app)))
    (let ((path (merge-pathnames (make-pathname :name "inflections" :type "rb")
                                   (implementation-subdirectory "ror" "config" "initializers"))))
        (with-open-file (inflect path :direction :output :if-exists :supersede)
          (format-file-notice inflect "configure-inflections")
          (format inflect "~%ActiveSupport::Inflector.inflections(:~a) do |inflect|"
                  (language-locale))
          (with-nesting
             (dolist (irr (remove-duplicates plurals :test #'equalp))
               (if (string= (car irr) (cadr irr))
                   (progn
                     ;; (seek-interactive-guidance
                     ;;  ("This plural and singular (~a) are identical. This is a big Rails headache." (car irr))
                     ;;  :actions (("Plow blindly ahead" nil)))
                     (format inflect "~%~ainflect.uncountable '~a'" (make-indent) (car irr)))
                   (format inflect "~%~ainflect.irregular '~a', '~a'" (make-indent)
                           (car irr) (cadr irr))))
            (dolist (acr acronyms)
              (format inflect "~%~ainflect.acronym '~a'" (make-indent) acr)))
          (format inflect "~%end~%~%")))))

(defun model-file-path (entity)
  (merge-pathnames (make-pathname :name (if (typep entity 'entity) (snake-case (name entity)) entity)
                                  :type "rb")
                   (model-directory)))

(defun load-file-path ()
  (merge-pathnames
   (make-pathname :name "load" :type "rb")
   (implementation-subdirectory "ror")))

(defun model-directory ()
  (implementation-subdirectory "ror" "app" "models"))

(defun controller-file-path (aspect)
  (merge-pathnames
   (make-pathname
    :name (snake-case (strcat (plural (entity aspect)) "_controller"))
    :type "rb")
   (controller-directory aspect)))

(defun controller-directory (&optional aspect)
  (apply #'implementation-subdirectory
         (list* "ror" "app" "controllers" (when aspect (unparse-namespace aspect :list)))))

(defun db-migration-file-path (entity)
  (merge-pathnames
   (make-pathname :name (strcat (timestamp) "_create_" (snake-case (plural entity))) :type "rb")
   (db-migration-directory)))

(defun db-migration-directory ()
  (implementation-subdirectory "ror" "db" "migrate"))

(defun db-directory ()
  (implementation-subdirectory "ror" "db"))

(defun javascript-directory (&rest sub-dirs)
  (apply #'implementation-subdirectory "ror" "app" "javascript" "packs" sub-dirs))

(defun javascript-file-path (file-name &rest subdirs)
  (merge-pathnames
   (make-pathname :name file-name :type "js")
   (apply #'javascript-directory subdirs)))

(defun css-directory (&rest sub-dirs)
  (apply #'implementation-subdirectory "ror" "app" "assets" "stylesheets" "components" sub-dirs))

(defun css-file-path (file-name &rest subdirs)
  (merge-pathnames
   (make-pathname :name file-name :type "css")
   (apply #'css-directory subdirs)))

(defun helper-directory ()
  (implementation-subdirectory "ror" "app" "helpers"))

(defun helper-file-path (file-name)
  (merge-pathnames
   (make-pathname :name file-name :type "rb")
   (helper-directory)))

(defun layout-directory (aspect)
  (apply #'implementation-subdirectory
         (append '("ror" "app" "views")
                 (unparse-namespace aspect :list)
                 (list (snake-case (plural (entity aspect)))))))

(defun layout-file-path (aspect panel)
  (merge-pathnames
   (make-pathname :name (strcat panel ".html") :type "erb")
   (layout-directory aspect)))

(defun routes-file-path()
  (merge-pathnames
   (make-pathname :name "routes" :type "rb")
   (implementation-subdirectory "ror" "config")))

(defun task-file-path(name)
  (merge-pathnames
   (make-pathname :name name :type "rake")
   (implementation-subdirectory "ror" "lib" "tasks")))

(defun test-data-file-path(name)
  (merge-pathnames
   (make-pathname :name name :type "rb")
   (implementation-subdirectory "ror" "db" "data")))

(defun schema-file-path()
  (merge-pathnames
   (make-pathname :name "schema" :type "rb")
   (db-directory)))

(defun seed-file-path(&optional (name "seeds"))
  (merge-pathnames
   (make-pathname :name name :type "rb")
   (db-directory)))

(defun db-structure-file-path()
  (merge-pathnames
   (make-pathname :name "structure" :type "sql")
   (db-directory)))

;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
