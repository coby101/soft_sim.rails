;;;===========================================================================
;;;
;;;   defsystem for Ruby on Rails application generator
;;;   to use with Software Simian projects
;;;
;;;===========================================================================

(asdf:defsystem #:rails-generator
  :description "A Ruby on Rails implementation generator for use with Software Simian application object models"
  :author "Coby Beck <coby101@gmail.com>"
  :license "probably Apache License 2.0"
  #+asdf-unicode :encoding #+asdf-unicode :utf-8
  :serial t
  :depends-on (#:soft-sim
               #:lisp-unit2)
  :components ((:file "load-unparsers")
               (:module unparsing
                :components
                ((:file "package")
                 (:file "general")
                 (:file "attribute-references")
                 (:file "migrations")
                 (:file "model-general")
                 (:file "model-validations")
                 (:file "translations")
                 (:file "styling")
                 (:file "view-general")
                 (:file "view-elements")))
               (:file "installation")
               (:module authentication
                :components
                ((:file "package")
                 (:file "devise")))
               (:module dependencies
                :components
                ((:file "package")
                 (:file "application_helper")
                 (:file "javascripts")
                 (:file "css") 
                 (:file "dependencies")))
               (:module model
                :components
                ((:file "package")
                 (:file "validations")
                 (:file "associations")
                 (:file "callbacks")
                 (:file "application_record")
                 (:file "model")))
               (:module app-config
                :components
                ((:file "package")
                 (:file "i18n")
                 (:file "initializers")
                 (:file "routes")))
               (:module view
                :components
                ((:file "package")
                 (:file "framework")
                 (:file "views")
                 (:file "shared-views")
                 (:file "core-partials")))
               (:module controller
                :components
                ((:file "package")
                 (:file "variables")
                 (:file "class")
                 (:file "framework")
                 (:file "controller")
                 (:file "core-controller")))
               (:module database
                :components
                ((:file "package")
                 (:file "schema")
                 (:file "seeds")
                 (:file "migrations")
                 (:file "database")))
               (:module app-tasks
                :components
                ((:file "package")
                 (:file "tasks")))
               (:module app-tests
                :components
                ((:file "package")
                 (:file "factory_bot")))
               (:file "package")
               (:file "ror")
               (:module tests
                :components
                ((:file "package")
                 (:file "test-framework")))
                 ))

;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
