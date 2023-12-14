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
               (:file "ror")
               (:file "installation")
               (:file "unparser")
               (:file "utilities")
               (:file "authentication/devise")
               (:file "config/i18n")
               (:module controller
                :components
                ((:file "framework")
                 (:file "controller")
                 (:file "core-controller")))
               (:module model
                :components
                ((:file "validations")
                 (:file "associations")
                 (:file "callbacks")
                 (:file "application_record")
                 (:file "model")))
               (:module database
                :components
                ((:file "schema")
                 (:file "migrations")
                 (:file "seeds")
                 (:file "database")))
               (:module view
                :components
                ((:file "styling")
                 (:file "views")
                 (:file "shared-views")
                 (:file "core-partials")
                 (:file "unparse-elements")))
               (:module dependencies
                :components
                ((:file "application_helper")
                 (:file "javascripts")
                 (:file "css") 
                 (:file "dependencies")))
               (:file "tasks")
               (:file "testing/factory_bot")
               (:file "config/routes")))

(format t "~&To generate a Rails application at any time load your software.simian project and run (ror:generate)")

;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
