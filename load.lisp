;;;===========================================================================
;;; file:   load.lisp
;;; auth:   Coby Beck
;;; date:   2021-06-25
;;;
;;;---------------------------------------------------------------------------
;;;   load file for code associated with generating a Ruby on Rails application
;;;---------------------------------------------------------------------------  
;;;
;;;  
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package simian)

(defpackage simian.rails-generator
  (:use :simian :cl :simian.calendar)
  (:nicknames :ror)
  (:export #:generate-application))

(in-package :ror)

(let ((sb-ext:*muffled-warnings* 'style-warning))
  (load-unparser "ruby")
  (load-unparser "sql")
  (load-unparser "html")
  (load-unparser "js")
  (load-unparser "css")
  (load-unparser "yaml")
  (load-unparser "english")
  (load (merge-pathnames "ror.lisp" *load-truename*))
  (format t "~%loaded ~a" (merge-pathnames "ror.lisp" *load-truename*))
  (load (merge-pathnames "general.lisp" *load-truename*))
  (format t "~%loaded ~a" (merge-pathnames "general.lisp" *load-truename*))
  (load (merge-pathnames "styling.lisp" *load-truename*))
  (format t "~%loaded ~a" (merge-pathnames "styling.lisp" *load-truename*))
  (load (merge-pathnames "unparser.lisp" *load-truename*))
  (format t "~%loaded ~a" (merge-pathnames "unparser.lisp" *load-truename*))
  (load (merge-pathnames "utilities.lisp" *load-truename*))
  (format t "~%loaded ~a" (merge-pathnames "utilities.lisp" *load-truename*))
  (load (merge-pathnames "authentication.lisp" *load-truename*))
  (format t "~%loaded ~a" (merge-pathnames "authentication.lisp" *load-truename*))
  (load (merge-pathnames "internationalization.lisp" *load-truename*))
  (format t "~%loaded ~a" (merge-pathnames "internationalization.lisp" *load-truename*))
  (load (merge-pathnames "framework.lisp" *load-truename*))
  (format t "~%loaded ~a" (merge-pathnames "framework.lisp" *load-truename*))
  (load (merge-pathnames "controller.lisp" *load-truename*))
  (format t "~%loaded ~a" (merge-pathnames "controller.lisp" *load-truename*))
  (load (merge-pathnames "core-controller.lisp" *load-truename*))
  (format t "~%loaded ~a" (merge-pathnames "core-controller.lisp" *load-truename*))
  (load (merge-pathnames "validations.lisp" *load-truename*))
  (format t "~%loaded ~a" (merge-pathnames "validations.lisp" *load-truename*))
  (load (merge-pathnames "associations.lisp" *load-truename*))
  (format t "~%loaded ~a" (merge-pathnames "associations.lisp" *load-truename*))
  (load (merge-pathnames "callbacks.lisp" *load-truename*))
  (format t "~%loaded ~a" (merge-pathnames "callbacks.lisp" *load-truename*))
  (load (merge-pathnames "application_record.lisp" *load-truename*))
  (format t "~%loaded ~a" (merge-pathnames "application_record.lisp" *load-truename*))
  (load (merge-pathnames "model.lisp" *load-truename*))
  (format t "~%loaded ~a" (merge-pathnames "model.lisp" *load-truename*))
  (load (merge-pathnames "database.lisp" *load-truename*))
  (format t "~%loaded ~a" (merge-pathnames "database.lisp" *load-truename*))
  (load (merge-pathnames "migrations.lisp" *load-truename*))
  (format t "~%loaded ~a" (merge-pathnames "migrations.lisp" *load-truename*))
  (load (merge-pathnames "views.lisp" *load-truename*))
  (format t "~%loaded ~a" (merge-pathnames "views.lisp" *load-truename*))
  (load (merge-pathnames "shared-views.lisp" *load-truename*))
  (format t "~%loaded ~a" (merge-pathnames "shared-views.lisp" *load-truename*))
  (load (merge-pathnames "core-partials.lisp" *load-truename*))
  (format t "~%loaded ~a" (merge-pathnames "core-partials.lisp" *load-truename*))
  (load (merge-pathnames "dependencies.lisp" *load-truename*))
  (format t "~%loaded ~a" (merge-pathnames "dependencies.lisp" *load-truename*))
  (load (merge-pathnames "javascripts.lisp" *load-truename*))
  (format t "~%loaded ~a" (merge-pathnames "javascripts.lisp" *load-truename*))
  (load (merge-pathnames "css.lisp" *load-truename*))
  (format t "~%loaded ~a" (merge-pathnames "css.lisp" *load-truename*))
  (load (merge-pathnames "tasks.lisp" *load-truename*))
  (format t "~%loaded ~a" (merge-pathnames "tasks.lisp" *load-truename*))
  (load (merge-pathnames "routes.lisp" *load-truename*))
  (format t "~%loaded ~a" (merge-pathnames "routes.lisp" *load-truename*))
  (if (probe-file (merge-pathnames "local.lisp" *load-truename*))
      (load (merge-pathnames "local.lisp" *load-truename*))
      (princ
       (format nil "there is no ~a file, which does not bother me. I would be happy to load one"
               (namestring (merge-pathnames "local.lisp" *load-truename*))))))

(format t "~&To generate a Rails application at any time run (ror:generate-application)")



;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
