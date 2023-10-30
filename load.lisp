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
  (:use :simian :cl)
  (:nicknames :ror :rails)
  (:export #:generate))

(in-package :ror)

(let ();(sb-ext:*muffled-warnings* 'style-warning))
  (load-unparser "ruby")
  (load-unparser "sql")
  (load-unparser "html")
  (load-unparser "js")
  (load-unparser "css")
  (load-unparser "yaml")
  (load-unparser "english")
  (flet ((ld (file &optional continue?)
           (let ((file-path (merge-pathnames file *load-truename*)))
             (if (probe-file file-path)
                 (progn
                   (load file-path)
                   (format t "~%loaded ~a" file-path))
                 (if continue?
                     (format t "there is no ~a file...skipping" file-path)
                     (error "can't find file ~a in ~a" file *load-truename*))))))
    (ld "ror.lisp")
    (ld "general.lisp")
    (ld "styling.lisp")
    (ld "unparser.lisp")
    (ld "utilities.lisp")
    (ld "authentication.lisp")
    (ld "internationalization.lisp")
    (ld "framework.lisp")
    (ld "controller.lisp")
    (ld "core-controller.lisp")
    (ld "validations.lisp")
    (ld "associations.lisp")
    (ld "callbacks.lisp")
    (ld "application_record.lisp")
    (ld "model.lisp")
    (ld "database.lisp")
    (ld "migrations.lisp")
    (ld "views.lisp")
    (ld "shared-views.lisp")
    (ld "core-partials.lisp")
    (ld "dependencies.lisp")
    (ld "javascripts.lisp")
    (ld "css.lisp")
    (ld "tasks.lisp")
    (ld "routes.lisp")
    (ld "local.lisp" t)))

(format t "~&To generate a Rails application at any time run (ror:generate)")



;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
