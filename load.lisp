;;;===========================================================================
;;;
;;;   load file for Ruby on Rails application generator
;;;
;;;===========================================================================

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
    (ld "unparser.lisp")
    (ld "utilities.lisp")
    (ld "authentication/devise.lisp")
    (ld "config/i18n.lisp")
    (ld "framework.lisp")
    (ld "controller/controller.lisp")
    (ld "controller/core-controller.lisp")
    (ld "model/validations.lisp")
    (ld "model/associations.lisp")
    (ld "model/callbacks.lisp")
    (ld "model/application_record.lisp")
    (ld "model/model.lisp")
    (ld "database/database.lisp")
    (ld "view/styling.lisp")
    (ld "view/views.lisp")
    (ld "view/shared-views.lisp")
    (ld "view/core-partials.lisp")
    (ld "view/unparse-elements.lisp")
    (ld "dependencies/dependencies.lisp")
    (ld "tasks.lisp")
    (ld "testing/factory_bot.lisp")
    (ld "config/routes.lisp")
    (ld "local.lisp" t)))

(format t "~&To generate a Rails application at any time load your software.simian project and run (ror:generate)")

;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
