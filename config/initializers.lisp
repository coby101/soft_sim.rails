;;;===========================================================================
;;;
;;;   code to write <rails>/config/initializers/inflections.rb
;;;
;;;===========================================================================

(in-package :app-config)

(defun language-locale () "en")

(defmethod irregular-plurals ((obj named-object))
  (remove nil
          (list
           (when (not (string-equal (cl-inflector:pluralize 2 (name obj))
                                    (plural obj)))
             (list (name obj) (plural obj)))
           (when (not (string-equal (cl-inflector:pluralize 2 (short-name obj))
                                    (short-plural obj)))
             (list (short-name obj) (short-plural obj)))
           (when (not (string-equal (cl-inflector:pluralize 2 (long-name obj))
                                    (long-plural obj)))
             (list (long-name obj) (long-plural obj))))))

(defun application-acronyms (&optional (app *application*))
  (remove-duplicates
   (remove nil
          (apply #'append
                (mapcar #'find-acronyms
                        (append (mapcar #'name (schema app))
                                (mapcar #'name (relationships app))))))
   :test #'string-equal))

(defun configure-inflections (&optional (app *application*))
  (let ((plurals
         (append (apply #'append (mapcar #'irregular-plurals (schema app)))
                 (apply #'append (mapcar #'irregular-plurals (relationships app)))))
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

;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
