;;;===========================================================================
;;;
;;;   Code for generating framework page layouts, routes and controllers
;;;
;;;===========================================================================

(in-package #:controller)

(defun framework-method (method body &optional (stream t))
  (princ (make-indent) stream)
  (format stream (ruby:unparse-method method body)))

(defun framework-home-method   (&optional (stream t)) (framework-method "home" nil stream))
(defun framework-about-method  (&optional (stream t)) (framework-method "about" nil stream))
(defun framework-denied-method (&optional (stream t)) (framework-method "denied" nil stream))
(defun framework-help-method   (&optional (stream t)) (framework-method "help" nil stream))

(defmethod framework-controller-definition (&optional (stream t))
  (format stream (controller-class-declaration "FrameworkController"))
  (terpri stream) (terpri stream)
  (with-nesting
    (framework-home-method stream) (terpri stream)    
    (framework-denied-method stream) (terpri stream)    
    (framework-about-method stream) (terpri stream)
    (framework-help-method stream) (terpri stream))
  (format stream "end~%"))

(defun framework_controller.rb ()
  (let ((file (merge-pathnames
               (make-pathname :name "framework_controller"
                              :type "rb")
               (controller-directory)))
        (*nesting-level* 0))
    (with-open-file (cont-file file :direction :output :if-exists :supersede)
      (format-file-notice cont-file "framework_controller.rb")
      (framework-controller-definition cont-file))))

;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
