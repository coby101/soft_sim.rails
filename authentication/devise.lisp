;;;===========================================================================
;;;
;;;   code associated with generating authentication code
;;;      - NB: based on the devise gem
;;;
;;;===========================================================================

(in-package #:authentication)

(defparameter *user-model* "User")
(defvar *authenticated-application?* nil)
(defvar *notified-application?* nil)

(defparameter *devise-modules*
  (list "database_authenticatable" "recoverable" "rememberable"
        "validatable" "lockable" "trackable"))

(defun logout-link ()
  "<%= link_to 'Logout', destroy_user_session_path, method: :delete if user_signed_in? %>")

(defun current-user-display ()
  "<i>You are logged in as <%= current_user.name %> </i>")

(defun recoverable-authentication? ()
  (member "recoverable" *devise-modules* :test #'string-equal))
(defun rememberable-authentication? ()
  (member "rememberable" *devise-modules* :test #'string-equal))
(defun trackable-authentication? ()
  (member "trackable" *devise-modules* :test #'string-equal))
(defun confirmable-authentication? ()
  (member "confirmable" *devise-modules* :test #'string-equal))
(defun lockable-authentication? ()
  (member "lockable" *devise-modules* :test #'string-equal))

(defun authenticating-model? (ent)
  (string-equal (name ent) *user-model*))

(defun authenticating-model ()
  (find-entity *user-model*))

(defun app-controller-notification-code()
  (format nil "after_action :send_notification, only: [:create, :update]"))
(defun send_notification-method ()
  "  def send_notification
    if response.status == 302 && !!params && !!params[:commit] && !!params[:action]
      data_to_send = nil
      params.each do |key, val|
        if val.class!=String
          data_to_send = val.to_unsafe_hash
        end
      end
      action = params[:action].downcase
      url = response.header[\"Location\"]
      commit = params[:commit]&.downcase
      message = commit&.sub! \"#{action} \",\"\"
      if params[:action]==\"create\"
        AlertNotification.with(alert:{message:\"#{current_user.name} created a new #{message}\", data:data_to_send, url:url}).deliver(current_user)
      else
        AlertNotification.with(alert:{message:\"#{current_user.name} updated a #{message}\", data:data_to_send, url:url}).deliver(current_user)
      end
    end
  end
")

(defun app-controller-authentication-code()
  (format nil "before_action :authenticate_~a!" (snake-case *user-model*)))

(defun insert-authentication-model-code (entity stream)
  (when (authenticating-model? entity)
      (format stream "~%~adevise ~{:~a~^, ~}" (make-indent) *devise-modules*)))


(defun insert-authentication-attributes ()
  (print "inserting user authentication attributes")
  (let ((user-model (authenticating-model))
        (attributes
          (append
           (list (make-user-attribute "Encrypted Password" :type :password :nullable? nil :default "")
                 (make-user-attribute "Email" :type :email :nullable? nil :constraints '(($unique))))
           (when (recoverable-authentication?)
             (list (make-user-attribute "Reset Password Token" :type :string :constraints '(($unique)))
                   (make-user-attribute "Reset Password Sent At" :type :datetime)))
           (when (rememberable-authentication?)
             (list (make-user-attribute "Remember Created At" :type :datetime)))
           (when (trackable-authentication?)
             (list (make-user-attribute "Current Sign In At" :type :datetime)
                   (make-user-attribute "Current Sign In IP" :type :string)
                   (make-user-attribute "Last Sign In At" :type :datetime)
                   (make-user-attribute "Last Sign In IP" :type :string)
                   (make-user-attribute "Sign In Count" :type :count :nullable? nil :default 0)))
           (when (confirmable-authentication?)
             (list (make-user-attribute "Confirmation Token" :type :password :constraints '(($unique)))
                   (make-user-attribute "Confirmed At" :type :datetime)
                   (make-user-attribute "Confirmation Sent At" :type :datetime)
                   (make-user-attribute "Unconfirmed Email" :type :email)))
           (when (lockable-authentication?)
             (list (make-user-attribute "Failed Attempts" :type :count :nullable? nil :default 0)
                   (make-user-attribute "Unlock Token" :type :password :constraints '(($unique)))
                   (make-user-attribute "Locked At" :type :datetime))))))
    (when user-model
      (dolist (att attributes)
        (add-entity-attribute user-model att))
      (resolve-constraint-objects))))




;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
