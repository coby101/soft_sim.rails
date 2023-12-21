(defpackage :simian.rails-generator.authentication
  (:use
   :cl :utilities :foundation :interrogation :construction :entity
   :rails-unparser
   )
  (:nicknames :authentication)
  (:export
   #:*authenticated-application?*
   #:*notified-application?*
   #:*devise-modules*
   #:*user-model*
   #:app-controller-authentication-code
   #:app-controller-notification-code
   #:authenticating-model?
   #:current-user-display
   #:logout-link
   #:insert-authentication-attributes
   #:insert-authentication-model-code
   #:send_notification-method
   ))
