;;;===========================================================================
;;; file:   generators/ror/css.lisp
;;; auth:   Coby Beck
;;; date:   2021-11-30
;;; update: 
;;;---------------------------------------------------------------------------
;;;    css data needed by other generated code
;;;---------------------------------------------------------------------------  
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :ror)

(pushnew
 '("nested-fields"
   "
.nested-fields {
    display: flex;
    opacity: 1;
    -webkit-transition: opacity 0.75s ease-in-out;
    -moz-transition: opacity 0.75s ease-in-out;
    -ms-transition: opacity 0.75s ease-in-out;
    -o-transition: opacity 0.75s ease-in-out;
    transition: opacity 0.75s ease-in-out;
}

.nested-fields.fadeout {
    opacity:0;
}
")
 *css-components* :test #'string-equal :key #'car)




;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
