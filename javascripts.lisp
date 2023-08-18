;;;===========================================================================
;;; file:   generators/ror/javascripts.lisp
;;; auth:   Coby Beck
;;; date:   2021-11-09
;;; update: 
;;;---------------------------------------------------------------------------
;;;    javascript packs needed by other generated code
;;;---------------------------------------------------------------------------  
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :ror)


(pushnew
  '("nested-fields"
    ("addFields"
     "
class addFields {
    // This executes when the function is instantiated.
    constructor() {
        this.links = document.querySelectorAll('.add_fields')
        this.iterateLinks()
    }

    iterateLinks() {
        // If there are no links on the page, stop the function from executing.
        if (this.links.length === 0) return
        // Loop over each link on the page. A page could have multiple nested forms.
        this.links.forEach(link => {
            link.addEventListener('click', e => {
                this.handleClick(link, e)
            })
            link.click()
        })
    }

    handleClick(link, e) {
        // Stop the function from executing if a link or event were not passed into the function.
        if (!link || !e) return
        // Prevent the browser from following the URL.
        e.preventDefault()
        // Save a unique timestamp to ensure the key of the associated array is unique.
        let time = new Date().getTime()
        // Save the data id attribute into a variable. This corresponds to `new_object.object_id`.
        let linkId = link.dataset.id
        // Create a new regular expression needed to find any instance of the `new_object.object_id` used in the fields data attribute if there's a value in `linkId`.
        let regexp = linkId ? new RegExp(linkId, 'g') : null
        // Replace all instances of the `new_object.object_id` with `time`, and save markup into a variable if there's a value in `regexp`.
        let newFields = regexp ? link.dataset.fields.replace(regexp, time) : null
       
        let id = this.extractElementByIdFromString(newFields)
        let elementExist = this.checkElementExist(id)
        
        if (!elementExist || e.isTrusted)
        {
            // Add the new markup to the form if there are fields to add.
            newFields ? link.insertAdjacentHTML('beforebegin', newFields) : null
        }
    }

    checkElementExist(id){
        let element =  document.getElementsByClassName('nested-fields');
        
        for(var i=0; i<element.length; i++){
            if (element[i].id.includes(id))
                return true
        }
        return false
    }

    extractElementByIdFromString(link) {
        let result;
        temp = document.createElement('div'); 
        temp.innerHTML = link;
        result = temp.querySelector('.' + 'nested-fields')
        if (result != null) {
           return result.id;
        }
    }
}

// Wait for turbolinks to load, otherwise `document.querySelectorAll()` won't work
window.addEventListener('turbolinks:load', () => new addFields())
")
    ("removeFields"
     "
class removeFields {
  // This executes when the function is instantiated.
  constructor() {
      this.iterateLinks()
  }

  iterateLinks() {
    document.addEventListener(\"ajax:beforeSend\", event => {
      // get the last character of the url
      const lastChar = event.target.href.substr(-1)
      // if url undefined or lastChar is not a number?
      if (event.target.href === undefined || isNaN(lastChar)) {
        // sanity check the target is valid and has class 'remove_fields'
        if (event.target && event.target.classList.contains('remove_fields')) {
            this.handleClick(event.target, event, false);
        }
      }
      else{
        if (event.target && event.target.classList.contains('remove_fields')) {
          this.handleClick(event.target, event , true);
        }
      }

      return false;
    });

  }

  handleClick(link, e, stopPrevent) {
    // Stop the function from executing if a link or event were not passed into the function.
    if (!link || !e) return
    // Prevent the browser from following the URL.
    if(stopPrevent) e.preventDefault()
    // Find the parent wrapper for the set of nested fields.
    let fieldParent = link.closest('.nested-fields')
    // If there is a parent wrapper, find the hidden delete field.
    let deleteField = fieldParent
      ? fieldParent.querySelector('input[type=\"hidden\"]')
      : null
    // If there is a delete field, update the value to `1` and hide the corresponding nested fields.
    if (deleteField) {
      deleteField.value = 1
      fieldParent.style.display = 'none'
    }
  }
}

// Wait for turbolinks to load, otherwise `document.querySelectorAll()` won't work
window.addEventListener('turbolinks:load', () => new removeFields())
"))
  *javascript-packs* :test #'string-equal :key #'car)

(defun google-address-js (aspect) "")


;;;===========================================================================
;;; Local variables:
;;; tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
