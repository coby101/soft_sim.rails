This directory contains the code for writing service objects. 

As an example, rails generator will create service objects for importing records from excel for entities that have specified a set of attributes as `:importable`
Given an entity defined as below:

```lisp
(define-entity ("Client")
  :attributes (("Name") :type name :nullable? nil)
               (("Email" "Email" "Primary Email Address") :type email :nullable? t)
               (("Address1" "Address" "Address (line 1)") :type short-text :nullable? t)
               (("Address2" "Address" "Address (line 2)") :type short-text :nullable? t)
               (("Locality" "Locality" "Municipality or Suburb") :type name :nullable? t)
               ("State"    :type name :nullable? t)
               ("PostCode" :type short-text :nullable? t))
  :importable (Name Email Address1 Address2 Locality State PostCode))

```

We get the code generated below in app/services/client_import_service.rb

```ruby

```
