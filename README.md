# soft_sim.rails

Ruby on Rails code generator for use with the soft_sim application parser. After you load your project, in your Lisp REPL eval: 

````
(ql:quickload :soft-sim)
(soft-sim:load-project "my-project")
(ql:quickload :rails-generator)
(rails:generate)
````
