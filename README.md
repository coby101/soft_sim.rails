# soft_sim.rails

Ruby on Rails code generator for use with the soft_sim application parser. After you load your project, in your Lisp REPL eval: 

````
(ql:quickload :rails-generator)
(in-package :rails)
(load-project "my-project")
(generate)
````

for just exploring you can also try:
````
(generate (soft-sim.tests:load-demo))
````
