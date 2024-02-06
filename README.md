# soft_sim.rails

Ruby on Rails code generator for use with the soft_sim application parser. In your Lisp REPL do the following: 

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
