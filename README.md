# soft_sim.rails

Ruby on Rails code generator for use with the soft_sim application parser. 

With Quicklisp installed in your lisp environment, navigate to its `local-projects` directory. Execute:

  - `git clone https://github.com/coby101/soft_sim`
  - `git clone https://github.com/coby101/soft_sim.rails`

Then, in your Lisp REPL do the following: 

````
(ql:quickload :rails-generator)
(in-package :rails)
````

To generate a rails implementation from an application specification, where the specification is in your quicklisp local-projects directory or you have specified the correct path in `*project-directory*`, do this:
````
(load-project "my-project")
(generate)
````

for just exploring you can also try:
````
(generate (soft-sim.tests:load-demo))
````
















(this repo has about 8000 lines of lisp code in about 70 files as of 07/02/2024)
