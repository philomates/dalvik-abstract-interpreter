Dalvik Interpreters
====================

Interpreter Descriptions
------------------------
interpreters/abstract-interpreter.rkt:
  - k-CFA style abstract interpreter adapted from concrete CESK machine. Explores approximation of a program's state space.

interpreters/concrete-interpreter.rkt:
  - partially implemented cesk machine that should be able to run Dalvik implementations fibonacci or factorial.


How to Run
----------

Enter an Android app directory:

  $ cd example_code/LambdaInterpreterApp

Notice already built Android app and the intermediary dex file:

  $ ls bin/*.apk

  $ ls bin/*.dex

Run the following to dedex .dex file, transform dedexed files into sexpressions readable by Racket, and copy stubbed out Java libraries:

  $ sh ../../bin/project2sexpr

Change the hard coded "app-directory" and "starting-function" in interpreters/abstract-interpreter.rkt and run in DrRacket. Uncomment last line of abstract-interpreter.rkt to create a graphviz dot file for graph creation.
