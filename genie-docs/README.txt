README
------

Documentation can be generated from the LATEX source in both postscript (.ps) and Adobe (.pdf) formats.

To build documentation from the LATEX source - from this (~cgenie.muffin/genie-docs) directory issue at the command line ($):

$ make cgenie-user-manual
Builds the user manual specific to the genie trunk of cGENIE.

$ make cgenie-examples
Builds documentation describing various example model configurations and experimental designs.

$ make cgenie-howto
The HOW-TO documentation (potted explanations of how to get stuff done).

To clean up previously build documentation:

$ make clean-cgenie
Cleans all the built documentation files for cgenie.muffin.

The origial GENIE documentation can still be built:

$ make user-manual
Builds the original GENIE user manual. [EXTREMELY OUT-OF-DATE ...]

$ make clean
Cleans the built old user-manual documentation.
