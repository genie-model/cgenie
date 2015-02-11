#
# $Id: makefile 7295 2012-01-05 00:13:43Z andy-ridgwell $
#
LOCALFLAGS=$(FFLAGS) $(F77FLAGS) $(FLAGR8)

LIBNAME = libutil1
OBJECTS = lnsig.$(OBJ_EXT) lnsig1.$(OBJ_EXT)

OTHER_FILES=makefile ../../genie-main/makefile.arc

$(LIBNAME).$(LIB_EXT): $(OBJECTS) $(OTHER_FILES)
	$(LIB_CMD) $(LIBNAME).$(LIB_EXT) $(OBJECTS)

.PHONY: clean tags
clean :
	\rm -f *.$(OBJ_EXT) *.$(LIB_EXT) *__genmod.f90 *__genmod.mod

tags:
	@ $(SHELL) -ec 'which $(ETAGS) > /dev/null; \
		$(ETAGS) -a -o $(TAGFILE) *.f'

lnsig1.$(OBJ_EXT)		: lnsig1.f $(OTHER_FILES)
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) lnsig1.f

lnsig.$(OBJ_EXT)		: lnsig.f $(OTHER_FILES)
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) lnsig.f
