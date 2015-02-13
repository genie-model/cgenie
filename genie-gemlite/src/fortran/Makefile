
LOCALFLAGS=$(FFLAGS) $(BOUNDS_FLAGS) $(FLAGR8)
ifeq ($(MACHINE),WIN32)
	CMNSG = ..\\..\\..\\genie-main\\src\\fortran\\cmngem\\
	MAIN_DIR = ..\\..\\..\\genie-main\\
#	BIODIC = ..\\..\\..\\genie-biogem\\src\\fortran\\
#	ATCDIC = ..\\..\\..\\genie-atchem\\src\\fortran\\
else
	CMNSG = ../../../genie-main/src/fortran/cmngem/
	MAIN_DIR = ../../../genie-main/
#	BIODIR = ../../../genie-biogem/src/fortran/
#	ATCDIR = ../../../genie-biogem/src/fortran/
endif
LOCINCPATH = $(MOD_INC_FLAG)$(CMNSG) $(MOD_INC_FLAG)$(MAIN_DIR)
LIBNAME = libgemlite
INCLUDE_FILES =
OTHER_FILES=../../../genie-main/makefile.arc Makefile

SRCS=gemlite_lib.f90 gemlite_box.f90 gemlite_data.f90 initialise_gemlite.f90 \
     cpl_comp_gemlite.f90 gemlite.f90 end_gemlite.f90
OBJECTS=$(SRCS:.f90=.$(OBJ_EXT))

all: $(OBJECTS) $(OTHER_FILES) $(INCLUDE_FILES) makefile.depend

.PHONY: clean tags
clean :
	\rm -f *.$(OBJ_EXT) *.mod makefile.depend

tags:
	@ $(SHELL) -ec 'which $(ETAGS) > /dev/null; \
		$(ETAGS) -a -o $(TAGFILE) *.F; \
		$(ETAGS) -a -o $(TAGFILE) *.f90'

$(OBJECTS): %.$(OBJ_EXT): %.f90 $(OTHER_FILES)
	$(F77) $(COMPILEONLY) $(LOCINCPATH) $(LOCALFLAGS) $(F90FLAGS) \
	$(FPPFLAGS) $< $(OBJ_FLAG)$@

makefile.depend: $(SRCS)
	$(MAIN_DIR)/tools/bin/makedepf90 $(SRCS) >> makefile.depend

ifneq ($(MAKECMDGOALS),clean)
include makefile.depend
endif
