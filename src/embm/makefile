
LOCALFLAGS=$(FFLAGS) $(BOUNDS_FLAGS) $(FLAGR8)
ifeq ($(MACHINE),WIN32)
	CMNSG = ..\\..\\..\\genie-main\\src\\fortran\\cmngem\\
	MAIN_DIR = ..\\..\\..\\genie-main\\
else
	CMNSG = ../../../genie-main/src/fortran/cmngem/
        MAIN_DIR = ../../../genie-main/
endif
LOCINCPATH = $(MOD_INC_FLAG)$(CMNSG) $(MOD_INC_FLAG)$(MAIN_DIR)
LIBNAME = libembm
INCLUDE_FILES =
OTHER_FILES=../../../genie-main/makefile.arc makefile

SRCS=embm.f90 embm_lib.f90 embm_netcdf.f90 embm_data.f90 embm_diag.f90
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
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) $(LOCINCPATH) $(F90FLAGS) \
	$(FPPFLAGS) $< $(OBJ_FLAG)$@

makefile.depend: $(SRCS)
	$(MAIN_DIR)/tools/bin/makedepf90 $(SRCS) >> makefile.depend

ifneq ($(MAKECMDGOALS),clean)
include makefile.depend
endif
