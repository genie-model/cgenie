
LOCALFLAGS=$(FFLAGS) $(SEDGEMPREC) $(BOUNDS_FLAGS) $(FLAGR8)
ifeq ($(MACHINE),WIN32)
	CMNSG = ..\\..\\..\\genie-main\\src\\fortran\\cmngem\\
	MAIN_DIR = ..\\..\\..\\genie-main\\
else
	CMNSG = ../../../genie-main/src/fortran/cmngem/
        MAIN_DIR = ../../../genie-main/
endif
LOCINCPATH = $(MOD_INC_FLAG)$(CMNSG) $(MOD_INC_FLAG)$(MAIN_DIR)
LIBNAME = libsedgem
INCLUDE_FILES =
OTHER_FILES=../../../genie-main/makefile.arc makefile

SRCS=sedgem_lib.f90 cpl_comp_sedgem.f90 cpl_flux_sedgem.f90 \
     sedgem_box_archer1991_sedflx.f90 sedgem_box_ridgwelletal2003_sedflx.f90 \
     sedgem_box_benthic.f90 sedgem_nnutils.f90 sedgem_box.f90 \
     sedgem_data_netCDF.f90 sedgem_data.f90 initialise_sedgem.f90 \
     sedgem.f90 end_sedgem.f90
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
