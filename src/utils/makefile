#
# $Id: makefile 7296 2012-01-05 00:13:50Z andy-ridgwell $
#
LOCALFLAGS=$(FFLAGS) $(F77FLAGS) $(BOUNDS_FLAGS) $(FLAGR8)
ifeq ($(MACHINE),WIN32)
  CMNSG=..\\..\\genie-main\\src\\fortran\\cmngem\\
  MAIN_DIR=..\\..\\genie-main\\
else
  CMNSG=../../genie-main/src/fortran/cmngem/
  MAIN_DIR=../../genie-main/
endif
LOCINCPATH=$(MOD_INC_FLAG)$(CMNSG) $(MOD_INC_FLAG)$(MAIN_DIR)

LIBNAME=libnc1
SRCS=close_file_nc.f90 find_dim_nc.f90 get1d_data_nc.f90 \
     get1d_data_point_nc.f90 get2d_data_nc.f90 get2d_data_slice_nc.f90 \
     get1d_slice_comp_nc.f90 get2d_slice_comp_nc.f90 get3d_data_nc.f90 \
     get3d_data_slice_nc.f90 get3d_slice_comp_nc.f90 get4d_data_nc.f90 \
     length_of_char_nc.f90 open_file_nc.f90 read1d_comp_nc.f90 read2d_nc.f90 \
     read3d_slice_nc.f90 writenc6.f90 extras.f90
OBJECTS=$(SRCS:.f90=.$(OBJ_EXT))
OTHER_FILES=makefile ../../genie-main/makefile.arc

$(LIBNAME).$(LIB_EXT): $(OBJECTS) $(OTHER_FILES) makefile.depend
	$(LIB_CMD) $(LIBNAME).$(LIB_EXT) $(OBJECTS)

.PHONY: clean tags
clean :
	\rm -f *.$(OBJ_EXT) *.$(LIB_EXT) *.mod makefile.depend

tags:
	@ $(SHELL) -ec 'which $(ETAGS) > /dev/null; \
		$(ETAGS) -a -o $(TAGFILE) *.f'

$(OBJECTS): %.$(OBJ_EXT): %.f90 $(OTHER_FILES)
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) $(LOCINCPATH) $(F90FLAGS) \
	$(FPPFLAGS) $< $(OBJ_FLAG)$@

makefile.depend: $(SRCS)
	$(MAIN_DIR)/tools/bin/makedepf90 $(SRCS) >> makefile.depend

ifneq ($(MAKECMDGOALS),clean)
include makefile.depend
endif
