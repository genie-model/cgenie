
ifeq ($(MACHINE),WIN32)
  LOC_INC_FLAGS=$(MOD_INC_FLAG)$(GENIE_ROOT_WIN)\\genie-main
else
  LOC_INC_FLAGS=$(MOD_INC_FLAG)$(MAIN_DIR)
endif
LOCALFLAGS= $(FPPFLAGS) $(FFLAGS) $(F77FLAGS) $(FLAGS) $(LOC_INC_FLAGS) $(FLAGR8)

.SUFFIXES: .$(OBJ_EXT) .f

.F.$(OBJ_EXT):
	$(F77) $(LOCALFLAGS) $(DOPTS) \
	$(COMPILEONLY) $*.F

.f.$(OBJ_EXT):
	$(F77) $(LOCALFLAGS) \
	$(COMPILEONLY) $*.f

.f:
	$(F77) $(LOCALFLAGS) \
	$(OBJ_FLAG) $<

SRCS=diagend_seaice.f diagosc_sic.F diagsic.f diag4.f end_seaice.F \
     gold_seaice.F initialise_seaice.F inm_netcdf_sic.F inm_seaice.f \
     netcdf_sic.F outm_netcdf_sic.F outm_seaice.f tstepsic.f tstipsic.f \
     diag_goldsteinseaice_ice_annual_av.F
OBJS1=$(SRCS:.F=.$(OBJ_EXT))
OBJS=$(OBJS1:.f=.$(OBJ_EXT))

#FLAGS = -r8 -O -static
FLAGS = $(FLAGR8) $(GOLDOPTIM)
DOPTS = $(DEFINE)ebatmos

CMN = seaice.cmn netcdf_sic.cmn netcdf_grid.cmn makefile ../../../genie-main/makefile.arc

all : $(OBJS) $(CMN) makefile.depend

.PHONY : all clean tags
clean:
	\rm -f *.$(OBJ_EXT) makefile.depend

tags:
	@ $(SHELL) -ec 'which $(ETAGS) > /dev/null; \
		$(ETAGS) -a -o $(TAGFILE) *.F; \
		$(ETAGS) -a -o $(TAGFILE) *.f'

makefile.depend: $(SRCS)
	$(MAIN_DIR)/tools/bin/makedepf90 $(SRCS) >> makefile.depend

ifneq ($(MAKECMDGOALS),clean)
include makefile.depend
endif
