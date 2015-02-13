#
# $Id: makefile 7278 2012-01-05 00:09:00Z andy-ridgwell $
#

ifeq ($(MACHINE),WIN32)
  LOC_INC_FLAGS=$(MOD_INC_FLAG)$(GENIE_ROOT_WIN)\\genie-main
else
  LOC_INC_FLAGS=$(MOD_INC_FLAG)$(MAIN_DIR)
endif
LOCALFLAGS=$(FPPFLAGS) $(FFLAGS) $(F77FLAGS) $(FLAGS) $(LOC_INC_FLAGS) $(FLAGR8)

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

SRCS=diag3.f diaga.f diagend_embm.F diagosc_embm.F embm.F end_embm.F \
     initialise_embm.F inm_embm.f inm_netcdf_embm.F netcdf_embm.F \
     outm_embm.f outm_netcdf_embm.F outm_surf.f ocean_alb.F radfor.F \
     readroff.f surflux.F tstepa.f tstipa.f err_embm.F rmsnorm_embm_T.F \
     rmsnorm_embm_q.F field_interp.F diag_embm_T_annual_av.F \
     diag_embm_q_annual_av.F diag_embm_r_annual_av.F
OBJS1=$(SRCS:.F=.$(OBJ_EXT))
OBJS=$(OBJS1:.f=.$(OBJ_EXT))

#FLAGS = -r8 -O
FLAGS = $(FLAGR8) $(GOLDOPTIM)
DOPTS = $(DEFINE)dimpa $(DEFINE)ebatmos
#DOPTS = $(DEFINE)dimpa $(DEFINE)ebatmos
#DOPTS = $(DEFINE)dimpa $(DEFINE)ebatmos $(DEFINE)clock
LIBNAME = libembm
LOCAL_BIN_DIR = ../../bin

CMN = embm.cmn netcdf_embm.cmn netcdf_grid.cmn makefile ../../../genie-main/makefile.arc

all : $(OBJS) $(CMN) makefile.depend

.PHONY: all clean tags
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
