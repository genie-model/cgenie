#
# $Id: makefile 7478 2012-06-20 20:37:32Z andy-ridgwell $
#
# AY (23/06/04) : surf_ocn_sic.F added to subroutine list

.SUFFIXES: .$(OBJ_EXT) .f

ifeq ($(MACHINE),WIN32)
  LOC_INC_FLAGS=$(MOD_INC_FLAG)$(GENIE_ROOT_WIN)\\genie-main
else
  LOC_INC_FLAGS=$(MOD_INC_FLAG)$(MAIN_DIR)
endif
LOCALFLAGS=$(FPPFLAGS) $(FFLAGS) $(F77FLAGS) $(FLAGS) $(LOC_INC_FLAGS) $(FLAGR8)


.F.$(OBJ_EXT):
	$(F77) $(LOCALFLAGS) $(DOPTS) \
	$(COMPILEONLY) $*.F

.f.$(OBJ_EXT):
	$(F77) $(LOCALFLAGS) \
	$(COMPILEONLY) $*.f

.f:
	$(F77) $(LOCALFLAGS) \
	$(OBJ_FLAG) $<

SRCS=co.F diag.f diag2.f diagend.F diagopsi.f diagosc.F drgset.f ediff.F \
     end_goldstein.F eos.f gold_ocnsic_avg.F goldstein.F \
     initialise_goldstein.F inm.f inm_netcdf.F invert.f island.f \
     jbar.f krausturner.F matinv_gold.f netcdf.F outm.f outm_netcdf.F \
     outm_surf_ocn_sic.f surf_ocn_sic.F tstepo.F tstipo.F tstepo_flux.F \
     ubarsolv.f velc.f wind.f goldstein_delayfluxes.F get_hosing.F err_gold.F \
     rmsnorm_goldstein_S.F rmsnorm_goldstein_T.F diag_goldstein_T_annual_av.F \
     diag_goldstein_Tsurf_annual_av.F diag_goldstein_S_annual_av.F \
     diag_goldstein_Ssurf_annual_av.F diag_goldstein_circulation_annual_av.F
OBJS1=$(SRCS:.F=.$(OBJ_EXT))
OBJS=$(OBJS1:.f=.$(OBJ_EXT))
FLAGS = $(FLAGR8) $(GOLDOPTIM) $(BOUNDS_FLAGS)
DOPTS = $(DEFINE)ebatmos
#DOPTS = $(DEFINE)disles $(DEFINE)diso $(DEFINE)ebatmos
#DOPTS = $(DEFINE)diso $(DEFINE)ebatmos $(DEFINE)clock

CMN = ocean.cmn netcdf.cmn netcdf_grid.cmn makefile ../../../genie-main/makefile.arc

all : $(OBJS) $(CMN) makefile.depend

.PHONY: clean tags all
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
