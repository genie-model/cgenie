
#
# top-level makefile for the GENIE build
#
# Note the inclusion of several other makefiles
# inluding testing.mak and makefile.arc, which
# in turn includes user.mak
#
export

include makefile.arc

C_DIR             = src/c
CONFIG_DIR        = configs
F_DIR             = src/fortran

LOCALFLAGS = $(FFLAGS) $(BOUNDS_FLAGS) $(FLAGR8)

OTHER_FILES = makefile makefile.arc testing.mak user.mak

EXE_MAIN = genie.exe
EXECUTABLES = $(EXE_MAIN) nccompare

SRC_MAIN=genie.f90
SRCS4=initialise_genie.f90
SRCS5=end_genie.f90
SRCS6=write_netcdf_genie.f90
SRCS8=genie_ini_wrappers.f90 genie_loop_wrappers.f90 genie_end_wrappers.f90
CONTROL_SRCS=genie_control.f90 genie_global.f90 local_netcdf.f90 \
             genie_util.f90 local_output.f90

SOURCES = $(CONTROL_SRCS) $(SRCS6) $(SRCS8) $(SRCS4) $(SRCS5)
SOURCES_MAIN = $(SOURCES) $(SRC_MAIN)
SOURCES_ALL = $(SOURCES) $(SRC_MAIN)

OBJ_MAIN=$(SRC_MAIN:.f90=.$(OBJ_EXT))
OBJS4=$(SRCS4:.f90=.$(OBJ_EXT))
OBJS5=$(SRCS5:.f90=.$(OBJ_EXT))
OBJS6=$(SRCS6:.f90=.$(OBJ_EXT))
OBJS8=$(SRCS8:.f90=.$(OBJ_EXT))
CONTROL_OBJS=$(CONTROL_SRCS:.f90=.$(OBJ_EXT))

OBJECTS = $(CONTROL_OBJS) $(OBJS6) $(OBJS8) $(OBJS4) $(OBJS5)
OBJECTS_MAIN = $(OBJECTS) $(OBJ_MAIN)
OBJECTS_ALL = $(OBJECTS) $(OBJ_MAIN)

LOCAL_NAMES = gem

MODULE_NAMES = goldstein \
               embm \
               goldsteinseaice \
               ents \
               atchem \
               biogem \
               sedgem \
               rokgem \
               gemlite

UTIL_NAMES = nc1 \
             util1

# Construct variables/lists used for linking and dependencies
# construct vars for the 'external' modules
MODULE_LIBS_PROTO=$(MODULE_NAMES:%=$(GENIE_ROOT)/genie-%/lib/$(LIB_PREFIX))
MODULE_LIBS = $(addsuffix .$(LIB_EXT),$(join $(MODULE_LIBS_PROTO),$(MODULE_NAMES)))
MODULE_DATA=$(MODULE_NAMES:%=$(GENIE_ROOT)/genie-%/)
ifeq ($(MACHINE),WIN32)
	LINK_MODULE_LIBRARIES = $(MODULE_NAMES:%=$(LIB_SEARCH_FLAG)$(GENIE_ROOT_WIN)\\genie-%\\lib)
	LINK_MODULE_LIBRARIES += $(MODULE_NAMES:%=$(LIB_FLAG)lib%.$(LIB_EXT))
else
	LINK_MODULE_LIBRARIES = $(MODULE_NAMES:%=$(LIB_SEARCH_FLAG)$(GENIE_ROOT)/genie-%/lib)
	LINK_MODULE_LIBRARIES += $(MODULE_NAMES:%=$(LIB_FLAG)%)
endif

# the 'local' modules
LOCAL_LIBS_PROTO=$(LOCAL_NAMES:%=$(MAIN_DIR)/src/fortran/cmn%/$(LIB_PREFIX)cmn)
LOCAL_LIBS = $(addsuffix .$(LIB_EXT),$(join $(LOCAL_LIBS_PROTO),$(LOCAL_NAMES)))
ifeq ($(MACHINE),WIN32)
	LINK_MODULE_LIBRARIES += $(LOCAL_NAMES:%=$(LIB_SEARCH_FLAG)$(GENIE_ROOT_WIN)\\genie-main\\src\\fortran\\cmn%)
	LINK_MODULE_LIBRARIES += $(LOCAL_NAMES:%=$(LIB_FLAG)libcmn%.$(LIB_EXT))
else
	LINK_MODULE_LIBRARIES += $(LOCAL_NAMES:%=$(LIB_SEARCH_FLAG)$(MAIN_DIR)/src/fortran//cmn%)
	LINK_MODULE_LIBRARIES += $(LOCAL_NAMES:%=$(LIB_FLAG)cmn%)
endif

# f90 modules included
MODULE_INCLUDES=$(MODULE_NAMES:%=$(MOD_INC_FLAG)$(GENIE_ROOT)/genie-%/src/fortran)
INCLUDE_MODULE_LIBRARIES=$(MODULE_INCLUDES) $(MOD_INC_FLAG)$(INC_DIR)

UTIL_LIBS_PROTO=$(UTIL_NAMES:%=$(LIB_DIR)/lib%/$(LIB_PREFIX))
UTIL_LIBS = $(addsuffix .$(LIB_EXT),$(join $(UTIL_LIBS_PROTO),$(UTIL_NAMES)))
ifeq ($(MACHINE),WIN32)
	LINK_UTIL_LIBRARIES = $(UTIL_NAMES:%=$(LIB_SEARCH_FLAG)$(LIB_DIR_WIN)\\lib%)
	LINK_UTIL_LIBRARIES += $(UTIL_NAMES:%=$(LIB_FLAG)lib%.$(LIB_EXT))
else
	LINK_UTIL_LIBRARIES = $(UTIL_NAMES:%=$(LIB_SEARCH_FLAG)$(LIB_DIR)/lib%)
	LINK_UTIL_LIBRARIES += $(UTIL_NAMES:%=$(LIB_FLAG)%)
endif

# version of the model recorded in the code at build time
#REV:=$(shell svn info | grep 'Last Changed Rev:' | cut -d' ' -f 4,4 | sed 's/^/REV=/')
REV:=REV=9999

# this is very odd--I don't understand why this is necessary.
all: include $(TOOLS) $(EXECUTABLES)

data: $(MODULE_DATA)

.PHONY : cleanall clean cleanutils cleanmodules $(DIRS) \
         $(CLEAN_UTIL_TARGETS) $(CLEAN_MODULE_TARGETS) \
	 tags tagutils tagmodules $(CLEAN_LOCAL_TARGETS) \
         $(TAG_UTIL_TARGETS) $(TAG_MODULE_TARGETS) \
	 FORCE

# ================== BUILD DIRECTORIES ===================
.PHONY: include
include:
	-mkdir include

# ================= CLEANING RULES ===================
# construct target names for cleaning rules
CLEAN_LOCAL_TARGETS = $(LOCAL_NAMES:%=clean_%)
CLEAN_MODULE_TARGETS = $(MODULE_NAMES:%=clean_%)
CLEAN_UTIL_TARGETS = $(UTIL_NAMES:%=clean_%)

# cleanall - cleans locally, in modules and util libs
# Note that an individual module or util can be cleaned
# with an appropriate target, e.g. 'make clean_igcm3'
cleanall : clean cleanutils cleanmodules
	\rm -f include/*.mod makefile.depend

# clean  - just local files
clean : $(CLEAN_LOCAL_TARGETS)
	\rm -f *.$(OBJ_EXT) $(EXECUTABLES) *.dep *.mod
	cd $(C_DIR); $(MAKE) clean
	cd $(F_DIR); $(MAKE) -I ../../ clean

$(CLEAN_LOCAL_TARGETS) : clean_% :
	\cd $(F_DIR)/cmn$*; $(MAKE) clean

# cleanutils - just util libs
cleanutils : $(CLEAN_UTIL_TARGETS)
$(CLEAN_UTIL_TARGETS) : clean_% :
	\cd $(LIB_DIR)/$(LIB_PREFIX)$*; $(MAKE) clean

# cleanmodules - just modules
cleanmodules : $(CLEAN_MODULE_TARGETS)
$(CLEAN_MODULE_TARGETS) : clean_% :
	\cd $(GENIE_ROOT)/genie-$*; $(MAKE) clean

# =========== OBJECT FILES AND EXECUTABLE ============

# CONTROL_OBJS is made first, so that components can use
# e.g. the grid size information
$(EXE_MAIN) : $(CONTROL_OBJS) $(UTIL_LIBS) $(LOCAL_LIBS) $(MODULE_LIBS) $(OBJECTS_MAIN) $(OTHER_FILES)
	$(F77_LD) $(LDFLAGS) $(OBJECTS_MAIN) $(OUT_FLAG)$(EXE_MAIN) \
        $(LINK_MODULE_LIBRARIES) $(LINK_UTIL_LIBRARIES) \
	$(NETCDF) $(FORTRANLIBS) $(XLIB) $(BOUNDS_FLAGS)

$(EXE_COSTFN) : $(EXE) $(CONTROL_OBJS) $(UTIL_LIBS) $(LOCAL_LIBS) $(MODULE_LIBS) $(OBJECTS_COSTFN) $(OTHER_FILES)
	$(F77_LD) $(LDFLAGS) $(OBJECTS_COSTFN) $(OUT_FLAG)$(EXE_COSTFN) \
        $(LINK_MODULE_LIBRARIES) $(LINK_UTIL_LIBRARIES) \
	$(NETCDF) $(GLIMMER) $(FORTRANLIBS) $(XLIB) $(BOUNDS_FLAGS)

# Rule to make custom netcdf comparison program
nccompare :
	\cd $(C_DIR); $(MAKE)

# Rule to make custom netcdf comparison program
errfn_ea_go_gs :
	\cd $(F_DIR); $(MAKE) -I ../..

# rule to set up data directories of individual modules
$(MODULE_DATA): FORCE
	\cd $(@D); if [ -d data ]; then $(MAKE) data; fi

# rule to build 'local' libs
$(LOCAL_LIBS) : FORCE
	\cd $(@D); $(MAKE) $(@F)
	\cp $(@D)/*.mod $(INC_DIR)

# rule to build module libraries
$(MODULE_LIBS) : FORCE
	\cd $(@D)/..; $(MAKE) $(@F)
	if [ -d $(@D)/../mod ] ; then \
		\cp $(@D)/../mod/*.mod $(INC_DIR); \
	fi

# rule to build util libraries
$(UTIL_LIBS) : FORCE
	\cd $(@D); $(MAKE) $(@F)

# a forcing rule
FORCE:

# pattern rules to make object files
# one rule for compilation permutation
$(OBJ_MAIN): %.$(OBJ_EXT): %.f90 $(OTHER_FILES)
	$(F77) $(FPPFLAGS) $(COMPILEONLY) $(LOCALFLAGS) $(F90FLAGS) \
	$(INCLUDE_MODULE_LIBRARIES) $(DOPTS) $(NETCDF_INC) \
	$< $(OBJ_FLAG)$@

$(OBJS4): %.$(OBJ_EXT): %.f90 $(OTHER_FILES)
	$(F77) $(FPPFLAGS) $(COMPILEONLY) $(LOCALFLAGS) $(F90FLAGS) \
	$(LIBUTIL1PRECISIONOPTS) $(INCLUDE_MODULE_LIBRARIES) \
	$< $(OBJ_FLAG)$@

$(OBJS5): %.$(OBJ_EXT): %.f90 $(OTHER_FILES)
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) $(F90FLAGS) $< $(OBJ_FLAG)$@

$(OBJS6): %.$(OBJ_EXT): %.f90 $(OTHER_FILES)
	$(F77) $(FPPFLAGS) $(COMPILEONLY) $(LOCALFLAGS) $(F90FLAGS) \
	$(LIBUTIL1PRECISIONOPTS) $(INCLUDE_MODULE_LIBRARIES) \
	$(IGCMHRLATSOPTS) $< $(OBJ_FLAG)$@

$(OBJS8): %.$(OBJ_EXT): %.f90 $(OTHER_FILES)
	$(F77) $(FPPFLAGS) $(COMPILEONLY) $(LOCALFLAGS) $(F90FLAGS) \
	$(INCLUDE_MODULE_LIBRARIES) \
	$< $(OBJ_FLAG)$@

$(CONTROL_OBJS): %.$(OBJ_EXT): %.f90 $(OTHER_FILES)
	$(F77) $(FPPFLAGS) $(COMPILEONLY) $(LOCALFLAGS) $(F90FLAGS) \
	$(DEFINE)$(REV) $< $(OBJ_FLAG)$@

# ================== BUILD TOOLS ===================
TOOLDIR=$(MAIN_DIR)/tools/bin/
TOOLNAMES=makedepf90
TOOLS=$(addprefix $(MAIN_DIR)/tools/bin/, $(TOOLNAMES))

$(MAIN_DIR)/tools/bin/makedepf90: $(TOOLDIR)
	$(MAKE) -C $(MAIN_DIR)/tools/makedepf90-2.8.8 install

$(TOOLDIR):
	mkdir -p $(TOOLDIR)

# ================== TESTING RULES ===================
include testing.mak

# ==== RULES TO AUTO-CREATE DEPENDENCY FILES ====
makefile.depend: $(SOURCES) $(TOOLS)
	$(MAIN_DIR)/tools/bin/makedepf90 $(SOURCES) >> makefile.depend

ifneq ($(MAKECMDGOALS),clean)
include makefile.depend
endif
