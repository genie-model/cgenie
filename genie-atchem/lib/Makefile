#
# $Id: Makefile 4326 2008-06-30 09:11:55Z gethin-williams $
#

SRCDIR = ../src/fortran

.PHONY : all clean

all: $(LIBNAME).$(LIB_EXT)

$(LIBNAME).$(LIB_EXT): $(SRCDIR)/*.$(OBJ_EXT)
	$(LIB_CMD)$@ $^
ifeq ($(MACHINE),OSX)
	$(RANLIB_CMD) $@
endif	

clean:
	rm -f $(LIBNAME).$(LIB_EXT)
