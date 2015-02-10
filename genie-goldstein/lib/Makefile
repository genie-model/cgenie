#
# $Id: Makefile 3274 2007-04-30 14:51:29Z cvs-gw $
#

FOR_DIR = ../src/fortran

.PHONY : all clean

all: $(LIBNAME).$(LIB_EXT)

$(LIBNAME).$(LIB_EXT): $(FOR_DIR)/*.$(OBJ_EXT)
	$(LIB_CMD)$@ $^

clean:
	rm -f $(LIBNAME).$(LIB_EXT)
