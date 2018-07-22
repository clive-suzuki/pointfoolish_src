#FC = gfortran
FC = ifort

TARGET = ${HOME}/bin/pntfool
OBJECTS = mod_szk.o mod_globals.o mod_pointfoolish.o main.o
MOD_FILES = mod_szk.mod mod_globals.mod mod_pointfoolish.mod

FFLAGS =
LDFLAGS =


ifeq (${FC},gfortran)
	FFLAGS += -fimplicit-none
	FFLAGS += -fopenmp
endif

ifeq (${FC},ifort)
	FFLAGS += -parallel -qopenmp
#	FFLAGS += -mcmodel=large -shared-intel
#	FFLAGS += -ipo -inline-level=2 -inline-forceinline
	FFLAGS += -fpp
endif

.SUFFIXES : .o .f90
.f90.o:
	${FC} -c -fpp $<
${TARGET} : ${OBJECTS}
	${FC} -o $@ ${OBJECTS} ${LDFLAGS} ${FFLAGS}

.PHONY: clean
clean:
	${RM} ${TARGET} ${OBJECTS} ${MOD_FILES}
