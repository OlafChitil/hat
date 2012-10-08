include Makefile.inc

OBJDIR   = $(BUILDDIR)/$(OBJ)/lib/$(HC)
TARGDIR  = $(LIBDIR)/$(MACHINE)/lib/$(HC)
TARGET   = $(TARGDIR)/libHShat.a
PRECFLAGS = -DVERSION="\"$(HATVERSION)\"" \
            -DFILEVERSION="\"$(HATFILEVERSION)\"" \
            -I$(INCDIR)
HATDIRS  = Hat
OBJDIRS  = $(patsubst %, ${OBJDIR}/%, ${HATDIRS})
TARGDIRS = $(patsubst %, ${TARGDIR}/%, ${HATDIRS})
ifeq "$(findstring ghc, $(HC))" "ghc"
# must give package name whilst building, plus we need the FFI
  ifeq "504" "${GHCSYM}"
HFLAGS = -package-name hat -fglasgow-exts \
		$(shell ${LOCAL}fixghc ${GHCSYM} -package lang -package base) \
		-fno-warn-overlapping-patterns -fno-warn-missing-methods \
		-fwarn-duplicate-exports \
		-i$(OBJDIR) -I. -I$(INCDIR) $(shell echo ${BUILDOPTS}) \
		'-\#include "hat-c.h"'
PKGCONF = hat-package.conf
  else
    ifeq "602" "${GHCSYM}"
HFLAGS = -package-name hat -fglasgow-exts \
		-package base -package parsec \
		-fno-warn-overlapping-patterns -fno-warn-missing-methods \
		-fno-warn-duplicate-exports -fallow-undecidable-instances \
		-i$(OBJDIR) -I. -I$(INCDIR) $(shell echo ${BUILDOPTS}) \
		'-\#include "hat-c.h"'
PKGCONF = hat-package.conf
    else
      ifeq "604" "${GHCSYM}"
HFLAGS = -package-name hat -fglasgow-exts \
		-package base -package parsec -package mtl \
		-fno-warn-overlapping-patterns -fno-warn-missing-methods \
		-fno-warn-duplicate-exports -fallow-undecidable-instances \
		-i$(OBJDIR) -I. -I$(INCDIR) $(shell echo ${BUILDOPTS}) \
		'-\#include "hat-c.h"'
      else	# assume 606 or greater
HFLAGS = -package-name hat-$(HATVERSION) -fglasgow-exts \
		-package base -package parsec -package mtl \
		-fno-warn-overlapping-patterns -fno-warn-missing-methods \
		-fno-warn-duplicate-exports -fallow-undecidable-instances \
		-i$(OBJDIR) -I. -I$(INCDIR) $(shell echo ${BUILDOPTS}) \
		'-\#include "hat-c.h"'
      endif
PKGCONF = Hat.pkgconf
    endif
  endif
CFLAGS = -O -D__GLASGOW_HASKELL__ $(PRECFLAGS)
CPPFLAGS = -D__GLASGOW_HASKELL__=$(GHCSYM)
endif
ifeq "$(findstring nhc98, $(HC))" "nhc98"
HFLAGS = -I$(OBJDIR) -I. -I$(INCDIR) -package base +RTS -K12M -H64M -RTS \
		$(shell echo ${BUILDOPTS})
CFLAGS = -O -D__NHC__ $(PRECFLAGS)
CPPFLAGS = -D__NHC__=$(shell cat ${BUILDDIR}/nhcsym)
GHCONLYSRCS =
PKGCONF = hat.cabal
# for time profiling of traced programs:
#HFLAGS = -z +RTS -K16M -H64M -RTS
endif
export HFLAGS

# C sources are just compiled normally
CSRCS = hat-c.c
COBJS = $(patsubst %.c, $(OBJDIR)/%.o, $(CSRCS))

# plain Haskell sources are also compiled normally
PLAINSRCS = \
	Hat/Hat.hs Hat/Hack.hs Hat/PreludeBuiltinTypes.hs Hat/IOBuiltin.hs \
	Hat/SystemBuiltin.hs Hat/TimeBuiltin.hs Hat/RandomBuiltin.hs \
	Hat/DirectoryBuiltin.hs
PLAINOBJS = $(patsubst %.hs, $(OBJDIR)/%.o, $(PLAINSRCS))

# these Haskell sources undergo hat-trans before compilation
TRANSSRCS = \
	PreludeBuiltin.hs PreludeBasic.hs IOBuiltinTypes.hs \
	SystemBuiltinTypes.hs TimeBuiltinTypes.hs DirectoryBuiltinTypes.hs \
	Char.hs Ratio.hs List.hs Complex.hs Numeric.hs \
	Maybe.hs Monad.hs Ix.hs Array.hs IO.hs System.hs \
	Locale.hs CPUTime.hs Time.hs Random.hs Directory.hs
TRANSHATS = $(patsubst %, Hat/%,                $(TRANSSRCS))
TRANSOBJS = $(patsubst %.hs, $(OBJDIR)/Hat/%.o, $(TRANSSRCS))

# these Haskell sources are just like TRANSSRCS except we need to
# fiddle with the .hx file afterwards
FIDDLESRCS = Prelude.hs
FIDDLEHATS = $(patsubst %, Hat/%,                $(FIDDLESRCS))
FIDDLEOBJS = $(patsubst %.hs, $(OBJDIR)/Hat/%.o, $(FIDDLESRCS))


all: $(OBJDIR) $(OBJDIRS) $(TARGDIR) $(TARGDIRS) $(TARGET) $(PKGCONF)
	mkdir -p $(INCDIR)/hat 2>/dev/null || $(TRUE)
	#for dir in $(HIERDIRS); \
	#	do mkdir -p $(INCDIR)/hat/$$dir 2>/dev/null || $(TRUE); \
	#	done
	$(INSTALL) *.hx $(INCDIR)/hat
	#for dir in $(HIERDIRS); \
	#	do $(INSTALL) $$dir/*.hx $(INCDIR)/hat/$$dir; \
	#	done
	$(INSTALL) $(PKGCONF) $(TARGDIR)/hat-package.conf
	for dir in $(HATDIRS); \
		do $(INSTALL) $(OBJDIR)/$$dir/*.hi $(TARGDIR)/$$dir; \
		done
clean:
	-rm -rf $(OBJDIR) $(TRANSHATS) $(FIDDLEHATS) Hat/*.hi Hat/Hat.hx \
		$(patsubst %.hs, %.hx, $(TRANSSRCS))
$(OBJDIR) $(OBJDIRS) $(TARGDIR) $(TARGDIRS):
	mkdir -p $@ || $(TRUE)

$(PLAINOBJS): $(OBJDIR)/%.o: %.hs
	$(HC) $(HFLAGS) -cpp -c -o $@ $<
	mv $(patsubst %.hs, %.hi, $<) $(patsubst %.hs, ${OBJDIR}/%.hi, $<)

$(TRANSOBJS): $(OBJDIR)/Hat/%.o: %.hs
	$(LOCAL)hat-trans -P. -I. -trusted -prelude $(CPPFLAGS) $<
	$(HC) $(HFLAGS) -c -o $@ Hat/$<
	mv $(patsubst %.hs, Hat/%.hi, $<) $(patsubst %.hs, ${OBJDIR}/Hat/%.hi, $<)

$(FIDDLEOBJS): $(OBJDIR)/Hat/%.o: %.hs
	$(LOCAL)hat-trans -cpp -P. -I. -trusted -prelude $(CPPFLAGS) $<
	$(HC) $(HFLAGS) -c -o $@ Hat/$<
	mv $(patsubst %.hs, Hat/%.hi, $<) $(patsubst %.hs, ${OBJDIR}/Hat/%.hi, $<)
	$(LOCAL)hat-trans -cpp -DHX -P. -I. -trusted -prelude $(CPPFLAGS) $<

$(COBJS): $(OBJDIR)/%.o: %.c
	$(CC) $(CFLAGS) $(shell echo $(BUILDOPTS)) -c -o $@ $<

# single archive for all object files
$(TARGET): $(COBJS) $(PLAINOBJS) $(FIDDLEOBJS) $(TRANSOBJS)
	$(AR) cr $(TARGET) $(COBJS) $(PLAINOBJS) $(FIDDLEOBJS) $(TRANSOBJS)
	ranlib $(TARGET)

# the new (ghc-6.4) package story
Hat.pkgconf: hat.cabal
	cp hat.cabal $@
	echo "depends:		haskell98" >>$@
	echo "hs-libraries:	HShat" >>$@

# Here are the extra dependencies.
$(OBJDIR)/Hat/PreludeBuiltinTypes.o: $(OBJDIR)/Hat/Hat.o 
$(OBJDIR)/Hat/PreludeBuiltin.o: $(OBJDIR)/Hat/PreludeBuiltinTypes.o
$(OBJDIR)/Hat/PreludeBasic.o: $(OBJDIR)/Hat/PreludeBuiltin.o
$(OBJDIR)/Hat/Prelude.o: $(OBJDIR)/Hat/PreludeBasic.o
$(OBJDIR)/Hat/Char.o: $(OBJDIR)/Hat/PreludeBasic.o
$(OBJDIR)/Hat/IOBuiltin.o: $(OBJDIR)/Hat/Hat.o $(OBJDIR)/Hat/IOBuiltinTypes.o \
	$(OBJDIR)/Hat/PreludeBuiltinTypes.o $(OBJDIR)/Hat/Prelude.o
$(OBJDIR)/Hat/IOBuiltinTypes.o: $(OBJDIR)/Hat/Ix.o $(OBJDIR)/Hat/Prelude.o
$(OBJDIR)/Hat/Array.o: $(OBJDIR)/Hat/Ix.o $(OBJDIR)/Hat/List.o \
	$(OBJDIR)/Hat/Prelude.o
$(OBJDIR)/Hat/Complex.o: $(OBJDIR)/Hat/Prelude.o
$(OBJDIR)/Hat/IO.o: $(OBJDIR)/Hat/Ix.o $(OBJDIR)/Hat/PreludeBuiltinTypes.o \
	$(OBJDIR)/Hat/IOBuiltinTypes.o $(OBJDIR)/Hat/IOBuiltin.o
$(OBJDIR)/Hat/Ix.o: $(OBJDIR)/Hat/Prelude.o
$(OBJDIR)/Hat/List.o: $(OBJDIR)/Hat/Prelude.o $(OBJDIR)/Hat/Maybe.o
$(OBJDIR)/Hat/Maybe.o: $(OBJDIR)/Hat/Prelude.o
$(OBJDIR)/Hat/Monad.o: $(OBJDIR)/Hat/Prelude.o
$(OBJDIR)/Hat/Numeric.o: $(OBJDIR)/Hat/PreludeBasic.o
$(OBJDIR)/Hat/Ratio.o: $(OBJDIR)/Hat/PreludeBasic.o
$(OBJDIR)/Hat/SystemBuiltinTypes.o: $(OBJDIR)/Hat/Prelude.o
$(OBJDIR)/Hat/SystemBuiltin.o: $(OBJDIR)/Hat/SystemBuiltinTypes.o
$(OBJDIR)/Hat/System.o: $(OBJDIR)/Hat/SystemBuiltin.o
$(OBJDIR)/Hat/TimeBuiltinTypes.o: $(OBJDIR)/Hat/Prelude.o
$(OBJDIR)/Hat/TimeBuiltin.o: $(OBJDIR)/Hat/TimeBuiltinTypes.o \
	$(OBJDIR)/Hat/Locale.o
$(OBJDIR)/Hat/Time.o: $(OBJDIR)/Hat/TimeBuiltin.o $(OBJDIR)/Hat/Locale.o
$(OBJDIR)/Hat/RandomBuiltin.o: $(OBJDIR)/Hat/Prelude.o
$(OBJDIR)/Hat/Random.o: $(OBJDIR)/Hat/RandomBuiltin.o
$(OBJDIR)/Hat/DirectoryBuiltinTypes.o: $(OBJDIR)/Hat/Prelude.o
$(OBJDIR)/Hat/DirectoryBuiltin.o: $(OBJDIR)/Hat/DirectoryBuiltinTypes.o
$(OBJDIR)/Hat/Directory.o: $(OBJDIR)/Hat/DirectoryBuiltin.o

# C-files dependencies.
$(OBJDIR)/hat-c.o: hat-c.c hat-c.h $(INCDIR)/art.h

