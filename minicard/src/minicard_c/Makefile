###################################################################################################

.PHONY:	r sh lr lsh config all install install-headers install-lib\
        install-bin clean distclean
all:	r lr lsh

## Load Previous Configuration ####################################################################

-include config.mk

## Configurable options ###########################################################################

# Directory to store object files, libraries, executables, and dependencies:
BUILD_DIR      ?= build

# Include debug-symbols in release builds
MINISAT_RELSYM ?= -g

# Sets of compile flags for different build types
MINISAT_REL    ?= -O3 -D NDEBUG
MINISAT_FPIC   ?= -fPIC

# GNU Standard Install Prefix
prefix         ?= /usr/local

## Write Configuration  ###########################################################################

config:
	@( echo 'BUILD_DIR?=$(BUILD_DIR)'           ; \
	   echo 'MINISAT_RELSYM?=$(MINISAT_RELSYM)' ; \
	   echo 'MINISAT_REL?=$(MINISAT_REL)'       ; \
	   echo 'MINISAT_FPIC?=$(MINISAT_FPIC)'     ; \
	   echo 'prefix?=$(prefix)'                 ) > config.mk

## Configurable options end #######################################################################

INSTALL ?= install

# GNU Standard Install Variables
exec_prefix ?= $(prefix)
includedir  ?= $(prefix)/include
bindir      ?= $(exec_prefix)/bin
libdir      ?= $(exec_prefix)/lib
datarootdir ?= $(prefix)/share
mandir      ?= $(datarootdir)/man

# Target file names
MINISAT      = minicard#       Name of MiniCARD main executable.
MINISAT_SLIB = lib$(MINISAT).a#  Name of MiniCARD static library.
MINISAT_DLIB = lib$(MINISAT).so# Name of MiniCARD shared library.

# Shared Library Version
SOMAJOR=1
SOMINOR=2
SORELEASE?=.0#   Declare empty to leave out from library file name.

MINISAT_CXXFLAGS = -fPIC -I. -D __STDC_LIMIT_MACROS -D __STDC_FORMAT_MACROS -Wall -Wextra
MINISAT_LDFLAGS  = -Wall -lz

ifeq (Darwin,$(shell uname))
   SHARED_LDFLAGS += --verbose -shared -Wl,-install_name,$(MINISAT_DLIB).$(SOMAJOR)
   RELEASE_LDFLAGS +=
else
   SHARED_LDFLAGS += -shared -Wl,-soname,$(MINISAT_DLIB).$(SOMAJOR)
   RELEASE_LDFLAGS += -static
endif

ECHO=@echo
ifeq ($(VERB),)
VERB=@
else
VERB=
endif

SRCS = $(wildcard minicard/*.cc) $(wildcard utils/*.cc)
HDRS = $(wildcard minicard/*.h) $(wildcard mtl/*.h) $(wildcard utils/*.h)
OBJS = $(filter-out %Main.o, $(SRCS:.cc=.o))

r:	$(BUILD_DIR)/release/bin/$(MINISAT)
sh:	$(BUILD_DIR)/dynamic/bin/$(MINISAT)

lr:	$(BUILD_DIR)/release/lib/$(MINISAT_SLIB)
lsh:	$(BUILD_DIR)/dynamic/lib/$(MINISAT_DLIB).$(SOMAJOR).$(SOMINOR)$(SORELEASE)

## Build-type Compile-flags:
$(BUILD_DIR)/release/%.o:			MINISAT_CXXFLAGS +=$(MINISAT_REL) $(MINISAT_RELSYM)
$(BUILD_DIR)/dynamic/%.o:			MINISAT_CXXFLAGS +=$(MINISAT_REL) $(MINISAT_FPIC)

## Build-type Link-flags:
$(BUILD_DIR)/release/bin/$(MINISAT):		MINISAT_LDFLAGS += $(RELEASE_LDFLAGS) $(MINISAT_RELSYM)

## Executable dependencies
$(BUILD_DIR)/release/bin/$(MINISAT):	 	$(BUILD_DIR)/release/minicard/Main.o $(BUILD_DIR)/release/lib/$(MINISAT_SLIB)
$(BUILD_DIR)/dynamic/bin/$(MINISAT):	 	$(BUILD_DIR)/dynamic/minicard/Main.o $(BUILD_DIR)/dynamic/lib/$(MINISAT_DLIB)

## Library dependencies
$(BUILD_DIR)/release/lib/$(MINISAT_SLIB):	$(foreach o,$(OBJS),$(BUILD_DIR)/release/$(o))
$(BUILD_DIR)/dynamic/lib/$(MINISAT_DLIB).$(SOMAJOR).$(SOMINOR)$(SORELEASE)\
 $(BUILD_DIR)/dynamic/lib/$(MINISAT_DLIB).$(SOMAJOR)\
 $(BUILD_DIR)/dynamic/lib/$(MINISAT_DLIB):	$(foreach o,$(OBJS),$(BUILD_DIR)/dynamic/$(o))

## Compile rules
$(BUILD_DIR)/release/%.o:	%.cc
	$(ECHO) Compiling: $@
	$(VERB) mkdir -p $(dir $@)
	$(VERB) $(CXX) $(MINISAT_CXXFLAGS) $(CXXFLAGS) -c -o $@ $< -MMD -MF $(BUILD_DIR)/release/$*.d

$(BUILD_DIR)/dynamic/%.o:	%.cc
	$(ECHO) Compiling: $@
	$(VERB) mkdir -p $(dir $@)
	$(VERB) $(CXX) $(MINISAT_CXXFLAGS) $(CXXFLAGS) -c -o $@ $< -MMD -MF $(BUILD_DIR)/dynamic/$*.d

## Linking rule
$(BUILD_DIR)/release/bin/$(MINISAT) $(BUILD_DIR)/dynamic/bin/$(MINISAT):
	$(ECHO) Linking Binary: $@
	$(VERB) mkdir -p $(dir $@)
	$(VERB) $(CXX) $^ $(MINISAT_LDFLAGS) $(LDFLAGS) -o $@

## Static Library rule
%/lib/$(MINISAT_SLIB):
	$(ECHO) Linking Static Library: $@
	$(VERB) mkdir -p $(dir $@)
	$(VERB) $(AR) -rcs $@ $^

## Shared Library rule
$(BUILD_DIR)/dynamic/lib/$(MINISAT_DLIB).$(SOMAJOR).$(SOMINOR)$(SORELEASE)\
 $(BUILD_DIR)/dynamic/lib/$(MINISAT_DLIB).$(SOMAJOR)\
 $(BUILD_DIR)/dynamic/lib/$(MINISAT_DLIB):
	$(ECHO) Linking Shared Library: $@
	$(VERB) mkdir -p $(dir $@)
	$(VERB) $(CXX) $(MINISAT_LDFLAGS) $(LDFLAGS) -o $@ $(SHARED_LDFLAGS) $^
	$(VERB) ln -sf $(MINISAT_DLIB).$(SOMAJOR).$(SOMINOR)$(SORELEASE) $(BUILD_DIR)/dynamic/lib/$(MINISAT_DLIB).$(SOMAJOR)
	$(VERB) ln -sf $(MINISAT_DLIB).$(SOMAJOR) $(BUILD_DIR)/dynamic/lib/$(MINISAT_DLIB)

install:	install-headers install-lib install-bin

install-headers:
#       Create directories
	$(INSTALL) -d $(DESTDIR)$(includedir)/minicard
#       Install headers
	for h in $(HDRS) ; do \
	  $(INSTALL) -m 644 $$h $(DESTDIR)$(includedir)/$$h ; \
	done

install-lib: $(BUILD_DIR)/release/lib/$(MINISAT_SLIB) $(BUILD_DIR)/dynamic/lib/$(MINISAT_DLIB).$(SOMAJOR).$(SOMINOR)$(SORELEASE)
	$(INSTALL) -d $(DESTDIR)$(libdir)
	$(INSTALL) -m 644 $(BUILD_DIR)/dynamic/lib/$(MINISAT_DLIB).$(SOMAJOR).$(SOMINOR)$(SORELEASE) $(DESTDIR)$(libdir)
	ln -sf $(MINISAT_DLIB).$(SOMAJOR).$(SOMINOR)$(SORELEASE) $(DESTDIR)$(libdir)/$(MINISAT_DLIB).$(SOMAJOR)
	ln -sf $(MINISAT_DLIB).$(SOMAJOR) $(DESTDIR)$(libdir)/$(MINISAT_DLIB)
	$(INSTALL) -m 644 $(BUILD_DIR)/release/lib/$(MINISAT_SLIB) $(DESTDIR)$(libdir)

install-bin: $(BUILD_DIR)/dynamic/bin/$(MINISAT)
	$(INSTALL) -d $(DESTDIR)$(bindir)
	$(INSTALL) -m 755 $(BUILD_DIR)/dynamic/bin/$(MINISAT) $(DESTDIR)$(bindir)

clean:
	rm -f $(foreach t, release dynamic, $(foreach o, $(SRCS:.cc=.o), $(BUILD_DIR)/$t/$o)) \
          $(foreach t, release dynamic, $(foreach d, $(SRCS:.cc=.d), $(BUILD_DIR)/$t/$d)) \
	  $(foreach t, release dynamic, $(BUILD_DIR)/$t/bin/$(MINISAT)) \
	  $(foreach t, release, $(BUILD_DIR)/$t/lib/$(MINISAT_SLIB)) \
	  $(BUILD_DIR)/dynamic/lib/$(MINISAT_DLIB).$(SOMAJOR).$(SOMINOR)$(SORELEASE)\
	  $(BUILD_DIR)/dynamic/lib/$(MINISAT_DLIB).$(SOMAJOR)\
	  $(BUILD_DIR)/dynamic/lib/$(MINISAT_DLIB)

distclean:	clean
	rm -f config.mk

## Include generated dependencies
-include $(foreach s, $(SRCS:.cc=.d), $(BUILD_DIR)/release/$s)
-include $(foreach s, $(SRCS:.cc=.d), $(BUILD_DIR)/dynamic/$s)
