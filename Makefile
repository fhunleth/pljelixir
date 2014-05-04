# Variables to override
#
# CC            C compiler
# CROSSCOMPILE	crosscompiler prefix, if any
# CFLAGS	compiler flags for compiling all C files
# ERL_CFLAGS	additional compiler flags for files using Erlang header files
# ERL_EI_LIBDIR path to libei.a and liberl_interface.a
# LDFLAGS	linker flags for linking all binaries
# ERL_LDFLAGS	additional linker flags for projects referencing Erlang libraries
# MIX		path to mix

#ifeq ($(NERVES_ROOT),)
#    $(error Remember to source nerves-env.sh before building)
#endif

EILOC:=$(shell find /usr/local/lib/erlang /usr/lib/erlang -name ei.h -printf '%h\n' 2> /dev/null | head -1)
ERL_CFLAGS ?= -I/usr/local/include -I$(EILOC) -I/usr/lib/erlang/usr/include/

ERL_EI_LIBDIR ?= /usr/lib/erlang/usr/lib
ERL_LDFLAGS ?= -L$(ERL_EI_LIBDIR) -lerl_interface -lei

LDFLAGS += -lpthread
CFLAGS ?= -O2 -Wall -Wextra -Wno-unused-parameter
CC ?= $(CROSSCOMPILER)gcc
MIX ?= mix
ERL_LIB = $(NERVES_SDK_SYSROOT)/usr/lib/erlang/lib
ELX_LIB = $(NERVES_SDK_ROOT)/usr/lib/elixir/lib
REL2FW = $(NERVES_ROOT)/scripts/rel2fw.sh
QMAKE ?= qmake

all: firmware

deps: mix.exs
	$(MIX) deps.get
	# touch the deps folder so that we skip this step on
	# the next build
	touch deps

compile: deps
	$(MIX) compile

release: compile
	relx --system_libs $(ERL_LIB) -l $(ELX_LIB)

firmware: release
	$(REL2FW) _rel

src/Makefile:
	cd src && $(QMAKE) -after "target.path=../priv" console.pro

compile_port: src/Makefile
	+$(MAKE) -C src install

clean:
	$(MIX) clean; rm -fr _build _rel _images
	[ -e src/Makefile ] && $(MAKE) -C src clean && rm src/Makefile

distclean: clean
	-rm -fr ebin deps 

realclean: distclean

.PHONY: all firmware compile compile_port clean distclean realclean
