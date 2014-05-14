TOPDIR := $(shell pwd)
NERVES_VERSION = 0def35b534fd82f665556002e0c543d9b487551e
NERVES_URL = https://github.com/nerves-project/nerves-sdk.git
NERVES_RESULT = _nerves/buildroot/output/images/rootfs.tar

-include _nerves/nerves.mk

ELX_LIB = $(NERVES_SDK_ROOT)/usr/lib/elixir/lib

all: firmware

_nerves:
	git clone $(NERVES_URL) _nerves

.PRECIOUS: _nerves/nerves.mk
_nerves/nerves.mk $(NERVES_RESULT): _nerves $(TOPDIR)/config/nerves/nerves_defconfig
	cd _nerves && git fetch && git checkout $(NERVES_VERSION)
	ln -fs $(TOPDIR)/config/nerves/nerves_defconfig _nerves/configs/nerves_defconfig
	$(MAKE) -C _nerves nerves_defconfig
	$(MAKE) -C _nerves
	touch _nerves/nerves.mk

deps: mix.exs $(NERVES_RESULT)
	$(MIX) deps.get
	# touch the deps folder so that we skip this step on
	# the next build
	touch deps

compile: deps
	$(MIX) compile

release: compile
	$(RELX) -l $(ELX_LIB)

firmware: release
	$(REL2FW) _rel

src/Makefile:
	cd src && $(NERVES_HOST_MAKE_ENV) qmake -after "target.path=../priv" "LIBS+=-L$(ERL_EI_LIBDIR)" "INCLUDEPATH+=$(ERL_INTERFACE_DIR)" console.pro

compile_port: src/Makefile
	+$(MAKE) -C src install

clean:
	[ ! -e $(NERVES_RESULT) ] || ($(MIX) clean)
	[ ! -e src/Makefile ] || $(MAKE) -C src clean
	-rm -fr _build _rel _images src/Makefile

realclean: distclean
distclean:
	-rm -fr _build _rel _images src/Makefile src/*.o ebin deps _nerves

.PHONY: all firmware nerves compile compile_port clean distclean realclean
