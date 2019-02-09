
# To support multi-platform builds, we use GCC's -dumpmachine output
# as a directory name under which to store the target binary.  We then
# link the binary into the source's priv/ dir EVERY TIME such that the
# release tool can follow that symlink to create the tarball.  Note
# that this means release builds cannot be run in parallell for
# different architectures!

STUDIO_BUILD_DIR=$(REBAR_BUILD_DIR)/studio
STUDIO_BIN_DIR=$(STUDIO_BUILD_DIR)/$(shell $(CC) -dumpmachine)

STUDIO_ELF = $(STUDIO_BIN_DIR)/studio.elf
TARGETS := $(STUDIO_ELF)

.PHONY: all
all: $(TARGETS) $(patsubst %,../priv/%,$(STUDIO_EXTRA_TARGETS))
	set >$(STUDIO_BUILD_DIR)/env.log
	@echo Building in $(REBAR_BUILD_DIR)
	(cd ../priv ; ln -sf $(STUDIO_ELF) .) # re-create 'current target' link for release tool

.PHONY: clean
clean:
	rm -rf *.o ../priv/*.elf

# Clean this up.
DEFS := -DMAIN=main -DREAD=read -DWRITE=write

CFLAGS  += -std=c99 -I$(shell readlink -f ../../erl_tools/include)  # FIXME
LDFLAGS += -lasound -ljack

# Default target build
LIB_O := alsa.o jack_control.o jack_midi.o
%.o: %.c
	. ./default.env ; $(CC) $(CFLAGS) -I../include -Wall -c $< -o $@ $(DEFS)

$(STUDIO_BIN_DIR)/%.elf: %.o $(LIB_O)
	mkdir -p $(STUDIO_BIN_DIR)
	. ./default.env ; $(CC) $(LDFLAGS) -o $@ $< $(LIB_O)
	file $@




## From old Makefile, erlang.mk

# PROJECT = studio
# PROJECT_DESCRIPTION = Erlang Music Studio
# PROJECT_VERSION = 0.0.1
# DEPS = erl_tools erl_sqlite3

# C_SRC_TYPE = executable
# CFLAGS  += -std=c99 -I$(shell readlink -f deps/erl_tools/include)
# LDFLAGS += -lasound -ljack



# # If a package is not in the index, just add it like this:
# PACKAGES += erl_tools
# pkg_erl_tools_name = erl_tools
# pkg_erl_tools_description = Misc Erlang tools
# pkg_erl_tools_homepage = https://zwizwa.be/git/erl_tools
# pkg_erl_tools_fetch = git
# pkg_erl_tools_repo = https://zwizwa.be/git/erl_tools.git
# pkg_erl_tools_commit = 387e8461868f53222750d89e4531c0a29d90b673


# PACKAGES += erl_sqlite3
# pkg_erl_sqlite3_name = erl_sqlite3
# pkg_erl_sqlite3_description = Minimalistic SQLite3 Erlang Port
# pkg_erl_sqlite3_homepage = https://github.com/zwizwa/erl_sqlite3
# pkg_erl_sqlite3_fetch = git
# pkg_erl_sqlite3_repo = https://github.com/zwizwa/erl_sqlite3.git
# pkg_erl_sqlite3_commit = ad71ece83c33f9087e30a427da0fe24e968a0577

# include erlang.mk

# #erlang.mk:
# #	wget https://raw.githubusercontent.com/ninenines/erlang.mk/master/erlang.mk

# # Local dev
# update:
# 	cd deps/erl_tools   ; git pull ../erl_tools
# 	cd deps/erl_sqlite3 ; git pull ../erl_sqlite3

