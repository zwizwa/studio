PROJECT = studio
PROJECT_DESCRIPTION = Erlang Music Studio
PROJECT_VERSION = 0.0.1
DEPS = erl_tools erl_sqlite3

C_SRC_TYPE = executable
LDFLAGS += -lasound -ljack

CFLAGS_ERL_TOOLS := -I$(shell readlink -f deps/erl_tools/c_src)
CFLAGS += -std=c99 $(CFLAGS_ERL_TOOLS)



# If a package is not in the index, just add it like this:
PACKAGES += erl_tools
pkg_erl_tools_name = erl_tools
pkg_erl_tools_description = Misc Erlang tools
pkg_erl_tools_homepage = https://zwizwa.be/git/erl_tools
pkg_erl_tools_fetch = git
pkg_erl_tools_repo = ~/git/erl_tools
pkg_erl_tools_commit = master

PACKAGES += erl_sqlite3
pkg_erl_sqlite3_name = erl_sqlite3
pkg_erl_sqlite3_description = Minimalistic SQLite3 Erlang Port
pkg_erl_sqlite3_homepage = ~/git/erl_sqlite3
pkg_erl_sqlite3_fetch = git
pkg_erl_sqlite3_repo = ~/git/erl_sqlite3
pkg_erl_sqlite3_commit = master

include erlang.mk

#erlang.mk:
#	wget https://raw.githubusercontent.com/ninenines/erlang.mk/master/erlang.mk
