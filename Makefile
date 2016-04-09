PROJECT = studio
PROJECT_DESCRIPTION = Erlang Music Studio
PROJECT_VERSION = 0.0.1
DEPS = erl_tools erl_sqlite3

C_SRC_TYPE = executable
CFLAGS  += -std=c99 -I$(shell readlink -f deps/erl_tools/include)
LDFLAGS += -lasound -ljack



# If a package is not in the index, just add it like this:
PACKAGES += erl_tools
pkg_erl_tools_name = erl_tools
pkg_erl_tools_description = Misc Erlang tools
pkg_erl_tools_homepage = https://zwizwa.be/git/erl_tools
pkg_erl_tools_fetch = git
pkg_erl_tools_repo = https://zwizwa.be/git/erl_tools.git
pkg_erl_tools_commit = master

PACKAGES += erl_sqlite3
pkg_erl_sqlite3_name = erl_sqlite3
pkg_erl_sqlite3_description = Minimalistic SQLite3 Erlang Port
pkg_erl_sqlite3_homepage = https://github.com/zwizwa/erl_sqlite3
pkg_erl_sqlite3_fetch = git
pkg_erl_sqlite3_repo = https://github.com/zwizwa/erl_sqlite3.git
pkg_erl_sqlite3_commit = master

include erlang.mk

#erlang.mk:
#	wget https://raw.githubusercontent.com/ninenines/erlang.mk/master/erlang.mk

# Local dev
update:
	cd deps/erl_tools   ; git pull ../erl_tools
	cd deps/erl_sqlite3 ; git pull ../erl_sqlite3

