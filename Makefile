PROJECT = studio
PROJECT_DESCRIPTION = Erlang Music Studio
PROJECT_VERSION = 0.0.1
DEPS = erl_tools

C_SRC_TYPE = executable
LDFLAGS += -lasound -ljack

# If a package is not in the index, just add it like this:
PACKAGES += erl_tools
pkg_erl_tools_name = erl_tools
pkg_erl_tools_description = Misc Erlang tools
pkg_erl_tools_homepage = https://zwizwa.be/git/erl_tools
pkg_erl_tools_fetch = git
pkg_erl_tools_repo = http://github.com/zwizwa/erl_tools
pkg_erl_tools_commit = master

include erlang.mk

erlang.mk:
	wget https://raw.githubusercontent.com/ninenines/erlang.mk/master/erlang.mk
