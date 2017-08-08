PROJECT = test
PROJECT_DESCRIPTION = Server side for httpreq tests
PROJECT_VERSION = 0.1.0

LOCAL_DEPS = crypto

DEPS = mochiweb htmltmpl csv
dep_mochiweb_commit = v2.16.0
dep_htmltmpl = git https://github.com/lego12239/htmltmpl.git 0.4.0
dep_csv = git https://github.com/lego12239/csv.git 1.1.2

ERLC_OPTS := $(filter-out -Werror,$(ERLC_OPTS))

include erlang.mk

dev:
	export ERL_AFLAGS="-kernel inet_dist_use_interface '{127,0,0,1}'"; \
	export ERL_EPMD_ADDRESS="127.0.0.1"; \
	erl -pa ebin $PWD/deps/*/ebin -boot start_sasl -sname test_dev \
	-s inets -s ssl -s crypto -s test -s reloader

