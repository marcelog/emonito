CWD=$(shell pwd)
NAME=$(shell basename ${CWD})
DEPS=$(wildcard ${CWD}/deps/*/ebin)
HOST?=${NAME}.local
NODE?=${NAME}@${HOST}
REBAR?=${CWD}/rebar
COOKIE?=${NAME}
ERL?=/usr/bin/env erl
ERLARGS=-pa ${DEPS} -pa ebin -smp enable -name ${NODE} \
	-setcookie ${COOKIE} -boot start_sasl -s lager -s emonito

all: clean getdeps compile release

# Clean all.
clean:
	@${REBAR} clean

# Gets dependencies.
getdeps:
	@${REBAR} get-deps

# Compiles.
compile:
	@${REBAR} compile

# Generates doc
doc:
	${REBAR} skip_deps=true doc 

# This one runs without a release.
shell: compile
	${ERL} ${ERLARGS}
