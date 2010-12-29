VSN?=none
DIST?=none
ARCH=$(shell erl -noshell -eval 'io:format(erlang:system_info(system_architecture)), halt().')
WORDSIZE=$(shell erl -noshell -eval 'io:format(integer_to_list(erlang:system_info(wordsize)*8)), halt().')

PKG=hibari_$(VSN)-$(DIST)-$(ARCH)-$(WORDSIZE)
TGZ=$(PKG).tgz

OTPREL=$(shell erl -noshell -eval 'io:format(erlang:system_info(otp_release)), halt().')
PLT=$(HOME)/.dialyzer_plt.$(OTPREL)

.PHONY: all test package generate compile eunit build-plt check-plt dialyze dialyze-spec clean realclean distclean

all: package

test: eunit

package: generate
	@echo "packaging: $(PKG) ..."
	@echo "*** UNDER CONSTRUCTION ***"

generate: clean compile
	@echo "generating: $(PKG) ..."
	find ./lib -name svn -type l | xargs rm -f
	find ./lib -name rr-cache -type l | xargs rm -f
	./rebar generate

compile:
	@echo "compiling: $(PKG) ..."
	./rebar compile

eunit: compile
	@echo "eunit testing: $(PKG) ..."
	./rebar eunit

build-plt: $(PLT)

check-plt: $(PLT)
	dialyzer --plt $(PLT) --check_plt

dialyze: build-plt clean compile
	@echo "dialyzing: $(PKG) ..."
	dialyzer --plt $(PLT) -r ./lib --no_spec

dialyze-spec: build-plt clean compile
	@echo "dialyzing w/spec: $(PKG) ..."
	dialyzer --plt $(PLT) -r ./lib

clean:
	@echo "cleaning: $(PKG) ..."
	./rebar clean

realclean: clean
	@echo "realcleaning: $(PKG) ..."
	rm -f $(PLT)

distclean:
	@echo "distcleaning: $(PKG) ..."
	repo forall -v -c 'git clean -fdx --exclude=lib/'

$(PLT):
	@echo "building: $(PLT) ..."
	dialyzer --build_plt --output_plt $(PLT) --apps \
		asn1 \
		compiler \
		crypto \
		dialyzer \
		edoc \
		erts \
		et \
		eunit \
		gs \
		hipe \
		inets \
		kernel \
		mnesia \
		observer \
		odbc \
		parsetools \
		public_key \
		runtime_tools \
		sasl \
		ssl \
		stdlib \
		syntax_tools \
		tools \
		webtool \
		wx \
		xmerl
