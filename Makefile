VSN?=unknown
DIST?=unknown
ARCH=$(shell erl -noshell -eval 'io:format(erlang:system_info(system_architecture)), halt().')
WORDSIZE=$(shell erl -noshell -eval 'io:format(integer_to_list(erlang:system_info(wordsize)*8)), halt().')

RELPKG="hibari_$(VSN)-$(DIST)-$(ARCH)-$(WORDSIZE)"
RELTGZ="$(RELPKG).tgz"
RELSHA="hibari_$(VSN)-$(DIST)-$(ARCH)-$(WORDSIZE)-shasum.txt"

OTPREL=$(shell erl -noshell -eval 'io:format(erlang:system_info(otp_release)), halt().')
PLT=$(HOME)/.dialyzer_plt.$(OTPREL)

.PHONY: all test package generate compile eunit build-plt check-plt dialyze dialyze-spec dialyze-nospec ctags etags clean realclean distclean

all: compile

test: eunit

package: generate
	@echo "packaging: $(RELPKG) ..."
	@rm -f ../$(RELTGZ) ../$(RELSHA)
	@tar -C ./rel -cvzf ../$(RELTGZ) hibari
	@(cd .. && shasum $(RELTGZ) | tee $(RELSHA))
	@(cd .. && ls -l $(RELTGZ) $(RELSHA))

generate: clean compile
	@echo "generating: $(RELPKG) ..."
	find ./lib -name svn -type l | xargs rm -f
	find ./lib -name rr-cache -type l | xargs rm -f
	./rebar generate

compile:
	@echo "compiling: $(RELPKG) ..."
	./rebar compile

eunit: compile
	@echo "eunit testing: $(RELPKG) ..."
	./rebar eunit

build-plt: $(PLT)

check-plt: $(PLT)
	dialyzer --plt $(PLT) --check_plt

dialyze: dialyze-spec

dialyze-spec: build-plt clean compile
	@echo "dialyzing w/spec: $(RELPKG) ..."
	dialyzer --plt $(PLT) -r ./lib -Wunmatched_returns -Werror_handling -Wrace_conditions

dialyze-nospec: build-plt clean compile
	@echo "dialyzing w/o spec: $(RELPKG) ..."
	dialyzer --plt $(PLT) -r ./lib --no_spec

ctags:
	find ./lib -name "*.[he]rl" -print | grep -v .eunit | ctags -
	find ./lib -name "*.app.src" -print | grep -v .eunit | ctags -a -
	find ./lib -name "*.config" -print | grep -v .eunit | ctags -a -

etags:
	find ./lib -name "*.[he]rl" -print | grep -v .eunit | etags -
	find ./lib -name "*.app.src" -print | grep -v .eunit | etags -a -
	find ./lib -name "*.config" -print | grep -v .eunit | etags -a -

clean:
	@echo "cleaning: $(RELPKG) ..."
	./rebar clean

realclean: clean
	@echo "realcleaning: $(RELPKG) ..."
	rm -f $(PLT)

distclean:
	@echo "distcleaning: $(RELPKG) ..."
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
