VSN?=unknown
DIST?=unknown
ARCH=$(shell erl -noshell -eval 'io:format(erlang:system_info(system_architecture)), halt().')
WORDSIZE=$(shell erl -noshell -eval 'io:format(integer_to_list(erlang:system_info(wordsize)*8)), halt().')

RELPKG="hibari_$(VSN)-$(DIST)-$(ARCH)-$(WORDSIZE)"
RELTGZ="$(RELPKG).tgz"
RELSHA="hibari_$(VSN)-$(DIST)-$(ARCH)-$(WORDSIZE)-shasum.txt"

OTPREL=$(shell erl -noshell -eval 'io:format(erlang:system_info(otp_release)), halt().')
PLT=$(HOME)/.dialyzer_plt.$(OTPREL)

.PHONY: all test package generate compile eunit build-plt check-plt dialyze dialyze-spec dialyze-nospec dialyze-eunit dialyze-eunit-spec dialyze-eunit-nospec ctags etags clean realclean distclean

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
	dialyzer --plt $(PLT) -Wunmatched_returns -r ./lib

dialyze-nospec: build-plt clean compile
	@echo "dialyzing w/o spec: $(RELPKG) ..."
	dialyzer --plt $(PLT) --no_spec -r ./lib

dialyze-eunit: dialyze-eunit-spec

dialyze-eunit-spec: build-plt clean compile
	@echo "dialyzing .eunit w/spec: $(RELPKG) ..."
	./rebar eunit perform=0
	#TODO dialyzer --plt $(PLT) -Wunmatched_returns -r `find ./lib -name .eunit -print | xargs echo`
	dialyzer --plt $(PLT) -r `find ./lib -name .eunit -print | xargs echo`

dialyze-eunit-nospec: build-plt clean compile
	@echo "dialyzing .eunit w/o spec: $(RELPKG) ..."
	./rebar eunit perform=0
	dialyzer --plt $(PLT) --no_spec -r `find ./lib -name .eunit -print | xargs echo`

ctags:
	find ./lib -name "*.[he]rl" -print | fgrep -v .eunit | ctags -
	find ./lib -name "*.app.src" -print | fgrep -v .eunit | ctags -a -
	find ./lib -name "*.config" -print | fgrep -v .eunit | ctags -a -
	find ./lib -name "*.[ch]" -print | fgrep -v .eunit | ctags -a -

etags:
	find ./lib -name "*.[he]rl" -print | fgrep -v .eunit | etags -
	find ./lib -name "*.app.src" -print | fgrep -v .eunit | etags -a -
	find ./lib -name "*.config" -print | fgrep -v .eunit | etags -a -
	find ./lib -name "*.[ch]" -print | fgrep -v .eunit | etags -a -

clean:
	@echo "cleaning: $(RELPKG) ..."
	./rebar clean

realclean: clean
	@echo "realcleaning: $(RELPKG) ..."
	rm -f $(PLT) TAGS

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
		xmerl
