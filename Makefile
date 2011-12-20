XDIST?=dev
VSN?=$(shell grep '{rel, "hibari"' rel/reltool.config | sed 's/^.*rel, "hibari", "\(.*\)",/\1/')
ARCH=$(shell erl -noshell -eval 'io:format(erlang:system_info(system_architecture)), halt().')
WORDSIZE=$(shell erl -noshell -eval 'io:format(integer_to_list(try erlang:system_info({wordsize, external}) of Val -> 8*Val catch error:badarg -> 8*erlang:system_info(wordsize) end)), halt().')

RELPKG=hibari-$(VSN)-$(XDIST)-$(ARCH)-$(WORDSIZE)
RELTGZ=$(RELPKG).tgz
RELMD5=hibari-$(VSN)-$(XDIST)-$(ARCH)-$(WORDSIZE)-md5sum.txt

OTPREL=$(shell erl -noshell -eval 'io:format(erlang:system_info(otp_release)), halt().')
PLT=$(HOME)/.dialyzer_plt.$(OTPREL)

DIALYZE_IGNORE_WARN?=dialyze-ignore-warnings.txt
DIALYZE_NOSPEC_IGNORE_WARN?=dialyze-nospec-ignore-warnings.txt

ifeq ($(shell uname -s),Darwin)
	ifeq ($(shell uname -m),x86_64)
		otp_configure_flags= --enable-darwin-64bit
	else
		otp_configure_flags= --enable-darwin-universal
	endif
else
	otp_configure_flags=
endif

.PHONY: all test \
	compile compile-eqc compile-proper \
	eunit-compile eqc-compile proper-compile \
	eunit eqc proper \
	doc \
	build-plt check-plt \
	dialyze dialyze-spec dialyze-nospec \
	dialyze-eunit dialyze-eunit-spec dialyze-eunit-nospec \
	dialyze-eqc dialyze-eqc-spec dialyze-eqc-nospec \
	dialyze-proper dialyze-proper-spec dialyze-proper-nospec \
	ctags etags \
	clean realclean distclean \
	otp_make_release_tests otp_run_release_tests \
	bootstrap-package check-package package generate
all: compile

test: eunit

bootstrap-package: package
	@echo "bootstrapping package: $(RELPKG) ..."
	@-./tmp/hibari/bin/hibari stop &> /dev/null
	@rm -rf ./tmp
	@mkdir ./tmp
	tar -C ./tmp -xzf ../$(RELTGZ)
	./tmp/hibari/bin/hibari start
	@sleep 5
	./tmp/hibari/bin/hibari-admin bootstrap
	@sleep 1

check-package: bootstrap-package
	@echo "checking package: $(RELPKG) ..."
	./tmp/hibari/bin/hibari-admin client-add hibari@127.0.0.1
	@sleep 1
	./tmp/hibari/bin/hibari-admin client-list
	./tmp/hibari/bin/hibari-admin client-delete hibari@127.0.0.1
	./tmp/hibari/bin/hibari checkpoint
	./tmp/hibari/bin/hibari stop

package: generate
	@echo "packaging: $(RELPKG) ..."
	@rm -f ../$(RELTGZ) ../$(RELMD5)
	@tar -C ./rel -czf ../$(RELTGZ) hibari
	@(cd .. && (md5sum $(RELTGZ) 2> /dev/null || md5 -r $(RELTGZ) 2> /dev/null) | tee $(RELMD5))
	@(cd .. && ls -l $(RELTGZ) $(RELMD5))

generate: clean compile
	@echo "generating: $(RELPKG) ..."
	@find ./lib -name svn -type l | xargs rm -f
	@find ./lib -name rr-cache -type l | xargs rm -f
	./rebar generate
	@perl -i -pe 's/%% (.* generated) at .*//g;' rel/hibari/releases/*/*.{rel,script}

compile:
	@echo "compiling: $(RELPKG) ..."
	./rebar compile

compile-eqc:
	@echo "compiling-eqc: $(RELPKG) ..."
	./rebar compile -D QC -D QC_EQC

compile-proper:
	@echo "compiling-proper: $(RELPKG) ..."
	./rebar compile -D QC -D QC_PROPER

eunit-compile: compile
	@echo "eunit test compiling: $(RELPKG) ..."
	./rebar eunit-compile

eqc-compile: compile-eqc
	@echo "eqc test compiling: $(RELPKG) ..."
	./rebar eunit-compile -D QC -D QC_EQC

proper-compile: compile-proper
	@echo "proper test compiling: $(RELPKG) ..."
	./rebar eunit-compile -D QC -D QC_PROPER

eunit: eunit-compile
	@echo "eunit testing: $(RELPKG) ..."
	./rebar eunit skip_apps=meck

eqc: eqc-compile
	@echo "eqc testing: $(RELPKG) ... not implemented yet"

proper: proper-compile
	@echo "proper testing: $(RELPKG) ... not implemented yet"

doc: compile
	@echo "edoc generating: $(RELPKG) ..."
	./rebar doc

build-plt: $(PLT)

check-plt: $(PLT)
	dialyzer --plt $(PLT) --check_plt

dialyze: dialyze-spec

dialyze-spec: build-plt clean compile
	@echo "dialyzing w/spec: $(RELPKG) ..."
	dialyzer --plt $(PLT) -Wunmatched_returns -r ./lib | fgrep -v -f $(DIALYZE_IGNORE_WARN)

dialyze-nospec: build-plt clean compile
	@echo "dialyzing w/o spec: $(RELPKG) ..."
	dialyzer --plt $(PLT) --no_spec -r ./lib | fgrep -v -f $(DIALYZE_NOSPEC_IGNORE_WARN)

dialyze-eunit: dialyze-eunit-spec

dialyze-eunit-spec: build-plt clean eunit-compile
	@echo "dialyzing .eunit w/spec: $(RELPKG) ..."
	#TODO dialyzer --plt $(PLT) -Wunmatched_returns -r `find ./lib -name .eunit -print | xargs echo` | fgrep -v -f $(DIALYZE_IGNORE_WARN)
	dialyzer --plt $(PLT) -r `find ./lib -name .eunit -print | xargs echo` | fgrep -v -f $(DIALYZE_IGNORE_WARN)

dialyze-eunit-nospec: build-plt clean eunit-compile
	@echo "dialyzing .eunit w/o spec: $(RELPKG) ..."
	./rebar eunit-compile
	dialyzer --plt $(PLT) --no_spec -r `find ./lib -name .eunit -print | xargs echo` | fgrep -v -f $(DIALYZE_NOSPEC_IGNORE_WARN)

dialyze-eqc: dialyze-eqc-spec

dialyze-eqc-spec: build-plt clean eqc-compile
	@echo "dialyzing .eqc w/spec: $(RELPKG) ..."
	#TODO dialyzer --plt $(PLT) -Wunmatched_returns -r `find ./lib -name .eunit -print | xargs echo` | fgrep -v -f $(DIALYZE_IGNORE_WARN)
	dialyzer --plt $(PLT) -r `find ./lib -name .eunit -print | xargs echo` | fgrep -v -f $(DIALYZE_IGNORE_WARN)

dialyze-eqc-nospec: build-plt clean eqc-compile
	@echo "dialyzing .eqc w/o spec: $(RELPKG) ..."
	./rebar eqc-compile
	dialyzer --plt $(PLT) --no_spec -r `find ./lib -name .eunit -print | xargs echo` | fgrep -v -f $(DIALYZE_NOSPEC_IGNORE_WARN)

dialyze-proper: dialyze-proper-spec

dialyze-proper-spec: build-plt clean proper-compile
	@echo "dialyzing .proper w/spec: $(RELPKG) ..."
	#TODO dialyzer --plt $(PLT) -Wunmatched_returns -r `find ./lib -name .eunit -print | xargs echo` | fgrep -v -f $(DIALYZE_IGNORE_WARN)
	dialyzer --plt $(PLT) -r `find ./lib -name .eunit -print | xargs echo` | fgrep -v -f $(DIALYZE_IGNORE_WARN)

dialyze-proper-nospec: build-plt clean proper-compile
	@echo "dialyzing .proper w/o spec: $(RELPKG) ..."
	./rebar proper-compile
	dialyzer --plt $(PLT) --no_spec -r `find ./lib -name .eunit -print | xargs echo` | fgrep -v -f $(DIALYZE_NOSPEC_IGNORE_WARN)

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

otp: otp.git
	make -C $(CURDIR)/otp.git install

otp.git:
	rm -rf $(CURDIR)/otp
	mkdir -p $(CURDIR)/otp
	git clone git://github.com/erlang/otp.git otp.git
	(cd $(CURDIR)/otp.git && \
		git co OTP_R15B && \
		./otp_build autoconf && \
		./configure \
			--disable-hipe \
			--enable-debug \
			--enable-kernel-poll \
			--enable-threads \
			--enable-dynamic-ssl-lib \
			--enable-shared-zlib \
			--enable-smp-support \
			$(otp_configure_flags) \
			--prefix=$(CURDIR)/otp)
	make -C $(CURDIR)/otp.git

otp-debug: otp.git
	env ERL_TOP=$(CURDIR)/otp.git make -C otp.git/erts/emulator debug FLAVOR=smp

otp-valgrind: otp.git
	env ERL_TOP=$(CURDIR)/otp.git make -C otp.git/erts/emulator valgrind FLAVOR=smp

cerl-debug: otp.git
	env ERL_TOP=$(CURDIR)/otp.git otp.git/bin/cerl -debug

cerl-valgrind: otp.git
	env ERL_TOP=$(CURDIR)/otp.git otp.git/bin/cerl -valgrind

## See https://github.com/erlang/otp/wiki/Running-tests for details
otp_make_release_tests: otp.git
	rm -rf otp.git/release/tests
	env ERL_TOP=$(CURDIR)/otp.git ERL_LIBS=$(CURDIR)/otp.git/lib \
		make -C otp.git release_tests

otp_run_release_tests: otp_make_release_tests
	@echo ""
	@echo "** Warning killing all local beam, beam.smp, and epmd programs **"
	@echo ""
	sleep 10
	killall -q -9 beam || true
	killall -q -9 beam.smp || true
	killall -q -9 epmd || true
	@echo ""
	@echo "** Open '$(CURDIR)/otp.git/release/tests/test_server/index.html' in your browser**"
	@echo ""
	sleep 10
	(cd $(CURDIR)/otp.git/release/tests/test_server && \
		env ERL_TOP=$(CURDIR)/otp.git ERL_LIBS=$(CURDIR)/otp.git/lib \
			$(CURDIR)/otp.git/bin/erl \
				-s ts install \
				-s ts run \
				-s erlang halt)
