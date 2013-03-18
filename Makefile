
REBAR?=./rebar

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

ERLDIRS?=./lib
ERLEUNITDIRS=`find $(ERLDIRS) -name .eunit -print | xargs echo` .eunit
ERLQCDIRS=`find $(ERLDIRS) -name .qc -print | xargs echo` .qc

DIALYZE_IGNORE_WARN?=dialyze-ignore-warnings.txt
DIALYZE_NOSPEC_IGNORE_WARN?=dialyze-nospec-ignore-warnings.txt

#TBD DIALYZER_OPTS?=-Wunmatched_returns -Werror_handling -Wrace_conditions -Wunderspecs
DIALYZER_OPTS?=-Wunmatched_returns -Werror_handling -Wunderspecs
DIALYZER_NOSPEC_OPTS?=-Wno_undefined_callbacks

dialyzer=dialyzer -q --plt $(PLT) $(DIALYZER_OPTS) -r $(ERLDIRS)
dialyzer-nospec=dialyzer -q --plt $(PLT) --no_spec $(DIALYZER_NOSPEC_OPTS) -r $(ERLDIRS)
dialyzer-eunit=dialyzer -q --plt $(PLT) $(DIALYZER_OPTS) -r $(ERLEUNITDIRS)
dialyzer-eunit-nospec=dialyzer -q --plt $(PLT) --no_spec $(DIALYZER_NOSPEC_OPTS) -r $(ERLEUNITDIRS)
dialyzer-qc=dialyzer -q --plt $(PLT) $(DIALYZER_OPTS) -r $(ERLQCDIRS)
dialyzer-qc-nospec=dialyzer -q --plt $(PLT) --no_spec $(DIALYZER_NOSPEC_OPTS) -r $(ERLQCDIRS)

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
	bootstrap-package check-package package generate \
	compile eunit eunit-core eunit-thrift \
	eqc proper triq \
	compile-for-eunit comple-for-eqc compile-for-proper compile-for-triq \
	doc \
	clean realclean distclean \
	ctags etags \
	dialyze dialyze-nospec \
	updata-dialyzer-baseline update-dialyzer-nospec-baseline \
	dialyze-eunit dialyze-eunit-nospec \
	dialyze-eqc dialyze-eqc-nospec \
	dialyze-proper dialyze-proper-nospec \
	dialyze-triq dialyze-triq-nospec \
	build-plt check-plt \
	rebar rebar.git \
	otp otp.git otp-debug otp-valgrind cerl-debug cerl-valgrind \
	otp_make_release_tests otp_run_release_tests

all: compile

test: eunit

bootstrap-package: package
	@echo "bootstrapping package: $(RELPKG) ..."
	@-./tmp/hibari/bin/hibari stop &> /dev/null
	@rm -rf ./tmp
	@mkdir ./tmp
	tar -C ./tmp -xzf ../$(RELTGZ)
	./tmp/hibari/bin/hibari start
	@sleep 10
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
	@perl -i -pe 's/%% (.* generated) at .*//g;' \
		rel/hibari/releases/*/*.rel \
		rel/hibari/releases/*/*.script

compile:
	@echo "compiling: $(RELPKG) ..."
	$(REBAR) compile

eunit: compile-for-eunit
	@echo "eunit testing: $(RELPKG) ..."
	$(REBAR) eunit skip_apps='meck,asciiedoc,edown'

eunit-core: compile-for-eunit
	@echo "eunit testing (core): $(RELPKG) ..."
	$(REBAR) eunit skip_apps='ubf,gdss_ubf_proto,ubf_thrift,lager,meck,asciiedoc,edown'

eunit-thrift: compile-for-eunit
	@echo "eunit testing (thrift): $(RELPKG) ..."
	$(REBAR) eunit skip_apps='gdss_brick,gdss_client,gdss_admin,cluster_info,partition_detector,congestion_watcher,gmt_util,lager,meck,asciiedoc,edown'

eqc: compile-for-eqc
	@echo "eqc testing: $(RELPKG) ..."
	$(REBAR) eqc qc_opts=3000 skip_apps='lager,meck,ubf,ubf_thrift'

proper: compile-for-proper
	@echo "proper testing: $(RELPKG) ..."
	$(REBAR) proper skip_apps='meck,ubf,ubf_thrift'

triq: compile-for-triq
	@echo "triq testing: $(RELPKG) ..."
	$(REBAR) triq skip_apps='meck,ubf,ubf_thrift'

compile-for-eunit:
	@echo "compiling-eunit: $(RELPKG) ..."
	$(REBAR) compile eunit compile_only=true

compile-for-eqc:
	@echo "compiling-eqc: $(RELPKG) ..."
	$(REBAR) -D QC -D QC_EQC compile eqc compile_only=true skip_apps='ubf'

compile-for-proper:
	@echo "compiling-proper: $(RELPKG) ..."
	$(REBAR) -D QC -D QC_PROPER compile eqc compile_only=true

compile-for-triq:
	@echo "compiling-triq: $(RELPKG) ..."
	$(REBAR) -D QC -D QC_TRIQ compile triq compile_only=true

doc: compile
	@echo "edoc generating: $(RELPKG) ..."
	$(REBAR) doc

clean:
	@echo "cleaning: $(RELPKG) ..."
	$(REBAR) clean

realclean: clean
	@echo "realcleaning: $(RELPKG) ..."
	rm -f $(PLT) TAGS

distclean:
	@echo "distcleaning: $(RELPKG) ..."
	repo forall -v -c 'git clean -fdx --exclude=lib/'

#
# tags
#

ctags:
	find $(ERLDIRS) -name "*.[he]rl" -print | fgrep -v .eunit | fgrep -v .qc | ctags -
	find $(ERLDIRS) -name "*.app.src" -print | fgrep -v .eunit | fgrep -v .qc | ctags -a -
	find $(ERLDIRS) -name "*.config" -print | fgrep -v .eunit | fgrep -v .qc | ctags -a -
	find $(ERLDIRS) -name "*.[ch]" -print | fgrep -v .eunit | fgrep -v .qc | ctags -a -
	find $(ERLDIRS) -name "*.con" -print | fgrep -v .eunit | fgrep -v .qc | ctags -a -

etags:
	find $(ERLDIRS) -name "*.[he]rl" -print | fgrep -v .eunit | fgrep -v .qc | etags -
	find $(ERLDIRS) -name "*.app.src" -print | fgrep -v .eunit | fgrep -v .qc | etags -a -
	find $(ERLDIRS) -name "*.config" -print | fgrep -v .eunit | fgrep -v .qc | etags -a -
	find $(ERLDIRS) -name "*.[ch]" -print | fgrep -v .eunit | fgrep -v .qc | etags -a -
	find $(ERLDIRS) -name "*.con" -print | fgrep -v .eunit | fgrep -v .qc | etags -a -

#
# dialyzer
#

dialyze: build-plt clean compile
	-$(dialyzer) | grep -v '^ *$$' | tee $(DIALYZE_IGNORE_WARN).log | fgrep -v -f $(DIALYZE_IGNORE_WARN)

dialyze-nospec: build-plt clean compile
	-$(dialyzer-nospec) | grep -v '^ *$$' | tee $(DIALYZE_NOSPEC_IGNORE_WARN).log | fgrep -v -f $(DIALYZE_NOSPEC_IGNORE_WARN)

update-dialyzer-baseline: dialyze
	mv -f $(DIALYZE_IGNORE_WARN).log $(DIALYZE_IGNORE_WARN)

update-dialyzer-nospec-baseline: dialyze-nospec
	mv -f $(DIALYZE_NOSPEC_IGNORE_WARN).log $(DIALYZE_NOSPEC_IGNORE_WARN)

dialyze-eunit: build-plt clean compile-for-eunit
	-$(dialyzer-eunit) | grep -v '^ *$$' | tee $(DIALYZE_IGNORE_WARN).log | fgrep -v -f $(DIALYZE_IGNORE_WARN)

dialyze-eunit-nospec: build-plt clean compile-for-eunit
	-$(dialyzer-eunit-nospec) | grep -v '^ *$$' | tee $(DIALYZE_NOSPEC_IGNORE_WARN).log | fgrep -v -f $(DIALYZE_NOSPEC_IGNORE_WARN)

dialyze-eqc: build-plt clean compile-for-eqc
	-$(dialyzer-qc) | grep -v '^ *$$' | tee $(DIALYZE_IGNORE_WARN).log | fgrep -v -f $(DIALYZE_IGNORE_WARN)

dialyze-eqc-nospec: build-plt clean compile-for-eqc
	-$(dialyzer-qc-nospec) | grep -v '^ *$$' | tee $(DIALYZE_NOSPEC_IGNORE_WARN).log | fgrep -v -f $(DIALYZE_NOSPEC_IGNORE_WARN)

dialyze-proper: build-plt clean compile-for-proper
	-$(dialyzer-qc) | grep -v '^ *$$' | tee $(DIALYZE_IGNORE_WARN).log | fgrep -v -f $(DIALYZE_IGNORE_WARN)

dialyze-proper-nospec: build-plt clean compile-for-proper
	-$(dialyzer-qc-nospec) | grep -v '^ *$$' | tee $(DIALYZE_NOSPEC_IGNORE_WARN).log | fgrep -v -f $(DIALYZE_NOSPEC_IGNORE_WARN)

dialyze-triq: build-plt clean compile-for-triq
	-$(dialyzer-qc) | grep -v '^ *$$' | tee $(DIALYZE_IGNORE_WARN).log | fgrep -v -f $(DIALYZE_IGNORE_WARN)

dialyze-triq-nospec: build-plt clean compile-for-triq
	-$(dialyzer-qc-nospec) | grep -v '^ *$$' | tee $(DIALYZE_NOSPEC_IGNORE_WARN).log | fgrep -v -f $(DIALYZE_NOSPEC_IGNORE_WARN)

#
# dialyzer PLT
#

build-plt: $(PLT)

check-plt: $(PLT)
	dialyzer -q --plt $(PLT) --check_plt

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

#
# rebar
#

# $ rm -rf rebar rebar.git
# $ make -f rebar.mk rebar
rebar: rebar.git
	(source ~/erlang/r15b03-1/activate && cd $(CURDIR)/rebar.git && make clean && make && cp -f rebar ..)
	$(REBAR) -V
	echo git commit -m \"Update rebar \(`./rebar -V | cut -d ' ' -f 6`\)\" rebar

rebar.git:
	rm -rf $(CURDIR)/rebar
	git clone git://github.com/basho/rebar.git rebar.git

#
# Erlang/OTP
#

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
