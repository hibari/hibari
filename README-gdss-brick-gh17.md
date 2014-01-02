## README gdss-brick >> GH17 - Redesign Disk Storage and Checkpoint/Scavenger Processes

This is a short memo about how to build Hibari from a topic branch
**gbrick-gh17-redesign-disk-storage**.

https://github.com/hibari/gdss-brick/issues/17


### Erlang/OTP R16B

Erlang/OTP R16B03 is recommended. R16A or newer is required to build
the LevelDB port driver.


### Dependencies

Use the manifest from the dev branch (`-b dev`). Then manually
checkout branches as the following.

```console
$ cd ~/workhub/dev/hibari/hibari
$ repo forall -c "git branch dev origin/dev"
$ repo forall -c "git checkout dev"
$ git checkout gbrick-gh17-redesign-disk-storage
$ (cd lib/gdss_brick; git checkout gbrick-gh17-redesign-disk-storage)
$ (cd lib/gdss_admin; git checkout hibari-gh33-scavenger-bad-seq)
$ (cd lib/asciidoc; git checkout upstream-master)
$ (cd lib/edown; git checkout upstream-master)
$ (cd lib/meck; git checkout upstream-master)
$ (cd lib/qc; git checkout upstream-master)
$ (cd lib/ubf; git checkout upstream-master)
$ (cd lib/ubf_thrift; git checkout upstream-master)

$ repo status
project hibari/                                 branch gbrick-gh17-redesign-disk-storage
project hibari-doc/                             branch dev
project hibari/lib/asciiedoc/                   branch upstream-master
project hibari/lib/cluster_info/                branch dev
project hibari/lib/congestion_watcher/          branch dev
project hibari/lib/edown/                       branch upstream-master
project hibari/lib/gdss_admin/                  branch hibari-gh33-scavenger-bad-seq
project hibari/lib/gdss_brick/                  branch gbrick-gh17-redesign-disk-storage
project hibari/lib/gdss_client/                 branch dev
project hibari/lib/gdss_ubf_proto/              branch dev
project hibari/lib/gmt_util/                    branch dev
project hibari/lib/lager/                       branch dev
project hibari/lib/meck/                        branch upstream-master
project hibari/lib/partition_detector/          branch dev
project hibari/lib/qc/                          branch upstream-master
project hibari/lib/ubf/                         branch upstream-master
project hibari/lib/ubf_thrift/                  branch upstream-master
project patches/                                branch dev
```

Add the following projects (git repositories) under the lib directory.
Note that **leveldb_edrv** is not open-sourced yet.

```console
$ cd ~/workhub/dev/hibari/hibari/lib

$ git clone git://github.com/ubf/pmod_transform.git
$ git clone git://github.com/norton/leveldb.git
$ git clone git://github.com/norton/snappy.git
$ git clone git://github.com/uwiger/sext.git

# Not publicly available as of January 2nd, 2014.
$ git clone git://github.com/tatsuya6502/leveldb_edrv
```

### Set the console.log to debug level

```console
$ cd ~/workhub/dev/hibari/hibari
$ cat debug-log.patch
```

```diff
diff --git a/rel/files/sys.config b/rel/files/sys.config
index b09476f..16970b9 100644
--- a/rel/files/sys.config
+++ b/rel/files/sys.config
@@ -38,7 +38,7 @@
     [{lager_console_backend, info},
      {lager_file_backend,
       [{"log/error.log", error, 10485760, "$D0", 5},
-       {"log/console.log", info, 10485760, "$D0", 5}
+       {"log/console.log", debug, 10485760, "$D0", 5}
       ]}
     ]},
    %% Whether to write a crash log, and where. Undefined means no crash logger.
```

```console
$ git apply debug-log.patch
```


### Build and Run

Just as usual.

```console
$ cd ~/workhub/dev/hibari/hibari
$ make
$ make bootstrap-package
...

$ ./tmp/hibari/bin/hibari ping
pong
```


### Basho Bench

#### Setup

**TODO**

```console
$ ls -l ~/workhub/bashobench/
total 8
drwxr-xr-x 10 tatsuya users 4096 Dec 31 23:26 basho_bench
drwxr-xr-x  6 tatsuya users 4096 Dec 31 23:16 hibari

$ cd basho_bench
$ git remote -v
origin	git@github.com:basho/basho_bench.git (fetch)
origin	git@github.com:basho/basho_bench.git (push)

$ git branch -v
* master 5fa4fee Merge pull request #102 from basho/cd-riakc_pb_content_type

$ cd ../hibari/hibari
$ repo status
project hibari/                                 branch hibari-gh26-basho-bench
 -m     tools/basho_bench_driver/examples/hibari-native.config
project hibari-doc/                             branch dev
project hibari/lib/asciiedoc/                   branch upstream-master
project hibari/lib/cluster_info/                branch dev
project hibari/lib/congestion_watcher/          branch dev
project hibari/lib/edown/                       branch dev
project hibari/lib/gdss_admin/                  branch hibari-gh33-scavenger-bad-seq
project hibari/lib/gdss_brick/                  branch gbrick-gh17-redesign-disk-storage
project hibari/lib/gdss_client/                 branch dev
project hibari/lib/gdss_ubf_proto/              branch dev
project hibari/lib/gmt_util/                    branch dev
project hibari/lib/lager/                       branch dev
project hibari/lib/meck/                        branch dev
project hibari/lib/partition_detector/          branch dev
project hibari/lib/qc/                          branch upstream-master
project hibari/lib/ubf/                         branch upstream-master
project hibari/lib/ubf_thrift/                  branch upstream-master
project patches/                                branch dev

$ git diff  tools/basho_bench_driver/examples/hibari-native.config
```

```diff
diff --git a/tools/basho_bench_driver/examples/hibari-native.config b/tools/basho_bench_driver/examples/hibari-native.
index b4546e7..c1c4056 100644
--- a/tools/basho_bench_driver/examples/hibari-native.config
+++ b/tools/basho_bench_driver/examples/hibari-native.config
@@ -6,19 +6,22 @@

 %% {duration, 60}.
 %% {duration, 30}.
-{duration, 1}.
+{duration, 360}.

 %%% Operation rate. 'max' or '{rate, OperationsPerSecond}'
 %% {mode, max}.
-{mode, {rate, 50}}.
+%% {mode, {rate, 50}}.
+{mode, {rate, 5}}.

 %%% Hibari client settings
 {hibari_admin_nodes, ['hibari@127.0.0.1']}.
 {hibari_brick_nodes, ['hibari@127.0.0.1']}.
 {hibari_table, perf1}.
+%% {hibari_table, tab1}.

 %%% Keys
-{key_generator, {int_to_bin_bigendian, {uniform_int, 1000000}}}.
+%% {key_generator, {int_to_bin_bigendian, {uniform_int, 1000000}}}.
+{key_generator, {int_to_bin_bigendian, {uniform_int, 1000}}}.

 %%% Values
 {value_generator, {fixed_bin,     100}}. %%  100 Byts
@@ -33,7 +36,7 @@
               , {add, 5}
               , {replace, 5}
               , {set, 10}
-              , {rename, 5}  %% Since Hibari v0.3.0
+              %% , {rename, 5}  %% Since Hibari v0.3.0
               , {delete, 5}
              ]}.

@@ -46,11 +49,11 @@
 %%%
 {driver, basho_bench_driver_hibari_native}.

-{code_paths, ["/home/tatsuya/workhub/dev/hibari/hibari/tools/basho_bench_driver",
-              "/home/tatsuya/workhub/dev/hibari/hibari/lib/cluster_info",
-              "/home/tatsuya/workhub/dev/hibari/hibari/lib/gmt_util",
-              "/home/tatsuya/workhub/dev/hibari/hibari/lib/gdss_brick",
-              "/home/tatsuya/workhub/dev/hibari/hibari/lib/gdss_client"
+{code_paths, ["/home/tatsuya/workhub/bashobench/hibari/hibari/tools/basho_bench_driver",
+              "/home/tatsuya/workhub/bashobench/hibari/hibari/lib/cluster_info",
+              "/home/tatsuya/workhub/bashobench/hibari/hibari/lib/gmt_util",
+              "/home/tatsuya/workhub/bashobench/hibari/hibari/lib/gdss_brick",
+              "/home/tatsuya/workhub/bashobench/hibari/hibari/lib/gdss_client"
              ]}.

 %%% value_generator_source_size must be larger than
```

#### Run

Create perf1 table.

```console
$ cd ~/workhub/dev/hibari/hibari
$ ./tmp/hibari/bin/hibari ping
pong
$ ./tmp/hibari/bin/hibari-admin create-table perf1 -bigdata -disklogging -local 3
```

Run Basho Bench.

```console
$ cd ~/workhub/bashobench/basho_bench
$ ./basho_bench -N 'hibaribench@127.0.0.1' ../hibari/hibari/tools/basho_bench_driver/examples/hibari-native.config
```


### Metadata DB Tool

```console
$ cd ~/workhub/dev/hibari/hibari
$ ./tmp/hibari/bin/hibari ping
pong
$ ./tmp/hibari/bin/hibari attach
Attaching to /home/tatsuya/workhub/dev/hibari/hibari/tmp/hibari/tmp/erlang.pipe.1 (^D to exit)

(hibari@127.0.0.1)1>
```

```erl
(hibari@127.0.0.1)1> gmt_hlog_common:tool_list_md(perf1_ch1_b1, 10).
1: --------------------
MDKey: <<16,0,0,0,2,18,152,203,224,16,8,4,16,8,8,255,255,255,254,128,127,255,113,24,46,58,209,51,153,0,8,255>>
Metadata: {<<49,47,0,0,0,4>>,
           1388659321851085,
           {-66,463733},
           100,
           [{md5,<<229,39,56,164,140,152,170,237,90,186,69,204,99,47,202,31>>}]}
2: --------------------
MDKey: <<16,0,0,0,2,18,152,203,224,16,8,4,20,8,8,255,255,255,254,128,127,255,113,24,46,59,19,200,202,0,8,255>>
Metadata: {<<49,47,0,0,0,5>>,
           1388659319650155,
           {-66,461619},
           100,
           [{md5,<<112,172,52,10,176,178,86,121,35,187,108,105,215,138,245,
                   244>>}]}
...

ok
(hibari@127.0.0.1)2> ^D
$
```
