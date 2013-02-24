# Welcome to Hibari

Hibari is a production-ready, distributed, key-value, big-data
store (NOSQL database).  Hibari uses chain replication for strong
consistency, high-availability, and durability. Hibari has excellent
performance especially for read and large value operations.

**Product Info** (Cloudian, Inc.)

- [Hibari White Paper](http://www.cloudian.com/cloud-storage-products/white-papers/2011-08-Hibari-Whitepaper.pdf) (PDF format, 558KB)
- [Hibari NOSQL Database (Japanese)](http://www.cloudian.jp/technologies/hibari-nosql-database.html)


# News

- Feb 4, 2013 - **Hibari v0.1.10 Released**. [Release Note](https://github.com/hibari/hibari/blob/master/relnotes/v0.1.10.txt)


# Quick Start

Please read Getting Started section of Hibari Application Developer Guide.

- [English Version](http://hibari.github.com/hibari-doc/hibari-app-developer-guide.en.html#getting-started)
- [Japanese Version](http://hibari.github.com/hibari-doc/hibari-app-developer-guide.ja.html#getting-started)


# Hibari Documentation

- [English Version](http://hibari.github.com/hibari-doc/)
- [Japanese Version](http://hibari.github.com/hibari-doc/)

They are a bit **outdated**, sorry, but documentation rework is
planned for Hibari v0.6.


# Mailing Lists

- [Google Group **hibaridb-users** for English Speakers](http://groups.google.com/forum/#!forum/hibaridb-users)
- [Google Group **hibaridb-users-ja** for Japanese Speakers](http://groups.google.com/forum/#!forum/hibaridb-users-ja)

# Hibari Clients

As of Hibari v0.1 (since year 2010), only the native Erlang client is
used in production. All other client APIs (Thrift, JSON-RPC, UBF, and
S3) are still in proof of concept stage and only implement basic
operations.

Hibari v0.5 will have a full-featured Thrift client.


# Roadmap

Short-term roadmap (as of February 2013)

## v0.1 series (unstable)

Hibari v0.1.x are very early releases after forking proprietary Gemini
Distributed Storage Service (GDSS). GDSS is a rock-solid key-value
store and it has used in Tier 1 telecom sector since 2010. However
GDSS only has native Erlang client in production and also requires to
write Erlang codes to do basic system administration tasks.

Hibari v0.1.x releases contain bug fixes, updates for recent
Erlang/OTP releases (R14 and R15), prototype implementations for
non-Erlang client APIs, and Linux shell scripts to perform some of the
basic system administration tasks.

These v0.1.x releases are considered **unstable**.

- **v0.1.10** - Released on Feb 4, 2013
  * Minor bug fixes
  * Update for Erlang/OTP R15
  * Support for building on Ubuntu, including ARMv7 architecture
  * Remove S3 and JSON-RPC components from Hibari distribution (remove
    from the `repo` manifests.)  S3 and JSON-RPC will become separate
    projects and will use `rebar` to manage dependencies. (Hibari will
    continue using `repo`)


## v0.3 series (unstable)

Hibari v0.3 series will have some client API changes as well as having
Erlang sample client codes and Basho Bench driver.

These v0.3.x releases will be considered **unstable**.

- **v0.3.0**
  * **Client API changes - Server Side Timestamp**. brick_server will
    generate the timestamp for a key-value instead of client. This
    will change the return values from `add`, `replace`, and `set`
    operations.
    Link to [Updated Documentation](https://github.com/hibari/hibari-doc/blob/norton-server-rename/src/hibari/hibari-app-developer-guide.en.txt#L1424),
    Link to [GitHub Issue](https://github.com/hibari/gdss-client/issues/2)
  * **New Client API - Server Side Rename**.
    Link to [Updated Documentation](https://github.com/hibari/hibari-doc/blob/norton-server-rename/src/hibari/hibari-app-developer-guide.en.txt#L1876),
    Link to [GitHub Issue](https://github.com/hibari/gdss-brick/issues/2)
  * [Basho/Lager](https://github.com/basho/lager) as the primary logging facility. [GitHub Issue](https://github.com/hibari/hibari/issues/19)
  * Update for Erlang/OTP R16
  * The latest external libraries (ubf, ubf-thrift, riak-err)
  * Support for building and running on Joyent SmartOS (illmos based
    OpenSolaris variant)
  * Sample codes for Erlang native client (`brick_simple`)
  * Basho Bench driver


## v0.5 series (unstable)

Hibari v0.5 series will have a complete Thrift API for non-Erlang
clients.

These v0.5.x releases will be considered **unstable**.

- **v0.5**
  * **A complete Thrift API** (`do`, `get_many`, and `testset` flag
    for `set` and `replace`)
  * More `hibari` and `hibari-admin` commands (e.g. control and
    monitor scavenger)
  * Delete table operation, which is missing in v0.1 series
  * Server-side counter


## v0.6 series (stable)

Hibari v0.6 series will be considered **stable**. These releases will
have source code package and Opscode Chef cookbook for easier build
and deployment. They will also have major documentation updates and
more test cases.

- **v0.6**
  * **Source code package.** Application developers can build Hibari
    without `repo` by using this package. (Hibari developers will
    continue using `repo`.)
  * **Documentation rework**
  * More sample codes
  * Cookbook for Opscode Chef for automated single-node and multi-node
    deployments
  * More QuickCheck and EUnit test cases


## Unscheduled Features

- MapReduce (mruby? or lua?)
- SNMP support
- LETS (Google LevelDB) or HanoiDB as an alternative key storage for
  optimizing RAM usage
- Scavenger enhancements
- Big writes/reads (`append` to a value and `partial_read` from a value)
- Multi-datacenter deployment


# License

Copyright (c) 2005-2013 Hibari developers.  All rights reserved.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.


## Note for License

Hibari has decided to display "Hibari developers" as the copyright
holder name in the source code files and manuals. Actual copyright
holder names (contributors) will be listed in the AUTHORS file.


_EOF_
