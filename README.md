# Welcome to Hibari

Hibari is a production-ready, distributed, key-value, big-data
store (NOSQL database).  Hibari uses chain replication for strong
consistency, high-availability, and durability. Hibari has excellent
performance especially for read and large value operations.

**Product Info** (Cloudian, Inc.)

- [Hibari White Paper](http://www.cloudian.com/cloud-storage-products/white-papers/2011-08-Hibari-Whitepaper.pdf) (PDF Format, 558KB)
- [Hibari NOSQL Database (Japanese)](http://www.cloudian.jp/technologies/hibari-nosql-database.html)


# News

- Feb 4, 2013 - Hibari v0.1.10 Released. [Release Note](https://github.com/hibari/hibari/blob/master/relnotes/v0.1.10.txt)


# Quick Start

Please read Getting Started section of Hibari Application Developer Guide.

- [English Version](http://hibari.github.com/hibari-doc/hibari-app-developer-guide.en.html#getting-started)
- [Japanese Version](http://hibari.github.com/hibari-doc/hibari-app-developer-guide.ja.html#getting-started)


# Hibari Documentation

- [English Version](http://hibari.github.com/hibari-doc/)
- [Japanese Version](http://hibari.github.com/hibari-doc/)


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

- v0.1.10
  * Minor bug fixes
  * Update for Erlang/OTP R15
  * Support for building on Ubuntu, including ARMv7 architecture
  * Move S3 and JSON-RPC components out from Hibari distribution
    (remove from the `repo` manifests.)  S3 and JSON-RPC will become
    separate projects and will use `rebar` to manage
    dependencies. (Hibari will continue using `repo`)


## v0.5 series (unstable)

- v0.5
  * A complete Thrift API (`do`, `get_many`, and `testset` flag for
    `set` and `replace`)
  * More `hibari-admin` commands (e.g. control and monitor scavenger)
  * Delete table operation
  * Server-side timestamp and rename (Server-side timestamp is
    required by Thrift API)
  * Server-side counter
  * The latest external libraries (ubf, ubf-thrift, riak-err)
  * Basho Bench driver


## v0.6 series (stable)

- v0.6
  * Source code package. Application developers can build Hibari
    without `repo` by using this package. (Hibari developers will
    continue using `repo`.)
  * Documentation rework
  * Sample codes
  * Cookbook for Opscode Chef for automated single-node and multi-node
    deployments
  * More QuickCheck and EUnit test cases


## Unscheduled Features

- MapReduce (mruby?)
- SNMP support
- LETS (Google LevelDB) or HanoiDB as an alternative key storage for
  optimizing RAM usage
- Scavenger enhancements
- Big writes/reads (`append` to a value and `partial_read` from a value)


## License

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
name in the source code files and manuals. Actual copyright holder
names (contributors) will be listed in the AUTHORS file.


_EOF_
