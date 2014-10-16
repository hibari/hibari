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

- [**More News**](https://github.com/hibari/hibari/wiki/Hot-News)


# Quick Start

Please read **Getting Started** section of Hibari Application
Developer Guide.

- [English Version](http://hibari.github.com/hibari-doc/hibari-app-developer-guide.en.html#getting-started)
- [Japanese Version](http://hibari.github.com/hibari-doc/hibari-app-developer-guide.ja.html#getting-started)


# Hibari Documentation

- [English Version](http://hibari.github.com/hibari-doc/)
- [Japanese Version](http://hibari.github.com/hibari-doc/)

They are a bit **outdated** -- sorry, but documentation rework is
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


# Supported Platforms

Hibari is written in pure Erlang/OTP and runs on many Unix/Linux
platforms.

Please see
the [**Supported Platforms**](https://github.com/hibari/hibari/wiki/Supported-Platforms)
page in Hibari Wiki for details.


# Roadmap

Please see
the [**Roadmap**](https://github.com/hibari/hibari/wiki/Roadmap) page
in Hibari Wiki for the planned features for Hibari v0.3, v0.5, and v0.6.


# License

Copyright (c) 2005-2014 Hibari developers.  All rights reserved.

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
