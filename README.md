## Welcome to Hibari

### A Distributed, Consistent, Ordered Key-Value Store

Hibari is a distributed, ordered key-value store with strong
consistency guarantee. Hibari is written in Erlang and designed for
being:

- **Fast, Read Optimized:** Hibari serves read and write requests in
  short and predictable latency. Hibari has excellent performance
  especially for read and large value operations

- **High Bandwidth:** Batch and lock-less operations help to achieve
  high throughput while ensuring data consistency and durability

- **Big Data:** Can store Peta Bytes of data by automatically
  distributing data across servers. The largest production Hibari
  cluster spans across 100 of servers

- **Reliable:** High fault tolerance by replicating data between
  servers. Data is repaired automatically after a server failure

Hibari is able to deliver scalable high performance that is
competitive with leading open source NOSQL (Not Only SQL) storage
systems, while also providing the data durability and strong
consistency that many systems lack. Hibari's performance relative to
other NOSQL systems is particularly strong for reads and for large
value (> 200KB) operations.

As one example of real-world performance, in a multi-million user
webmail deployment equipped with traditional HDDs (non SSDs), Hibari
is processing about 2,200 transactions per second, with read latencies
averaging between 1 and 20 milliseconds and write latencies averaging
between 20 and 80 milliseconds.


### Distinct Features

Unlike many other distributed databases, Hibari uses "*chain
replication methodology*" and delivers distinct features.

- **Ordered Key-Values:** Data is distributed across "chains" by key
  prefixes, then keys within a chain are sorted by lexicographic order

- **Always Guarantees Strong Consistency**: This simplifies creation
  of robust client applications

  * **Compare and Swap (CAS):** key timestamping mechanism that
    facilitates "test-and-set" type operations
  * **Micro-Transaction:** multi-key atomic transactions, within
    range limits

- **Custom Metadata**: per-key custom metadata
- **TTL (Time To Live)**: per-key expiration times


## Travis CI Status

http://travis-ci.org/hibari/hibari-ci-wrapper

Branch                            | Erlang/OTP Versions  | Status                              | Remarks
--------------------------------- | -------------------- | ----------------------------------- | ------------
master                            | 17.5, R16B03-1       | ![master](https://travis-ci.org/hibari/hibari-ci-wrapper.svg?branch=master) |
dev                               | 18.1, 17.5, R16B03-1 | ![dev](https://travis-ci.org/hibari/hibari-ci-wrapper.svg?branch=dev) |
hibari-gh54-thrift-api            | 18.1, 17.5, R16B03-1 | ![hibari-gh54-thrift-api](https://travis-ci.org/hibari/hibari-ci-wrapper.svg?branch=hibari-gh54-thrift-api)|
gbrick-gh17-redesign-disk-storage | 18.1, 17.5           | ![gbrick-gh17-redesign-disk-storage](https://travis-ci.org/hibari/hibari-ci-wrapper.svg?branch=gbrick-gh17-redesign-disk-storage) | no tests, compile only


## News

- Apr 5, 2015 - **Hibari v0.1.11 Released**. [Release Notes](https://github.com/hibari/hibari/blob/master/relnotes/v0.1.11.txt)
  * Update for Erlang/OTP 17 and R16. (Note: Erlang/OTP releases prior
    to R16 are no longer supported)
  * Update external libraries such as UBF to the latest versions
  * Enhanced client API: server side rename and server side timestamp
  * New logging format. Introduce Basho Lager for more traditional
    logging that plays nicely with Unix logging tools like logrotate and
    syslog

- Feb 4, 2013 - **Hibari v0.1.10 Released**. [Release Notes](https://github.com/hibari/hibari/blob/master/relnotes/v0.1.10.txt)
  * A bug fix in Python EBF Client
  * Update for Erlang/OTP R15
  * Support for building on Ubuntu, including ARMv7 architecture
  * Remove S3 and JSON-RPC components from Hibari distribution. They
    will become separate projects

- [**Older News**](https://github.com/hibari/hibari/wiki/Hot-News)


## Quick Start

Please read **Getting Started** section of Hibari Application
Developer Guide.

- [English Version](http://hibari.github.com/hibari-doc/hibari-app-developer-guide.en.html#getting-started)
- [Japanese Version](http://hibari.github.com/hibari-doc/hibari-app-developer-guide.ja.html#getting-started)


## Hibari Documentation

- [English Version](http://hibari.github.com/hibari-doc/)
- [Japanese Version](http://hibari.github.com/hibari-doc/)

They are a bit **outdated** -- sorry, but documentation rework is
planned for Hibari v0.6.


## Mailing Lists

- [Google Group **hibaridb-users** for English Speakers](http://groups.google.com/forum/#!forum/hibaridb-users)
- [Google Group **hibaridb-users-ja** for Japanese Speakers](http://groups.google.com/forum/#!forum/hibaridb-users-ja)


## Hibari Clients

As of Hibari v0.1 (since year 2010), only the native Erlang client is
used in production. All other client APIs (Thrift, JSON-RPC, UBF, and
S3) are still in proof of concept stage and only implement basic
operations.

If you need a client library for other programming language, please
feel free to post a request at the
[Hibari mailing list](http://groups.google.com/forum/#!forum/hibaridb-users).


## Supported Platforms

Hibari is written in pure Erlang/OTP and runs on many Unix/Linux
platforms.

Please see
the [**Supported Platforms**](https://github.com/hibari/hibari/wiki/Supported-Platforms)
page in Hibari Wiki for details.


## Roadmap

Please see
the [**Roadmap**](https://github.com/hibari/hibari/wiki/Roadmap) page
in Hibari Wiki for the planned features for Hibari v0.3, v0.5, and v0.6.


## Hibari's Origins

Hibari was originally written by Cloudian, Inc., formerly Gemini
Mobile Technologies, to support mobile messaging and email services.
Hibari was open-sourced under the Apache Public License version 2.0 in
July 2010.

Hibari has been deployed by multiple telecom carriers in Asia and
Europe. Hibari may lack some features such as monitoring, event and
alarm management, and other "production environment" support services.
Since telecom operator has its own data center support infrastructure,
Hibari's development has not included many services that would be
redundant in a carrier environment.

We hope that Hibari's release to the open source community will close
those functional gaps as Hibari spreads outside of carrier data
centers.


### What does Hibari mean?

The word "Hibari" means skylark in Japanese; the Kanji characters
stand for **"cloud bird"**.


## License

```
Copyright (c) 2005-2017 Hibari developers.  All rights reserved.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
```


### Note for License

Hibari has decided to display "Hibari developers" as the copyright
holder name in the source code files and manuals. Actual copyright
holder names (contributors) will be listed in the AUTHORS file.


_EOF_
