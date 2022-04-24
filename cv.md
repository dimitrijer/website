---
htmltitle: Curriculum Vitae
title: 'Dimitrije Radojević'
twitter: dradojevic
github: dimitrijer
linkedin: dimitrijer
website: dimitrije.website
email: me@dimitrije.website
date: \today
---
---
nocite: |
  @*
---

# Curriculum Vitae

_(this page is also available for [download in PDF](/files/CV_Dimitrije_Radojevic.pdf))_

I am a software engineer with more than ten years of professional experience
in backend architecture and development, video game development, infrastructure
and DevOps, machine learning engineering, and, most recently, Site
Reliability Engineering.

My diverse background provides me with broad knowledge of computer systems,
from low-level hardware intricacies to high-level software abstractions, from
user-facing interfaces to transaction processing and number-crunching
batch jobs. This, along with the ability to quickly learn new concepts and
effectively map them to what I know, gives me unique versatility.  Over the
years I have come to appreciate this as my biggest strength.

I enjoy designing and developing distributed, fault-tolerant and highly
concurrent architectures. My passion is to find concise, elegant, maintainable
and performant solutions, and build them with the right tools for the job. I
have many such tools under my belt, having worked with multitude of well-known
and widely used technologies.

With a knack for explaining and teaching, and a big pool of patience and
understanding, I love sharing my knowledge and helping out. At the same time, I
communicate clearly and efficiently, and thrive in challenging environments
where I, too, can learn and grow. I am sensitive, compassionate and warm to
others, with strong sense of justice and fair play. All of the above makes me
a pretty good team player.

My hobbies include playing video and board games, reading science fiction and
fantasy, toying with SoC boards, riding my bike. I enjoy these even more in the
wonderful company of my wife and my dog.
 
# Experiences

- January 2021 - present: _SWE-SRE_, [Google](https://about.google.com)

    * My team maintains a platform for automated capacity planning[^1] that is
      used by many Google services. 

    * The objective is to provision services in line with forecasted
      demand, so that they fit within available supply, run at desired
      level of reliability, while being resource efficient at the same time.

    * Some of my day-to-day work: core SRE work -- make sure the platform runs
      reliably by applying best production practices, participate in oncall
      rotation and take part in incident management[^2], design and develop new
      features, help customers onboard to the platform, facilitate training and
      learning sessions on capacity planning in the team and beyond, perform
      the role of Scrum Master.

- March 2018 - October 2020: *Machine Learning Engineer*, [Nordeus](https://nordeus.com)

    * Part of ML/AI team with the mission of using bleeding edge machine
      learning algorithms for building unique and novel features in our games.

    * I was responsible for integrating proof-of-concept and prototype projects
      developed within the team with actual production infrastructure, while
      maintaining required level of performance and quality.

    * Also tasked with establishing and maintaining good engineering practices,
      without considerably hindering velocity of the research part of the team.

    * Designed, built, configured and maintained a machine-learning pipeline,
      which included data mining, multi-stage preprocessing, human-in-the-loop
      review process integration, feeding reviewed data to a machine learning
      algorithm, and automating testing and algorithm performance evaluation
      with regards to code changes.

    * Administered GPU cluster used for training and inference.

    * Applied classical AI algorithms, such as behaviour trees and utility
      functions, and reinforcement learning to develop several iterations of AI
      bots for [Heroic: Magic Duel](https://www.heroicgame.com/).

- February 2012 - March 2018: *Software Development Engineer*, [Nordeus](https://nordeus.com)

    * Worked as a video game developer on four different game projects,
      including Nordeus' most successful game, [Top
      Eleven](https://www.topeleven.com/).

    * Supported development of various game features on both backend and
      frontend side.

    * Designed and developed distributed login service that at its peak had ~7
      million unique daily logins.

    * Was a part of the effort to convert a legacy, monolithic backend service
      to microservice architecture.

    * Designed and built a proxy that allowed WebSocket frontends to
      communicate with backend services in proprietary binary protocol. This
      service eventually became the edge service for all clients.

    * Introduced several significant concepts to backend teams over the years,
      including key-value stores, message queues, observability in terms of
      ubiquitous metrics and distributed tracing, modern build tools and CI/CD
      pipeline, continuous code quality and code health metrics and centralized
      linting.

- October 2009 - May 2014: *Junior Teaching Assistant*, Seminar of Astronomy,
  [Petnica Science Center](http://petnica.rs)

    * Taught basics of programming to high-school students at the seminar, i.e.
      introduction to programming with C and Python, version control, working
      in terminal, writing data mining scrapers etc. Petnica Science
      Center is an independent and nonprofit organization for
      extracurricular, formal and informal science education.

# Tech Skills

- Programming languages, in order of proficiency:

  * __Java__ / __Golang__ - full professional proficiency. I did most of my
    backend development with these two. Personally, Java is my language of
    choice.

  * __C#__ - full professional proficiency. Most of my experience with C# comes
    from scripting in Unity game engine.

  * __Python__ - full professional proficiency. My favorite quick and dirty
    scripting language. Used it for developing small backend services, command
    line tools, machine learning and rapid GUI development.

  * __Clojure__ - high proficiency.

  * __C/C++__ - high proficiency.

  * Haskell - intermediate proficiency. My daydream language, I toy with it in
    hope that getting proficient in monads will give me an edge in day-to-day
    programming.

- Cloud products and providers: I have professional experience with GCS, and
  I've played with AWS and Oracle Cloud. I used various cloud offerings: VMs,
  serverless, managed K8s, managed RDBMs, managed object storage etc.

- Containers and orchestrators: I used Borg and Kubernetes in professional
  capacity. As for container runtimes, I mostly used Docker (`containerd`). I
  am familiar with primitives that make up containers, like namespaces,
  cgroups, and overlay filesystems.

- Versatile and under-the-hood knowledge of UNIX systems, especially GNU/Linux:
  usage, configuration and administration.

- Shell scripting, and broad knowledge of a plethora of command line utilities
  for manipulating and transforming data.

- Networking: understanding of L3/L4 protocols like TCP/IP and UDP, and higher
  level protocols like HTTP, HTTPS, HTTP/2, WebSockets, QUIC, DNS etc.
  Knowledge of low-level networking facilities like sockets, blocking and
  non-blocking syscalls (`select` and `epoll`), experience with well-known and
  widely used networking libraries, such as Netty, Jetty, requests, Flask,
  ring, compojure and zeromq, and debugging tools like `ss`, `netcat`, `iperf`,
  `tcpdump`, Wireshark etc. Some knowledge of network stack and its
  optimization on Linux systems.

- Multithreading primitives like mutexes, semaphores, locks, conditions, and
  high-level abstractions such as futures, promises, coroutines and async
  programming.

- IPC mechanisms such as pipes, shared memory, UNIX sockets and the MPI
  library.

- Databases:

  * __PostgreSQL__ - full professional proficiency. Thorough understanding of
    MVCC, locking, indexing, views, triggers, partitioning, stored procedures,
    as well as Postgres specific features: `plpgsql`, foreign data wrappers,
    materialized views, statistics tables (`pg_*`), advisory locks, access
    control, replication, backup, configuration, document data types, full text
    search support, functional indexes, GIN indexes etc. My database of choice,
    and one I used the most.

  * __SQLite__ - high proficiency. My favorite embeddable database. If querying
    data is any more complex than performing a dictionary lookup, this is what

  * __Redis__ - high proficiency, used for things such as game leaderboards and
    caching.

  * Aerospike, CouchDB, Cassandra, MongoDB, memcached - basic proficiency.

- Machine learning frameworks and libraries, such as numpy, scipy,
  scikit-learn, Keras and Tensorflow.

- Build tools, such as Makefiles, GNU autotools, CMake, Maven, Gradle,
  Leiningen, and msbuild/xbuild.

- Observability tooling, such as Prometheus, Icinga and Zabbix for monitoring
  and alerting, Grafana for dashboards and Jaeger and Zipkin for distributed
  tracing.

- Provisioning with Ansible and Vagrant.

- Managing infrastructure with Terraform (IaC).

- Editors and IDEs, in order of preference: Vim, Eclipse, IntelliJ IDEA, Visual
  Studio.

- Version control: Git, Mercurial, SVN.

# Education

- October 2009 - October 2016: *BSc in Computer Science*, University of
  Electrotechnics, Belgrade, Serbia

- September 2005 - Jun 2009: *High School Degree*, Mathematics Dpt., Gymnasium
  Užice, Serbia

# Languages

- English: full professional proficiency.

- German: very basic proficiency.

- Serbian: native proficiency.

# Publications

::: {#refs}
:::

[^1]: See [this chapter](https://sre.google/sre-book/software-engineering-in-sre/) in the SRE book for more info on capacity planning.
[^2]: More information about incident management at Google can be found in [another chapter](https://sre.google/sre-book/managing-incidents/) of the SRE book.
