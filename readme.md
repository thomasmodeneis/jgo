Description
===========

![JGO](http://jgo.herokuapp.com/images/jgo3.png) The [JGo Project](http://jgo.herokuapp.com/) aims to provide a complete compiler and runtime environment for the Go programming language to/on the Java Virtual Machine.
The jgoc compiler is written in Scala and the runtime in Java.


Drone.io continuous integration service
---------------------------------------

[![Build Status](https://drone.io/github.com/thomasmodeneis/jgo/status.png)](https://drone.io/github.com/thomasmodeneis/jgo/latest)


Gitter Chat
-----------
[![Gitter](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/thomasmodeneis/jgo?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge)

Why the JVM?
============

The selection of the JVM was motivated by:

Compatibility & JVM debug/profiling
------------------------------------

A lot of excellent libraries are written for the JVM. 
[JGo](http://jgo.herokuapp.com/) hopes to make existing Go users more productive by enabling them to use these. 
Furthermore, many groups have large codebases written in Java, Scala, and other JVM languages. 
[JGo](http://jgo.herokuapp.com/) is an attempt to bring these groups into the fold, group and expose them.

![JGO](http://jgo.herokuapp.com/images/banner_jgo.png)

Status of the Project
=====================

JGo is a work in progress. 
Many features of the Go programming language are not yet fully supported. 

Currently, these include:

* Structs; (nearing completion)
* Methods;
* Packages and multi-file compilation;
* Interfaces;
* Lambda expressions;
* Closures (sub-feature of lambda expressions);
* Concurrency;
* defer, panic, and recover; and Switch statements.

DOCS
=====

[JGO-DOCS](http://jgo.herokuapp.com/api/)


Contribute
==========

Contributing to JGO
=========================================

JGO welcomes contributions, feel free to play and pull requests :)

Issues
------

Feel free to submit issues and enhancement requests.

Contributing
------------

Please refer to each project's style guidelines and guidelines for submitting patches and additions. In general, we follow the "fork-and-pull" Git workflow.

 1. Fork the repo on GitHub
 2. Raise a issue on our issue tracker
 3. Commit changes to a branch in your fork using the issue, eg: git commit -m "#1"
 4. Pull request with your changes
 5. Thank you o/

NOTE: Be sure to merge the latest from "upstream" before making a pull request!

This project contains a Ubuntu 12.04 Sandbox, a vagrant [Vagrant](http://vagrantup.com/)- based Puppet
development environment used for running and testing JGo with the OpenJDK 8 modules.



OpenJDK
========

![OpenJDK](https://soujavablog.files.wordpress.com/2015/05/openjdk.jpg)
JGO Uses OpenJDK 8.


Requirements
============

To use this, you must have the following items installed and working:

* [VirtualBox](https://www.virtualbox.org/)
* [Vagrant 1.7+](http://vagrantup.com/)

Usage
=====

Initial Startup
---------------

To bring up the Puppet Sandbox environment, issue the following command:

```vagrant up```

Puppet will install Git and OpenJDK 8.

```vagrant ssh```

Run SBT
-------

When the shell command for vagrant is ready run sbt

```
$ cd /opt/jgo/
$ sbt
Getting org.scala-sbt sbt 0.13.8 ...
[info] Set current project to jgo (in build file:/opt/jgo/)
```

```
> run hello.go package.class
[info] Updating {file:/opt/jgo/}jgo...
[info] Resolving org.fusesource.jansi#jansi;1.4 ...
[info] Done updating.
```

Run SBT Command
---------------

When the command line for sbt shows type run hello.go package.class
This will compile and execute the following go code into the JDK8:
```
func say(msg string) {
	print msg
}
func main(args [4]string) {
	msg := "hello, world"
	say(msg)
	
	for i := 0; i < 10; i++ {
		print i
	}
}
```


Output:
```
> run hello.go package.class
[info] Updating {file:/opt/jgo/}jgo...
(...)
[info] Compiling 132 Scala sources and 17 Java sources to /opt/jgo/target/scala-2.10/classes...
(...)
[info] Running jgo.tools.compiler.Main hello.go package.class
LoadVar <msg: string>
PrintString
Decl <msg: string>
PushStr "hello, world"
StoreVar <msg: string>
LoadVar <msg: string>
InvokeFunction Function(say, func(string))
Decl <i: int>
PushInt 0, I32
StoreVar <i: int>
goto:  [cond of for 5]
Lbl top of for 5
LoadVar <i: int>
PrintNumeric I32
Lbl continue 5
Incr <i: int>, 1, I32
Lbl cond of for 5
LoadVar <i: int>
PushInt 10, I32
if < I32:  [top of for 5]
Lbl break 5
Undecl <i: int>
Undecl <msg: string>
```

License
=======


GNU 3
-----

GNU General Public License, version 3.


Project migrated from https://code.google.com/p/jgo/ by  Harrison Klaperman.







