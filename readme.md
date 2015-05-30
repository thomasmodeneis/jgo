
Description
===========

The JGo Project aims to provide a complete compiler and runtime environment for the Go programming language to/on the Java Virtual Machine. 
The jgoc compiler is written in Scala and the runtime in Java.

Project migrated from https://code.google.com/p/jgo/ by Harrison Klaperman.


Why the JVM?
============

The selection of the JVM was motivated by these two reasons:

Compatibility & JVM debug/profiling
------------------------------------

A lot of excellent libraries are written for the JVM. 
JGo hopes to make existing Go users more productive by enabling them to use these. 
Furthermore, many groups have large codebases written in Java, Scala, and other JVM languages. 
JGo is an attempt to bring these groups into the fold and expose them to Go.

Not another golang native compiler
------------------------------------

There is no need for yet another Go implementation targeting native code.

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

License
=======
GNU General Public License, version 3.
