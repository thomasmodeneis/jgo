The JGo Project aims to provide a complete compiler and runtime environment for the Go programming language to/on the Java Virtual Machine.  The `jgoc` compiler is written in Scala and the runtime in Java.

JGo is copyright 2011–2012 by Harrison Klaperman and is provided under the [GNU General Public License, version 3](http://www.gnu.org/licenses/gpl-3.0.html).

(Note: This project is on indefinite hiatus and has not been worked on in several years. I've stopped work on JGo for three reasons: first, I felt as though I had learned almost all of what I set out to learn when I started (this was originally an independent study project for my senior year of high school); second, the remaining work mostly would have involved extraordinary contortions to map Go's freewheeling constructions (e.g., interfaces) onto the rigid JVM; and third, there was only moderate interest in a JVM implementation of Go. It's possible that one or both of the latter two points have changed in the past few years. I'm still around, so if you're strongly interested in this project or related things, feel free to contact me, though I cannot guarantee I will be able to commit resources. The rest of this page has been left as it was before I decided to suspend work.)

---


# Status of the Project #
JGo is a work in progress.  Many features of the Go programming language are not yet fully supported.  Currently, these include:
  * Structs;  _(nearing completion)_
  * Methods;
  * Packages and multi-file compilation;
  * Interfaces;
  * Lambda expressions;
  * Closures (sub-feature of lambda expressions);
  * Concurrency;
  * `defer`, `panic`, and `recover`; and
  * Switch statements.
I have spent a lot of time laying down foundations, and am confident in my goal for a fully conformant implementation of Go on the JVM.

# Why the JVM? #
My selection of the JVM as the target for my compiler does _not_ reflect any thesis about the performance of the platform.  Rather, it was motivated by these two reasons:
  1. Compatibility. A lot of excellent libraries are written for the JVM.  JGo hopes to make existing Go users more productive by enabling them to use these.  Furthermore, many groups have large codebases written in Java, Scala, and other JVM languages.  JGo is an attempt to bring these groups into the fold and expose them to Go.
  1. There is no need for yet another Go implementation targeting native code.