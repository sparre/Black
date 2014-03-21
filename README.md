Black [1]
=========

A simple HTTP and Websocket library written in Ada with minimal external dependencies.

We constrain ourselves to use only Ada 2005 (as Debian/stable doesn't have a good Ada 2012 compiler yet).

You can find examples (well, right now only one) for Black in the examples repository [5].


Dependencies
------------

- GNU Make
  + for build management.
- GNAT
  + as we haven't written build scripts for other compilers yet.
- Ahven
  + for testing.
- libesl
  + for URL encoding and decoding.
- Bash
  + for managing test cases.
- Zsh
  + used to implement some tests.


Requirements
------------

Requirements for the library are kept as [issues](https://github.com/AdaHeads/Black/issues?labels=requirement) under the project on GitHub.


Included tests
--------------

1. 'cyclomatic_complexity' - reports excessive cyclomatic complexities in the
   project sources.
2. 'unused_units' - warns about units which aren't compiled.
3. 'ahven' - unit tests.
4. 'task_interfaces_and_extended_return' - testing for a bug in GNAT.


Links
-----

If you want to find free Ada tools or libraries AdaIC [2] is an excellent
starting point.  You can also take a look at the other source text
repositories belonging to AdaHeads K/S [3] or those belonging to Jacob
Sparre Andersen [4].

1. Source text repository:
   https://github.com/AdaHeads/Black
2. Free Ada Tools and Libraries:
   http://www.adaic.org/ada-resources/tools-libraries/
3. Our repositories on GitHub:
   https://github.com/AdaHeads
4. Jacob Sparre Andersen's repositories on Bitbucket:
   http://repositories.jacob-sparre.dk/
5. Examples for Black:
   https://github.com/AdaHeads/Black-examples

