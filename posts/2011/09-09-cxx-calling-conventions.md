---
title: C++ Calling Conventions
tags: c++, programming, compilers
date: 2011-09-09
---

My advisor presented me with an interesting problem the other day.
Calling some simple C++ free functions that took C++ objects by value
using Python's [ctypes](http://docs.python.org/library/ctypes.html
"ctypes") produced strange results.  We eventually narrowed the test
case down to the following:

~~~~~~~~~~~~~~~~~ {.cpp .numberLines}
#include <stdio.h>

struct Simple {
  int x,y,z;
};

struct Fancy {
  int x,y,z;
  ~Fancy() {
    printf("Destroying a Fancy\n");
  }
};

extern "C" {
  void printSimple(Simple s) {
    fprintf(stderr, "Entering printSimple\n");
    fprintf(stderr, "x=%d, y=%d, z=%d\n", s.x, s.y, s.z);
    fprintf(stderr, "Exiting printSimple\n");
  }

  void printFancy(Fancy f) {
    fprintf(stderr, "Entering printFancy\n");
    fprintf(stderr, "x=%d, y=%d, z=%d\n", f.x, f.y, f.z);
    fprintf(stderr, "Exiting printFancy\n");
  }
}
~~~~~~~~~~~~~~~~~

This code sample exports the two interesting functions with unmangled
names to make calling it from Python easier.  The two structs are
structurally identical.  Allocating these in Python is easy enough
using ctypes.  Calling the first one with a structure by-value works
as expected.  Calling the second prints out nonsense.  However,
calling the second with a _pointer_ to a struct works just fine.  The
raw x86_64 assembly is a bit hard to read, but a dump of the LLVM IR for
this code is slightly more enlightening:

~~~~~~~~~~~~~ {.gnuassembler .numberLines}
define void @printSimple(%"struct.<anonymous namespace>::Fancy"* byval %s) {
; ...
  ret void
}

define void @printFancy(%"struct.<anonymous namespace>::Fancy"* %f) {
; ...
  ret void
}
~~~~~~~~~~~~~

Clearly, the second function _must_ be called with the parameter
passed by pointer.  But why?  We figured that it must be some kind of
calling convention issue.  It looks like the relevant standard is the
[Itanium C++ ABI (Section 3.1.1)](http://sourcery.mentor.com/public/cxx-abi/abi.html#calls
"Itanium C++ ABI").  Nobody uses Itanium anymore, but g++ uses this
ABI for most (all?) platforms that do not define their own (including
i386 and x86_64).  Basically, if a C++ object has any non-trivial
constructor or destructor, the _caller_ allocates the copy of the
object being passed by value.  When making the call it has to pass the
address of this local temporary to the callee.  Presumably this
simplifies exception handling and unwinding.  This was a bit
surprising and adds a whole new set of complexities to calling C++
from other languages.
