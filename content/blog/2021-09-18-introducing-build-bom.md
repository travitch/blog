+++
title = "Introducing build-bom"
date = 2021-09-18
[taxonomies]
tags = ["llvm", "analysis", "build-systems", "rust", "build-bom"]
+++

# TL;DR

The [build-bom](https://github.com/travitch/build-bom) tool makes it easy to obtain LLVM IR for C/C++ programs without requiring any modifications to build systems by using the low-level debugger primitive `ptrace`. This is something you may want if you build tools to analyze C/C++ programs. A typical use of `build-bom` looks like:

```bash
  ./configure
  build-bom generate-bitcode -- make
  build-bom extract-bitcode /path/to/binary --output binary.bc
```

# Background

## What is LLVM IR?

LLVM IR is the intermediate representation of code used by LLVM. The IR is a form of three address code that can be thought of as an abstract load/store machine with unlimited virtual registers. It is both straightforward to generate from many imperative languages and amenable to the analyses and transformations necessary for an optimizing compiler.

Note that this is often referred to as LLVM Bitcode. To be precise, the bitcode is the format of a data stream that is used to represent LLVM IR. The distinction is slightly academic, as the LLVM bitcode format is not used for anything besides LLVM IR. I will refer to it as LLVM IR in this post.

## Why LLVM IR?

There are, presumably, many things one can do with programs represented in LLVM IR. I will focus on the needs of program analysis (e.g., static analysis and verification) tools, which analyze programs to learn things about them.  By virtue of maintaining [Static Single Assignment (SSA) form](https://en.wikipedia.org/wiki/Static_single_assignment_form), it is especially amenable to static analysis. Common program analysis tasks include finding bugs or proving that programs satisfy some specification (e.g., proving the absence of memory errors).

While analyzing C and C++ source directly would be ideal{% sidenote() %}
It would be worth devoting an entire post to some of the nuanced differences between source-level analysis and analysis of LLVM IR.
{% end %}, it is very difficult in practice due to the complexity of parsing and semantically analyzing C (and particularly C++). By analyzing programs represented in LLVM IR instead{% sidenote() %}
One might ask: why LLVM IR over one of the nearly countless other compiler IRs out there? The LLVM project has a thriving community with many contributors, as well as one of the most mature C/C++ frontends available. It has also maintained a reasonably high degree of compatibility over the years (at least at the LLVM IR level). There are also libraries in many different programming languages for reading and creating LLVM IR.
{% end %}, tools are able to reuse the parser and semantic analysis of the Clang compiler, which has the benefit of many thousands of person hours of engineering. Beyond simplifying parsing, the number of constructs in LLVM IR (i.e., the number of instructions) is significantly smaller than the number of syntactic constructs in C. This simpler representation, which still precisely captures the semantics of the program, in turn simplifies the implementation of program analysis tools.

## Where does LLVM IR Come From?

To turn a C source file into LLVM IR, one usually uses `clang` with the `-emit-llvm` argument:

```bash
clang -emit-llvm -c foo.c -o foo.bc
```

That works well for a single source file, but creating LLVM IR for an entire program is more complicated. In fact, it is as complicated as the build system for the program, which can be arbitrarily complicated. In simple cases, one could modify the build system to generate LLVM IR instead of object files and then combine them into a single larger LLVM IR file using `llvm-link`. Ideally, there would be a tool to make the process easier.

## Challenging Build Features

Before looking at possible implementations of a tool to generate LLVM IR for a C/C++ codebase, it is helpful to think about the constraints that such a tool should satisfy. These constraints are embodied by challenging features of build systems that appear in the wild.

1. **There is no standard build tooling for C/C++.** Since there is no standard build system, every C/C++ project invents its own build system. While there are a number of common build systems (e.g., autotools and `cmake`), every instance of these build systems has its own unique quirks. _A general LLVM IR generator cannot assume anything about the build system._
2. **Build systems can invoke build intermediate executable artifacts.** This case is not common, but arises for projects that build an intermediate code generator that generates additional source files. These intermediate binaries are necessary and must work, or the build will fail. _A general LLVM IR generator must generate executable artifacts faithfully._
3. **Build systems can delete intermediate files and directories.** Some build systems, particularly `cmake`, aggressively discard intermediate files and directories. _A general LLVM IR generator must not leave LLVM IR in the build tree._
4. **Build systems can be configured via environment variables (except when they cannot)** Some build systems, like `cmake`, can be configured through environment variables (e.g., `CC`). However, others reset environment variables either to achieve hermetic builds (or incidentally). _A general LLVM IR generator cannot rely on being able to pass environment variables to all stages of a build process._
5. **Build systems can invoke other build systems at build time.** While recursive make is a famous and often maligned example, the reality of build systems in the wild can be even worse. For example, there are a number of build systems based on autotools that invoke other autotools build systems (i.e., `configure` scripts) at build time{% sidenote() %}
I remember encountering this when attempting to generate LLVM IR for gdb, which contains a few sub-libraries with their own configure scripts.
{% end %}.  It is usually very difficult to control the build flags passed to those recursively-invoked build systems, as they are not designed for customization. _A general LLVM IR generator cannot require passing arguments to the build system._
6. **Configure scripts are very clever.** Not all `configure` scripts (or variants for other build systems) are overly clever; however, some go to extraordinary lengths to fingerprint the system compiler. _A general LLVM IR generator cannot fool a configure script into believing it is the system compiler without actually being the system compiler._
7. **Build systems can hard-code absolute paths to tools.** While it is not common, the configure stage of a build system may pick up absolute paths to build tools, which can become difficult to modify after configuration time. _A general LLVM IR generator cannot rely on substituting build tools post-configuration._
8. **Build systems may be externally-orchestrated.** Some projects with complex dependencies or complex run-time setup requirements{% sidenote() %}
The build system for the [NASA F' (F prime) system](https://github.com/nasa/fprime) is orchestrated by a number of python scripts that aggressively regenerate the build system after any changes. This is convenient for development, but is a pain when attempting to modify the build system temporarily.
{% end %} provide orchestration scripts. Those scripts can aggressively reconfigure the build system, making it difficult to make spot modifications to substitute non-standard build tools. _A general LLVM IR generator cannot rely on being able to modify the build system._
9. **Build system binaries can be statically linked.** Build systems written in Golang or running on a fully statically linked distribution are more difficult to interpose upon. _A general LLVM IR generator cannot rely on `LD_PRELOAD` hooks._

## Related Tools

With some background on the requirements imposed on an LLVM IR generator by the challenges of real build systems, it is useful to look at the other tools in the ecosystem that try to solve this problem.


### Clang Compilation Database

The `clang` compiler natively supports maintaining a _compilation database_, which records all of the commands used in a build. See the `-MJ` command line option and the [Compilation Database documentation](https://clang.llvm.org/docs/JSONCompilationDatabase.html) for details. Some build systems, like `cmake`, provide native support for generating a compilation database. For well-behaved builds, one can easily write a script to replay the compilation database and generate LLVM IR for all of the input files.

This approach is elegant, but only works for build systems that are already compatible with `clang` _and_ that do not dynamically generate/delete source files. Dynamic source generation can be problematic because entries in the database may not exist after a build completes, so replaying the build accurately can be impossible.

### Clang Link Time Optimization (LTO)

The `clang` compiler also now supports Link Time Optimization (LTO), which enables whole-program optimization by causing `clang` to populate object files with LLVM IR rather than native machine code. It then performs whole-program optimization at link time over a combined whole-program LLVM IR file. This is exactly what we want in principle, but in practice can be tricky to work with.

- Supporting LTO requires the build system to be modified to support LTO, which can be a non-trivial effort.
- Users would need to manually collect all of the LLVM IR they want to analyze, which can be difficult if a build system is set up to build multiple independent binaries or libraries that should not have their constituent object files combined for analysis.

### [wllvm](https://github.com/travitch/whole-program-llvm)

I started writing the `wllvm` tool in 2011 to solve the problem of generating LLVM IR for entire programs and libraries{% sidenote() %}
While I wrote `wllvm`, it has been graciously maintained for the last few years by [Ian Mason](https://github.com/ianamason), as most of my work has not involved LLVM until recently.
{% end %}. `wllvm` is a set of Python scripts that mimic a compiler driver, which attempts to be a drop-in replacement for `gcc`. Typically, one uses `wllvm` by configuring the build system to use `wllvm` as an alternative compiler (e.g., via the `CC` environment variable for autotools builds or `cmake` builds).

When invoked, the `wllvm` script compiles its input twice: first by invoking the underlying compiler with all of the requested flags and then again by using `clang` to generate LLVM IR{% sidenote() %}
Interpreting compiler command line arguments is one of the major sources of complexity in `wllvm`. The GCC family of compilers support a large number of complex command line arguments. `wllvm` needs to be able to parse command lines to pick out the names of input files and output files so that it can recompile the inputs and attach metadata to the generated object files. To do this, `wllvm` attempts to implement a command line argument parser compatible with GCC.
{% end %}. The script then writes the path{% sidenote() %}
`wllvm` saves the _path_ of the LLVM IR in object files. Originally, it saved the contents of the LLVM IR into the special ELF section directly. This is more convenient and ensures that LLVM IR never gets lost. However, it was occasionally problematic on 32 bit systems when building LLVM IR with debug information. The maximum size for an ELF section on a 32 bit platform is 4GB, which could be exceeded in debug builds for large programs.
{% end %} to the generated LLVM IR file into a special ELF section of the object file (`.llvm_bc`). When the object files for a binary (shared library or executable) are linked together, the linker concatenates the contents of all of the sections with the same name, which collects all of the LLVM IR file paths into a single section. `wllvm` comes with a helper script, `extract-bc`, to extract the LLVM IR file paths, collect all of the corresponding files on disk, and link them into a single monolithic LLVM IR file using `llvm-link`.

In order to support build systems that delete intermediate files or directories, `wllvm` supports saving generated LLVM IR into a separate directory tree that the build system does not touch. `wllvm` supports generating LLVM IR using either `clang` or the Dragonegg plugin, which is a (since abandoned) GCC plugin that supports generating LLVM IR. The Dragonegg codepath was important in the early days of the LLVM project, when the `clang` frontend was less mature and was unable to compile some common codebases. These days, `clang` can handle nearly every somewhat modern codebase.

I had a great deal of success with `wllvm`. The cases where it fails tend to involve complex multi-stage build systems where when:

- The build system invokes autoconf scripts in ways that make it impossible to specify an alternative compiler
- The build system is too clever and detects that `wllvm` is an unrecognized compiler and refuses to build
- The build system makes it difficult to impossible to specify an alternative compiler
- The build system makes it difficult to impossible to replace the detected compiler after `configure` runs (e.g., because it refers to build tools with absolute paths)
- The build system pipes input files to the compiler{% sidenote() %}
Piped inputs are problematic because `wllvm` needs to compile them twice. The compilation to a normal object file drains the contents of the pipe, leaving an empty pipe for the second compilation with `clang`.
{% end %}

Note that none of these cases are particularly common (i.e., `wllvm` is very effective), but the rare difficult build systems are incredibly frustrating.

### [gllvm](https://github.com/SRI-CSL/gllvm)

The `gllvm` tool is substantially similar to `wllvm`. It was written by [Ian Mason](https://github.com/ianamason), who is also the primary maintainer of `wllvm`. `gllvm` operates in essentially the same manner as `wllvm`. In contrast, `gllvm` is:

- Written in Golang
- Easier to maintain
- More actively maintained
- Easier to distribute{% sidenote() %}
`gllvm` is easier to distribute because it uses libraries to manipulate ELF and Mach-O files, rather than command line tools (which `wllvm` assumes are installed and available).
{% end %}
- Faster{% sidenote() %}
`gllvm` is faster than `wllvm` because it builds the object file and LLVM IR in parallel.
{% end %}

The only disadvantage to using `gllvm` is that it does not support the Dragonegg plugin, which is much less significant in 2021. My personal feeling is that `wllvm` should probably be deprecated in favor of `gllvm` at this point.


### [blight](https://github.com/trailofbits/blight)

The `blight` system is a set of scripts that provide a convenient interface for executing actions (hooks) before and after commands are invoked. It provides hooks by exporting environment variables that many build systems respect (e.g., `CC`, `CXX`, and `LD`). Each of the scripts that implement pre- and post-hooks can be configured by other environment variables.

`blight` aims to make adding custom pre- and post-hooks as simple as possible, making it a more general tool than `wllvm` and `gllvm`. It can be used to generate LLVM IR by hooking `CC` and `CXX` (i.e., the C and C++ compilers) and using a post-hook to invoke `clang` to generate LLVM IR.

As `blight` relies on manipulating the environment, both through build system configuration and `PATH` manipulation, it can fail in the same ways that `wllvm` and `gllvm` can.

### [Bear](https://github.com/rizsotto/Bear)

The `Bear` tool does not generate LLVM IR directly; instead, it generates a Clang compilation database from nearly arbitrary builds for later processing (e.g., generating LLVM IR). It operates somewhat differently from the others. It uses an `LD_PRELOAD`{% sidenote() %}
`LD_PRELOAD` is an environment variable that can be used to inject a shared library into the address space of a process on Linux and many other UNIX-like systems. If `LD_PRELOAD` contains the path to a shared library, the dynamic loader will arrange the address space such that symbols defined in the shared library _override_ any dynamic symbol table entries with the same name. This means that `LD_PRELOAD` only works for dynamically-linked binaries and is only able to override functions with appropriate linkage (e.g., static functions cannot be hooked).
{% end %} hook to observe build systems and catch invocations of compilers and records them in a compilation database. Specifically, it observes calls to the `exec` family of functions and records their arguments.

Several caveats apply:

- Relying on `LD_PRELOAD` means that statically linked build tools are not supported; in a nod to this limitation, `Bear` has a fallback mode based on compiler wrapper scripts
- Builds cannot always be replayed from an artifact like a compilation database

# Designing build-bom

The `build-bom` tool is inspired by the tools described above, but attempts to learn from and improve upon them. The goal of `build-bom` is to generate LLVM IR for arbitrarily complex projects without requiring any modifications at all to their build systems. `build-bom`:

1. **Observes** build processes and,
2. For each source file _f_ compiled with a recognized compiler, **rebuilds** _f_ using `clang` to generate LLVM IR.

Much like `wllvm`, it parses command line arguments to recognize sources and targets so that it can attach LLVM IR to object files in a dedicated ELF section. It provides a sub-command to extract LLVM IR after builds complete.

## Initial Design

In the original design of `build-bom` I aspired to a two stage process much like `Bear`, where the tool would record builds and replay them to generate LLVM IR and also enable post-build analysis of dependencies (e.g., constructing a Software Bill of Materials). In my mind, the distinguishing feature of `build-bom` was that it would use `ptrace` to observe uses of the `execve` system call (i.e., trace spawned processes) instead of using `LD_PRELOAD` hooks. By using the lower-level facilities provided by the kernel rather than the dynamic loader, it would be more robust and work for statically-linked build system and for build systems that alter the environment.

During the initial testing of this design, it became apparent that replaying arbitrary builds is not practical. A few of the observed failures included:

- **Build systems that move files cannot be replayed.** While it may seem like recording a file move (and all other file operations) should solve this problem, most file move operations do not correspond to a single system call{% sidenote() %}
While file move operations within a single filesystem can be performed with the `rename` system call, it does not work across filesystems. Most programs that need to move files use a mix of I/O calls and integrity checks provided by their programming language standard library.
{% end %}. This means that recording and replaying file move operations reliably is not really possible.
- **The effects of shell scripts invoked by build systems are difficult to record.** The executables invoked by a shell script can be observed and recorded using `ptrace`. However, shell actions (e.g., file redirections and pipes) cannot be easily replayed. This means that files _created_ by shell scripts in the build process cannot be recreated while replaying the build; if the build system removed them, they are gone.

In principle, replaying every `write` system call---along with its arguments---should be sufficient to reconstruct any files. Tracking operations at this granularity seems problematically complex, and could ultimately require recording a huge range of system calls.

## Actual Design

Rather than recording traces of build events, `build-bom` is now a hybrid of the `wllvm` and Bear approaches. It uses `ptrace` to attach to the build process (e.g., `make` or `ninja`) and generates LLVM IR with `clang` as it observes `execve` system calls that spawn recognized compilers. Like `wllvm`, it parses compiler command line arguments to determine source files and targets, enabling it to both generate LLVM IR for source files and attach it to the corresponding target (i.e., object file) created by the original compilation command.

By attaching to the build process (and all child processes) with `ptrace`, `build-bom` is able to invoke `clang`{% sidenote() %}
Note that `build-bom` does not support generating LLVM IR with the Dragonegg plugin.
{% end %} to generate LLVM IR in the narrow window between the time the original compilation command finishes, but before it finishes terminating. This sequencing of events prevents race conditions by ensuring that the build system cannot move, rename, or remove files we need (e.g, renaming generated object files).

In contrast to `wllvm`, `build-bom` does not invent a custom format for storing LLVM IR in an ELF section. Instead, the contents of the distinguished ELF section is a tar file. When `build-bom` generates LLVM IR for a source file, it wraps the LLVM IR in a tar file that it injects into the distinguished ELF section using `objcopy`. When the system linker combines the LLVM IR ELF sections, it concatenates their contents. It turns out that concatenating tar files produces a valid tar file with the combined contents of the original tar files. This small change means that extracting LLVM IR after the build completes is somewhat simpler, requiring just:
1. Extracting the contents of the tar file into a temporary directory
2. Linking together all of the individual LLVM bitcode files using `llvm-link`


### Interesting Implementation Details

Implementing `build-bom` was an interesting experience, as it shared some similarities with implementing a debugger.

The system call tracing in `build-bom` uses the `ptrace` system call on Linux{% sidenote() %}
The same approach should work on the BSDs (but likely not MacOS).
{% end %} through the excellent [pete](https://github.com/ranweiler/pete) library. In order to use `ptrace`, at least in the way that `build-bom` requires:

1. The process to be traced (i.e., the build process) needs to invoke `ptrace(PTRACE_TRACEME, ...)` to halt the tracee and allow the tracer to attach
2. The tracer (i.e., `build-bom`) needs to invoke `ptrace(PTRACE_ATTACH, ...)` to attach to the tracee
3. The tracer repeatedly invokes `ptrace(PTRACE_SYSCALL, ...)` to resume the tracee, but halt at the next system call
4. Each time the tracer stops the tracee at a system call, it can inspect the tracee state to determine what system call was invoked and what arguments were passed to it

While `ptrace` supports reading memory from the tracee using `ptrace(PTRACE_PEEKDATA, ...)`, it only supports reading a pointer-sized value at a time{% sidenote() %}
The BSD implementations of `ptrace` support an additional command, `PT_IO`, which supports reading (or writing) arbitrary amounts of memory from the traced process.
{% end %}. This is too inefficient to read large amounts of data. `build-bom` uses the typical workaround of performing bulk reads by reading from `/proc/<pid>/mem` with helper functions from the pete library.

Many build systems pass relative path names to build tools, rather than absolute paths. This is entirely reasonable from the point of view of the build system, but it does pose a slight challenge for `build-bom`. Throughout a build, a build tool will often change its working directory, which means that the working directory of the build tool and `build-bom` get out of sync. When the build system uses relative paths, `build-bom` must correct them in order to be able to generate LLVM IR through recompilation. While `build-bom` could change working directories with the build system, this would become complicated and inefficient with parallel builds. Instead, it normalizes relative paths into absolute paths when reading arguments passed to system calls.

Interpreting paths passed as arguments to system calls turned out to be a surprisingly tricky proposition. On Linux, paths are `NUL` terminated strings with `/` as a separator. There are no constraints on the encoding for paths beyond that, nor are there indicators as to what the encoding of any particular path is. The rust `String` type is explicitly a UTF-8 encoded string. This means that paths cannot be treated as rust strings; instead, `build-bom` represents them using the raw `OsString` type, which have no particular encoding for their contents. Semantically, this is entirely correct; however, it is a bit painful when trying to parse, manipulate, and print paths in `build-bom`.

## Evaluation Against Requirements

Earlier in this post, I outlined a list of challenging build features that a general LLVM IR generation tool should be able to handle. Since `build-bom` attempts to be a general LLVM IR generation tool, let's evaluate how many it addresses.

1. ✓ `build-bom` is build system agnostic
2. ✓ `build-bom` unobtrusively generates both the original executable build artifacts and LLVM IR, enabling intermediate build artifacts to be executed
3. ✓ `build-bom` generates LLVM IR for each source file at the same time the original object file is generated (recall, in the very narrow window between when the compiler completes but before it exists), ensuring that intermediate files are accessible
4. ✓ `build-bom` does not require any control over environment variables to work, nor does it require build systems to respect any environment variables
5. ✓ `build-bom` traces commands run in an entire process /tree/, which transparently includes child build processes
6. ✓ `build-bom` does not interpose in configuration processes or ever modify the arguments passed to the compiler
7. ✓ `build-bom` observes build commands, rather than interposing via scripts, so absolute paths in build systems (e.g., for compilers) are not problematic
8. ✓ `build-bom` does not need to directly trace or even be aware of the actual build process; running any orchestration scripts under `build-bom` will indirectly trace the build process
9. ✓ `build-bom` traces system calls with kernel support (i.e., through `ptrace`), and so does not rely on the dynamic loader

## Design Consequences

Relying on `ptrace` to observe system calls issued by the build system serializes all of them through `build-bom`. This significantly reduces the parallelism available in the build system (though processes still execute in parallel when not waiting in system calls).

The reliance on `ptrace` also ties `build-bom` to UNIX-like systems. This is not a hard limitation, as MacOS and Windows both support other mechanisms that would enable the same functionality. A `build-bom` backend can be implemented for any system with kernel support for debugging.
- While MacOS has the `ptrace` system call, it is not fully-featured; MacOS debuggers require additional Mach system calls
- Windows has analogous system calls

Since `build-bom` is essentially a debugger, other debuggers cannot attach to any of the processes being traced.

The `build-bom` approach is less well-suited to builds where build commands are actually executed by a separate daemon process. Thus, using `build-bom` with systems like `distcc` and `bazel` is tricky. Avoiding `distcc` to generate LLVM IR is fairly easy. In the case of a build based on `bazel` (or similar systems), the server process would need to be traced.

# Future Directions

There are a few major features that I plan to add to `build-bom` as they become needed:
- **Support for response files** Response files are a feature provided by both `gcc` and `clang` to make it possible to pass in large numbers of arguments, especially on Windows{% sidenote() %}
Operating systems typically limit the number of command line arguments, as well as the total storage occupied by command line arguments. See the definition of `ARG_MAX` on POSIX-like systems, which is typically a few megabytes on Linux. The limit is considerably lower on Windows ([about 32kb](https://docs.microsoft.com/en-us/windows/win32/api/processthreadsapi/nf-processthreadsapi-createprocessw)), which is usually the motivation for supporting response files.
{% end %}. A response file simply records each argument to a process on its own line; `build-bom` would need to parse response files to determine the inputs and outputs of compilation commands.
- **Support piped inputs** Currently `build-bom` does not work correctly with file inputs that are provided to the compiler via pipe. The original compilation command drains the pipe, leaving `build-bom` with no input file contents to pass to `clang` to generate bitcode. To fix this, `build-bom` will need to inspect input files and, if they are pipes, replace the pipe file with a temporary file on disk with the contents of the pipe{% sidenote() %}
Note that this requires modifying the command line arguments in the build process to point to the new temporary file. This is somewhat complicated, as it requires finding space in the build process to store the new path without clobbering any existing data. It remains to be seen how this can best be handled.
{% end %}.
- **Support architectures besides x86_64** `build-bom` contains a hard-coded table mapping system call numbers to names to be matched on. Unfortunately, system call numbers differ across architectures on Linux. Adding support for new architectures is straightforward.
- **Support ELF modification via library** To attach LLVM IR to an object file, `build-bom` calls the `objcopy` binary, which it assumes is installed on the system. Using a library to read and write ELF files would reduce the run-time dependency footprint.
- **Support for Windows and MacOS** Extending support to Windows and MacOS requires understanding the necessary debugging system calls, but is not conceptually difficult.
- **Support for arbitrary build command interposition** Currently, `build-bom` runs extra commands to generate LLVM IR. It is a small delta to being able to arbitrarily modify build commands. I envision a `sed`/`awk`-like interface for matching and rewriting command line arguments. Potential uses include, but are not limited to:
  - Forcing the generation of debugging information
  - Forcing certain optimization levels
  - Enabling Address Sanitizer or Thread Sanitizer


# Summary

The `build-bom` tool simplifies generating LLVM IR for C and C++ codebases. Compared to previous tools in this space, `build-bom` supports a wider range of build system features without requiring any modification to the build system in order to run. The major insight of `build-bom` is to use `ptrace`, the system call that provides debugging services on Linux, to observe build processes and generate LLVM IR just-in-time, avoiding many of the complexities inherent to other approaches.

