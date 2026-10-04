POIU: Parallel Operator on Independent Units
============================================

POIU is an ASDF extension that will parallelize your Common Lisp builds,
for some build speedup, both through parallelization and reduced GC.

POIU builds a precise and complete dependency graph,
based on which it schedules performing of actions by worker subprocesses.
This dependency graph can also be extracted and used for other purposes.


Version Compatibility
---------------------

This tree has been updated and tested with the ASDF 3.3.1 bundled with SBCL 2.6.9,
on macOS (Apple Silicon). See [Testing](#testing) for the regression coverage
and [Performance](#performance) for measured speedups.


How It Works
------------

POIU records the complete dependency graph of the actions in an ASDF plan.
When performing the plan, the main process keeps a pool of up to `*max-forks*`
background *workers*, forked copies of itself, which perform the actions that create
files, such as compiling Lisp files, while the main process performs the actions that
modify the current image, such as loading the FASLs compiled so far.

A worker sees the state of the main process at the time it was forked.
Each in-image action that modifies the image starts a new "generation" of the image,
and each action requires the latest generation produced by its (transitive) dependencies.
A worker is reused for any action requiring no later generation than its own, and a
new worker is forked only when no idle worker is recent enough. Reusing workers matters:
forking a large image is expensive (on macOS, SBCL's fork costs time proportional to the
*reserved* `--dynamic-space-size`, about 7ms per GB). When only one action can run at all,
POIU performs it in the main process rather than fork a worker for it.

The output of each worker is captured and replayed by the main process when its action
completes, so the output of concurrent compilations doesn't get mixed up.

**Deterministic builds** (`poiu:*parallel-plan-deterministic-p*`, true by default):
in-image actions, e.g. loading FASLs, are performed in exactly the same order as in a
sequential ASDF build; only file-creating actions are reordered and run in parallel.
Setting it to `nil` lets the main process load files as soon as they are compiled,
which is faster (see [Performance](#performance)), but changes the load order
of files with no declared dependency between them.

**Missing dependencies**: many systems build sequentially only thanks to the order of
their components, without declaring all the dependencies between files.
POIU compensates in two ways:

- When an action fails in a worker, it is retried in the main process once all the
  in-image actions planned before it are done, i.e. in the same state as in a sequential
  build, so the build still succeeds (or fails the same way as a sequential build).
- A file that uses a macro from a file it doesn't depend on may be compiled by a worker
  in which the macro isn't defined yet, without error. POIU notes the undefined functions
  reported when compiling each file, and before loading the file, it checks that none of
  them has since become a macro; if one has, it compiles the file again in the main process.
  This check is exact in deterministic builds. In non-deterministic builds, the macro may
  only be loaded after the file; POIU then warns about it at the end of the build.

`poiu:*last-build-statistics*` and `poiu:*last-build-timeline*` describe the last
parallel build: how many actions ran where, how many workers were forked, and when
each action started and ended.


Introduction
------------

POIU is a modification of ASDF that may `operate` on your systems in parallel.
Each version of POIU is designed to work with a matching version of ASDF;
it will not work on older versions, and
may or may not work on more recent versions.

POIU performs file-creating actions such as compilation of Lisp files
in forked worker processes, in parallel with other such operations.
On the other hand, in-image actions such as loading of FASLs happens serially
as the dependencies for these actions are completed.

POIU will only make a difference with respect to ASDF
if the dependencies are not serial. Thus,
there will be no behavioral difference within
systems that use `:serial t` everywhere.

You can however use Andreas Fuchs's
[`ASDF-DEPENDENCY-GROVEL`](https://gitlab.common-lisp.net/xcvb/asdf-dependency-grovel)
to autodetect minimal dependencies from an ASDF system (or a set of multiple such).

POIU may speed up compilation by utilizing all CPUs of an SMP machine.
POIU may also reduce the memory pressure on the main (loading) process
by off-loading the compilation onto forked subprocesses;
this could help reduce the performance hit of Garbage Collection (GC).
POIU will enforce separation between compile- and load- time environments,
helping you detect
[when `:LOAD-TOPLEVEL` is missing in `EVAL-WHEN`'s](https://fare.livejournal.com/146698.html),
as needed for incremental compilation even with vanilla ASDF.
POIU will also catch *some* missing dependencies as exist between the
files that it will happen to compile in parallel. But POIU will not catch all
dependencies that may otherwise be missing from some systems.

When a compilation fails in a parallel process, POIU will retry compiling
in the main (loading) process so you get the usual ASDF error behavior,
with a chance to debug the issue and restart the operation at your regular REPL.
The retry happens once all in-image actions that a sequential build would have
performed earlier are done, so that systems with missing dependencies still build.

POIU was currently only made to work with Allegro, CCL, CLISP and SBCL.
[NB: the CLISP port is somewhat less stable.]
Porting to another Lisp implementation that supports ASDF
should not be difficult.
When unable to fork because the implementation is unsupported,
or because multiple threads are currently in use,
POIU will fall back to compiling everything in the main process.

Warning to CCL users: you need to save a CCL image that doesn't start threads
at startup in order to use POIU (or anything that uses fork).
Watch [QITAB](https://common-lisp.net/project/qitab/)
for a package that does just that: `SINGLE-THREADED-CCL`.

To use POIU, (1) make sure `asdf.lisp` is loaded.
We require a recent enough ASDF; see specific requirement in [poiu.asd](poiu.asd).
Usually, you can just:
```
(require "asdf")
```

(2) configure ASDF's `SOURCE-REGISTRY` or its `*CENTRAL-REGISTRY*`,
then load POIU:
```
(asdf:load-system :poiu)
```

(3) POIU is active by default. You can just
```
(asdf:load-system :your-system)
```

and POIU will be used to compile it.
Once again, you may want to first use `asdf-dependency-grovel`
to minimize the dependencies in your system.

POIU was initially written by Andreas Fuchs in 2007
as part of an experiment funded by ITA Software, Inc.
It was subsequently maintained by Francois-Rene Rideau at ITA Software,
who adapted POIU for use with XCVB in 2009,
wrote the CCL and CLISP ports, moved code from POIU to ASDF, and
eventually rewrote both of them together in a simpler way.
The original copyright and (MIT-style) licence of ASDF (below) applies to POIU.


Usage
-----

POIU overrides your ASDF 3's `asdf::*plan-class*`,
and thereafter all compilation goes through POIU by default.
Bind this variable back to `'asdf::sequential-plan` to restore the default,
and explicitly to `'asdf::parallel-plan` to go parallel again.
You can also explicitly pass a `:plan-class` parameter to `asdf:operate` & co,
or you can call the parallel-operate functions defined by POIU.

You can control how many worker processes POIU may use at a time
by binding `poiu/fork:*max-forks*`.
The default is normally the number of cpus on which the machine POIU was loaded,
which if resuming from a dumped image might not be the same as
the machine on which it is now running, so you may want to reset that variable
in e.g. uiop's image-restore hook.
You can recompute the number of processors on the current machine with:
`(poiu/fork:ncpus)`.
In case this function fails to find an answer, it returns NIL,
in which case POIU defaults the `*max-forks*` to 16.
You may also set `POIU_MAX_FORKS` in the environment before loading POIU.
On machines with both performance and efficiency cores, using only as many workers
as there are performance cores may be about as fast and use less energy.


Installation
------------

POIU 1.35.0 depends on the new plan-making internals of ASDF 3.3.0,
but for bug fix purposes, we recommend ASDF 3.3.3 or later.

To use POIU, just make sure you use a recent enough ASDF,
and in your build scripts, after you `(require "asdf")`
but before you build the rest of your software, include the line:

    (asdf:load-system "poiu")

It automatically will hook into `asdf::*plan-class*`,
though you can reset it.


Support
-------

The official web pages for POIU are:
    <http://common-lisp.net/project/qitab/>
    <http://cliki.net/poiu>

The proper mailing-lists on which to ask questions are
`asdf-devel` and `qitab-devel`, both on `common-lisp.net`.


Testing
-------

The tests below are SBCL-only, and each uses a repo-local `XDG_CACHE_HOME`.

  * `sh tests/run-concurrency-check.sh` builds a synthetic system with four
    independent files that each spend two seconds in `:compile-toplevel`, and checks
    that they compile concurrently.

  * `sh tests/run-missing-dependency-check.sh` builds a system whose files lack
    dependencies on files that define a package, a macro and a special variable they use,
    in both deterministic and non-deterministic modes, and checks the results.

  * `sh tests/run-real-world-check.sh [system ...]` builds real libraries from scratch,
    both sequentially with plain ASDF and with POIU, printing `elapsed=...` for each.
    With `POIU_RUN_TESTS=1`, it also runs each library's test suite (`asdf:test-system`).
    By default it builds `ironclad`, `str`, `osicat`, `alexandria`, `split-sequence`,
    `april` and `petalisp`.

  * `sh tests/run-scaling-benchmark.sh [shape ...]` generates synthetic systems with
    various dependency graphs, builds each from scratch in a fresh process, sequentially
    and with POIU at various worker counts, checks the result, and reports speedups.
    Set `POIU_SCALING_WORKERS` (e.g. `"1 2 4 8"`) and `POIU_SCALING_REPEAT` as desired.

The older `test.lisp` builds `exscribe` and needs the test files described in it.

Real-world results, building from scratch on a 10-core Apple M-series machine
(4 performance + 6 efficiency cores), SBCL 2.6.9, 1GB heap, 10 workers,
deterministic mode; all libraries' own test suites pass in both modes:

| System         | Sequential | POIU   | Notes                                                    |
|----------------|-----------:|-------:|----------------------------------------------------------|
| ironclad       |     13.43s |  6.25s | many independent files                                   |
| april          |     11.56s | 11.65s | mostly a serial chain of dependencies                    |
| petalisp       |     29.80s | 29.76s | one file takes 21s to compile; Typo lacks dependencies   |
| str            |      2.45s |  2.67s | serial dependencies                                      |
| osicat         |      1.20s |  1.14s | `cffi-grovel` generated files                            |
| swank          |      0.81s |  0.88s | declares no dependencies between its files at all        |
| alexandria     |      0.28s |  0.42s | `alexandria-2` lacks a dependency on `alexandria-1`      |
| split-sequence |      0.21s |  0.30s | small                                                    |

`shcl` and `cloture` fail to build in this environment with or without POIU
(a compile error in `shcl/core/utility/file-type`, and an incompatibility
between `cloture` and the installed version of `fset`).


Performance
-----------

POIU only helps when a build has independent files to compile; it can't make a serial
chain of dependencies faster, or a single slow file. For small builds, the cost of forking
workers can make POIU slightly slower than sequential ASDF.

Speedups from `tests/run-scaling-benchmark.sh` on the same machine (best of 2 runs;
each synthetic file takes about 0.1-0.3s to compile, except in `tiny`):

| Shape                             | Sequential | 1 worker | 2     | 4     | 8     | 10    |
|-----------------------------------|-----------:|---------:|------:|------:|------:|------:|
| wide: 60 independent files        |     19.82s |    1.00x | 1.93x | 3.27x | 4.18x | 4.41x |
| layered: 5 layers of 12 files     |     19.75s |    0.99x | 1.91x | 3.01x | 3.52x | 3.62x |
| large: 400 independent files      |     37.91s |    0.98x | 1.83x | 2.96x | 3.77x | 3.93x |
| chain: 30 files in a serial chain |      6.14s |    1.02x | 1.02x | 1.03x | 1.02x | 1.05x |
| tiny: 1000 independent tiny files |      0.94s |    0.73x | 1.12x | 1.57x | 1.27x | 1.20x |

Up to the 4 performance cores, speedup is close to linear; with 1 worker there is no
overhead, and a serial chain is never slower than sequential ASDF.

On machines with both performance and efficiency cores, speedups flatten beyond the
number of performance cores, since compiling on an efficiency core is about twice as slow.

**Large heaps.** On macOS (Apple Silicon), SBCL's `fork` costs time proportional to the
*reserved* dynamic space (it maps it executable): about 15ms with the default 1GB, 75ms
with `--dynamic-space-size 8GB`. The pool of reusable workers and compiling serial parts
of the build in the main process keep the number of forks low (e.g. 31 forks for 107 files
in `ironclad`), but builds with a large heap still gain less. For instance, ironclad builds
in 6.9s with an 8GB heap, vs 11.7s sequentially.

**Non-deterministic mode** (`(setf poiu:*parallel-plan-deterministic-p* nil)`) loads
each file as soon as it is compiled, rather than in plan order, which is faster when
a slow compilation would otherwise hold back the loading of later files
(e.g. ironclad builds in 4.3s instead of 5.4s). But it is only safe for systems
that declare all their dependencies: for instance, Typo (a dependency of petalisp) has
files that call functions from other files at load time without depending on them,
and fails to build in this mode.


Determinism
-----------

By default, POIU performs in-image actions in the same order as a sequential build
(see [How It Works](#how-it-works)), so the main image goes through the same states as
with plain ASDF. Compilation happens in workers forked from earlier states of the image,
which is *deterministic given the incremental state*, but not *given the source only*:
a worker compiling a file sees the files loaded so far, which may include files
the compiled file doesn't depend on, depending on timing. POIU detects the cases where
missing definitions would silently change the compiled code (see above), but not all
of them; e.g. a variable not named `*like-this*` that is bound lexically because its
special declaration was missing.


TODO: Support Build Phases
--------------------------

ASDF 3.3 introduced a builtin notion of multiple build phases in a same session.
These phases properly model system-definition time dependencies,
typically using using `defsystem-depends-on`, though for backward-compatibility,
ASDF also recognizes manual calls to `load-system` or `operate` within a `.asd` file.
POIU needs to be updated to support this notion.

POIU uses mutable hash-tables to represent "the" dependency graph, and
currently copies that graph once at "the" start of the build.
To properly handle multiple build phases,
POIU may have to maintain two explicit distinct graphs:

  1. the graph of "all the dependencies",
     which only grows as more are discovered, and

  2. the graph for "all pending dependencies",
     which grows with discovery and shrinks when performing actions.

The latter graph is used to schedule actions, while the former can be used
to report progress, verify consistency, display dependencies, etc.
Because of the multiple build phases, you can't actually precompute the former
then copy it into the latter before you start `perform`'ing the plan;
instead, you must start both from an empty state, and compute them concurrently
as you both discover dependencies and perform those from earlier phases.

Note that due to how `defsystem-depends-on` dependencies work,
to even compute the graph, you need to perform all actions in all phases
except possibly the very last one.
You could imaginably cache the results of this graph off-image,
but there still needs be some image that builds all those systems
with matching source code versions before you may prime the cache
and later use it.

To display the graph, you could output `dot` using `CL-DOT`,
or some JSON data for use with JavaScript D3.
Ideally, you'd probably want to compute the maximum build phase depth,
then pick an according color scheme wherein nodes and dependency arrows
get a different color based on how deep a phase they are built at,
with extra width for a defsystem-depends-on dependency.


As seen on TV!
--------------

Code from POIU could be seen in the
[Swedish TV series ‘Äkta Människor’](https://moviecode.tumblr.com/post/88245186920/some-lisp-code-taken-from-swedish-tv-series-%C3%A4kta), in
[Season 2 Episode 6 around 17:59](https://moviecode.tumblr.com/post/88826139010/in-real-humans-%C3%A4kta-m%C3%A4nniskor-some-common-lisp).
