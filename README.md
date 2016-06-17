# Experiments with user threads in Ada

## What is this?

This is an attempt to bring the kind of user-mode asynchronous tasking facilities that can be seen in C++
libraries such as Intel TBB or HPX to a native Ada package.

At the moment, it is very much experimental, and even the general design is still open for discussion. I would
thus strongly recommend against basing any work on this library yet.


## Can you elaborate on the design?

### Introduction

The goal of any user mode tasking library is to permit the existence of a large amount of cooperative tasks in
a program, in order to expose internal concurrency and allow latency hiding of IO operations, while bounding
the number of underlying OS threads to a reasonable amount (typically more or less the amount of CPU cores or
hardware threads on the host).

The low-level interface to this specific library was heavily influenced by the design of out-of-order OpenCL
command queues, which I feel provide a very elegant model for asynchronous computation, that has proven to be
portable to a very wide range of hardware configurations. A higher-level source of inspiration is HPX.

### Events

Events, in our model, are a 1-N intertask communication primitive which represent an asynchronous process.
They are little state machines which, over their lifetime, transition exactly once from a pending state to one
of several completion state. Currently, the following process states are defined:

- **Pending**: Process is ongoing
- **Done**: Process completed normally
- **Error**: Process was erronerous (additional exception information may be retrieved from the event object)
- **Canceled**: Process was aborted, generally on user request

Event state may be tracked in one of three ways:

- Polling (Easy to use, not suitable for waiting)
- Blocking wait for completion (CPU-efficient but not scalable to many waiters, only intended for use in the
  main application program)
- Callback objects (Preferred solution for most waiting scenarios. Unlike regular callbacks, these objects are
  stateful, and can thus be easily made context aware. Moreover, because any suitably tagged Ada type may be
  used, thread-safety can be achieved quite easily using protected types.)

Events are composable using an AND-gate like relationship: a composite event is Done if all its children are
Done, and edge cases are handled the way you would expect. The design of Ada protected types makes it
somewhat harder to implement other composition relationships such as OR gates, however it is nothing that
cannot be worked around if a clear use case for such composition is demonstrated.

Cancelation of events is also supported, in the sense that any entity with a reference to an event can request
the cancelation of the underlying asynchronous process. How quickly the underlying process will actually stop
is implementation-dependent, but it is guaranteed that all pending dependents of the event will also be
stopped, since otherwise their execution would be erronerous as they would incorrectly assume that all their
dependencies have completed successfully.

This is an area where I need feedback on use cases: this cancelation model is not as general as a model where
dependent tasks are merely notified of parent cancelation. However, it is also easier for developers to use,
and seems applicable in most asynchronous computation scenarios. Do you foresee a major use case where it
would not be appropriate?

### Asynchronous tasks

An asynchronous task is a user-defined cooperative multi-tasking work-item, implemented as a tagged type which
communicates with the underlying task scheduler using a dispatching method.

When an asynchronous task is scheduled for execution, the library provides the client with an event, which may
be used to track the progress of said task. In addition, a task may, both at scheduling time and at any later
time during execution, opt to wait for a list of events. This effectively creates a dynamic event-based task
dependency graph, which is a very powerful primitive when composing complex asynchronous computations.

By design, this execution model makes it hard to accidentally create a cyclic dependency graph which would
result in program deadlock. This improves usability and voids the need for expensive and complex runtime cycle
detection algorithms.

A task is scheduled for execution as soon as a CPU core is available, and then has multiple avenues for
interacting with the scheduler and its clients. Depending on the program requirements, an asynchronous task
may opt to run continuously to completion in order to maximize its performance and minimize its latency, or it
may yield control to the scheduler often in order to avoid starving other tasks and receive up-to-date
information from the outside world.

Errors are propagated using exceptions, which are transparently caught by the scheduler and transmitted
to clients for processing.

### Executors

Executor objects are the interface through which CPU cores are reserved and asynchronous tasks are submitted
for execution. Under the hood, they take care of allocating CPUs, mapping asynchronous tasks to preemptive
Ada tasks, and managing the state of task instances across iterations.

The external interface to executor objects is meant to be easy to use, and to accomodate for all envisioned
usage scenarios. For example, if fire-and-forget semantics are desired, as is typically the case when emitting
debugging output, a client can easily choose not to receive the output event of the associated task.

A core goal of this execution model is to be scalable to non-shared memory scenarios, such as distributed
and heterogeneous computing. In this respect, no issue is foreseen with a usage model where programs use one
executor per core, as in OpenCL. Events should also be reasonably scalable provided that clients accept to
tolerate some caching and state propagation latencies.

In the future, executors may also acquire more functionality, such as performing watchdog monitoring of
runaway tasks, depending on which limitations people run into with the simple execution model. As of now, the
plan is to keep things simple and consolidate the basic tasking and execution model before moving on with more
advanced topics.

### Longer-term ideas

If this experiment is successful, I plan to work on a higher-level frontend which would provide data-parallel
primitives (e.g. map, reduce and filter) and reduce the need for explicit event synchronization by silently
accounting for data dependencies, using suitable abstractions such as futures.

I know that there is quite a bit of existing and ongoing work regarding data parallelism and futurization in
the Ada community (see e.g. paraffin, https://groups.google.com/forum/#!topic/comp.lang.ada/v0ZXkaG8rek ).
Collaboration with these projects would most certainly be beneficial.

Another area that should be worked on is NUMA awareness and affinity. At the moment, tasks are run on an
arbitrarily selected CPU core, which is fine for symmetric multiprocessing machines but will fail to achieve
optimal performance on higher-end multi-socket machines. Addressing this problem requires a combination of
both CPU pinning and careful memory management, and this will raise interesting interface design questions.

Going in the same general direction, one can also implement distributed computing support. The HPX team has
shown that the asynchronous tasking model can scale remarkably well in a distributed setting, and there are
definitely many interesting avenues to explore in this area.

Debugging and profiling tools are often a sore point of asynchronous tasking libraries, and one area which I
would definitely love to explore more. Here, what I would envision is an interactive visualization of the
asynchronous task dependency graph, akin to what is provided by the LabView IDE from National Instruments.
This would help a user to pinpoint the exact origin of errors, along with the execution hot spots.

Asynchronous tasking is also strongly dependent on the availability of nonblocking IO, which is in general a
core component of any scalable software system that interacts with the outside world. If this idea is
successful, this is definitely an area which I will want to explore as well.
