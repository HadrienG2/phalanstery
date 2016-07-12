# Phalanstery

## What is this?

This project is an attempt to bring the kind of user-mode asynchronous tasking facilities that can be seen in
C++ libraries such as Intel TBB or HPX to a native Ada package.

At the moment, it is very much experimental, and even the general design is still open for discussion. I would
thus strongly recommend against basing any work on this library yet.


## Can you elaborate on the design?

### Introduction

Any user mode tasking library strives to simultaneously fulfill two goals, which are incompatible when relying
solely on OS threads as a multitasking abstraction:

- Allow a program to be decomposed into a relatively large amount of asynchronous jobs, so as to leverage the
  concurrent processing abilities of modern hardware and allow for optimal IO scheduling.
- Keep the amount of OS threads relatively small, ideally more or less the amount of CPU cores or hardware
  threads on the host, as most thread implementations use round-robin scheduling, which does not scale to
  large number of tasks.

As far as design inspirations are concerned, the low-level interface to Phalanstery was heavily influenced by
the design of out-of-order OpenCL command queues, a model for asynchronous computation and I/O which I think
is very elegant and has proven to be portable to a very wide range of hardware configurations. Higher-level
sources of inspiration include HPX and Intel TBB.

### Outcome objects

Being able to run some jobs asynchronously is not very useful unless there is a way to wait for them to
finish, order them with respect to one another, and handle operational errors correctly. Another important
use case is to be able to cancel previously started tasks without it being treated as an error. In
Plalanstery, all this functionality is handled through the use of Outcome objects.

An Outcome object is a client-server inter-task synchronization primitive which aims to address these issues.
By using it...

- Job processing code can notify clients that a job has completed normally.
- It can also throw exceptions when an error occurs, knowing that the runtime will propagate it to clients.
- A client can attempt to cancel a job, and the runtime will propagate this request to the underlying worker.

Outcome objects are not restricted to pure Phalanstery asynchronous jobs. It would be reasonably easy to map
their interface onto third-party asynchronous programming primitives, such as the asynchronous I/O that is
provided by some operating systems. This, in turn, improves the interoperability of Phalanstery code.

An outcome objects is implemented as a state machine, which can transition exactly once in its lifetime from 
the "pending" state to one of several final states. The following outcome states are currently defined:

- **Pending**: The asynchronous job has not finished yet.
- **Done**: The job was performed as expected.
- **Error**: The job was aborted due to an error (additional exception information is available as needed).
- **Canceled**: The job was aborted voluntarily, on user request or as part of an early exit strategy.

As a design constraint which allows client code to be written a lot more efficiently, once an outcome object
has transitioned to one of the finished states, it is guaranteed to stay forever in this state. Attempts to
change the outcome after that will currently be ignored, although there are discussions to change this
behaviour in the future at it can lead to errors being ignored silently in some circumstances.

The outcome of a job may be tracked in three different ways:

- It may be queried in a nonblocking fashion. This interface is easy to use, and efficient for quick state
  queries, but it should not be used in a loop as a way to wait for state changes.
- A client may wait for the completion of the job. In its most straightforward form, this operation blocks an
  OS thread, and it is thus only really suitable for use in the main program, as blocking a Phalanstery
  thread can lead to performance loss and deadlocks. However, there are ways for an asynchronous job to wait
  for an event to occur without blocking a whole OS thread to this end.
- A client may also register a callback object, a dispatching method of which will be invoked when the job
  finishes. Callback objects are a strict superset of the regular callbacks that are sometimes used in other
  asynchronous libraries: any global Ada procedure may be turned into a callback object, but a callback object
  may in addition also retain state, giving it some context awareness, which in turn greatly eases
  thread-safe implementations.

As mentioned earlier, a job can also be canceled through its outcome object. The semantics of this operation
are defined as follow:

- If the underlying process has not started yet, the Phalanstery runtime ensures that it will never run. Any
  task which waited for that process to finish is notified of the cancelation in a configurable way, the
  default behaviour of which is to similarly prevent the dependent task to run.
- If the underlying process has already begun, it is usually not safe to abort it in the middle of its
  operation. For this reason, Phalanstery mereley notifies the job of the cancelation request, requesting it
  to complete earlier if possible. Dependent of the job are still canceled as before.
- If the underlying process has completed, cancelation has no effect. It is too late, because the completion
  signal has already fired, and clients have already taken it into account.

Finally, more often than not, it is necessary to wait not just for the outcome of one job, but for that of a
set of jobs, using some composition relationship such as AND (all dependencies have completed) or OR (at least
one dependency has completed). Currently, only AND composition is supported, as it is the most immediately
useful relationship. But adding some form of OR composition is also under discussion in the Issues.

### Asynchronous jobs

An asynchronous job is a user-defined cooperative multi-tasking work-item, implemented as a tagged type whose
behaviour is defined or customized using dispatching methods.

There are a couple of programming restrictions on asynchronous jobs, which stem from the way they are
implemented and interact with the rest of the world. First, an asynchronous job should not use any blocking
thread synchronization mechanism, as failure to do so will lead to performance loss (one CPU going idle) and
potentially even cause deadlocks. Second, because synchronization and cancelation operates at job granularity
granularity, asynchronous jobs should be designed with transactional semantics, in the sense that they
should avoid leaving any data structure in an inconsistent state upon termination. The later job intended to
fix this mess may, after all, never run.

When an asynchronous job is scheduled for execution, the library internally makes a copy of it (allowing for
a job to be scheduled multiple times) and provides the client with an outcome object, which represents the
job's progression and eventual completion. In addition, a job may, at scheduling time or during execution, be
programmed to wait for the completion of some ongoing operations before proceeding. Given these two features,
a client can easily create a dynamic event-based job dependency graph, which is a very powerful primitive
when composing complex asynchronous computations.

Because the outcome object associated to a job instance is not created until the job is scheduled, and is
required in order to schedule other jobs depending on this specific instance, the aforementioned execution
model makes it hard to accidentally create a circular job dependency graph where a job directly or indirectly
waits for itself, resulting in a deadlock. This improves usability and voids the need for expensive and
complex runtime dependency cycle detection algorithms.

A job is scheduled for execution as soon as a CPU core is available, and then has multiple avenues for
interacting with the scheduler and its clients. Depending on the program requirements, an asynchronous job
may opt to run continuously to completion in order to maximize its performance and minimize its latency, or it
may yield control to the scheduler often in order to avoid starving other jobs and receive up-to-date
information from the outside world.

Errors are propagated using exceptions, which are transparently caught by the scheduler and transmitted
to clients for processing.

### Executors

Executor objects are the interface through which CPU cores are reserved and asynchronous jobs are submitted
for execution. Under the hood, they take care of allocating CPUs, mapping asynchronous jobs to preemptive
Ada tasks, and managing the state of job instances across iterations.

The external interface to executor objects is meant to be easy to use, and to accomodate for all envisioned
usage scenarios. For example, if fire-and-forget semantics are desired, as is typically the case when emitting
debugging output, a client can easily choose not to receive the outcome object of the freshly spawned job.

A core goal of this execution model is to be scalable to non-shared memory scenarios, such as distributed
and heterogeneous computing. In this respect, no issue is foreseen in usage scenarios where programs use one
executor per locality, as in OpenCL. Outcome objects should also be reasonably scalable provided that clients
accept to tolerate some caching and state propagation latencies.

In the future, executors may also acquire more functionality, such as performing watchdog monitoring of
runaway jobs, depending on which limitations people run into with the simple execution model. As of now, the
plan is to keep things simple and consolidate the basic tasking and execution model before moving on with more
advanced topics.

### Longer-term ideas

If this experiment is successful, I plan to work on a higher-level frontend which would provide data-parallel
primitives (e.g. map, reduce and filter) and reduce the need for explicit event synchronization by silently
accounting for data dependencies, using suitable abstractions such as futures.

I know that there is quite a bit of existing and ongoing work regarding data parallelism and futurization in
the Ada community (see e.g. paraffin, https://groups.google.com/forum/#!topic/comp.lang.ada/v0ZXkaG8rek ).
Collaboration with these projects would most certainly be beneficial.

Another area that should be worked on is NUMA awareness and affinity. At the moment, jobs are run on an
arbitrarily selected CPU core, which is fine for symmetric multiprocessing machines but will fail to achieve
optimal performance on higher-end multi-socket machines. Addressing this problem requires a combination of
both CPU pinning and careful memory management, and this will raise interesting interface design questions.

Going in the same general direction, one can also implement distributed and heterogeneous computing support.
The HPX team has shown that the asynchronous tasking model can scale remarkably well in a distributed setting,
and there are definitely many interesting avenues to explore in this area.

Debugging and profiling tools are often a sore point of asynchronous tasking libraries, and one area which I
would definitely love to explore more. Here, what I would envision is an interactive visualization of the
asynchronous job dependency graph, akin to what is provided by the LabView IDE from National Instruments.
This would help a user to pinpoint the exact origin of errors, along with the execution hot spots.

Asynchronous tasking is also strongly dependent on the availability of nonblocking IO, which is in general a
core component of any scalable software system that interacts with the outside world. If this idea is
successful, it is definitely an area which I will want to explore as well.
