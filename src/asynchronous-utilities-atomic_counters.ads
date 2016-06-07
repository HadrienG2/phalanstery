-- In order to implement reference counting, we need access to some kind of atomic counter.
-- For now, we will use those provided by GNAT. If wider hardware or compiler portability is desired, we
-- will need to find another package, or make our own.
with System.Atomic_Counters;
package Asynchronous.Utilities.Atomic_Counters renames System.Atomic_Counters;
