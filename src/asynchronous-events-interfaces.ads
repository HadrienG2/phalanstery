with Ada.Exceptions;

package Asynchronous.Events.Interfaces with Preelaborate is

   -- At its heart, the asynchronous programming model is about events. These are little state machines which represent
   -- some asynchronous operation in progress that will complete eventually. We define the following event states:
   type Event_Status is (Pending, Done, Canceled, Error);
   subtype Finished_Event_Status is Event_Status range Done .. Error;

   -- Events are typically referred to using proxy objects. To tell which objects refer to the same event,
   -- one can simply use the comparison operator. Proxy objects also have an invalid "null" state at creation time,
   -- and must be taken away from this state through an explicit allocator.
   type Event_Base is limited interface;
   function Is_Null (Who : Event_Base) return Boolean is abstract;
   function "=" (A, B : Event_Base) return Boolean is abstract;

   -- Events may be canceled by both the client and the server. For client-side cancelation, this has the semantics of
   -- a request to the server : not all operations are abortable, however we can guarantee that if a pending
   -- asynchronous operation is canceled, all operations depending on it will be recursively canceled as well.
   procedure Cancel (Who : in out Event_Base) is abstract
     with Pre'Class => (not Who.Is_Null);

   -- Events have servers and clients, whose reponsibility is divided as follows:
   --    - A server "owns" the event. It may change its status, create clients, and listen to cancelation requests.
   --    - The client may query the state of the event, and request the cancellation of the underlying operation.
   type Event_Server is limited interface and Event_Base;
   type Event_Client is interface and Event_Base;

   -- The most basic level of client access is polling : fast, but not scalable to long waits as it wastes CPU time.
   function Status (Who : Event_Client) return Event_Status is abstract
     with Pre'Class => (not Who.Is_Null);
   procedure Get_Error (Who  : Event_Client;
                        What : out Ada.Exceptions.Exception_Occurrence) is abstract
     with Pre'Class => (not Who.Is_Null);

   -- Waiting is more CPU-efficient, but it blocks OS threads and is thus still not scalable to many waiters.
   -- If the event ends up throwing an exception, it will be propagated to the waiter upon wakeup.
   procedure Wait_Completion (Who  : Event_Client;
                              Final_Status : out Finished_Event_Status) is abstract
     with Pre'Class => (not Who.Is_Null);

   -- Finally, we may ask the event to synchronously notify some listener about status changes using a callback.
   -- This approach scales much better to large amounts of long-running events with moderate amounts of listeners.
   --
   -- The problem with callbacks as implemented in most libraries is that they are thread-unsafe and lack context
   -- information. We adress the later by asking that callbacks be objects instead. In Ada, they may be protected
   -- objects or tasks, which addresses thread-safety for free when their synchronization overhead is acceptable.
   --
   -- By moving to stateful objects, however, we create an issue of state management. Because we do not know the
   -- specifics of the callback object being used, allocation and liberation must be left up to individual clients.
   --
   -- To cut a long story short, we want a copyable reference to a thread-safe object, which may be assumed to be
   -- at least as long lived as the references that we hold. The client can ensure this property in a number of
   -- ways, from reference counting to careful scoping, and we neither need to care, nor are able to.
   type Event_Listener is limited interface;
   procedure Notify_Event_Status_Change (Where : in out Event_Listener;
                                         What  : Finished_Event_Status) is abstract;
   type Event_Listener_Reference is interface and Event_Listener;

   -- Given such a reference, we may store listener references within the event client and notify them later on.
   procedure Add_Listener (Where : in out Event_Client;
                           Who   : in out Event_Listener_Reference'Class) is abstract
     with Pre'Class => (not Where.Is_Null);

   -- On the server side, the most basic ability is that to alter the underlying event's status.
   procedure Mark_Done (Who : in out Event_Server) is abstract
     with Pre'Class => (not Who.Is_Null);
   procedure Mark_Error (Who  : in out Event_Server;
                         What : Ada.Exceptions.Exception_Occurrence) is abstract
     with Pre'Class => (not Who.Is_Null);

   -- In addition, servers may listen to cancelation requests. Normally, the idiomatic way to do this in Ada would be
   -- asynchronous tranfer of control. However, the usefulness of ATC as a cancelation primitive is highly
   -- implementation-dependent. Moreover, using ATC would entain providing direct access to the event protected object,
   -- which becomes quite inconvenient once reference-counting comes into play.
   function Is_Canceled (Who : Event_Server) return Boolean is abstract
     with Pre'Class => (not Who.Is_Null);

end Asynchronous.Events.Interfaces;
