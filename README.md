A concurrent queue for Lwt.  Taking elements from the queue will
'block' when the queue is empty.  Similarly, adding elements to the
queue will 'block' when the queue is full.  The maximum size of the
queue can be set after its creation.