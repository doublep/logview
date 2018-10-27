## Required for 1.0

* Much improved submode guessing.  This is partially done in 0.5 by
  using 'datetime' library.


## Other ideas

* Yet more movement commands, e.g. move inside the same thread.  This
  is not so difficult to implement, but requires pondering on how to
  make the commands comfortable to use.

* Undo/redo for various filtering and explicit hiding operations.

* Context when filtering (like grep -C): optionally show N entries
  before/after each that matches filter.

* Sections: somehow make certain entries stand out and add navigation
  to the section start, narrow to section etc.  The idea is that
  sections can be made to span single request to your server
  (optionally bind to threads too).
