## Required for 1.0

* Much improved submode guessing.  This is partially done in 0.5 by
  using 'datetime' library.


## Other ideas

* More movement commands: move inside the same thread, move across
  entries of some view without activating it.  This is not so
  difficult to implement, but requires pondering on how to make the
  commands comfortable to use.

* Idle buffer parsing/filtering, otherwise mode is semi-useless in
  huge logs.

* Undo/redo for various filtering and explicit hiding operations.

* Context when filtering (like grep -C): optionally show N entries
  before/after each that matches filter.

* Sections: somehow make certain entries stand out and add navigation
  to the section start, narrow to section etc.  The idea is that
  sections can be made to span single request to your server
  (optionally bind to threads too).

* Replace timestamps with difference (likely to section start, as
  defined above) on demand.  E.g. something like this:

      18:11:03.038 [org.me.MyServer] processing request to 'Foo'
            +0.003 [org.me.SpecificServlet] initializing
            +0.004 [org.me.DatabaseUtils] querying the database: '...'

* Add a command to find big gaps in timestamps.  Alternatively or in
  addition to the requested jumping, it could also be used to define
  sections.  See https://github.com/doublep/logview/issues/5

* Maybe optionally highlight the current entry?  Though we already use
  background color heavily.
