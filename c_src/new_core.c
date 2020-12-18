/* Constraints / notes

   - only midi in, midi out.  this keeps interfacing very simple.
     protocols are arbitrary anyway.

   - don't make this depend on USB.  evntually i want a midi-only
     dedicated setup running on minimal hardware.

   - the "user interface" is implicit.  i want something that is not
     visual, purely mentally represented, hooked on a tactile
     interface, e.g. controllers and buttons.

   - the point is that the thing has state, but that it can be edited
     on the fly.  so really this is about creating an editor.

   - then visualisation/monitoring can be added as an optional
     element.

   - i do not want anything to do with touch screens.  the absence of
     tactile feedback is problematic.

   - should be C so it is easy to port, easy to generate, without
     malloc()

   - event-driven state machine with only midi timing coming in

   - create a separate machine that translates other time sources to
     midi.

   - jack_midi.c is a starting point, but needs to be set up such that
     it can work without an Erlang head.

   - the idea of mixer/track is great, but it needs to be tactile.

*/

