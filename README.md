studio

Incubator project for audio/midi processing code.  This is currently
partly hard-coded to my setup (mixer, analog synths, midi controllers,
a delta1010 and Pure Data + custom jack audio modules).

This project consists of:

- a jack-based "data plane" for low-latency audio and midi, and

- an Erlang-based "control plane" for user interface, network and
  storage interfacing


I've really enjoyed using Erlang in my payed work on data acquisition
system control plane code in these last couple of years, so I started
to apply the approach to the midi & audio world as well.

Additionally, I've gotten into development tools again, working on
"fast cycle development" which is incorporated in my (closed source)
exo project.  I've really enjoyed using Erlang in this setup.  Its
flexible code reload features work well for millisecond-level
edit-and-test cycle.  It's a great framework for applying the same
approach to C and Rust code development.



