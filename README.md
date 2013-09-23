
# About Wavy #

*Wavy* is a work in progress. The aim is to provide a core set of modules for sound encodings, synthesis
and manipulation.

# Supported encodings #

* PCM WAVE (.wav).

# Benchmarks #

Benchmarks are updated with any change in the library.
They are grouped into different categories. Using GHC 7.6.3 as shipped in the
[Haskell Platform](http://www.haskell.org/platform).

* [Synths](http://daniel-diaz.github.com/projects/wavy/wavy-bench-synth.html). In this benchmark we
compare the performance of synths of different types of waves, all generating 44,100Hz mono sounds
of 3 seconds. This means 132,300 samples.
* [Encoders](http://daniel-diaz.github.com/projects/wavy/wavy-bench-encoding.html). In this benchmark
we compare encoding of sounds at different bit depths. The sample sound used is a 3 seconds length
sine wave of maximal amplitude at 44,100Hz. Again, this means 132,300 samples.
