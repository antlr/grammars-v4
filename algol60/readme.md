# Algol60 Grammar

## Source
The source for the grammar is the last version of the Algol 60
specification, [ISO 1538](https://www.iso.org/standard/6126.html).
[A free copy of the spec](http://www.softwarepreservation.org/projects/ALGOL/report/ISO1538.pdf)
was hand-translated and then adapted to Antlr4.

Other sources you can find from the [Wikipedia](https://en.wikipedia.org/wiki/ALGOL_60)
page.

## Examples
The examples included were gathered from various free
sources on the Internet.

* https://github.com/erwan-kessler/ALGOL-60/tree/master/examples
    The samples in this repo are of unknown origin, but they appear
    valid with only a few simple departures from the Standard in
    the alphabet (e.g., using backquote and forward quotes instead of
    upper-left corner and upper-right corner characters). The grammar
    is tuned to these examples.
* https://github.com/hxw/Elliott-803/tree/main/Algol60-Samples
    The samples in this repo came some old tapes the author squirreled
    away, for an [Elliott-803](https://en.wikipedia.org/wiki/Elliott_803).
    The Algol 60 compiler and examples departs from the standard in various ways:
    use of single quote instead of semi-colon. The samples are not used here.
* https://github.com/JvanKatwijk/algol-60-compiler
    The samples in this Algol-60 compiler source, written by Jan van Katwijk,
    are described in the [User Manual](https://github.com/JvanKatwijk/algol-60-compiler/blob/master/doc/jff-manual.pdf).
    It is a derivative of the [”Modified Report
    on the algorithmic language ALGOL 60”](https://doi.org/10.1093/comjnl/19.4.364)
    (downloadable from [here](https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.104.1483&rep=rep1&type=pdf)).
    Some of the samples are included in the tests here, but others
    are not because they deviate from the spec, e.g.,
    a semi-colon after an "end".
