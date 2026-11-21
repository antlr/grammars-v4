# tcpdump grammar

An ANTLR 4 grammar for a subset of [tcpdump](https://www.tcpdump.org/) text
output. Reference: the [tcpdump manual](https://www.tcpdump.org/manpages/tcpdump.1.html).

The grammar parses timestamped `IP`, `IP6`, and `ARP,` summary lines. IP records
expose the protocol, source, direction, destination, and optional packet details.
Endpoints can be numeric IPv4/IPv6 addresses or resolved names with numeric or
named ports. Destinations include the trailing colon used by tcpdump. Protocol
details, such as TCP flags, DNS messages, and ICMP descriptions, are retained as
tokens without validating their internal syntax.

Timestamps use `HH:MM:SS` with an optional fractional part and optional
`YYYY-MM-DD` date prefix (`-tttt` output). Hours, minutes, and seconds are
range-checked lexically; dates, addresses, and port ranges are not validated.
LF and CRLF are supported, with optional final newlines and blank lines.

This is not a parser for packet bytes, pcap files, capture filters, or every
tcpdump output mode. Ethernet headers, verbose multiline packets, hex dumps,
capture-status messages, epoch timestamps, and timestamp-free output are outside
its scope.

All ten inputs in [`examples/`](examples/) are tested with the `dump` entry point,
which consumes the entire input. Run `mvn -pl tcpdump clean test` from the
repository root, or `mvn clean test` from this directory. `desc.xml` configures
the same entry point for Trash Toolkit drivers.
