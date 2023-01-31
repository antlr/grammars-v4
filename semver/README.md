# A parser for Semantic Version 2.0

## The What

This is a parser for [semver 2.0 spec](https://semver.org/).
In addition the spec adherence, it supports version "tags" such as "beta3" or "rc.2" - this would allow implementers to implement version comparison not only with major/minor/patch versions but with the numbers of beta/rc versions.
Therefore, it would be possible to compare and see that "1.2.3-rc2" > "1.2.3-beta23" > "1.2.3-beta8" > "1.2.3-alpha8"  

## The How

An example implementation in C# can be found [here](https://github.com/myarichuk/ReleaseTools/tree/master/src/Parser.ConventionalCommit).
