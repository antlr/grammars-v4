### A parser for Semantic Version 2.0 spec

This is a parser for [semver 2.0 spec](https://semver.org/).
In addition to the semver spec, the grammar supports version "tags" such as "beta3" or "rc.2" - this would allow implementers to implement version comparison not only with major/minor/patch versions but with the numbers of beta/rc versions.
Therefore, it should be easier to implement comparison between semvers. 
```
"1.2.3-rc2" > "1.2.3-beta23" > "1.2.3-beta8" > "1.2.3-alpha8"
```