# FBX Grammar

An ANTLR4 grammar for text-based [FBX][1] files.

FBX (**F**ilm**b**o**x**) is a proprietary file format (.fbx),
originally developed by Kaydara but now owned by Autodesk since 2006.
It is used to provide interoperability between digital content creation
applications, although usage of this text-based format appears to be
less common than the alternative binary format.

There are actually _two_ parsers:
* FBX - parses just the nested node structures and you have to figure
  the names out on your own.
* FBXSemantic - tries to recognise the individual node names and more
  strictly checks their contents. It is likely very incomplete as we
  don't have official documentation for the format, but parses all
  current examples fine.

Most likely, if you are going to implement this format, you are going to
also want to implement a corresponding parser for the binary format,
map the two to the same abstract tree structure, and then work with the
tree structure.

Adapted from references:
* [A Quick Tutorial About the FBX ASCII Format][2]


[1]: https://en.wikipedia.org/wiki/FBX
[2]: https://banexdevblog.wordpress.com/2014/06/23/a-quick-tutorial-about-the-fbx-ascii-format/
