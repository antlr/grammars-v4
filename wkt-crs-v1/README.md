# WKT CRS v1

Grammar for parsing Well Known Text Coordinate Reference System, version 1.

References:

- [WKT](https://en.wikipedia.org/wiki/Well-known_text_representation_of_coordinate_reference_systems)
- [WKT v1](https://github.com/geotools/geotools/blob/main/modules/library/opengis/src/main/java/org/opengis/referencing/doc-files/WKT.html)

The grammar tries to adhere to the spec but have not made extensive tests.

This grammar is extended with ability to parse multiple WTK definitions from a properties file of the format EPSG_CODE=WKTCRS.
Specifically it ca parse [GeoTools epsg.properties](https://raw.githubusercontent.com/geotools/geotools/main/modules/plugin/epsg-wkt/src/main/resources/org/geotools/referencing/epsg/wkt/epsg.properties).

Grammar rules:

- if the input is properties file, use `propsFile` starting rule.
- if the input is pure WKT, use `wkt` starting rule.
