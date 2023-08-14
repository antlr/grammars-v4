# WKT CRS v1

Grammar for parsing Well Known Text Coordinate Reference System, version 1.

Reference: [Specification](https://docs.ogc.org/is/12-063r5/12-063r5.html)

The grammar tries to adhere to the spec but have not made extensive tests.

This grammar is extended with ability to parse properties file, containing EPSG=WKTCRS type of definitions.
For example one can parse [GeoTools epsg.properties](https://raw.githubusercontent.com/geotools/geotools/main/modules/plugin/epsg-wkt/src/main/resources/org/geotools/referencing/epsg/wkt/epsg.properties) directly.

If the input is properties file, use `propsFile` starting rule.

If the input is pure WKT, use `wkt` starting rule.

## License

BSD