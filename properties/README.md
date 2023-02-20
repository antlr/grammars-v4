## [Properties File Format](https://docs.oracle.com/cd/E23095_01/Platform.93/ATGProgGuide/html/s0204propertiesfileformat01.html)
The properties files read by Nucleus must follow a format that is recognized by the class java.util.Properties. The rules for the format are as follows:

+ Entries are generally expected to be a single line of the form, one of the following:
```properties
 propertyName=propertyValue
 propertyName:propertyValue
```


+ White space that appears between the property name and property value is ignored, so the following are equivalent.

```properties
name=Stephen
name = Stephen
```

White space at the beginning of the line is also ignored.

+ Lines that start with the comment characters ! or # are ignored. Blank lines are also ignored.

+ The property value is generally terminated by the end of the line. White space following the property value is not ignored, and is treated as part of the property value.

+ A property value can span several lines if each line is terminated by a backslash (‘\’) character. For example:

```properties
targetCities=\
        Detroit,\
        Chicago,\
        Los Angeles
```

This is equivalent to targetCities=Detroit,Chicago,Los Angeles (white space at the beginning of lines is ignored).

+ The characters newline, carriage return, and tab can be inserted with characters \n, \r, and \t, respectively.

+ The backslash character must be escaped as a double backslash. For example:

```properties
path=c:\\docs\\doc1
```

**Note**: It is not necessary to escape backslashes in property values when you use the ATG Control Center Components window; the ATG Control Center handles the escape characters automatically.

+ UNICODE characters can be entered as they are in a Java program, using the \u prefix. For example, `\u002c`.