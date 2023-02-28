CREATE XML SCHEMA COLLECTION MyColl AS '
   <schema
    xmlns="http://www.w3.org/2001/XMLSchema"
    targetNamespace="https://MySchema/test_xml_schema">
      <element name="root" type="string"/>
  </schema>'
-- Modify the collection.
ALTER XML SCHEMA COLLECTION MyColl ADD '
  <schema xmlns="http://www.w3.org/2001/XMLSchema"
         targetNamespace="https://MySchema/test_xml_schema">
     <element name="anotherElement" type="byte"/>
 </schema>';
