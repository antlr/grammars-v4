package body Attribute_Reference is 
  Name : constant := A_Type'Class (expression);
end;

package body Subtype_Mark is 
  Name : A_Type'Class := expression;
end;