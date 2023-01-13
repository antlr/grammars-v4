  interface class IntfBaseA; pure virtual
  function bit funcBase()
  ;
  endclass
  interface class IntfBaseB; pure virtual
  function string funcBase()
  ;
  endclass
class ClassA implements IntfBaseA, IntfBaseB;
  virtual function bit funcBase();
    return (0);
  endfunction
endclass
