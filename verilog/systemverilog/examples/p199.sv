  interface class IntfBase1; pure virtual
  function bit funcBase()
  ;
  endclass
  interface class IntfBase2; pure virtual
  function bit funcBase()
  ;
  endclass
virtual class ClassBase;
  pure virtual function bit funcBase();
endclass
class ClassExt extends ClassBase implements IntfBase1, IntfBase2;
  virtual function bit funcBase();
    return (0);
  endfunction
endclass
