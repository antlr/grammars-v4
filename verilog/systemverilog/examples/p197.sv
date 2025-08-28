  interface class IntfClass; pure virtual
  function bit funcBase()
  ; pure virtual
  function bit funcExt()
  ;
  endclass
class BaseClass;
  virtual function bit funcBase();
    return (1);
  endfunction
endclass
class ExtClass extends BaseClass implements IntfClass;
  virtual function bit funcExt();
    return (0);
  endfunction
endclass
