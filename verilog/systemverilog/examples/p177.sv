class id;
  static int current = 0;
  static function int next_id();
    next_id = ++current;
  endfunction
endclass
