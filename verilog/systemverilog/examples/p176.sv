class E #(
    type T = int
) extends C;
  T x;
  function new(T x_init);
    super.new();
    x = x_init;
  endfunction
endclass
