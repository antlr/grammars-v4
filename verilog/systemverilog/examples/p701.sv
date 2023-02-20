module renamed_concat (
    .a({b, c}),
    f,
    .g(h[1])
);
endmodule
module same_input (
    a,
    a
);
  input a;
endmodule
module mixed_direction (
    .p({a, e})
);
  input a;
  output e;
endmodule
