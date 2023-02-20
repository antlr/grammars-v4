class C;
  rand int A[];
  constraint c1 {A.size inside {[1 : 10]};}
  constraint c2 {foreach (A[k]) (k < A.size - 1) -> A[k+1] > A[k];}
endclass
