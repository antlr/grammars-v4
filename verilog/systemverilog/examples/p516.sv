class C;
  rand byte A[];
  constraint C1 {
    foreach (A[i])
    A[i] inside {2, 4, 8, 16};
  }
  constraint C2 {foreach (A[j]) A[j] > 2 * j;}
endclass
