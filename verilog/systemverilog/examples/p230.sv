task automatic do_n_way(int N);
  process job[] = new[N];
  foreach (job[j])
  fork
    automatic int k = j;
    begin
      job[k] = process::self();
    end
  join_none
  foreach (job[j]) wait (job[j] != null);
  job[1].await();
  foreach (job[j]) begin
    if (job[j].status != process::FINISHED) job[j].kill();
  end
endtask
