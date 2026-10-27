begin
   for i in (
      select
          1 as fnc,
          2 as pkb,
          3 as pks,
          4 as prc,
          5 as trg,
          6 as vw,
          7 as disabled,
          8 as counted
      from dual
   ) loop
       null;
   end loop;
end;
/