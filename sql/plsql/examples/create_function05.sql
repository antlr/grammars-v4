CREATE FUNCTION fun_streaming(p ref_cursor_type) RETURN tab_rec_type PIPELINED
  CLUSTER p BY Region
  PARALLEL_ENABLE(PARTITION p BY Region) IS
  ret_rec rec_type;
  cnt number;
  sum number;
BEGIN
  FOR rec IN p LOOP
    IF (first rec in the group) THEN
        cnt := 1;
        sum := rec.Sales;
    ELSIF (last rec in the group) THEN
      IF (cnt <> 0) THEN
        ret_rec.Region := rec.Region;
          ret_rec.AvgSales := sum/cnt;
          PIPE ROW(ret_rec);
        END IF;
    ELSE
       cnt := cnt + 1;
      sum := sum + rec.Sales;
    END IF;
  END LOOP;
  RETURN;
END;