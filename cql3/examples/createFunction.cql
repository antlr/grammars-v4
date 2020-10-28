CREATE OR REPLACE FUNCTION cycling.avgFinal ( state tuple<int,bigint> ) 
CALLED ON NULL INPUT 
RETURNS double 
LANGUAGE java AS 
  $$ double r = 0; 
     if (state.getInt(0) == 0) return null; 
     r = state.getLong(1); 
     r/= state.getInt(0); 
     return Double.valueOf(r); $$ 
;