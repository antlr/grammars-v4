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

CREATE OR REPLACE FUNCTION setMin(input set<int>) RETURNS NULL ON NULL INPUT RETURNS int LANGUAGE java AS '
                int min = Integer.MAX_VALUE;
                for (Object i : input) { min = Math.min(min, (Integer) i); }
                return min;
            ';
