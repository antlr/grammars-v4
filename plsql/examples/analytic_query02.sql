select time_id, product
   , last_value(quantity ignore nulls) over (partition by product order by time_id) quantity
   , last_value(quantity respect nulls) over (partition by product order by time_id) quantity
   from ( select times.time_id, product, quantity 
             from inventory partition by  (product) 
                right outer join times on (times.time_id = inventory.time_id) 
   where times.time_id between to_date('01/04/01', 'dd/mm/yy') 
      and to_date('06/04/01', 'dd/mm/yy')) 
   order by  2,1


