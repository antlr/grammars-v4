select warehouse_name warehouse, warehouse2."water", warehouse2."rail"
from warehouses,
 xmltable('/warehouse'
  passing warehouses.warehouse_spec
  columns
   "water" varchar2(6) path '/warehouse/wateraccess',
   "rail" varchar2(6) path '/warehouse/railaccess')
warehouse2
