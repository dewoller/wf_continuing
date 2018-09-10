

select * 
FROM continuing.continuing o
JOIN continuing.continuing b ON (


);


explain analyze

create table temp.barb_op_concordance_rr as 
with barb as (
  select pin, id, supply_date as sd, supply_date + (days_multiplier * quantity) as ed
  from continuing.continuing_rr 
  JOIN continuing.item USING (item_code)
  where type_code=10
), op as (
  select pin, id, supply_date as sd, supply_date + (days_multiplier * quantity) as ed
  from continuing.continuing_rr 
  JOIN continuing.item USING (item_code)
  where type_code<10
) 
select b.pin, b.id as b_id, o.id as o_id
FROM barb b 
JOIN op o ON (( b.pin = o.pin ) AND
(
  ( o.sd > b.sd AND o.sd < b.ed )   -- o.sd between b
  OR
  (o.ed > b.sd AND o.ed < b.ed )    -- o.ed between b
  OR
  (b.sd > o.sd AND b.sd < o.ed)     -- b between o
))
; 



distinct dispensations oid and bid respectively

+---------+---------+
| count   | count   |
|---------+---------|
| 1552711 / 5870372 
| 933229 / 2577945  |
+---------+---------+
SELECT 1
(END)







