
select * from continuing.item where type_code =10 and standard_pack_size is null;

--- default
--47
update continuing.item set standard_pack_size = null  where type_code=10 ;


--16
update continuing.item 
set standard_pack_size = 25 
where type_code=10 
and ( item_form ~  ' 25 '  
  or item_form ~ ' 25$');

--10
update continuing.item 
set standard_pack_size = 50 
where type_code=10 
  and ( item_form ~ ' 50 '  
  or item_form ~ ' 50$');

--3
update continuing.item 
set standard_pack_size = 30 
where type_code=10 
and ( item_form~ ' 30 '  
  or item_form ~ ' 30$');

--4
  update continuing.item 
  set standard_pack_size = 100 
  where type_code=10 
  and ( item_form~ ' 100 '  
    or item_form ~ ' 100$');

-- 2
update continuing.item 
  set standard_pack_size = 5 
  where type_code=10 
  and ( item_form~ ' 5 '  
    or item_form ~ '\[5\]');


-- www.guildlink.com.au/gc/ws/ro/pi.cfm?product=roprivot10314  
-- oral, drops, for children only??
--1
update continuing.item set standard_pack_size = 25 where item_code='05342E'; 

--7 + 1
update continuing.item set standard_pack_size = 1 where standard_pack_size is null and item_form like '%do Not Approve%' and type_code=10;
update continuing.item set standard_pack_size = 1 where standard_pack_size is null and item_form like '%do No Approve%' and type_code=10;

-- https://www.medicines.org.uk/emc/product/5783/smpc
-- 5mg / day minimum dosage
--1
update continuing.item set standard_pack_size = 20 where item_code='02669L'; 

-- www.medsafe.govt.nz/profs/datasheet/r/Rivotriltabdropinj.pdf
-- 3mg / day clozanapal
--2
update continuing.item set standard_pack_size = 20 where item_code in ('01808E', '05339B') ; 


update continuing.item set standard_pack_size = 2 where atc = 'N02AE01' and standard_pack_size is null ; 
update continuing.item set standard_pack_size = 30 where item_code = '04522B' and standard_pack_size is null ; 
update continuing.item set standard_pack_size = 14 where item_code = '97997R' and standard_pack_size is null ; 
update continuing.item set standard_pack_size = 5 where item_form like '% 5 X %' and pbs_route='INJECTION' and standard_pack_size is null ; 


--https://www.mims.co.uk/tramadol-available-drops/pain/article/1086125
-- 100 mg / ml, 400 mg/ day, 10ml
update continuing.item set standard_pack_size = 2  where standard_pack_size is null  and item_code = '05150';   -- 1000
update continuing.item set standard_pack_size = 5 where standard_pack_size is null  and item_code = '02122';  -- 400
update continuing.item set standard_pack_size = 5 where standard_pack_size is null  and item_code = '02123';  -- 1000
update continuing.item set standard_pack_size = 5 where standard_pack_size is null  and item_code = '02124';  -- 2000
update continuing.item set standard_pack_size = 5 where standard_pack_size is null  and item_code = '05238';  -- 1000
update continuing.item set standard_pack_size = 5 where standard_pack_size is null  and item_code = '08843';  -- 1000
