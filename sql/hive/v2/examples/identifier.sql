insert overwrite table pre_qdas_channelsys_indexpool partition (p_day_id,p_index_id)
select  p_day_id,appkey,ver,channel,split(valueA,':')[0],split(valueA,':')[1],p_day_id,split(valueA,':')[0]
from (
         select a.p_day_id,
                a.appkey,
                a.ver,
                a.channel,
                CONCAT_WS(",",CONCAT_WS(":" ,case when a.p_day_id =$today$    then "new_pay1_mids"
                                                  when a.p_day_id =$daydiff1$ then "new_pay2_mids"
                                                  when a.p_day_id =$daydiff2$ then "new_pay3_mids"
                                                  when a.p_day_id =$daydiff3$ then "new_pay4_mids"
                                                  when a.p_day_id =$daydiff4$ then "new_pay5_mids"
                                                  when a.p_day_id =$daydiff5$ then "new_pay6_mids"
                                                  when a.p_day_id =$daydiff6$ then "new_pay7_mids"
                                                  when a.p_day_id =$daydiff13$ then "new_pay14_mids"
                                                  when a.p_day_id =$daydiff29$ then "new_pay30_mids"
                                                  else "othen_mids"
                    end ,count(distinct m2) ),
                          CONCAT_WS(":" ,case when a.p_day_id =$today$    then "new_pay1_amount"
                                              when a.p_day_id =$daydiff1$ then "new_pay2_amount"
                                              when a.p_day_id =$daydiff2$ then "new_pay3_amount"
                                              when a.p_day_id =$daydiff3$ then "new_pay4_amount"
                                              when a.p_day_id =$daydiff4$ then "new_pay5_amount"
                                              when a.p_day_id =$daydiff5$ then "new_pay6_amount"
                                              when a.p_day_id =$daydiff6$ then "new_pay7_amount"
                                              when a.p_day_id =$daydiff13$ then "new_pay14_amount"
                                              when a.p_day_id =$daydiff29$ then "new_pay30_amount"
                                              else "othen_amount"
                              end ,sum(amount))) values
         from pre_qdas_channelsys_eqtreg a
             join pre_channel_pay_info_daily b
         on a.m2 = b.m2 and a.appkey=b.p_app_id
         where a.p_day_id
             in ( $today$,$daydiff1$,$daydiff2$,$daydiff3$,$daydiff4$,$daydiff5$,$daydiff6$,$daydiff13$,$daydiff29$ )
           and b.p_day_id >= $daydiff29$ and b.p_day_id <= $today$
         group by  a.p_day_id,
             a.appkey,
             a.ver,
             a.channel ) a
    lateral view explode(split(values,',')) adtable as valueA
where valueA <>'NULL' and valueA <>'';