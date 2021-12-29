{plans} = this->R1023
for a_plan in {plans} do
  %generate METSSP1:Plan_Active(); to a_plan  
endfor

{plan_detectors} = this->R1234
for a_detector in {plan_detectors} do
  if plan.Status != WAITING_STATUS then
    report = a_detector->R1077
    %generate METSREP3:Update_Request(); to report
  endif
endfor

{setting_changes} = plan->R1046 ordered by Created
for a_change in {setting_changes} do
  change_type = a_change.Type
  unlink plan R1046 a_change
  %generate METSSC2:Change_Processed(); to a_change
  breakif a_change.Created > this.Last_update_time
endfor

{updates} = this->R1046 ordered by Created
for an_update in {updates} do
  unlink this R1046 update
  link this R1045 update
  %generate METSU2:Update_Scheduled(); to an_update
  break
endfor

