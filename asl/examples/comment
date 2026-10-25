# Get all plans
{plans} = this->R1023
# Iterate through plans
for a_plan in {plans} do
  # Activate plan
  %generate METSSP1:Plan_Active(); to a_plan  
endfor

if plan_active then
  # Raise alarm
  %generate METSA1:Plan_Active();
else
  update = this->R1066
  %generate METSU2:Update_Processed(); to update
  # Wait
  # and
  # try
  # again
  %generate TIM1:Set_Timer(10, METSDET5, this) ; to timer_id
endif
