define structure detector_struct
  geog_addr Text
  status    METS_DETECTOR.Status_type
enddefine

define structure plan_struct
  plan_number Integer
  signs       sign_struct
  signals     signal_struct
enddefine

{detector_status} is detector_struct
for detector in {detectors} do
   append [detector.Geog_addr, detector.Status] to {detector_status}
endfor
{plan_devices} is plan_struct
for a_plan in {sensor_plans} do
   {signs} = a_plan->R1001
   {signals} = a_plan->R1002
   append [plan.Plan_no, {signs}, {signals}] to {plan_devices} \
          ordered by plan_number
endfor

for [address, status] in {detector_status} do
   %generate E_METSDET5:Detector_Status(address, status);
endfor

for [plan_no, {signs}, {signals}] in {plan_devices} do
   %generate E_METSPL6:Plan_Devices(plan_no);
   for [address, setting] in {signs} do
      %generate E_METSPL7:Plan_Sign(address, setting);
   endfor
   for [address, setting] in {signals} do
      %generate E_METSPL8:Plan_Signal(address, setting);
   endfor
   %generate E_METSPL9:Plan_Data_Complete();
endfor

{ordered_dets} = {detectors} ordered by address
{ordered_updates} = {updates} reverse ordered by time_of_update

{detector_updates} = {all_updates} where geog_addr = address
{ib_signals} = {all_indicators} where type = SIGNAL & inboundary = TRUE
