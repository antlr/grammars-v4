found = FALSE
switch input
  case 1
    value = 1
    found = TRUE
  case 2
    value = 2
    found = TRUE
endswitch

switch plan_mode
  case AUTOMATIC
    %generate METSSP1:Plan_Active(); to this
  case SEMI_AUTO
    this.Pending_status = PENDING
    this.Pending_time = current-time
  case MANUAL
    %generate METSSP5:Manual_Plan_Active(); to this
  case SOME_TEXT
    plan = this->R12
    %generate METSPL8:Change_Complete(); to plan
endswitch

det_type = this->R34
switch det_type.Category
  case FOG
    %generate METSDET1:Detector_Active(FOG); to this
  case WIND
    %generate METSDET1:Detector_Active(WIND); to this
  default
    %generate METSDET3:Update_Complete(); to this
endswitch

