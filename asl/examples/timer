[timer_id] = Create_Timer[]
[this.Inactivity_timer] = Create_Timer[]
%generate TIM1:Set_Timer(10, METSDET5, this) ; to timer_id
%generate TIM1:Set_Timer(10, METSDET5, detector) ; to timer_id
%generate TIM1:Set_Timer(this.Check_timeout, METSI5, this) ; to this.Check_timer
%generate TIM2:Reset_Timer(); to this.Timer_id
%generate TIM3:Set_Chimer(this.Test_period, this.Test_period, METSDET5, this) ; to this.Timer_id
%generate TIM3:Set_Chimer(1, 10, METSDET5, this) ; to this.Timer_id
%generate TIM10:Set_Absolute_Timer(this.Year, this.Month, this.Day_of_month, this.Hour, this.Minute, this.Second, METSDET5, this) ; to this.Timer_id
%generate TIM10:Set_Absolute_Timer(year, month, date, 12, 0, 0, LOGSCONT2, this) ; to this.Timer_id
