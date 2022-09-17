position = 0
loop
  position = position + 1
  next_char = first_char + position
  breakif next_char = " "
endloop

fault_no = 1
loop
  fault = find-one LOGS_FAULT where Fault_number = fault_no
  breakif fault = UNDEFINED
  fault_no = fault_no + 1
endloop
