%generate E_MENUC7:Print_Request(printer_number);
print_complete = FALSE
start_new_print = FALSE
loop
  if a_data_item = UNDEFINED | \
    a_data_item.Sequence > last_line_number then
    print_complete = TRUE
  else
    if (a_data_item.Sequence - first_line_number) >= 1000 then
      # Need to start a new print
      first_line_number = a_data_item.Sequence
      start_new_print = TRUE
    else
      %generate E_MENUC8:Print_Item(a_data_item.Data);
      a_data_item = a_data_item->R3153."precedes"
   endif
  endif
  breakif (print_complete | start_new_print)
endloop
%generate E_MENUC9:Print_Data_Complete();
