if plan_active then
  %generate METSSP1:Plan_Active(); to this
endif

if plan_active then
  %generate METSSP1:Plan_Active(); to this
else
  %generate METSSP2:Plan_Inactive(); to this
endif

if found = FALSE then
  %generate DEVCDCTR7:Invalid_Device(address); to init
endif

if (address != UNDEFINED) and (found = FALSE) then
  %generate DEVCDCTR7:Invalid_Device(address); to init
endif

if (address != UNDEFINED) and (not device_found) then
  %generate DEVCDCTR7:Invalid_Device(address); to init
endif

if (address != UNDEFINED) and !device_found then
  %generate DEVCDCTR7:Invalid_Device(address); to init
endif

if address != UNDEFINED and device_found then
  %generate DEVCDCTR2:Valid_Device(address); to init
endif

if address != UNDEFINED then
  detector = find-one DEVC_DETECTOR where Address = address
  if detector != UNDEFINED then
    %generate DEVCDCTR2:Valid_Device(address); to init
  else
    %generate DEVCDCTR3:Invalid_Device(address); to init
  endif
endif

