if ((this.NFS_server != UNDEFINED) | (local_site_data != UNDEFINED)) and \
  (new_mode = LOADING | new_mode = INITIALISATION | new_mode = NORMAL) then
  status_notification = create unique NMCS_SUBSYSTEM_STATUS_NOTIFICATION
  %generate NMCSSSN1:Subsystem_Status_Changed(this.Subsystem_code, new_mode); to status_notification
endif

if (address != UNDEFINED) and (not device_found) then
  %generate DEVCDCTR7:Invalid_Device(address); to init
endif

if (address != UNDEFINED) and !device_found then
  %generate DEVCDCTR7:Invalid_Device(address); to init
endif

if !(address = UNDEFINED) and device_found then
  %generate DEVCDCTR7:Invalid_Device(address); to init
endif

if !((address = UNDEFINED) and (type = IP)) then
  %generate DEVCDCTR7:Invalid_Device(address); to init
endif

if !((address = UNDEFINED) and !(type = IP)) then
  %generate DEVCDCTR7:Invalid_Device(address); to init
endif

if ((this.NFS_server != UNDEFINED) | (local_site_data != UNDEFINED)) and \
  not (mode = LOADING) then
  %generate NMCSSSN1:Subsystem_Status_Changed(this.Subsystem_code, mode); to status_notification
endif

