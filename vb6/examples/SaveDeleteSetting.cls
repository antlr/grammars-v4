' registry access

Dim StringVar As String
Const StringConst As String = "Section2"
StringVar = "Section3"

SaveSetting "AppName", "Section", "Key", 20
SaveSetting "AppName" , "Section" ,"Key2", 40
SaveSetting "AppName" , "Section" ,"Key2", 40
SaveSetting "AppName" , StringConst ,"Key3", 50
SaveSetting "AppName" , StringVar ,"Key4", StringConst


DeleteSetting "AppName", "Section", "Key"
DeleteSetting "AppName", "Section"
DeleteSetting "AppName", StringVar, "Key3"