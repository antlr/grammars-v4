print("moduleN")
_G.num_loaded = _G.num_loaded + 1
local moduleM = require("moduleM")
return {"moduleN", M=moduleM}
