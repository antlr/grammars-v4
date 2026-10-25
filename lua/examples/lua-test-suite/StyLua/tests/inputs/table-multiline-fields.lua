local obj = { method1 = function(self) end, method2 = function(self, name) end }

local obj = { method1 = function(self) print(true) end, method2 = function(self, name) end }

local obj = { method1 = function(self) end, method2 = function(self, name) end, method3 = function(self) end, method4 = function(self, name) end, ["some-method"] = function(self) end, ["another-method"] = function(self, name) end, ["some-another-method"] = function(self) end, ["yet-another-method"] = function(self, name) end }