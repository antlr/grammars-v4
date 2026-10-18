-- Taken from https://github.com/JohnnyMorganz/StyLua/blob/main/tests/inputs/multiline-expressions-3.lua
do
	do
		do
			do
				local text = "Players: " .. #Server_Container.ARandomVariableWhichIsVeryLongSoThatThisGetsOverTheColumnLimit.Players_F:GetChildren() - 1 .. "/20"
				local ratio = (minAxis - minAxisSize) / delta * (self.props.maxScaleRatio - self.props.minScaleRatio) + self.props.minScaleRatio
				local ratio2 = (minAxis - minAxisSize) / delta * (self.props.maxScaleRatio - self.props.minScaleRatio) * self.props.aRandomVariableWhichIsVeryLong + self.props.minScaleRatio
			end
		end
	end
end
