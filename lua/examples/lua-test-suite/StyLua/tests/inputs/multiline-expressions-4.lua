function SetCallsign(Player, Callsign)
	if Settings.PolicingSetup.Radio or (table.find(Settings.PolicingSetup.CallsignPrefix, string.sub(Callsign, 1, 2)) and tonumber(string.sub(Callsign, 3, 4))) then
		Player:SetAttribute("Callsign", Callsign)
	end
end