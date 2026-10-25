function foo()
	while NextFreq.Access ~= true and not Llama.List.find(NextFreq.Access, Players.LocalPlayer.Team.Name) and NextIndex > #Constants.RADIO_CHANNEL_ORDER do
		print("test")
	end
end