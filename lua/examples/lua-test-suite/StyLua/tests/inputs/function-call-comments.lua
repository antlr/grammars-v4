local AppRodux = RoactRodux.connect(
	function(state, props)
		return {
			
		}
	end
	-- function(dispatch)
	--   return {
	--     setCrossSize = function(crossSize)
	--       dispatch({
	--         type = "SetCrossSize",
	--         crossSize = crossSize
	--       })
	--     end
	--   }
	-- end
)(AppComponent)