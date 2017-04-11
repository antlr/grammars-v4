Dim I, J
I = 20
J = 30

While I > 0
	I = I - 1
Wend


I = 100

While I > 0
	While I Mod 15 <> 0
		I = I - 1
	Wend
Wend