# GameGenie Grammar

An ANTLR4 grammar for [Game Genie](https://en.wikipedia.org/wiki/Game_Genie) codes.

A Game Genie code is a 6 or 8 character string drawn from the 16-letter
alphabet `A P Z L G I T Y E O X U K S V N`, where each letter encodes one
hex digit. Multiple codes can be combined into a single equation with `+`
to patch more than one memory address at once.

Example codes taken from [here](https://www.mariomayhem.com/consoles/super_mario_bros_game_genie_codes/files/Nes_game_genie_full.txt).
