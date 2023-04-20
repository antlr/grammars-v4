#!/usr/bin/awk -f
{
	match($0, /(https?:\/\/)?(www\.)?([a-zA-Z0-9-]+)\.[a-z]{2,}/, match_array)
	if (match_array[3]) {
		print match_array[3]
	}
}
