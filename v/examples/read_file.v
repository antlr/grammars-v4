import os

// Print file lines that starth with "DEBUG:"
fn main() {
	// `read_file` returns an optional (`?string`), it must be checked
	text := os.read_file('app.log') or {
		println('failed to read the file')
		return
	}
	lines := text.split_into_lines()
	for line in lines {
		if line.starts_with('DEBUG:') {
			println(line)
		}
	}
}
