inline fun bar(block: (() -> String)): String {
return (block)!!()
}
inline fun bar2(): String {
return bar({return "def"})
}