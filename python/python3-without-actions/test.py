def headline2(
    text,           # type: str
    width=80,       # type: int
    fill_char='-',  # type: str
):                  # type: (...) -> str
    return f" {text.title()} ".center(width, fill_char)