    .bss        # .bss is the last segment in the program image - the page after it will be unampped
    .align 12   # force page alignment (2^12 == 4096)
    .zero 4085  # write a bunch of zeros so on_page_boundary is at the end of the page
    .globl _on_page_boundary
_on_page_boundary:
    .zero 10
