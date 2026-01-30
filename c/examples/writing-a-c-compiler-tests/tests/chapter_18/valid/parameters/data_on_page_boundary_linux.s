    .bss        # .bss is the last segment in the program image - the page after it will be unampped
    .align 4096 # force page alignment
    .zero 4085  # write a bunch of zeros so on_page_boundary is at the end of the page
    .globl on_page_boundary
on_page_boundary:
    .zero 11
    .section	".note.GNU-stack","",@progbits
