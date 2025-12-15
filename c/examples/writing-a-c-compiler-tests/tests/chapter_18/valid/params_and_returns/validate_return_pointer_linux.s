    .globl main
main:
    pushq	%rbp
    movq	%rsp, %rbp
    # allocate stack space for result struct
    subq    $32, %rsp
    movq    %rsp, %rdi # rsp points to result space
    movq    $0, %rax # make sure RAX isn't a valid pointer to start
    callq   return_in_mem
    # check result, using RAX as base pointer
    cmpq    $1, 0(%rax)
    jne .Lfail
    cmpq    $2, 8(%rax)
    jne .Lfail
    cmpq    $3, 16(%rax)
    jne .Lfail
    # success
    movq    $0, %rax
    movq    %rbp, %rsp
    popq    %rbp
    retq
.Lfail:
    # fail - raise SIGSEGV
    movl	$11, %edi
    callq	raise@PLT
    movq    %rbp, %rsp
    popq    %rbp
    retq
	.section	".note.GNU-stack","",@progbits
