	# validate that space allocated for return address does not overlap
	# with storage for other objects we can access from callee (this is an ABI requirement)
	# case 1: overlap w/ global variable
	.data
	.globl globvar
	.align 8
globvar:
	.zero 24
	.text
	# globvar = overlap_with_globvar()
	.globl overlap_with_globvar
overlap_with_globvar:
	# load address of globvar into RSI
	leaq	globvar(%rip),	%rsi
	# make sure return address (in RDI) and globvar's address (in RSI)
	# are at least 24 bytes apart
	subq	%rdi, %rsi
	# get absolute value of difference between addresses (in RSI)
	# (absolute value implementation from https://stackoverflow.com/a/11927940)
	movq	%rsi, %rcx	# copy RSI into RCX
	negq	%rsi	# negate RSI
	## if RSI is now negative, its restore original (positive) value
	cmovl	%rcx, %rsi
	# compare diference to 24
	cmpq	$24, %rsi
	jl	.Loverlap_detected
	# no overlap, so go ahead and return value
	movq	$400, (%rdi)
	movq	$500, 8(%rdi)
	movq	$600, 16(%rdi)
	movq	%rdi, %rax
	ret
.Loverlap_detected:
	# space for return value overlaps w/ globvar, so exit with error code
	movl	$1, %edi
	call	exit

	# case 2: overlap w/ pointer passed as arg
	# x = overlap_with_pointer(&x)
	.globl	overlap_with_pointer
overlap_with_pointer:
	# copy pointer into RDX
	leaq	globvar(%rip), %rdx
	# make sure return address (in RDI) and globvar's address (in RDX)
	# are at least 24 bytes apart
	subq	%rdi, %rdx
	# get absolute value of difference between addresses (in RDX)
	# (absolute value implementation from https://stackoverflow.com/a/11927940)
	movq	%rdx, %rcx	# copy RDX into RCX
	negq	%rdx			# negate RDX
	## if rdx is now negative, its restore original (positive) value
	cmovl	%rcx, %rdx
	# compare diference to 24
	cmpq	$24, %rdx
	jl	.Loverlap_detected.0
	# no overlap, so go ahead and return value
	# set each member to twice its original value (accessed thru RSI)
	# l1
	movq	(%rsi), %rcx
	imul	$2, %rcx
	movq	%rcx, (%rdi)
	# l2
	movq	8(%rsi), %rcx
	imul	$2, %rcx
	movq	%rcx, 8(%rdi)
	# l3
	movq	16(%rsi), %rcx
	imul	$2, %rcx
	movq	%rcx, 16(%rdi)
	movq	%rdi, %rax
	ret
.Loverlap_detected.0:
	# space for return value overlaps w/ space pointed to by argument, so exit with error code
	movl	$3, %edi
	call	exit
	.section	".note.GNU-stack","",@progbits
