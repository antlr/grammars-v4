    ## callee returns 10.0 in XMM0 and zeros out all the other XMM registers,
    ## to test that we don't expect them to be preserved across function calls

    ## return value
	.literal8
Lretval:
    .double 10.0

    ## function
    .text
    .globl _callee
_callee:
    ## move return value into XMM0
    movsd Lretval(%rip), %xmm0
    ## zero out XMM1-XMM15
    xorpd %xmm1, %xmm1
    xorpd %xmm2, %xmm2
    xorpd %xmm3, %xmm3
    xorpd %xmm4, %xmm4
    xorpd %xmm5, %xmm5
    xorpd %xmm6, %xmm6
    xorpd %xmm7, %xmm7
    xorpd %xmm8, %xmm8
    xorpd %xmm9, %xmm9
    xorpd %xmm10, %xmm10
    xorpd %xmm11, %xmm11
    xorpd %xmm12, %xmm12
    xorpd %xmm13, %xmm13
    xorpd %xmm14, %xmm14
    xorpd %xmm15, %xmm15
    retq
