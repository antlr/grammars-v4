.attribute arch, rv64imafdc
# You can only use instructions from the i, m, a, f, d and c extensions.
memcpy_general:
    add     a5,a1,a2
    beq     a1,a5,.L2
    add     a2,a0,a2
    mv      a5,a0
.L3:
    addi    a1,a1,1
    addi    a5,a5,1
    lbu     a4,-1(a1)
    sb      a4,-1(a5)
    bne     a5,a2,.L3
.L2:
    ret

.option push     # Push current options to the stack.
.option arch, +v # Enable vector extension, we can use any instruction in imafdcv extension.
memcpy_vec:
    mv a3, a0
.Lloop:
    add a1, a1, t0
    sub a2, a2, t0
    add a3, a3, t0
    bnez a2, .Lloop
    ret
.option pop   # Pop current option from the stack, restore the enabled ISA extension status to imafdc.

.option push     # Push current option to the stack.
.option arch, -c # Disable compressed extension, we can't use any instruction in extension.
memcpy_norvc:
    add     a5,a1,a2
    beq     a1,a5,.L2
    add     a2,a0,a2
    mv      a5,a0
.L3:
    addi    a1,a1,1
    addi    a5,a5,1
    lbu     a4,-1(a1)
    sb      a4,-1(a5)
    bne     a5,a2,.L3
.L2:
    ret
csrrw t1,frm,t1
.set arch, 123
/*
    test
    test
*/
lbdads:
.option pop   # Pop current option from the stack, restore the enabled ISA extension status to imafdc.

.option push  # Push current option to the stack.
.option arch, rv64imc # Set arch to rv64imc.
    nop
.option pop   # Pop current option from the stack, restore the enabled ISA extension status to imafdc.
