.align 2
.include "cfg.inc"
.equ UART_REG_TXFIFO,   0

.section .text
.globl _start

_start:
        csrr  t0, mhartid             # read hardware thread id (`hart` stands for `hardware thread`)
        bnez  t0, halt                # run only on the first hardware thread (hartid == 0), halt all the other threads

        la    sp, stack_top           # setup stack pointer

        la    a0, msg                 # load address of `msg` to a0 argument register
        jal   puts                    # jump to `puts` subroutine, return address is stored in ra regster

halt:   j     halt                    # enter the infinite loop

puts:                                 # `puts` subroutine writes null-terminated string to UART (serial communication port)
                                      # input: a0 register specifies the starting address of a null-terminated string
                                      # clobbers: t0, t1, t2 temporary registers

        li    t0, UART_BASE           # t0 = UART_BASE
1:      lbu   t1, (a0)                # t1 = load unsigned byte from memory address specified by a0 register
        beqz  t1, 3f                  # break the loop, if loaded byte was null

                                      # wait until UART is ready
2:      lw    t2, 0(t0)               # t2 = uart[UART_REG_TXFIFO]
        bltz  t2, 2b                  # t2 becomes positive once UART is ready for transmission
        sw    t1, 0(t0)               # send byte, uart[UART_REG_TXFIFO] = t1

        addi  a0, a0, 1               # increment a0 address by 1 byte
        j     1b

3:      ret

.section .rodata
msg:
     .string "Hello."
