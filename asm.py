
define(TOS=r27)
define(TOSL=r26)
define(Working=r16)
define(Base=r8)

org(SRAM_START)
buffer_length = 0x40
label(buffer, reserves=buffer_length)
label(data_stack)

org(0x0000)
jmp(RESET)
for _ in range(4):   # Let's pretend there are four interrupt vectors.
  jmp(BAD_INTERUPT)

label(BAD_INTERUPT)
jmp(0x0000)

label(RESET)
cli()

ldi(Working, low(RAMEND))
out(SPL, Working)
ldi(Working, high(RAMEND))
out(SPH, Working)

ldi(YL, low(data_stack))
ldi(YH, high(data_stack))

rcall(UART_INIT)

ldi(Working, 23)
sts(TWBR, Working) # set bitrate
ldi(Working, 1)
sts(TWSR, Working) # set prescaler

ldi(Working, 10)
mov(Base, Working)

sei()

label(MAIN)
rcall(INTERPRET_PFA)
rcall(DOTESS_PFA)
rjmp(MAIN)

