
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

label(UART_INIT)
ldi(r17, high(520)) # 2400 baud w/ 20Mhz osc
ldi(r16, low(520))  # See Datasheet
sts(UBRR0H, r17)
sts(UBRR0L, r16)
# The chip defaults to 8N1 so we won't set it here even though we should.
ldi(r16, (1 << TXEN0) | (1 << RXEN0)) # Enable transmit/receive
sts(UCSR0B, r16)
ret()

label(KEY)
dw(0x0000)
db(3, "key")

label(KEY_PFA)
lds(Working, UCSR0A)
sbrs(Working, RXC0)
rjmp(KEY_PFA)

rcall(DUP_PFA)
lds(TOS, UDR0)

rcall(ECHO_PFA)
ret()
