
.nolist
.include "m328Pdef.inc"
.list
.listmac

.def TOS = r27 ; XH
.def TOSL = r26 ; XL

.def Working = r16

.def word_counter = r17

.def Base = r8

.def number_pointer = r9

.dseg
.org SRAM_START

buffer: .byte 0x40

data_stack: .org 0x0140 ; SRAM_START + buffer

.MACRO popup
  ld TOSL, -Y
.ENDMACRO

.cseg

.org 0x0000
  jmp RESET
  jmp BAD_INTERUPT ; INT0 External Interrupt Request 0
  jmp BAD_INTERUPT ; INT1 External Interrupt Request 1
  jmp BAD_INTERUPT ; PCINT0 Pin Change Interrupt Request 0
  jmp BAD_INTERUPT ; PCINT1 Pin Change Interrupt Request 1
  jmp BAD_INTERUPT ; PCINT2 Pin Change Interrupt Request 2
  jmp BAD_INTERUPT ; WDT Watchdog Time-out Interrupt
  jmp BAD_INTERUPT ; TIMER2 COMPA Timer/Counter2 Compare Match A
  jmp BAD_INTERUPT ; TIMER2 COMPB Timer/Counter2 Compare Match B
  jmp BAD_INTERUPT ; TIMER2 OVF Timer/Counter2 Overflow
  jmp BAD_INTERUPT ; TIMER1 CAPT Timer/Counter1 Capture Event
  jmp BAD_INTERUPT ; TIMER1 COMPA Timer/Counter1 Compare Match A
  jmp BAD_INTERUPT ; TIMER1 COMPB Timer/Coutner1 Compare Match B
  jmp BAD_INTERUPT ; TIMER1 OVF Timer/Counter1 Overflow
  jmp BAD_INTERUPT ; TIMER0 COMPA Timer/Counter0 Compare Match A
  jmp BAD_INTERUPT ; TIMER0 COMPB Timer/Counter0 Compare Match B
  jmp BAD_INTERUPT ; TIMER0 OVF Timer/Counter0 Overflow
  jmp BAD_INTERUPT ; SPI, STC SPI Serial Transfer Complete
  jmp BAD_INTERUPT ; USART, RX USART Rx Complete
  jmp BAD_INTERUPT ; USART, UDRE USART, Data Register Empty
  jmp BAD_INTERUPT ; USART, TX USART, Tx Complete
  jmp BAD_INTERUPT ; ADC ADC Conversion Complete
  jmp BAD_INTERUPT ; EE READY EEPROM Ready
  jmp BAD_INTERUPT ; ANALOG COMP Analog Comparator
  jmp BAD_INTERUPT ; TWI 2-wire Serial Interface
  jmp BAD_INTERUPT ; SPM READY Store Program Memory Ready
BAD_INTERUPT:
  jmp 0x0000

RESET:
  cli

ldi Working, low(RAMEND)
out SPL, Working
ldi Working, high(RAMEND)
out SPH, Working

ldi YL, low(data_stack)
ldi YH, high(data_stack)

rcall UART_INIT

ldi Working, 10
mov Base, Working

sei

MAIN:
  rcall WORD_PFA
  rcall NUMBER_PFA
  rcall EMIT_PFA
  rjmp MAIN

UART_INIT:
  ldi r17, high(520) ; 2400 baud w/ 20Mhz osc
  ldi r16, low(520)  ; See Datasheet
  sts UBRR0H, r17
  sts UBRR0L, r16
  ; The chip defaults to 8N1 so we won't set it here even though we
  ; should.
  ldi r16, (1 << TXEN0) | (1 << RXEN0) ; Enable transmit/receive
  sts UCSR0B, r16
  ret

KEY:
  .dw 0x0000
  .db 3, "key"

KEY_PFA:
  lds Working, UCSR0A
  sbrs Working, RXC0
  rjmp KEY_PFA

rcall DUP_PFA
lds TOS, UDR0

rcall ECHO_PFA
ret

DUP:
  .dw KEY
  .db 3, "dup"
DUP_PFA:
  st Y+, TOSL ; push TOSL onto data stack
  mov TOSL, TOS
  ret

EMIT:
  .dw DUP
  .db 4, "emit"
EMIT_PFA:
  rcall ECHO_PFA
  rcall DROP_PFA
  ret

ECHO:
  .dw EMIT
  .db 4, "emit"

ECHO_PFA:
  lds Working, UCSR0A
  sbrs Working, UDRE0
  rjmp ECHO_PFA
  sts UDR0, TOS
  ret

DROP:
  .dw ECHO
  .db 4, "drop"
DROP_PFA:
  mov TOS, TOSL
  popup
  ret

WORD:
  .dw DROP
  .db 4, "word"
WORD_PFA:

rcall KEY_PFA

cpi TOS, ' '
brne _a_key

rcall DROP_PFA
rjmp WORD_PFA

_a_key:
  ldi ZL, low(buffer)
  ldi ZH, high(buffer)
  ldi word_counter, 0x00

_find_length:
  cpi word_counter, 0x40
  breq _a_key

st Z+, TOS
rcall DROP_PFA
inc word_counter

rcall KEY_PFA
cpi TOS, ' '
brne _find_length

mov TOS, word_counter
ret

NUMBER:
  .dw WORD
  .db 6, "number"
NUMBER_PFA:

ldi ZL, low(buffer)
ldi ZH, high(buffer)

mov number_pointer, TOS
ldi Working, 0x00
ld TOS, Z+
rjmp _convert

_convert_again:
  mul Working, Base
  mov Working, r0
  ld TOS, Z+

_convert:

cpi TOS, '0'
brlo _num_err
cpi TOS, ':' ; the char after '9'
brlo _decimal

rjmp _num_err

_decimal:
  subi TOS, '0'
  rjmp _converted


_num_err:
  rcall ECHO_PFA
  mov TOS, number_pointer
  ret

_converted:
  add Working, TOS
  dec number_pointer
  brne _convert_again

mov TOS, Working
ret
