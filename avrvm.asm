;Bot Initial Draft MCP.
;
;    This program is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    This program is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;

.nolist
.include "m328Pdef.inc"
.list
.listmac

; Keep the top two items (bytes) on the stack in the X register.
.def TOS = r27 ; XH
.def TOSL = r26 ; XL

; Y register is our Data Stack Pointer.
; Z register will be used for diggin around in the dictionary.

; Buffer pointers
.def Current_key = r14
.def Buffer_top = r15

.def Working = r16

; Registers used by WORD word.
.def word_temp = r17

; Registers used by FIND word.
.def find_temp_0 = r10
.def find_temp_1 = r11
.def find_temp_2 = r12
.def find_temp_3 = r13
.def find_low = r18
.def find_high = r19
.def find_current_low = r20
.def find_current_high = r21

; Registers used by "to PFA" word.
.def tpfa_temp_high = r22
.def tpfa_temp_low = r23


;#######################################################################
; Storage for variables in the SRAM.
; Create a 256-byte heap at the bottom of RAM and allot some initial
; system variables.

.dseg
heap: .org 0x0100 ; On the ATmega328P the SRAM proper begins at 0x100.
State_mem: .byte 1
Latest_mem: .byte 2
Here_mem: .byte 1


;#######################################################################
; Next we have a buffer for input. For now, 128 bytes.
.dseg
.org 0x0200
buffer: .byte 0x80

;#######################################################################
; The Parameter (Data) Stack begins just after the buffer and grows upward
; towards the Return Stack at the top of RAM. Note that the first two bytes
; of stack are kept in the X register. Due to this the initial two bytes of
; the data stack will be filled with whatever was in X before the first
; push, unless you load X (i.e. TOS and Just-Under-TOS) "manually" before
; dropping into the interpreter loop.
.dseg
data_stack: .org 0x0280


;#######################################################################
; From jonesforth.S, some of the Forth standard system vars for
; reference:
;        STATE           Is the interpreter executing code (0) or compiling a word (non-zero)?
;        LATEST          Points to the latest (most recently defined) word in the dictionary.
;        HERE            Points to the next free byte of memory.  When compiling, compiled words go here.
;        S0              Stores the address of the top of the parameter stack.
;        BASE            The current base for printing and reading numbers.

.cseg
;#######################################################################
.org 0x0000            ; Interupt Vectors
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

;#######################################################################
RESET:
  cli

  ldi Working, low(RAMEND) ; Set up the stack.
  out SPL, Working
  ldi Working, high(RAMEND)
  out SPH, Working

  ldi YL, low(data_stack) ; Initialize Data Stack Pointer.
  ldi YH, high(data_stack)

  ldi Working, low(Here_mem) + 1 ; Set HERE to point to just after itself.
  ldi ZL, low(Here_mem)
  ldi ZH, high(Here_mem)
  st Z, Working

  ldi Working, low(buffer) ; Reset input buffer
  mov Current_key, Working
  mov Buffer_top, Working

  ; Initialize latest
  ldi ZL, low(Latest_mem)
  ldi ZH, high(Latest_mem)
  ldi Working, low(CURRENT_KEY_WORD) ; Current_key is currently Latest.
  st Z+, Working
  ldi Working, high(CURRENT_KEY_WORD)
  st Z, Working


  sei

; TODO: Set up a Stack Overflow Handler and put its address at RAMEND
; and set initial stack pointer to RAMEND - 2 (or would it be 1?)
; That way if we RET from somewhere and the stack is underflowed we'll
; trigger the handler instead of just freaking out.


;#######################################################################
; Some data stack manipulation macros to ease readability.

; Make room on TOS and TOSL by pushing everything down two cells.
.MACRO pushdownw
  st Y+, TOSL ; push TOSL onto data stack
  st Y+, TOS  ; push TOS onto data stack
.ENDMACRO

.MACRO popup
  ld TOSL, -Y ; pop from data stack to TOSL register.
.ENDMACRO
; Note that you are responsible for preserving the previous value of TOSL
; if you still want it after using the macro. (I.e. mov TOS, TOSL)

; Essentially "drop drop".
.MACRO popupw
  ld TOS, -Y
  ld TOSL, -Y
.ENDMACRO


;#######################################################################
MAIN:
  rcall CHECKIT
  rjmp MAIN


; This is a test/exercise subroutine for tracing in the debugger.
CHECKIT:
  rcall HERE_PFA ; Put address of Here_mem onto the stack
; rcall DUP_PFA
; inc TOS
; rcall SWAP_PFA
; rcall DROP_PFA
  rcall FILL_BUFFER
  rcall INTERPRET_PFA
  ret

COMMAND: .db 7, " dup @ "

FILL_BUFFER:
  ldi Working, low(buffer)
  mov Current_key, Working
  mov Buffer_top, Working
  pushdownw
  ldi TOSL, low(COMMAND)
  ldi TOS, high(COMMAND)
  rcall LEFT_SHIFT_WORD_PFA
  movw Z, X
  ldi TOSL, low(buffer)
  ldi TOS, high(buffer)
  lpm r0, Z+ ; count
  add Buffer_top, r0 ; set the buffer_top to the end of the string.
_fill_buffer_loop:
  lpm Working, Z+
  st X+, Working
  dec r0
  brne _fill_buffer_loop
  popupw
  ret


;#######################################################################
; Let's make words.

DROP: ; ----------------------------------------------------------------
  .dw 0 ; Initial link field is null.
  .db 4, "drop"
DROP_PFA:
  mov TOS, TOSL
  popup
  ret

SWAP_: ; ---------------------------------------------------------------
  .dw DROP
  .db 4, "swap"
SWAP_PFA:
  mov Working, TOS
  mov TOS, TOSL
  mov TOSL, Working
  ret

DUP: ; -----------------------------------------------------------------
  .dw SWAP_
  .db 3, "dup"
DUP_PFA:
  st Y+, TOSL ; push TOSL onto data stack
  mov TOSL, TOS
  ret

KEY: ; -----------------------------------------------------------------
  .dw DUP
  .db 3, "key"
KEY_PFA:
  cp Current_key, Buffer_top ; If they're the same we're out of input data.
  breq Out_of_input
  ldi ZH, high(buffer) ; Load the char's address in the buffer into Z.
  mov ZL, Current_key
  rcall DUP_PFA
  ld TOS, Z ; Get char from buffer
  inc Current_key
  ret
Out_of_input:
  rcall DUP_PFA
  ldi TOS, 0x15 ; ASCII NACK byte.
  ret

WORD: ; ----------------------------------------------------------------
  .dw KEY
  .db 4, "word"
WORD_PFA:
  rcall KEY_PFA ; Get next char (or NACK, 0x15) onto stack.

  cpi TOS, 0x15 ; Check for error, EOF.
  breq Out_of_input ; This leaves TOSL with an extra copy but should work.

  ; valid char, is it blank?
  cpi TOS, ' '
  brne _a_key
  rcall DROP_PFA ; remove the space
  rjmp WORD_PFA ; get the next char.

_a_key:
  ; put the start offset into a register for later
  mov word_temp, Current_key
  rcall DROP_PFA ; clear the char from the stack.

_find_length:
  rcall KEY_PFA
  cpi TOS, 0x15
  breq Out_of_input
  cpi TOS, ' '
  breq _done_finding
  rcall DROP_PFA ; ditch the char from the stack
  rjmp _find_length ; continue searching for end of word.

_done_finding:
  rcall DUP_PFA ; make room on the stack
  mov TOS, word_temp ; start offset in TOS
  dec TOS ; one less than current key.
  mov TOSL, Current_key ; length in TOSL (replacing leftover last char)
  clc ; clear carry bit just in case
  sbc TOSL, word_temp ; subtract old from new to get length
  ret

LEFT_SHIFT_WORD: ; -----------------------------------------------------
  .dw WORD
  .db 3, "<<w"
LEFT_SHIFT_WORD_PFA:
  mov Working, TOS
  clc ; clear carry flag
  clr TOS ; clear TOS
  lsl TOSL
  brcc _no_carry_var_does ; If the carry bit is clear skip incrementing TOS
  inc TOS ; copy carry flag to TOS[0]
_no_carry_var_does:
  lsl Working
  or TOS, Working
  ; X now contains left-shifted word, and carry bit reflects TOS carry.
  ret

DATA_FETCH: ; ----------------------------------------------------------
  .dw LEFT_SHIFT_WORD
  .db 1, "@"
DATA_FETCH_PFA:
  ldi ZH, high(heap)
  mov ZL, TOS
  ld TOS, Z ; Get byte from heap.
  ret

FIND: ; ----------------------------------------------------------------
  .dw DATA_FETCH
  .db 4, "find"
FIND_PFA:
  ldi ZH, high(Latest_mem) ; Get the address of the latest word from the
  ldi ZL, low(Latest_mem)  ; heap.
  ld find_low, Z+ ; And put it into find_high:find_low register pair.
  ld find_high, Z
_look_up_word:
  movw Z, find_high:find_low ; address of word is in Z and find_high:find_low.
  movw find_current_high:find_current_low, find_high:find_low ; Save current addy
  or find_low, find_high ; check for nullity
  breq _not_in_dictionary

  ; Use X (TOS,TOSL) to prepare the address (left shift it.)
  ; FIXME: Why not just store the address already shifted?
  pushdownw
  movw X, Z
  rcall LEFT_SHIFT_WORD_PFA
  movw Z, X
  popupw

  lpm find_low, Z+ ; Load Link Field Address of next word in the dictionary
  lpm find_high, Z+ ; into the find_high:find_low register pair.
  lpm Working, Z+ ; Load length-of-name byte into a register

  ; compare it to TOSL (length of word-name we're looking for.)
  cp TOSL, Working
  brne _look_up_word ; Well, it ain't this one...

  ; If they're the same length walk through both and compare them ;
  ; character by character. Buffer offset is in TOS, Name address is in Z,
  ; length is in Working and TOSL.

  ; We clobber TOSL to fetch from the buffer using X and we decrement
  ; Working to track our loop iterations so we need another copy of
  ; length to reset TOSL with if we have to go to the next word.
  mov find_temp_2, Working
  mov find_temp_3, TOS ; We increment X as we scan the string, so save this too.


  ldi TOS, high(buffer) ; Going to look up bytes in the buffer.
  mov TOSL, find_temp_3

_compare_name_and_target_byte:

  ; 1. Get char from buffer into find_temp_0
  ld find_temp_0, X+

  ; 2. Get char from word name into find_temp_1
  lpm find_temp_1, Z+

  ; 3. Compare them.
  cp find_temp_0, find_temp_1
  breq _okay_dokay

  ; not equal, clean up and go to next word.
  ;
  ; mov TOSL, find_temp_2 ; replace target length in TOSL.
  ; mov TOS, find_temp_3 ; and initial offset in TOS.
  ;
  movw X, find_temp_3:find_temp_2 ; replace target length in TOSL and initial offset in TOS.
  rjmp _look_up_word

_okay_dokay:
  ; The chars are the same
  dec Working
  brne _compare_name_and_target_byte ; More to do?

  ; If we get here we've checked that every character in the name and the
  ; target term match.
  movw X, find_current_high:find_current_low
  ret

_not_in_dictionary:
  ldi TOS, 0xff ; consume TOS/TOSL and return 0xffff (we don't have that
  ldi TOSL, 0xff ; much RAM so this is not a valid address value.)
  popup
  ret

TPFA: ; ----------------------------------------------------------------
  .dw FIND
  .db 4, ">pfa"
TPFA_PFA:
  ; LFA of word should be on the stack (i.e. in X.)
  adiw X, 1         ; point to name length.
  movw tpfa_temp_high:tpfa_temp_low, X   ; set prog mem pointer value aside for later.
  rcall LEFT_SHIFT_WORD_PFA ; Adjust the address
  movw Z, X         ; and put it into our prog-mem-addressing Z register.
  movw X, tpfa_temp_high:tpfa_temp_low
  lpm Working, Z    ; get the length.
                    ; We need to map from length in bytes to length in words
  lsr Working       ; while allowing for the padding bytes in even-length names.
  inc Working       ; n <- (n >> 1) + 1
  add TOSL, Working ; Add the adjusted name length to our prog mem pointer.
  brcc _done_adding
  inc TOS           ; Account for the carry bit if set.
_done_adding:
  ret

INTERPRET: ; -----------------------------------------------------------
  .dw TPFA
  .db 9, "interpret"
INTERPRET_PFA:
  rcall WORD_PFA ; get offset and length of next word in buffer.
  cpi TOS, 0x15
  breq _byee
  rcall FIND_PFA ; find it in the dictionary, (X <- LFA)
  cpi TOS, 0xff
  breq _byee
  rcall TPFA_PFA ; get the PFA address (X <- PFA)
  movw Z, X ; put it in Z for indirect call
  popupw ; clear the stack for the "client" word
  icall ; and execute it.
  rjmp INTERPRET_PFA
_byee:
  popupw ; ditch the "error message"
  ret


;#######################################################################
; Variables and system variable words.

VAR_DOES: ; ------------------------------------------------------------
  .dw INTERPRET
  .db 8, "var_does"
VAR_DOES_PFA:
  ; Get the address of the calling variable word's parameter field off
  ; the return stack.  Pop the address to cancel the call to VAR_DOES by
  ; the "instance" variable word.
  pushdownw
  pop TOS
  pop TOSL
  rcall LEFT_SHIFT_WORD_PFA
  ; Stack now contains left-shifted PFA address.

  ; Use it to look up the variable's memory address (in SRAM heap)
  ; Put that address on the data stack (TOS). We only use the low byte
  ; because we'll restrict access to SRAM in the fetch ("@") word.
             ;
  movw Z, X  ; Copy address to Z
  popup      ; adjust the stack
  lpm TOS, Z ; and use Z (PFA of variable instance word) to get the SRAM
             ; offset of the variable's storage.

  ret ; to the word that called the variable word.

HERE_WORD: ; -----------------------------------------------------------
  .dw VAR_DOES
  .db 4, "here"
HERE_PFA:
  rcall VAR_DOES_PFA
  .db low(Here_mem), high(Here_mem) ; Note: I'm putting the full address
                   ; here but the VAR_DOES machinery only uses low byte.
  ; We don't need to ret here because VAR_DOES will consume the top of
  ; the return stack. (I.e. the address of the Here_mem byte above.)

LATEST_WORD: ; ---------------------------------------------------------
  .dw HERE_WORD
  .db 6, "latest"
Latest_PFA:
  rcall VAR_DOES_PFA
  .db low(Latest_mem), high(Latest_mem)

STATE_WORD: ; ----------------------------------------------------------
  .dw LATEST_WORD
  .db 5, "state"
STATE_PFA:
  rcall VAR_DOES_PFA
  .db low(State_mem), high(State_mem)

CURRENT_KEY_WORD: ; ----------------------------------------------------
  .dw STATE_WORD
  .db 4, "ckey"
CURRENT_KEY_PFA:
  rcall DUP_PFA
  mov TOS, Current_key
  ret


;#######################################################################
