=====================
Bot Initial Draft MCP
=====================

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.



Definitions
-----------

Let's include the definitions for the ATmega328P::

  .nolist
  .include "m328Pdef.inc"
  .list
  .listmac

Keep the top two items (bytes) on the stack in the X register::

  .def TOS = r27 ; XH
  .def TOSL = r26 ; XL

Y register is our Data Stack Pointer.
Z register will be used for diggin around in the dictionary.

Base (numeric base for converting digits to numbers)::

  .def Base = r8

Buffer pointers::

  .def Current_key = r14
  .def Buffer_top = r15
  .def counter = r3 ; used for looping, etc.
  .def Hmm_reg = r4

We need a general use register::

  .def Working = r16

Register used by WORD word::

  .def word_temp = r17

Registers used by FIND word::

  .def find_buffer_char = r10
  .def find_name_char = r11
  .def find_temp_offset = r12
  .def find_temp_length = r13

Registers used by "to PFA" word::

  .def tpfa_temp_high = r22
  .def tpfa_temp_low = r23

This flag is used in the name-length byte of a word definition header::

  .equ IMMED = 0x80


Macros
~~~~~~

Some data stack manipulation macros to ease readability.

Make room on TOS and TOSL by pushing them onto the data stack::

  .MACRO pushdownw
    st Y+, TOSL
    st Y+, TOS
  .ENDMACRO

Pop from data stack to TOSL. Note that you are responsible for preserving
the previous value of TOSL if you still want it after using the macro.
(I.e. mov TOS, TOSL)::

  .MACRO popup
    ld TOSL, -Y
  .ENDMACRO

Essentially "drop drop"::

  .MACRO popupw
    ld TOS, -Y
    ld TOSL, -Y
  .ENDMACRO

Load Z register pair with SRAM address of next free byte in heap.
This is faster than calling the here and data-fetch words::

  .MACRO z_here
    ldi ZL, low(Here_mem)
    ldi ZH, high(Here_mem)
    ld ZL, Z
    ldi ZH, high(heap)
  .ENDMACRO

Write out the counted string::

  .MACRO emits
    pushdownw
    ldi TOSL, low(@0)
    ldi TOS, high(@0)
    rcall EMITSTR_PFA
  .ENDMACRO

Emit one character. (Kind of a debugging aid.)::

  .MACRO one_char
    rcall DUP_PFA
    ldi TOS, @0
    rcall EMIT_PFA
  .ENDMACRO

  .MACRO one_chreg
    rcall DUP_PFA
    mov TOS, @0
    rcall EMIT_PFA
  .ENDMACRO

Data (SRAM) Organization
------------------------

::

  .dseg

Storage for User Variables (Heap)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Create a 256-byte heap at the bottom of RAM and allot some initial
system variables. On the ATmega328P the SRAM proper begins at 0x100::

  heap: .org 0x0100
  State_mem: .byte 1
  Latest_mem: .byte 2
  Here_mem: .byte 1


Input Buffer
~~~~~~~~~~~~

Next we have a buffer for input. For now, 128 bytes::

  .org 0x0200
  buffer: .byte 0x80


Data Stack
~~~~~~~~~~

The Parameter (Data) Stack begins just after the buffer and grows upward
towards the Return Stack at the top of RAM. Note that the first two bytes
of stack are kept in the X register. Due to this the initial two bytes of
the data stack will be filled with whatever was in X before the first
push, unless you load X (i.e. TOS and Just-Under-TOS) "manually" before
dropping into the interpreter loop::

  data_stack: .org 0x0280



Code (Flash RAM)
----------------

::

  .cseg

Interupt Vectors
~~~~~~~~~~~~~~~~

::

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

Initial reset vector
~~~~~~~~~~~~~~~~~~~~

Disable interrupts and reset everything::

  RESET:
    cli

Initialize Base::

  ldi Working, 10
  mov Base, Working

Set up the Return Stack::

  ldi Working, low(RAMEND)
  out SPL, Working
  ldi Working, high(RAMEND)
  out SPH, Working

Initialize Data Stack::

  ldi YL, low(data_stack)
  ldi YH, high(data_stack)

Set State to immediate (0)::

  ldi Working, 0x00
  ldi ZL, low(State_mem)
  ldi ZH, high(State_mem)
  st Z, Working

Set HERE to point to just after itself::

  ldi Working, low(Here_mem) + 1
  ldi ZL, low(Here_mem)
  ldi ZH, high(Here_mem)
  st Z, Working

Reset input buffer::

  ldi Working, low(buffer)
  mov Current_key, Working
  mov Buffer_top, Working

Initialize Latest (current_key is Latest right now.)::

  ldi ZL, low(Latest_mem)
  ldi ZH, high(Latest_mem)
  ldi Working, low(CURRENT_KEY_WORD)
  st Z+, Working
  ldi Working, high(CURRENT_KEY_WORD)
  st Z, Working

Initialize the USART. This could be broken out into words but I'm eager
to get up and running::

  ldi r17, high(520) ; 2400 baud w/ 20Mhz osc
  ldi r16, low(520)
  sts UBRR0H, r17
  sts UBRR0L, r16
  ; The chip defaults to 8N1 so we won't set it here even though we
  ; should.
  ldi r16, (1 << TXEN0) | (1 << RXEN0) ; Enable transmit/receive
  sts UCSR0B, r16
  
  ldi TOS, 'O'
  ldi TOSL, 'k'

Re-enable interrupts::

  sei

TODO: Set up a Stack Overflow Handler and put its address at RAMEND
and set initial stack pointer to RAMEND - 2 (or would it be 1?)
That way if we RET from somewhere and the stack is underflowed we'll
trigger the handler instead of just freaking out.


Main Loop
~~~~~~~~~

Our (very simple) main loop just calls "quit" over and over again::

  MAIN:
    rcall WRITE_BANNER
    ; rcall WORD_PFA
    rcall KEY_PFA
    rcall DOTESS_PFA
    ; rcall QUIT_PFA
    rjmp MAIN


Print this banner when starting::

  BANNER: .db 9, "Welcome", 0x0d, 0x0a
  DONE_WORD: .db 11, "word read", 0x0d, 0x0a
  HMMDOT: .db 1, '.'

This routine takes the banner above and copies it to UART::

    WRITE_BANNER:
      emits BANNER
      ret


Let's make words
~~~~~~~~~~~~~~~~


Data Stack
^^^^^^^^^^

drop::

    DROP:
      .dw 0 ; Initial link field is null.
      .db 4, "drop"
    DROP_PFA:
      mov TOS, TOSL
      popup
      ret

swap::

    SWAP_:
      .dw DROP
      .db 4, "swap"
    SWAP_PFA:
      mov Working, TOS
      mov TOS, TOSL
      mov TOSL, Working
      ret

dup::

    DUP:
      .dw SWAP_
      .db 3, "dup"
    DUP_PFA:
      st Y+, TOSL ; push TOSL onto data stack
      mov TOSL, TOS
      ret

Emit and Reset
^^^^^^^^^^^^^^

emit::

    EMIT:
      .dw DUP
      .db 4, "emit"
    EMIT_PFA:
      lds Working, UCSR0A
      sbrs Working, UDRE0
      rjmp EMIT_PFA
      sts UDR0, TOS
      mov TOS, TOSL
      popup
      ret

This word takes the address of a (length, buffer) datastructure in
program RAM and writes it to the UART. It consumes TOS and TOSL::

    EMITSTR:
      .dw EMIT
      .db 7, "emitstr"
    EMITSTR_PFA:
      push ZH
      push ZL
      rcall LEFT_SHIFT_WORD_PFA
      movw Z, X
      popupw
      lpm counter, Z+
    _emitstr_loop:
      lpm Working, Z+
    _taptaptap:
      lds Hmm_reg, UCSR0A
      sbrs Hmm_reg, UDRE0
      rjmp _taptaptap
      sts UDR0, Working
      dec counter
      brne _emitstr_loop
      pop ZL
      pop ZH
      ret

    EMIT_WORD_BUFFER:
      .dw EMITSTR
      .db 6, "ewbuff"
    EMIT_WORD_BUFFER_PFA:
      emits (EMIT_WORD_BUFFER + 1)

      rcall DUP_PFA
      mov TOS, Buffer_top
      ldi Working, '0'
      add TOS, Working
      rcall EMIT_PFA

      one_char '_'

      ; rcall EMIT_CRLF_PFA
      mov Working, Buffer_top
      cpi Working, 0x00
      brne _theres_a_word
      ret

    _theres_a_word:
      mov counter, Buffer_top
      ldi ZL, low(buffer)
      ldi ZH, high(buffer)

    _ewb_loop:
      ld Working, Z+
      one_chreg Working
      dec counter
      one_char ','
      ; emits HMMDOT
      brne _ewb_loop

      rcall EMIT_CRLF_PFA
      ret

    EMIT_CRLF:
      .dw EMIT_WORD_BUFFER
      .db 4, "crlf"
    EMIT_CRLF_PFA:
      rcall DUP_PFA
      ldi TOS, 0x0d ; CR
      rcall EMIT_PFA
      rcall DUP_PFA
      ldi TOS, 0x0a ; LF
      rcall EMIT_PFA
      ret

reset::

    RESET_BUTTON:
      .dw EMIT_CRLF
      .db 5, "reset"
    RESET_BUTTON_PFA:
      rjmp 0x0000

dot-ess::

    DOTESS:
      .dw RESET_BUTTON
      .db 2, ".s"
    DOTESS_PFA:
      rcall EMIT_CRLF_PFA
      one_char '['
      one_chreg TOS
      one_char '-'
      mov Working, TOSL
      one_chreg Working
      one_char ' '

     ; ldi ZH, high(data_stack)
     ; ldi ZL, low(data_stack)
      movw Z, Y

    _inny:
      ldi Working, low(data_stack)
      cp ZL, Working
      ldi Working, high(data_stack)
      cpc ZH, Working
      breq _out

      ld Working, -Z
      one_chreg Working
      one_char ' '

      rjmp _inny

    _out:
      one_char ']'
      rcall FUK
      rcall EMIT_CRLF_PFA
      ret

    FUK:
      cpse TOS, TOSL
      rjmp _nah
      one_char '+'
      ret
    _nah:
      one_char '%'
      ret

Parsing
^^^^^^^

key::

    KEY:
      .dw RESET_BUTTON
      .db 3, "key"
    KEY_PFA:
    ;  rcall DUP_PFA
    ;  nop
    ;  ret

      emits (KEY + 1)
      one_char '>'
      one_char ' '
    _keyey:
      lds Working, UCSR0A
      sbrs Working, RXC0
      rjmp _keyey
      rcall DUP_PFA
      lds TOS, UDR0
      rcall DUP_PFA
      rcall EMIT_PFA ; echo
      ret

word::

    WORD:
      .dw KEY
      .db 4, "word"
    WORD_PFA:
      emits (WORD + 1)
      rcall KEY_PFA ; Get next char onto stack.
      ; is it blank?
      cpi TOS, ' '
      brne _a_key
      rcall DROP_PFA ; remove the space
      rjmp WORD_PFA ; get the next char.

    _a_key:
      ; set up buffer
      ldi ZL, low(buffer)
      ldi ZH, high(buffer)
      ldi Working, 0x00
      mov Current_key, Working
      mov Buffer_top, Working

    _find_length:
      st Z+, TOS ; save the char to the buffer
      rcall DROP_PFA ; ditch the char from the stack
      inc Buffer_top
      emits HMMDOT
      rcall KEY_PFA
      cpi TOS, ' '
      breq _done_finding
      rjmp _find_length ; continue searching for end of word.

    _done_finding:
      emits DONE_WORD
      rcall EMIT_WORD_BUFFER_PFA
      rcall EMIT_CRLF_PFA
      st Y+, TOSL ; make room on stack
      ldi TOS, 0x00 ; start offset in TOS (replacing leftover last char)
      mov TOSL, Buffer_top ; length in TOSL
      ret

number Parse a number from "stdin"::

    NUMBER:
      .dw WORD
      .db 6, "number"
    NUMBER_PFA:
      ; offset in TOS, length in TOSL
      ldi Working, 0
      mov word_temp, TOSL ; length
      mov TOSL, TOS
      ldi TOS, high(buffer)
      ; X points to digits
      movw Z, X

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
      cpi TOS, 'a'
      brlo _num_err
      cpi TOS, 0x7b ; '{', the char after 'z'
      brsh _num_err
      subi TOS, 87 ; convert 'a'-'z' => 10-35
      rjmp _converted
    _decimal:
      subi TOS, '0'
      rjmp _converted
    _num_err:
      rcall DUP_PFA
      rcall EMIT_PFA
      mov TOSL, TOS
      mov TOS, word_temp
      ret
    _converted:
      add Working, TOS
      dec word_temp
      brne _convert_again

      rcall DUP_PFA
      mov TOS, Working
      ret


Core Interpreting and Compiling Words
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

"<<w" shift a 16-bit value in TOS:TOSL one bit to the left::

    LEFT_SHIFT_WORD:
      .dw NUMBER
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

"@" fetch a byte from the heap given its offset in TOS::

    DATA_FETCH:
      .dw LEFT_SHIFT_WORD
      .db 1, "@"
    DATA_FETCH_PFA:
      ldi ZH, high(heap)
      mov ZL, TOS
      ld TOS, Z ; Get byte from heap.
      ret

create::

    CREATE:
      .dw DATA_FETCH
      .db 6, "create"
    CREATE_PFA:
      ; offset in TOS, length in TOSL, of new word's name

      z_here ; Z now points to next free byte on heap.
      adiw Z, 2 ; reserve space for the link to Latest

      st Y+, TOSL ; store for later
      mov word_temp, TOSL ; count
      st Z+, TOSL ; store name length in compiling word
      mov TOSL, TOS
      ldi TOS, high(buffer)
      ; X now points to the name in the buffer, Z to the destination

    _create_char_xfer:
      ld Working, X+
      st Z+, Working
      dec word_temp
      brne _create_char_xfer

      ld TOSL, -Y ; pop length
      lsr TOSL
      brcs _word_aligned ; odd number, no alignment byte needed
      clr TOSL
      st Z+, TOSL ; write alignment byte
    _word_aligned:
      ; The name has been laid down in SRAM.
      ; Write ZL to Here_mem and we're done.
      ldi TOSL, low(Here_mem)
      ldi TOS, high(Here_mem)
      st X, ZL
      popupw ; ditch offset and (right-shifted) length
      ret

find::

    FIND:
      .dw CREATE
      .db 4, "find"
    FIND_PFA:
      ; TOS holds the offset in the buffer of the word to search for and TOSL
      ; holds the length.
      mov find_temp_offset, TOS
      mov find_temp_length, TOSL
      ldi ZH, high(Latest_mem)
      ldi ZL, low(Latest_mem)
      ld TOSL, Z+
      ld TOS, Z

    _look_up_word:
    ; LFA in TOS:TOSL, Z is free

    ; Check if TOS:TOSL == 0x0000
      cpi TOSL, 0
      brne _non_zero
      cpse TOSL, TOS ; ComPare Skip Equal
      rjmp _non_zero
      ; if TOS:TOSL == 0x0000 we're done.
      ldi TOS, 0xff ; consume TOS/TOSL and return 0xffff (we don't have that
      ldi TOSL, 0xff ; much RAM so this is not a valid address value.)
      ret

    _non_zero:
      ; Save current addy
      pushdownw
      ; now stack has ( - LFA, LFA)

      ; Load Link Field Address of next word in the dictionary
      ; into the X register pair.
      rcall LEFT_SHIFT_WORD_PFA
      movw Z, X
      lpm TOSL, Z+
      lpm TOS, Z+
      ; now stack has ( - LFA_next, LFA_current)

      lpm Working, Z+ ; Load length-of-name byte into a register
      andi Working, 0x7f ; IMM_MASK
      cp Working, find_temp_length
      breq _same_length

      ; Well, it ain't this one...
      ; ditch LFA_current
      sbiw Y, 2
      rjmp _look_up_word

    _same_length:
      ; If they're the same length walk through both and compare them ;
      ; character by character.
      ;
      ; Buffer offset is in find_temp_offset
      ; length is in Working and find_temp_length
      ; Z holds current word's name's first byte's address in program RAM.
      ; TOS:TOSL have the address of the next word's LFA.
      ; stack has ( - LFA_next, LFA_current)

      ; Put address of search term in buffer into X (TOS:TOSL).
      pushdownw
      ldi TOS, high(buffer) ; Going to look up bytes in the buffer.
      mov TOSL, find_temp_offset
      ; stack ( - &search_term, LFA_next, LFA_current)

    _compare_name_and_target_byte:
      ld find_buffer_char, X+ ; from buffer
      lpm find_name_char, Z+ ; from program RAM
      cp find_buffer_char, find_name_char
      breq _okay_dokay

      ; not equal, clean up and go to next word.
      popupw ; ditch search term address
      sbiw Y, 2 ; ditch LFA_current
      rjmp _look_up_word

    _okay_dokay:
      ; The chars are the same
      dec Working
      brne _compare_name_and_target_byte ; More to do?

      ; If we get here we've checked that every character in the name and the
      ; target term match.
      popupw ; ditch search term address
      popupw ; ditch LFA_next
      ret

">pfa" Given a word's LFA (Link Field Address) in TOS:TOSL, find its PFA::

    TPFA:
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
      andi Working, 0x7f; IMM_MASK
                        ; We need to map from length in bytes to length in words
      lsr Working       ; while allowing for the padding bytes in even-length names.
      inc Working       ; n <- (n >> 1) + 1
      add TOSL, Working ; Add the adjusted name length to our prog mem pointer.
      brcc _done_adding
      inc TOS           ; Account for the carry bit if set.
    _done_adding:
      ret

quit Oddly enough, the Forth main loop is called "quit"::

    QUIT:
      .dw TPFA
      .db 4, "quit"
    QUIT_PFA:
      ldi Working, low(RAMEND) ; reset return stack
      out SPL, Working
      ldi Working, high(RAMEND)
      out SPH, Working
      rcall DUP_PFA
      ldi TOS, '>'
      rcall EMIT_PFA
      rcall DUP_PFA
      ldi TOS, ' '
      rcall EMIT_PFA
      rcall INTERPRET_PFA
      rjmp QUIT_PFA

interpret::

    INTERPRET:
      .dw QUIT
      .db 9, "interpret"
    INTERPRET_PFA:
      rcall WORD_PFA ; get offset and length of next word in buffer.
      cpi TOS, 0x15
      breq _byee
      pushdownw ; save offset and length
      rcall FIND_PFA ; find it in the dictionary, (X <- LFA)
      cpi TOS, 0xff
      brne _is_word

      ; is it a number?
      popupw ; get the offset and length back
      rcall NUMBER_PFA
      cpi TOS, 0x00 ; all chars converted?
      brne _byee
      mov TOS, TOSL
      rcall EMIT_PFA
      ret

    _is_word:
      sbiw Y, 2 ; ditch offset and length
      pushdownw ; save a copy of LFA on the stack

      ; Calculate PFA and save it in Z.
      rcall TPFA_PFA ; get the PFA address (X <- PFA)
      movw Z, X

      ; Check if the word is flagged as immediate.
      popupw ; get the LFA again
      st Y+, ZL ; save PFA on stack to clear Z for IMMEDIATE_P
      st Y+, ZH
      rcall IMMEDIATE_P_PFA ; stack is one (byte) cell less ( LFA:LFA - imm? )
      mov ZH, TOSL ; restore PFA to Z from stack
      ld ZL, -Y
      breq _execute_it

      ; word is not immediate, check State and act accordingly
      st Y+, TOSL ; free up X register pair (Z still holds PFA)
      ldi TOSL, low(State_mem)
      ldi TOS, high(State_mem)
      ld TOS, X
      popup
      cpi TOS, 0x00 ; immediate mode?
      breq _execute_it

      ; compile mode
      st Y+, TOSL
      movw X, Z ; PFA on stack
      z_here
      st Z+, TOSL ; write PFA to 'here'
      st Z+, TOS
      mov Working, ZL ; set here to, uh, here
      ldi ZL, low(Here_mem)
      ldi ZH, high(Here_mem)
      st Z, Working
      ret

    _execute_it:
      mov TOS, TOSL ; clear the stack for the "client" word
      popup
      ijmp ; and execute it.

    _byee:
      popupw ; ditch the "error message"
      rcall DUP_PFA
      ldi TOS, '?'
      rcall EMIT_PFA
      rcall DUP_PFA
      ldi TOS, 0x0d
      rcall EMIT_PFA
      rcall DUP_PFA
      ldi TOS, 0x0a
      rcall EMIT_PFA
      ret

immediate_p::

    IMMEDIATE_P:
      .dw INTERPRET
      .db 4, "imm?"
    IMMEDIATE_P_PFA:
      ; LFA on stack
      adiw X, 1
      rcall LEFT_SHIFT_WORD_PFA
      movw Z, X
      lpm TOS, Z
      popup
      andi TOS, IMMED
      cpi TOS, IMMED
      ret

colon_does::

    COLON_DOES:
      .dw IMMEDIATE_P
      .db 10, "colon_does"
    COLON_DOES_PFA:
      pop ZH
      pop ZL
    _aaagain:
      push ZL
      push ZH
      pushdownw
      movw X, Z
      rcall LEFT_SHIFT_WORD_PFA
      movw Z, X
      popupw
      lpm Working, Z+
      lpm ZH, Z
      mov ZL, Working
      icall
      pop ZH
      pop ZL
      adiw Z, 1
      rjmp _aaagain

exit::

    EXIT:
      .dw COLON_DOES
      .db 4, "exit"
    EXIT_PFA:
      ; ditch return PC from the icall and the stored pointer to next PFA.
      in ZL, SPL
      in ZH, SPH
      adiw Z, 4
      out SPL, ZL
      out SPH, ZH
      ret

test routine for colon_does::

    TEST_COL_D:
      .dw EXIT
      .db 3, "tcd"
    TCD_PFA:
      rcall COLON_DOES_PFA
      .dw DUP_PFA
      .dw EXIT_PFA

"["::

    LBRAC:
      .dw TEST_COL_D
      .db (1 & IMMED), "["
    LBRAC_PFA:
      ldi ZL, low(State_mem)
      ldi ZH, high(State_mem)
      ldi Working, 0x00
      st Z, Working
      ret

"]"::

    RBRAC:
      .dw LBRAC
      .db 1, "]"
    RBRAC_PFA:
      ldi ZL, low(State_mem)
      ldi ZH, high(State_mem)
      ldi Working, 0x01
      st Z, Working
      ret

":"::

    COLON:
      .dw RBRAC
      .db 1, ":"
    COLON_PFA:
      rcall WORD_PFA
      rcall CREATE_PFA
      ; Write COLON_DOES_PFA to HERE and update HERE
      z_here
      ldi Working, low(COLON_DOES_PFA)
      st Z+, Working
      ldi Working, high(COLON_DOES_PFA)
      st Z+, Working
      ; Write ZL to Here_mem
      mov Working, ZL
      ldi ZL, low(Here_mem)
      ldi ZH, high(Here_mem)
      st Z, Working
      ; switch to compiling mode
      rcall RBRAC_PFA
      ret

";"::

    SEMICOLON:
      .dw COLON
      .db (1 & IMMED), ";"
    SEMICOLON_PFA:
      z_here
      ldi Working, low(EXIT_PFA)
      st Z+, Working
      ldi Working, high(EXIT_PFA)
      st Z+, Working
      mov Working, ZL
      ldi ZL, low(Here_mem)
      ldi ZH, high(Here_mem)
      st Z, Working
      ; switch back to immediate mode
      rcall LBRAC_PFA
      ret


Variables and system variable words
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

var_does::

    VAR_DOES:
      .dw SEMICOLON
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

here::

    HERE_WORD:
      .dw VAR_DOES
      .db 4, "here"
    HERE_PFA:
      rcall VAR_DOES_PFA
      .db low(Here_mem), high(Here_mem) ; Note: I'm putting the full address
                       ; here but the VAR_DOES machinery only uses low byte.
      ; We don't need to ret here because VAR_DOES will consume the top of
      ; the return stack. (I.e. the address of the Here_mem byte above.)

Latest::

    LATEST_WORD:
      .dw HERE_WORD
      .db 6, "latest"
    Latest_PFA:
      rcall VAR_DOES_PFA
      .db low(Latest_mem), high(Latest_mem)

State::

    STATE_WORD:
      .dw LATEST_WORD
      .db 5, "state"
    STATE_PFA:
      rcall VAR_DOES_PFA
      .db low(State_mem), high(State_mem)

Current_key::

    CURRENT_KEY_WORD:
      .dw STATE_WORD
      .db 4, "ckey"
    CURRENT_KEY_PFA:
      rcall DUP_PFA
      mov TOS, Current_key
      ret

