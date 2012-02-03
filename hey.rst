==============
Robot Firmware
==============

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

Introduction
------------

This is a simple sort-of Forth system for AVR microcontrollers. It's
written for the `Pololu Baby Orangutan robot controller`_

The system in general probably won't make much sense unless you're
already at least a little familiar with Forth. Two helpful sources are
`Brad Rodriguez' "Moving Forth" series`_ and `Richard
Jones'`_ wonderful `jonesforth "literate Forth"`_.

There's no attempt to implement or adhere to any standard Forth. I'm just
noodling around and creating the easiest version of what seems like it
will work.

.. _Pololu Baby Orangutan robot controller: http://www.pololu.com/catalog/product/1220

.. _Brad Rodriguez' "Moving Forth" series: http://www.bradrodriguez.com/papers/moving1.htm

.. _jonesforth "literate Forth": http://git.annexia.org/?p=jonesforth.git;a=summary

.. _Richard Jones': http://rwmj.wordpress.com/2010/08/07/jonesforth-git-repository/


Definitions
-----------

The Pololu Baby Orangutan is (currently) built around the Amtel
ATmega328P, so let's start by including the definitions for that. (This
file comes with the AVR Studio 4 software from Amtel.)::

  .nolist
  .include "m328Pdef.inc"
  .list
  .listmac

The AVR chip has a Harvard architecture with separate memories and buses for
program and data RAM. The data bus, SRAM, and registers are eight bits wide,
while the address bus and Flash memory (for persistent program storage)
are sixteen bits wide.

I've decided to try having the Top-Of-Stack (TOS) and the next 8-bit
"cell" underneath it (which I call TOSL below) in two registers with the
rest of the stack pointed to by another pair of registers.

Certain pairs of 8-bit registers (namely the "top" six registers r26 -
r31) can be used as 16-bit registers (namely X, Y, and Z) for addressing
and some math functions

If we use a pair of such registers as our TOS and TOSL it gives us the
ability to, say, put the low and high bytes of an address in program or
data RAM onto the stack and then efficiently use the 16-bit value to
fetch or store from that location.

Keep the top two items (bytes) on the stack in the X register::

  .def TOS = r27 ; XH
  .def TOSL = r26 ; XL

Y register is our Data Stack Pointer.
Z register will be used for diggin around in the dictionary.

We also use a working register::

  .def Working = r16



Data (SRAM) Organization
------------------------

::

  .dseg


Data Stack
~~~~~~~~~~

The Parameter (Data) Stack grows upward
towards the Return Stack at the top of RAM. Note that the first two bytes
of stack are kept in the X register. Due to this the initial two bytes of
the data stack will be filled with whatever was in X before the first
push, unless you load X (i.e. TOS and Just-Under-TOS) "manually" before
dropping into the interpreter loop::

  data_stack: .org 0x0100



Code (Flash RAM)
----------------

Macros
~~~~~~

Some data stack manipulation macros to ease readability.

Pop from data stack to TOSL. Note that you are responsible for preserving
the previous value of TOSL if you still want it after using the macro.
(I.e. mov TOS, TOSL)::

  .MACRO popup
    ld TOSL, -Y
  .ENDMACRO

Begining of code proper
~~~~~~~~~~~~~~~~~~~~~~~

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

Set up the Return Stack::

  ldi Working, low(RAMEND)
  out SPL, Working
  ldi Working, high(RAMEND)
  out SPH, Working

Initialize Data Stack::

  ldi YL, low(data_stack)
  ldi YH, high(data_stack)

Set the UART to talk to a serial port::

  rcall UART_INIT

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
    rcall KEY_PFA
    rjmp MAIN

Initialize the USART
~~~~~~~~~~~~~~~~~~~~

::

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


Words
-----

These are the basic commands of the system that work together to
implement the interpreter.

Key
~~~~~

Read a character from the serial port and push it onto the stack::

    KEY:
      .dw 0x0000
      .db 3, "key"

First, loop on the RXC0 bit of the UCSR0A register, which indicates that
a byte is available in the receive register::

    KEY_PFA:
      lds Working, UCSR0A
      sbrs Working, RXC0
      rjmp KEY_PFA

Make room on the stack and load the character onto it from the UART's data register::

      rcall DUP_PFA
      lds TOS, UDR0

Echo the char to the serial port::

      rcall ECHO_PFA
      ret

Dup
~~~~~

Duplicate the top value on the stack::

    DUP:
      .dw KEY
      .db 3, "dup"
    DUP_PFA:
      st Y+, TOSL ; push TOSL onto data stack
      mov TOSL, TOS
      ret

Emit
~~~~~

emit - Pop the top item from the stack and send it to the serial port::

    EMIT:
      .dw DUP
      .db 4, "emit"
    EMIT_PFA:
      rcall ECHO_PFA
      rcall DROP_PFA
      ret

Echo
~~~~~

Write the top item on the stack to the serial port::

    ECHO:
      .dw EMIT
      .db 4, "emit"

First, loop on the UDRE0 bit of the UCSR0A register, which indicates that
the data register is ready for a byte::

    ECHO_PFA:
      lds Working, UCSR0A
      sbrs Working, UDRE0
      rjmp ECHO_PFA
      sts UDR0, TOS
      ret

drop
~~~~~

Drop the top item from the stack::

    DROP:
      .dw ECHO
      .db 4, "drop"
    DROP_PFA:
      mov TOS, TOSL
      popup
      ret


