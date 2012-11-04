import sys, pprint
from collections import defaultdict
##from intelhex import IntelHex
##
##ih = IntelHex("avrvm.hex")
#ih.dump(sys.stdout)


def _labels():
  n = 0
  while True:
    yield 'label_%i' % n
    n += 1


G = dict(
    SRAM_START=0x200,
    r26=0x26,
    r27=0x27,
    )


class AVRAssembly(object):

  def __init__(self, initial_context=None):
    if initial_context is None:
      initial_context = G.copy()
    self.context = defaultdict(_labels().next)
    self.context.update(initial_context)
    for f in (
      self.define,
      self.org,
      self.jmp,
      self.label,
      ):
      self.context[f.__name__] = f
    self.here = 0
    self.data = {}

  def define(self, **v):
    for k_v in v.iteritems():
      print 'defining %s = %#x' % k_v
    self.context.update(v)

  def org(self, address):
    print 'setting org to', address
    self.here = int(address)

  def jmp(self, address):
    instruction = 'jmp instruction at %#06x to %r' % (self.here, address)
    print 'assembling', instruction
    addr = '%#06x' % (self.here,)
    assert addr not in self.data
    self.data[addr] = instruction
    self.here += 2

  def label(self, label_value):
    assert label_value.startswith('label_'), repr(label_value)
    for k, v in self.context.iteritems():
      if v == label_value:
        name = k
        break
    else:
      raise Exception('wtf %r' % (label_value,))

    print 'label', name, '=>', self.here
    self.context[name] = self.here

  def assemble(self, text):
    exec text in self.context
    del self.context['__builtins__']


aa = AVRAssembly()

# .def TOS = r27 ; XH
# .def TOSL = r26 ; XL

aa.assemble('''
define(TOS=r27)
define(TOSL=r26)

org(SRAM_START)
##  buffer: .byte 0x40
##
##  data_stack: .org 0x0140 ; SRAM_START + buffer

org(0x0000)
jmp(RESET)
jmp(BAD_INTERUPT)

label(BAD_INTERUPT)
jmp(0x0000)

label(RESET)
''')

print ; print ; print
pprint.pprint(dict(aa.context))
print ; print ; print
pprint.pprint(dict(aa.data))

##  .dseg
##  
##
##  buffer: .byte 0x40
##
##  data_stack: .org 0x0140 ; SRAM_START + buffer
##
##  .MACRO popup
##    ld TOSL, -Y
##  .ENDMACRO
##
##  .MACRO pushdownw
##    st Y+, TOSL
##    st Y+, TOS
##  .ENDMACRO
##
##  .MACRO popupw
##    ld TOS, -Y
##    ld TOSL, -Y
##  .ENDMACRO
##
##  .cseg
##
##  .org 0x0000
##    jmp RESET
##    jmp BAD_INTERUPT ; INT0 External Interrupt Request 0
##    jmp BAD_INTERUPT ; INT1 External Interrupt Request 1
##    jmp BAD_INTERUPT ; PCINT0 Pin Change Interrupt Request 0
##    jmp BAD_INTERUPT ; PCINT1 Pin Change Interrupt Request 1
##    jmp BAD_INTERUPT ; PCINT2 Pin Change Interrupt Request 2
##    jmp BAD_INTERUPT ; WDT Watchdog Time-out Interrupt
##    jmp BAD_INTERUPT ; TIMER2 COMPA Timer/Counter2 Compare Match A
##    jmp BAD_INTERUPT ; TIMER2 COMPB Timer/Counter2 Compare Match B
##    jmp BAD_INTERUPT ; TIMER2 OVF Timer/Counter2 Overflow
##    jmp BAD_INTERUPT ; TIMER1 CAPT Timer/Counter1 Capture Event
##    jmp BAD_INTERUPT ; TIMER1 COMPA Timer/Counter1 Compare Match A
##    jmp BAD_INTERUPT ; TIMER1 COMPB Timer/Coutner1 Compare Match B
##    jmp BAD_INTERUPT ; TIMER1 OVF Timer/Counter1 Overflow
##    jmp BAD_INTERUPT ; TIMER0 COMPA Timer/Counter0 Compare Match A
##    jmp BAD_INTERUPT ; TIMER0 COMPB Timer/Counter0 Compare Match B
##    jmp BAD_INTERUPT ; TIMER0 OVF Timer/Counter0 Overflow
##    jmp BAD_INTERUPT ; SPI, STC SPI Serial Transfer Complete
##    jmp BAD_INTERUPT ; USART, RX USART Rx Complete
##    jmp BAD_INTERUPT ; USART, UDRE USART, Data Register Empty
##    jmp BAD_INTERUPT ; USART, TX USART, Tx Complete
##    jmp BAD_INTERUPT ; ADC ADC Conversion Complete
##    jmp BAD_INTERUPT ; EE READY EEPROM Ready
##    jmp BAD_INTERUPT ; ANALOG COMP Analog Comparator
##    jmp BAD_INTERUPT ; TWI 2-wire Serial Interface
##    jmp BAD_INTERUPT ; SPM READY Store Program Memory Ready
##  BAD_INTERUPT:
##    jmp 0x0000
##
##  RESET:
##    cli
##
##  ldi Working, low(RAMEND)
##  out SPL, Working
##  ldi Working, high(RAMEND)
##  out SPH, Working
##
##  ldi YL, low(data_stack)
##  ldi YH, high(data_stack)
##
##  rcall UART_INIT
##
##  ldi Working, 23
##  sts TWBR, Working ; set bitrate
##  ldi Working, 1
##  sts TWSR, Working ; set prescaler
##
##  ldi Working, 10
##  mov Base, Working
##
##  sei
##
##  MAIN:
##    rcall INTERPRET_PFA
##    rcall DOTESS_PFA
##    rjmp MAIN
##
##  UART_INIT:
##    ldi r17, high(520) ; 2400 baud w/ 20Mhz osc
##    ldi r16, low(520)  ; See Datasheet
##    sts UBRR0H, r17
##    sts UBRR0L, r16
##    ; The chip defaults to 8N1 so we won't set it here even though we
##    ; should.
##    ldi r16, (1 << TXEN0) | (1 << RXEN0) ; Enable transmit/receive
##    sts UCSR0B, r16
##    ret
