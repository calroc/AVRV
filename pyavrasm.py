import sys, pprint
from collections import defaultdict
from myhdl import intbv, bin as biny
##from intelhex import IntelHex
##
##ih = IntelHex("avrvm.hex")
#ih.dump(sys.stdout)


def update(a, b):
  '''
  In-place update of an intbv with the value of another one.
  '''
  assert len(a) <= len(b), repr((a, b))
  for n in range(len(a)):
    a[n] = b[n]


ADDRESS_MAX = 2048


def int2addr(i):
  return intbv(i, min=0, max=ADDRESS_MAX)


#: Global definitions.
G = dict((k, int2addr(v)) for k, v in dict(

    SRAM_START=0x200,
    r26=0x26,
    r27=0x27,

    ).iteritems())


class AVRAssembly(object):

  def __init__(self, initial_context=None):
    if initial_context is None:
      initial_context = G.copy()

    self.context = defaultdict(lambda: int2addr(0))
    self.context.update(initial_context)
    for f in (
      self.define,
      self.org,
      self.jmp,
      self.label,
      self.cli,
      ):
      self.context[f.__name__] = f

    self.here = int2addr(0)
    self.data = {}

  # Directives

  def define(self, **v):
    for k_v in v.iteritems():
      print 'defining %s = %#x' % k_v
    self.context.update(v)

  def org(self, address):
    if isinstance(address, int):
      address = int2addr(address)
    print 'setting org to %#06x' % (address,)
    update(self.here, address)

  def label(self, label_thunk):
    name = self._name_of_address_thunk(label_thunk)
    print 'label %s set from %#06x => %#06x' % (name, label_thunk, self.here)
    update(label_thunk, self.here)

  # Instructions

  def jmp(self, address):
    if isinstance(address, int):
      name = '%#06x' % address
      address = int2addr(address)
    else:
      assert isinstance(address, intbv), repr(address)
      name = self._name_of_address_thunk(address)

    addr = '%#06x' % (self.here,)
    assert addr not in self.data

    print 'assembling jmp instruction at %s to %s' % (addr, name)
    self.data[addr] = ('jmp', address)
    self.here += 2

  def cli(self):
    addr = '%#06x' % (self.here,)
    assert addr not in self.data
    print 'assembling cli instruction at %s' % (addr,)
    self.data[addr] = ('cli',)
    self.here += 2

  # Assembler proper

  def assemble(self, text):
    exec text in self.context
    del self.context['__builtins__']

  def _name_of_address_thunk(self, thunk):
    for k, v in self.context.iteritems():
      if v is thunk:
        return k
    raise Exception('wtf %r' % (thunk,))



aa = AVRAssembly()

# .def TOS = r27 ; XH
# .def TOSL = r26 ; XL

aa.assemble('''
define(TOS=r27)
define(TOSL=r26)

org(SRAM_START)
print '...define some data pointers here'
##  buffer: .byte 0x40
##
##  data_stack: .org 0x0140 ; SRAM_START + buffer

org(0x0000)
jmp(RESET)
jmp(BAD_INTERUPT)
jmp(BAD_INTERUPT)
jmp(BAD_INTERUPT)
jmp(BAD_INTERUPT)

label(BAD_INTERUPT)
jmp(0x0000)

label(RESET)
cli()
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
