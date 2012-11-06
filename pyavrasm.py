import sys, pprint
from collections import defaultdict
from myhdl import intbv, bin as biny
from pass2 import ops


def update(a, b):
  '''
  In-place update of an intbv with the value of another one.
  '''
  for n in range(len(a)):
    a[n] = b[n]


ADDRESS_MAX = 0x3fff
LOW_BYTE = (1 << 8) - 1
HIGH_BYTE = LOW_BYTE << 8


def int2addr(i):
  return intbv(i, min=0, max=ADDRESS_MAX)


def _i(i):
  if isinstance(i, int):
    return intbv(i)
  if not isinstance(i, intbv):
    raise ValueError
  return i


#: Global definitions.
G = dict((k, int2addr(v)) for k, v in dict(

    SRAM_START=0x100,
    RAMEND=0x08ff,

    SPL=0x3d,
    SPH=0x3e,
    YL=28,
    YH=29,

    r8=8,
    r16=16,
    r17=17,
    r26=26,
    r27=27,

    TWBR=0xb8,
    TWSR=0xb9,
    UBRR0L=0xc4,
    UBRR0H=0xc5,
    UCSR0A=0xc0,
    UCSR0B=0xc1,
    UCSR0C=0xc2,
    TXEN0=3, # Transmitter Enable
    RXEN0=4, # Receiver Enable

    RXC0=7, # USART Receive Complete
    UDR0=0xc6,

    ).iteritems())


def low(i):
  i = _i(i)
  return (i & LOW_BYTE)[8:]


def high(i):
  i = _i(i)
  return low((i & HIGH_BYTE) >> 8)


G.update(
  low=low,
  high=high,
  range=xrange,
  )


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
      self.dw,
      self.db,
      self.cli,
      self.ldi,
      self.out,
      self.rcall,
      self.sts,
      self.mov,
      self.sei,
      self.rjmp,
      self.ret,
      self.lds,
      self.sbrs,
      ):
      self.context[f.__name__] = f

    self.here = int2addr(0)
    self.data = {}

  # Directives

  def define(self, **defs):
    for k, v in defs.iteritems():
      if isinstance(v, int):
        defs[k] = v = _i(v)
      print 'defining %s = %#x' % (k, v)
    self.context.update(defs)

  def org(self, address):
    address = _i(address)
    print 'setting org to %#06x' % (address,)
    update(self.here, address)

  def label(self, label_thunk, reserves=0):
    assert isinstance(label_thunk, intbv), repr(label_thunk)
    assert label_thunk == 0, repr(label_thunk)
    name = self._name_of_address_thunk(label_thunk)
    print 'label %s => %#06x' % (name, self.here)
    update(label_thunk, self.here)
    if reserves:
      assert reserves > 0, repr(reserves)
      self.here += reserves

  def dw(self, *values):
    addr = self._get_here()
    data = ops['dw'](values)
    nbytes = len(data)
    print 'assembling %i data words at %s for %s => %r' % (nbytes/2, addr, values, data)
    self.data[addr] = ('dw', values)
    self.here += nbytes
##    values_bin_str = values_to_dw(values)
##    self.here += len(values_bin_str)

  def db(self, *values):
    addr = self._get_here()
    data = ops['db'](values)
    nbytes = len(data)
    print 'assembling %i data bytes at %s for %s => %r' % (nbytes, addr, values, data)
    self.data[addr] = ('db', values)
    self.here += nbytes

  # Instructions

  def jmp(self, address):
    self._one('jmp', address)
    self.here += 2

  def rjmp(self, address):
    self._one('rjmp', address)

  def rcall(self, address):
    self._one('rcall', address)

  def cli(self):
    self._none('cli')

  def sei(self):
    self._none('sei')

  def ret(self):
    self._none('ret')

  def ldi(self, target, address):
    self._two('ldi', target, address)

  def out(self, target, address):
    self._two('out', target, address)

  def sts(self, address, register):
    self._two('sts', address, register)
    self.here += 2

  def mov(self, target, source):
    self._two('mov', target, source)

  def lds(self, target, address):
    self._two('lds', target, address)
    self.here += 2

  def sbrs(self, target, address):
    self._two('sbrs', target, address)

  def _none(self, op):
    addr = self._get_here()
    print 'assembling %s instruction at %s' % (op, addr,)
    self.data[addr] = (op,)
    self.here += 2

  def _one(self, op, address):
    name, address = self._name_or_addr(address)
    addr = self._get_here()
    print 'assembling %s instruction at %s to %s' % (op, addr, name)
    self.data[addr] = (op, address)
    self.here += 2

  def _two(self, op, target, address):
    tname, taddress = self._name_or_addr(target)
    name, address = self._name_or_addr(address)
    addr = self._get_here()
    print 'assembling %s instruction at %s %s <- %s' % (op, addr, tname, name)
    self.data[addr] = (op, target, address)
    self.here += 2

  # Assembler proper

  def assemble(self, text):
    exec text in self.context
    del self.context['__builtins__']

  def assemble_file(self, filename):
    execfile(filename, self.context)
    del self.context['__builtins__']

  def pass2(self):
    accumulator = []
    for addr in sorted(self.data):
      instruction = self.data[addr]
      op, args = instruction[0], instruction[1:]
      opf = ops.get(op, lambda *args: args)
      data = opf(*args)
      if op in ('db', 'dw'):
        print 10 * ' ', op, len(data), 'bytes:', repr(data)
      else:
        try:
          print '%-10x' % (data,), instruction
        except TypeError:
          print 10 * '.', instruction
      accumulator.append(data)
    return accumulator

  def _name_of_address_thunk(self, thunk):
    for k, v in self.context.iteritems():
      if v is thunk:
        return k
    return '%#06x' % thunk

  def _get_here(self):
    addr = '%#06x' % (self.here,)
    assert addr not in self.data
    return addr

  def _name_or_addr(self, address):
    if isinstance(address, int):
      name = '%#06x' % address
      address = int2addr(address)
    else:
      assert isinstance(address, intbv), repr(address)
      name = self._name_of_address_thunk(address)
    return name, address


if __name__ == '__main__':
  aa = AVRAssembly()
  aa.assemble_file('asm.py')

##  print ; print ; print
##  pprint.pprint(dict(aa.context))
##  print ; print ; print
##  pprint.pprint(dict(aa.data))
  print ; print ; print
##  pprint.pprint(aa.pass2())
  aa.pass2()
