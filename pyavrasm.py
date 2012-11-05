import sys, pprint
from collections import defaultdict
from myhdl import intbv, bin as biny


def update(a, b):
  '''
  In-place update of an intbv with the value of another one.
  '''
  assert len(a) <= len(b), repr((a, b))
  for n in range(len(a)):
    a[n] = b[n]


ADDRESS_MAX = 2048
LOW_BYTE = (1 << 8) - 1
HIGH_BYTE = LOW_BYTE << 8


def int2addr(i):
  return intbv(i, min=0, max=ADDRESS_MAX)


#: Global definitions.
G = dict((k, int2addr(v)) for k, v in dict(

    SRAM_START=0x100,
    RAMEND=ADDRESS_MAX - 1,

    SPL=0x12,
    SPH=0x13,
    YL=0x28,
    YH=0x29,

    r16=0x16,
    r26=0x26,
    r27=0x27,

    ).iteritems())


def low(i):
  if isinstance(i, int):
    address = int2addr(i)
  return (i & LOW_BYTE)[8:]


def high(i):
  if isinstance(i, int):
    address = int2addr(i)
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
      self.cli,
      self.ldi,
      self.out,
      ):
      self.context[f.__name__] = f

    self.here = int2addr(0)
    self.data = {}

  # Directives

  def define(self, **defs):
    for k, v in defs.iteritems():
      if isinstance(v, int):
        defs[k] = v = intbv(v)
      print 'defining %s = %#x' % (k, v)
    self.context.update(defs)

  def org(self, address):
    if isinstance(address, int):
      address = int2addr(address)
    print 'setting org to %#06x' % (address,)
    update(self.here, address)

  def label(self, label_thunk, reserves=0):
    assert label_thunk == 0, repr(label_thunk)
    name = self._name_of_address_thunk(label_thunk)
    print 'label %s => %#06x' % (name, self.here)
    update(label_thunk, self.here)
    if reserves:
      assert reserves > 0, repr(reserves)
      self.here += reserves

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

  def ldi(self, target, address):
    addr = '%#06x' % (self.here,)
    assert addr not in self.data
    print 'assembling ldi instruction at %s %#04x <- %#04x' % (addr, target, address)
    self.data[addr] = ('ldi', target, address)
    self.here += 2

  def out(self, target, address):
    addr = '%#06x' % (self.here,)
    assert addr not in self.data
    print 'assembling out instruction at %s %#04x <- %#04x' % (addr, target, address)
    self.data[addr] = ('out', target, address)
    self.here += 2

  # Assembler proper

  def assemble(self, text):
    exec text in self.context
    del self.context['__builtins__']

  def assemble_file(self, filename):
    execfile(filename, self.context)
    del self.context['__builtins__']

  def _name_of_address_thunk(self, thunk):
    for k, v in self.context.iteritems():
      if v is thunk:
        return k
    raise Exception('wtf %r' % (thunk,))



if __name__ == '__main__':
  aa = AVRAssembly()
  aa.assemble_file('asm.py')

  print ; print ; print
  pprint.pprint(dict(aa.context))
  print ; print ; print
  pprint.pprint(dict(aa.data))
