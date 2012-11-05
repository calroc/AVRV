import sys, pprint
from collections import defaultdict
from myhdl import intbv, bin as biny


def update(a, b):
  '''
  In-place update of an intbv with the value of another one.
  '''
  for n in range(len(a)):
    a[n] = b[n]


ADDRESS_MAX = 2048
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
    RAMEND=ADDRESS_MAX - 1,

    SPL=0x12,
    SPH=0x13,
    YL=0x28,
    YH=0x29,

    r8=0x8,
    r16=0x16,
    r17=0x17,
    r26=0x26,
    r27=0x27,

    TWBR=0x23,
    TWSR=0x24,

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
      self.cli,
      self.ldi,
      self.out,
      self.rcall,
      self.sts,
      self.mov,
      self.sei,
      self.rjmp,
      self.ret,
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

  # Instructions

  def jmp(self, address):
    return self._one('jmp', address)

  def rjmp(self, address):
    return self._one('rjmp', address)

  def rcall(self, address):
    return self._one('rcall', address)

  def cli(self):
    return self._none('cli')

  def sei(self):
    return self._none('sei')

  def ret(self):
    return self._none('ret')

  def ldi(self, target, address):
    return self._two('ldi', target, address)

  def out(self, target, address):
    return self._two('out', target, address)

  def sts(self, target, address):
    return self._two('sts', target, address)

  def mov(self, target, address):
    return self._two('mov', target, address)

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

  print ; print ; print
  pprint.pprint(dict(aa.context))
  print ; print ; print
  pprint.pprint(dict(aa.data))
