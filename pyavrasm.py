import sys, pprint
from collections import defaultdict
from struct import pack
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

    X=26,
    Y=28,
    Z=30,

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


class InstructionsMixin(object):

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

  def st_post_incr(self, ptr, register):
    if ptr == 26:
      op = 'st_post_incr_X'
    elif ptr == 28:
      op = 'st_post_incr_Y'
    elif ptr == 30:
      op = 'st_post_incr_Z'
    else:
      raise Exception("Invalid target for st: %#x" % (ptr,))
    self._one(op, register)

  def ld_post_incr(self, register, ptr):
    if ptr == 26:
      op = 'ld_post_incr_X'
    elif ptr == 28:
      op = 'ld_post_incr_Y'
    elif ptr == 30:
      op = 'ld_post_incr_Z'
    else:
      raise Exception("Invalid target for ld: %#x" % (ptr,))
    self._one(op, register)

  def ld_pre_decr(self, register, ptr):
    if ptr == 26:
      op = 'ld_pre_decr_X'
    elif ptr == 28:
      op = 'ld_pre_decr_Y'
    elif ptr == 30:
      op = 'ld_pre_decr_Z'
    else:
      raise Exception("Invalid target for ld: %#x" % (ptr,))
    self._one(op, register)

  def lpm_post_incr(self, register, ptr):
    if ptr == 26:
      op = 'lpm_post_incr_X'
    elif ptr == 28:
      op = 'lpm_post_incr_Y'
    elif ptr == 30:
      op = 'lpm_post_incr_Z'
    else:
      raise Exception("Invalid target for lpm: %#x" % (ptr,))
    self._one(op, register)

  def cpi(self, register, immediate):
    self._two('cpi', register, immediate)

  def brne(self, address):
    self._one('brne', address)

  def breq(self, address):
    self._one('breq', address)

  def inc(self, address):
    self._one('inc', address)

  def mul(self, target, source):
    self._two('mul', target, source)

  def brlo(self, address):
    self._one('brlo', address)

  def subi(self, target, source):
    self._two('subi', target, source)

  def add(self, target, source):
    self._two('add', target, source)

  def dec(self, address):
    self._one('dec', address)

  def clr(self, address):
    self._one('clr', address)

  def lsl(self, address):
    self._one('lsl', address)

  def brcc(self, address):
    self._one('brcc', address)

  def or_(self, target, source):
    self._two('or_', target, source)

  def push(self, address):
    self._one('push', address)

  def swap(self, address):
    self._one('swap', address)

  def pop(self, address):
    self._one('pop', address)

  def movw(self, target, source):
    self._two('movw', target, source)

  def andi(self, target, source):
    self._two('andi', target, source)

  def adiw(self, target, source):
    self._two('adiw', target, source)

  def lpm(self, target, source):
    assert source == 30, repr(source) # Must be Z
    self._one('lpm', target)

  def cp(self, target, source):
    self._two('cp', target, source)

  def cpc(self, target, source):
    self._two('cpc', target, source)

  def brsh(self, address):
    self._one('brsh', address)

  def cpse(self, target, source):
    self._two('cpse', target, source)

  def sbiw(self, target, source):
    self._two('sbiw', target, source)

  def lsr(self, address):
    self._one('lsr', address)

  def ijmp(self):
    self._none('ijmp')

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

  def _instruction_namespace(self):
    for n in dir(InstructionsMixin):
      if n.startswith('_'):
        continue
      yield n, getattr(self, n)


class AVRAssembly(InstructionsMixin, object):

  def __init__(self, initial_context=None):
    if initial_context is None:
      initial_context = G.copy()

    self.context = defaultdict(lambda: int2addr(0))
    self.context.update(initial_context)
    self.context.update(self._instruction_namespace())
    for f in (
      self.define,
      self.org,
      self.label,
      self.dw,
      self.db,
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
    data = compute_dw(values)
    nbytes = len(data)
    print 'assembling %i data words at %s for %s => %r' % (nbytes/2, addr, values, data)
    self.data[addr] = ('dw', values, data)
    self.here += nbytes
##    values_bin_str = values_to_dw(values)
##    self.here += len(values_bin_str)

  def db(self, *values):
    addr = self._get_here()
    data = compute_db(values)
    nbytes = len(data)
    print 'assembling %i data bytes at %s for %s => %r' % (nbytes, addr, values, data)
    self.data[addr] = ('db', values, data)
    self.here += nbytes

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
      n, data = opf(*args)

      if n == -1:
        bindata = data
      elif n == 16:
        bindata = pack('H', data)
      else:
        bindata = pack('2H', data[32:16], data[16:])

      if op in ('db', 'dw'):
        print addr, 10 * ' ', op, len(data), 'bytes:', repr(data)
      else:
        try:
          fdata = '%-10x' % (data,)
        except TypeError:
          print addr, 10 * '.', instruction, repr(data)
        else:
          print addr, fdata, instruction, repr(bindata)
      accumulator.append(bindata)
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
    if isinstance(address, str):
      assert len(address) == 1, repr(address)
      address = ord(address)
    if isinstance(address, int):
      name = '%#06x' % address
      address = int2addr(address)
    else:
      assert isinstance(address, intbv), repr(address)
      name = self._name_of_address_thunk(address)
    return name, address


def compute_dw(values):
  accumulator = []
  for value in values:
    accumulator.append(pack('H', value))
  return ''.join(accumulator)


def compute_db(values):
  accumulator = []
  for value in values:
    if isinstance(value, str):
      accumulator.append(value)
    elif isinstance(value, int):
      accumulator.append(pack('B', value))
  data = ''.join(accumulator)
  if len(data) % 2:
    data = data + '\0'
  return data


if __name__ == '__main__':
  aa = AVRAssembly()
  aa.assemble_file('asm.py')

##  print ; print ; print
##  pprint.pprint(dict(aa.context))
##  print ; print ; print
##  pprint.pprint(dict(aa.data))
  print ; print ; print
  pprint.pprint(aa.pass2())
##  data = aa.pass2()
##  from intelhex import IntelHex
##  ih = IntelHex()
##  for addr, val in data.iteritems():
##    addr = 2 * int(addr, 16)
##    if isinstance(val, str):
##      ih.puts(addr, val)
##    else:
##      print 'non-str', addr, repr(val)
##  ih.dump()
