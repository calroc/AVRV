from functools import wraps
from struct import pack
from myhdl import intbv, concat


def A(func):
  @wraps(func)
  def inner(address):
    return K(func.__doc__, k=address)
  return inner


def B(func):
  @wraps(func)
  def inner(register, address):
    return K(func.__doc__, d=register, k=address)
  return inner


def B_reversed(func):
  @wraps(func)
  def inner(address, register):
    return K(func.__doc__, d=register, k=address)
  return inner


def C(func):
  @wraps(func)
  def inner(io_port, register):
    return K(func.__doc__, a=io_port, r=register)
  return inner


def D(func):
  @wraps(func)
  def inner(Rd, Rr):
    return K(func.__doc__, d=Rd, r=Rr)
  return inner


def E(func):
  @wraps(func)
  def inner(Rr, bit):
    return K(func.__doc__, r=Rr, b=bit)
  return inner


def K(pattern, **values):
  counts = dict((variable_letter, 0) for variable_letter in values)
  p = list(reversed(''.join(pattern.lower().split())))
  accumulator = []
  for i, bit in enumerate(p):
    if bit in '10':
      accumulator.append(bool(int(bit)))
      continue
    assert bit in values, repr((i, bit, values))
    index, value = counts[bit], values[bit]
    counts[bit] += 1
    bit = value[index]
    accumulator.append(bit)
  return concat(*reversed(accumulator))


def pack_word_name(name):
  n = len(name)
  n = n + 2 - (n % 2)
  return pack(str(n) + 'p', name)


_mark = set(dir())
_mark.add('_mark')

@A
def jmp(address):
  '''
  1001 010k kkkk 110k
  kkkk kkkk kkkk kkkk
  '''


def cli():
  return 0b1001010011111000


@B
def ldi(register, immediate):
  '''
  1110 KKKK dddd KKKK
  '''


@C
def out(io_port, register):
  '''
  1011 1AAr rrrr AAAA
  '''


@A
def rcall(address):
  '''
  1101 kkkk kkkk kkkk
  '''


@B_reversed
def sts(address, register):
  '''
  1001 001d dddd 0000
  kkkk kkkk kkkk kkkk
  '''


@D
def mov(Rd, Rr):
  '''
  0010 11rd dddd rrrr
  '''


def sei():
  return 0b1001010001111000


def ret():
  return 0b1001010100001000


@A
def rjmp(address):
  '''
  1100 kkkk kkkk kkkk
  '''


@B
def lds(register, address):
  '''
  1001 000d dddd 0000
  kkkk kkkk kkkk kkkk
  '''


@E
def sbrs(register, bit):
  '''
  1111 111r rrrr 0bbb
  '''


def dw(values):
  accumulator = []
  for value in values:
    accumulator.append(pack('H', value))
  return ''.join(accumulator)


def db(values):
  length, name = values
  assert length == len(name)
  return pack_word_name(name)


ops = dict(
  (name, func)
  for name, func in locals().iteritems()
  if name not in _mark
  )


if __name__ == '__main__':
  import pprint
  pprint.pprint(ops)
