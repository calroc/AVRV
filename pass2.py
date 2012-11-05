from functools import wraps
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


ops = dict((f.__name__, f) for f in (
  jmp,
  cli,
  ldi,
  out,
  rcall,
  sts,
  mov,
  ))


if __name__ == '__main__':
  z = intbv(0b1111000011100011001010)
  x = jmp(z)


##j = K(bits, k=intbv(1<<21))
##k = K(bits, k=intbv((1<<22) - 1))
