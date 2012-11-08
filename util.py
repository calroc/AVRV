from struct import pack
from myhdl import intbv


ADDRESS_MAX = 0x3fff


def update(a, b):
  '''
  In-place update of an intbv with the value of another one.
  '''
  for n in range(len(a)):
    a[n] = b[n]


def int2addr(i):
  return intbv(i, min=0, max=ADDRESS_MAX)


def low(i):
  i = ibv(i)
  return i[8:]


def high(i):
  i = ibv(i)
  return i[16:8]


def ibv(i):
  if isinstance(i, int):
    return intbv(i)
  if not isinstance(i, intbv):
    raise ValueError
  return i


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


