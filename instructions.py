from util import A, B, B_reversed, C, D, E, F, G, H


_mark = set(dir()) ; _mark.add('_mark')


@A
def jmp(address):
  '''
  1001 010k kkkk 110k
  kkkk kkkk kkkk kkkk
  '''


def cli():
  return 16, 0b1001010011111000


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
  return 16, 0b1001010001111000


def ret():
  return 16, 0b1001010100001000


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


def dw(values, data):
  return -1, data


def db(values, data):
  return -1, data


@F
def st_post_incr_Y(Rr):
  '''
  1001 001r rrrr 1001
  '''


@F
def st_post_incr_Z(Rr):
  '''
  1001 001r rrrr 0001
  '''


@G
def ld_pre_decr_Y(Rd):
  '''
  1001 000d dddd 1010
  '''


@G
def ld_post_incr_X(Rd):
  '''
  1001 000d dddd 1101
  '''


@G
def ld_post_incr_Z(Rd):
  '''
  1001 000d dddd 0001
  '''


@G
def ld_pre_decr_Z(Rd):
  '''
  1001 000d dddd 0010
  '''


@G
def lpm(Rd):
  '''
  1001 000d dddd 0100
  '''


@G
def lpm_post_incr_Z(Rd):
  '''
  1001 000d dddd 0101
  '''


@B
def cpi(register, immediate):
  '''
  0011 KKKK dddd KKKK
  '''


@A
def brne(address):
  '''
  1111 01kk kkkk k001
  '''


@A
def breq(address):
  '''
  1111 00kk kkkk k001
  '''


@A
def brlo(address):
  '''
  1111 00kk kkkk k000
  '''


@G
def lsr(Rd):
  '''
  1001 010d dddd 0110
  '''


@G
def lsl(Rd):
  '''
  0000 11dd dddd dddd
  '''


@D
def add(Rd, Rr):
  '''
  0000 11rd dddd rrrr
  '''


@A
def brcc(address):
  '''
  1111 01kk kkkk k000
  '''


@G
def inc(Rd):
  '''
  1001 010d dddd 0011
  '''


@G
def dec(Rd):
  '''
  1001 010d dddd 1010
  '''


def ijmp():
  return 16, 0b1001010000001001


@D
def cp(Rd, Rr):
  '''
  0001 01rd dddd rrrr
  '''


@D
def cpse(Rd, Rr):
  '''
  0001 00rd dddd rrrr
  '''


@D
def cpc(Rd, Rr):
  '''
  0000 01rd dddd rrrr
  '''


@A
def brsh(address):
  '''
  1111 01kk kkkk k000
  '''


@D
def movw(Rd, Rr):
  '''
  0000 0001 dddd rrrr
  '''


@B
def andi(register, immediate):
  '''
  0111 KKKK dddd KKKK
  '''


@H
def sbis(register, bit):
  '''
  1001 1011 AAAA Abbb
  '''


@G
def clr(Rd):
  '''
  0010 01dd dddd dddd
  '''


@F
def push(Rr):
  '''
  1001 001r rrrr 1111
  '''


@G
def pop(Rd):
  '''
  1001 000d dddd 1111
  '''


@D
def or_(Rd, Rr):
  '''
  0010 10rd dddd rrrr
  '''


@G
def swap(Rd):
  '''
  1001 010d dddd 0010
  '''


@B
def adiw(register, immediate):
  '''
  1001 0110 KKdd KKKK
  '''


@B
def sbiw(register, immediate):
  '''
  1001 0111 KKdd KKKK
  '''


@B
def subi(register, immediate):
  '''
  0101 KKKK dddd KKKK
  '''


@D
def mul(Rd, Rr):
  '''
  1001 11rd dddd rrrr
  '''


ops = dict(
  (name, func)
  for name, func in locals().iteritems()
  if name not in _mark
  )


if __name__ == '__main__':
  import pprint
  pprint.pprint(ops)
