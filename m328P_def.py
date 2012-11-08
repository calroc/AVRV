from util import int2addr


defs = dict((k, int2addr(v)) for k, v in dict(

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

