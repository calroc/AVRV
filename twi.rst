

The TWI/I2C driver uses a register "twi" to maintain a state variable
that indicates the status of the ongoing communication.

Each TWI processing word checks the state upon entry and if it is
non-zero it exits the *calling* word. This means that any code that sets
twi to a non-zero value will cancel further TWI/I2C interactions (of the
outer *calling* command word.

In order to facilitate this we define a macro for use in TWI words::

    .MACRO check_twi
          cpi twi, 0x00
          brne _twi_fail
    .ENDMACRO

The ``_twi_fail`` routine is quite simple. It removes the caller's return
location from the return stack and then returns, effectively returning
from the caller::

    _twi_fail:
      pop Working
      pop Working
      ret

We are using a busywait loop on the ``TWINT`` bit of the ``TWCR``
register to wait until the TWI subsystem has completed the current task
(transmitting a byte or whatever.)  Later when we switch to an
interrupt-based system we can rewrite this routine and the rest of the
driver code should function correctly::

    _twinty:
      lds Working, TWCR
      sbrs Working, TWINT
      rjmp _twinty
      ret

Once the TWI task has been completed the result will be in the ``TWSR``
register (along with the prescaler settings which have to be masked off)
so let's have a routine that fetches it for us::

    FETCH_TWSR:
      lds Working, TWSR
      andi Working, 0b11111000
      ret

All TWI/I2C interactions start with sending a START signal on the bus,
which is achieved on the AVR by setting certain bits in the ``TWCR``
register::

    Send_START:
      check_twi
      ldi Working, (1 << TWINT)|(1 << TWSTA)|(1 << TWEN)
      sts TWCR, Working
      ret

Once the START signal has been transmitted the result status code will be
in the ``TWSR`` register. We need a word that checks that the expected
status is there::

    EXPECT_TWI_START:
      check_twi
      cpi Working, TWI_START
      brne _twi_false
      ret

If you set ``twi`` to any non-zero value it will cancel further
processing thanks to the ``check_twi`` macro at the start of (most)
words. This is a sort of generic "cancel" routine::

    _twi_false:
      ldi twi, 1
      ret

Now we have enough low-level words to define the complete action of
sending a START signal to the I2C bus::

    TWI_START:

Trigger the TWI peripheral to initiate a START signal::

      rcall Send_START

Wait for that to complete::

      rcall _twinty

Get the result status::

      rcall FETCH_TWSR

Check that it's what we expect and continue or abort::

      rcall EXPECT_TWI_START
      ret

And that's the complete sequence to cause the AVR to send a START signal
on the I2C bus
