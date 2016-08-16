# p12hex #

This project is a disassembler for the PIC 12F family of microcontrollers. The
origin of the project was the Reverse Engineering Challenge at DEFCON 24
(Hardware Hacking Villiage).

Currently, the disassembler merely interprets a the given hex format file and
"pretty" prints the records contained therein with the data sections split
into MSB-first 16-bit words. The 14-bit PIC12F instructions are packed into
the lower 14 bits of each word.
