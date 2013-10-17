#include Reg9s12.h
#include IntVec.h

                ORG $2000       ;Start at memory address 2000
                LDX #$0000      ;Register base is 0x00
                BRA MAIN        ;GOTO main

COUNT:          RMB 2
BUFFER:         RMB 10
INDEX:          RMB 2
SPACE:          RMB 1
        
        
MAIN:           MOVB #$FF, DDRA
                MOVB #$0F, DDRB
                JSR TIMSET
                MOVW #$0000, COUNT      ;Init count
                MOVW #$0000, INDEX      ;Init index
                MOVB #$00, SPACE        ;Init space
                JSR SCISET              ;Setup SCI
                CLI                     ;Enable interrupts
        
LOOP:           WAI
                BRA LOOP


 ;-------------------------------------------------------------------------------
;void TIMSET(void)
;Sets up the timer for timeroverflow
;-------------------------------------------------------------------------------
TIMSET:         MOVB #$80, TSCR         ;Turn on timer
                LDD #INTH               ;Store the ISR in the vector table
                STD TOIvec
                LDAA TMSK2              ;Set timer overflow interrupt
                ORAA #$80
                STAA TMSK2
                LDAA TFLG2              ;Clear overflow bit
                ANDA #$80
                STAA TFLG2
                RTS
;-------------------------------------------------------------------------------
;void COV7SEG(int number)
;Converts the number to a seven seg display
;-------------------------------------------------------------------------------
COV7SEG:        LDAB 2,SP
                CMPB #$00
                BEQ COV7SEG0
                CMPB #$01
                BEQ COV7SEG1
                CMPB #$02
                BEQ COV7SEG2
                CMPB #$03
                BEQ COV7SEG3
                CMPB #$04
                BEQ COV7SEG4
                CMPB #$05
                BEQ COV7SEG5
                CMPB #$06
                BEQ COV7SEG6
                CMPB #$07
                BEQ COV7SEG7
                CMPB #$08
                BEQ COV7SEG8
                CMPB #$09
                BEQ COV7SEG9
                LDAA #%01111001
                RTS
COV7SEG0:       LDAA #%00111111
                RTS
COV7SEG1:       LDAA #%00000110
                RTS
COV7SEG2:       LDAA #%01011011
                RTS
COV7SEG3:       LDAA #%01001111
                RTS
COV7SEG4:       LDAA #%01100110
                RTS
COV7SEG5:       LDAA #%01101101
                RTS
COV7SEG6:       LDAA #%01111101
                RTS
COV7SEG7:       LDAA #%00000111
                RTS
COV7SEG8:       LDAA #%01111111
                RTS
COV7SEG9:       LDAA #%01101111
                RTS

;-------------------------------------------------------------------------------
;void INTH(void)
;Handles the timer overflow interrupt
;-------------------------------------------------------------------------------
INTH:                LDD COUNT       ;Increment count
                ADDD #$0001
                STD COUNT
                LDX #$0002      ;---
                IDIV            ;Divide by 2 and see what our remainder is
                CPD #$0000      ;If the remainder is 0 then send power to the
                BNE INT2AN      ;first anode and otherwise send power to the
                LDAA PORTA      ;second anode
                ORAA #$80       ;--
                STAA PORTA
                BRA INTCHKIO
INT2AN:         LDAA PORTA
                ANDA #$7F
                STAA PORTA
INTCHKIO:       LDD COUNT
                CPD #$005F      ;See if we need to check the keypad
                BNE INTCLR
                MOVW #$0000, COUNT ;Reset the value of count
                JSR KEYIO       ;Check to see if we have input
                CMPA #$11
                BEQ INTNOKY
                CMPA #$0F       ;Check to see if we flush the buffer
                BNE INTBUF
                LDY INDEX       ;Flush key was pressed
                STAA BUFFER,Y
                TFR Y,D
                ADDD #$0001
                STD INDEX
                LDY #$0000      ;Flush key was pressed
INTHLP:         CPY INDEX
                BEQ INTHELP
                LDAA BUFFER,Y   ;Load byte into A register
                PSHA
                JSR SNDSCI      ;Call send byte subroutine
                LEAS 1,SP
                TFR Y,D         ;Add 1 to Y register which means we have to transfer
                ADDD #$0001
                TFR D,Y
                BRA INTHLP
INTHELP:        MOVW #$0000, INDEX ;Reset the buffer
                SWI
                BRA INTCLR
INTBUF:         LDAB SPACE      ;Make sure there was a space inbetween
                CMPB #$00
                BEQ INTCLR
                PSHA
                JSR COV7SEG     ;Convert number into 7 seg display
                STAA PORTA
                PULA
                LDY INDEX
                STAA BUFFER,Y   ;Store the keystroke into the buffer
                LDD INDEX       ;Increment index
                ADDD #$0001
                STD INDEX
                MOVB #$00,SPACE          ;Set space = 0
                BRA INTCLR
INTNOKY:        MOVB #$01,SPACE         ;Set space to 1
INTCLR:         LDAA TFLG2              ;Clear overflow bit
                ANDA #$80
                STAA TFLG2
                RTI                     ;Return

;-------------------------------------------------------------------------------
;void SCISET(void)
;Sets up the SCI interface
;-------------------------------------------------------------------------------
SCISET:         LDD     #156
                STAB    sc1bdl
                STAA    sc1bdh
                CLRA
                STAA    sc1cr1
                LDAA    #$0E
                STAA    sc1cr2
                RTS

;-------------------------------------------------------------------------------
;void SNDSCI(byte data)
;Send the value at the address over the SCI
;-------------------------------------------------------------------------------
SNDSCI:         LDAA SC1SR1
                ANDA #$80
                BEQ SNDSCI      ;Make sure that we can send the bit
                LDAA 2,SP       ;Get the data off the stack
                STAA SC1DRL     ;Transmit
                RTS

;-------------------------------------------------------------------------------
;void BWAIT(void)
;Busy waits by counting down from 0xFFFF
;-------------------------------------------------------------------------------
BWAIT:          LDD #$FFFF      ;Load 0xFFFF into register D
BLP:            CPD #$0000      ;Compare it with 0x0000
                BEQ BEND        ;End the loop if they are equal
                SUBD #$0001     ;Decrement 1 from D
                BRA BLP         ;Loop
BEND:           RTS             ;Return to sub routine

;-------------------------------------------------------------------------------
;int CINPUT(int column)
;Sees if there is any input on the column and returns it in the A register
;If there is no input then it returns 17
;-------------------------------------------------------------------------------
CINPUT:         LDAA PORTB      ;Load port b into
                ANDA #$F0       ;Get the upper nibble
                CMPA #$00       ;See if there is any input
                BEQ CNOIN       ;There is no input
                CMPA #$10       ;Compare to the bit pattern 0x10 (first row)
                BEQ  CINR0
                CMPA #$20       ;Compare to the bit pattern 0x20 (second row)
                BEQ  CINR1
                CMPA #$40       ;Compare to the bit pattern 0x40 (thrid row)
                BEQ  CINR2
                CMPA #$80       ;Compare to the bit pattern 0x80 (fourth row)
                BEQ  CINR3
                LDAA #$11       ;Should never get here
                RTS
CINR0:          LDAA #$00
                ADDA 2,SP
                RTS
CINR1:          LDAA #$04
                ADDA 2,SP
                RTS
CINR2:          LDAA #$08
                ADDA 2,SP
                RTS
CINR3:          LDAA #$0C
                ADDA 2,SP
                RTS
CNOIN:          LDAA #$11        ;Load 17 into A register
                RTS

;-------------------------------------------------------------------------------
;int KEYIO(void)
;Sees if there is any input on keypad and returns it in the A register
;If there is no input then it returns 17
;-------------------------------------------------------------------------------
KEYIO:          LDAA #$08       ;Set 0x08 into Port B
                STAA PORTB      ;This sets PB4 as HIGH all other are LOW
                LDAA #$00       ;Load 0 into A
                PSHA            ;Push it onto the stack
                JSR CINPUT      ;Jump to the subroutine
                LEAS 1,SP       ;Put the stack back to normal
                CMPA #$11
                BNE HASIN
                LDAA #$04       ;Set 0x04 into Port B
                STAA PORTB      ;This sets PB3 as HIGH all other are LOW
                LDAA #$01       ;Load 1 as column parameter
                PSHA
                JSR CINPUT      ;Jump to the subroutine
                LEAS 1,SP       ;Put the stack back to normal
                CMPA #$11
                BNE HASIN
                LDAA #$02       ;Set 0x02 into Port B
                STAA PORTB      ;This sets PB1 as HIGH all other are LOW
                LDAA #$02       ;Load 2 into A
                PSHA            ;Push it onto the stack
                JSR CINPUT      ;Jump to the subroutine
                LEAS 1,SP       ;Put the stack back to normal
                CMPA #$11
                BNE HASIN
                LDAA #$01       ;Set 0x01 into Port B
                STAA PORTB      ;This sets PB0 as HIGH all other are LOW
                LDAA #$03       ;Load 2 into A
                PSHA            ;Push it onto the stack
                JSR CINPUT      ;Jump to the subroutine
                LEAS 1,SP       ;Put the stack back to normal
                CMPA #$11
                BNE HASIN
                LDAA #$11
HASIN:          RTS