; EPROM entry points

MESSAGE  EQU $E263
PRNSTRZ  EQU $EA9C
CHROUT   EQU $E13F

; memory locations

VIDEOPTR EQU $FE24     ; points to video memory

; other constants

TMPSTACK EQU $F980     ; temporary stack during copy from EPROM to RAM

; **************************************************************************************
; Copies the content of the EPROM in ram at $E000.
; The destination address HL is calculated in a strange
; way, first a routine made of:
;   POP HL
;   PUSH HL
;   RET
; is written in RAM at $F900 and executed with stack pointing
; at $F980. The resulting HL is used as destination address for
; copying the EPROM

E000: F3                         ENTRY: DI                     ; disables interrupts
E001: 31 80 F9                          LD      SP,TMPSTACK    ; set a stack
E004: 21 E1 E5                          LD      HL,0E5E1h      ; POP HL, PUSH HL opcodes
E007: 22 00 F9                          LD      (0F900h),HL    ; write opcodes in mem
E00A: 3E C9                             LD      A,0C9h         ; RET opcode
E00C: 32 02 F9                          LD      (0F902h),A     ; write opcode in mem
E00F: CD 00 F9                          CALL    0F900h         ; calls POP HL+PUSH HL+RET
E012: 11 EE FF                          LD      DE,0FFEEh      ; ?DE = -17
E015: 19                                ADD     HL,DE          ; HL = HL - 17
E016: 11 00 E0                          LD      DE,ENTRY       ; source = start of this EPROM
E019: 01 00 10                          LD      BC,1000h       ; copies 4K of data
E01C: ED B0                             LDIR                   ; do the copy
E01E: C3 21 E0                          JP      LE021          ; continue in RAM

;
; Boots the EPROM from RAM
;

E021: F3                         LE021: DI                     ; disables interrupts
E022: 31 FF FC                          LD      SP,0FCFFh      ; set temporary stack pointer
E025: AF                                XOR     A              ; A=0
E026: D3 12                             OUT     (12h),A        ; write zero to ?unknown port
E028: D3 13                             OUT     (13h),A        ; write zero to ?unknown port
E02A: D3 10                             OUT     (10h),A        ; write zero to ?unknown port
E02C: D3 11                             OUT     (11h),A        ; write zero to ?unknown port

E02E: DB 34                             IN      A,(34h)        ; ?read mem config dip switches
E030: E6 05                             AND     05h            ; mask %0000-0111
E032: 11 00 20                          LD      DE,2000h       ; video memory
E035: 21 00 10                          LD      HL,1000h
E038: FE 01                             CP      01h
E03A: CA 4E E0                          JP      Z,LE04E        ; if bit 0 is on
E03D: 11 00 40                          LD      DE,4000h       ; video memory
E040: 21 00 40                          LD      HL,4000h
E043: FE 04                             CP      04h
E045: CA 4E E0                          JP      Z,LE04E        ; if bit 2 is on
                                                               ; if bit 1 is on
E048: 11 00 20                          LD      DE,2000h       ; video memory
E04B: 21 00 80                          LD      HL,8000h
E04E: 22 22 FE                   LE04E: LD      (0FE22h),HL    ; save pointer
E051: ED 53 24 FE                       LD      (VIDEOPTR),DE  ; save start of video memory
E055: 21 2B E2                          LD      HL,0E22Bh      ; unknown table
E058: CD 91 EA                          CALL    LEA91
E05B: CD EF E3                          CALL    LE3EF
E05E: CD BB E8                          CALL    LE8BB
E061: 21 63 E2                          LD      HL,MESSAGE     ; message
E064: CD 9C EA                          CALL    PRNSTRZ        ; print message

E067: DB 34                      LE067: IN      A,(34h) ; '4'
E069: E6 0A                             AND     0Ah            ; mask 0000-1010
E06B: 0E 00                             LD      C,00h
E06D: CA 80 E0                          JP      Z,LE080        ; if none of the bit are set
E070: 0E 01                             LD      C,01h
E072: FE 08                             CP      08h
E074: CA 80 E0                          JP      Z,LE080        ; if bit 3 is set
E077: 0E 02                             LD      C,02h
E079: FE 02                             CP      02h            ; if bit 1 is set
E07B: CA 80 E0                          JP      Z,LE080
E07E: 0E 03                             LD      C,03h

                                 ; C=0 config xxxx-0x0x
                                 ; C=1 config xxxx-1x0x
                                 ; C=2 config xxxx-0x1x
                                 ; C=3 config xxxx-1x1x

E080: 1E 01                      LE080: LD      E,01h
E082: CD AB E1                          CALL    LE1AB       ; HL=table in mem from C
E085: 7C                                LD      A,H
E086: B5                                OR      L
E087: CA 67 E0                          JP      Z,LE067     ; if HL=0 repeat

E08A: D5                                PUSH    DE
E08B: DD E1                             POP     IX
E08D: CD 0D E4                          CALL    LE40D
E090: 01 80 00                          LD      BC,0080h
E093: CD E2 E1                          CALL    LE1E2
E096: CD A6 E1                          CALL    LE1A6
E099: 01 01 00                          LD      BC,0001h
E09C: CD DD E1                          CALL    LE1DD
E09F: CD E7 E1                          CALL    LE1E7
E0A2: A7                                AND     A
E0A3: C2 67 E0                          JP      NZ,LE067
E0A6: 21 90 00                          LD      HL,0090h
E0A9: 11 CF E2                          LD      DE,0E2CFh
E0AC: 01 32 00                          LD      BC,0032h
E0AF: ED B0                             LDIR
E0B1: DD 21 CF E2                       LD      IX,0E2CFh
E0B5: CD 0D E4                          CALL    LE40D
E0B8: 2A 84 00                          LD      HL,(0084h)
E0BB: 22 A7 E2                          LD      (LE2A7),HL
E0BE: 22 A9 E2                          LD      (LE2A9),HL
E0C1: 2A 86 00                          LD      HL,(0086h)
E0C4: 22 AB E2                          LD      (LE2AB),HL
E0C7: 2A 88 00                          LD      HL,(0088h)
E0CA: 22 AD E2                          LD      (LE2AD),HL
E0CD: 21 01 00                          LD      HL,0001h
E0D0: 22 AF E2                          LD      (LE2AF),HL
E0D3: 21 00 00                          LD      HL,0000h
E0D6: 22 B1 E2                          LD      (LE2B1),HL
E0D9: 21 CF E2                          LD      HL,0E2CFh
E0DC: 22 B3 E2                          LD      (LE2B3),HL
E0DF: 1E 01                             LD      E,01h
E0E1: 0E 00                             LD      C,00h
E0E3: CD AB E1                          CALL    LE1AB
E0E6: 7C                                LD      A,H
E0E7: B5                                OR      L
E0E8: CA 67 E0                          JP      Z,LE067
E0EB: ED 4B A7 E2                LE0EB: LD      BC,(LE2A7)
E0EF: CD E2 E1                          CALL    LE1E2
E0F2: ED 4B B1 E2                       LD      BC,(LE2B1)
E0F6: CD D8 E1                          CALL    LE1D8
E0F9: ED 4B AF E2                       LD      BC,(LE2AF)
E0FD: CD DD E1                          CALL    LE1DD
E100: CD E7 E1                          CALL    LE1E7
E103: A7                                AND     A
E104: C2 67 E0                          JP      NZ,LE067
E107: 2A AB E2                          LD      HL,(LE2AB)
E10A: 2B                                DEC     HL
E10B: 22 AB E2                          LD      (LE2AB),HL
E10E: 7C                                LD      A,H
E10F: B5                                OR      L
E110: C2 17 E1                          JP      NZ,LE117
E113: 2A A9 E2                          LD      HL,(LE2A9)
E116: E9                                JP      (HL)

E117: 22 AB E2                   LE117: LD      (LE2AB),HL
E11A: ED 4B A7 E2                       LD      BC,(LE2A7)
E11E: 2A AD E2                          LD      HL,(LE2AD)
E121: 09                                ADD     HL,BC
E122: 22 A7 E2                          LD      (LE2A7),HL
E125: DD 2A B3 E2                       LD      IX,(LE2B3)
E129: 3A AF E2                          LD      A,(LE2AF)
E12C: DD BE 23                          CP      (IX+23h)
E12F: C2 38 E1                          JP      NZ,LE138
E132: 21 B1 E2                          LD      HL,0E2B1h
E135: 34                                INC     (HL)
E136: 3E 00                             LD      A,00h
E138: 3C                         LE138: INC     A
E139: 32 AF E2                          LD      (LE2AF),A
E13C: C3 EB E0                          JP      LE0EB

                                 ; prints to video ASCII character in C
E13F: ED 73 7E FD                CHROUT:LD      (0FD7Eh),SP      ; save SP
E143: 31 7E FD                          LD      SP,0FD7Eh        ; set tmp stack pointer
E146: E5                                PUSH    HL
E147: D5                                PUSH    DE
E148: C5                                PUSH    BC
E149: F5                                PUSH    AF
E14A: 2A 24 FE                          LD      HL,(VIDEOPTR)
E14D: 11 06 00                          LD      DE,0006h
E150: CD 5C E1                          CALL    LE15C
E153: F1                                POP     AF
E154: C1                                POP     BC
E155: D1                                POP     DE
E156: E1                                POP     HL
E157: ED 7B 7E FD                       LD      SP,(0FD7Eh)      ; restore SP
E15B: C9                                RET

                                        ; --- START PROC LE15C ---
E15C: D5                         LE15C: PUSH    DE
E15D: 11 00 00                          LD      DE,0000h
E160: 3E F0                             LD      A,0F0h
E162: A5                                AND     L
E163: 6F                                LD      L,A
E164: 7C                         LE164: LD      A,H
E165: B5                                OR      L
E166: CA 83 E1                          JP      Z,LE183
E169: 29                                ADD     HL,HL
E16A: DA 71 E1                          JP      C,LE171
E16D: 13                                INC     DE
E16E: C3 64 E1                          JP      LE164

E171: EB                         LE171: EX      DE,HL
E172: 29                                ADD     HL,HL
E173: E5                                PUSH    HL
E174: D1                                POP     DE
E175: 29                                ADD     HL,HL
E176: 29                                ADD     HL,HL
E177: 19                                ADD     HL,DE
E178: 11 03 E2                          LD      DE,0E203h
E17B: 19                                ADD     HL,DE
E17C: D1                                POP     DE
E17D: 19                                ADD     HL,DE
E17E: 5E                                LD      E,(HL)
E17F: 23                                INC     HL
E180: 56                                LD      D,(HL)
E181: EB                                EX      DE,HL
E182: E9                                JP      (HL)

E183: D1                         LE183: POP     DE
E184: C9                                RET

E185: DB                         LE185: DB      0DBh
E186: 2D                                DB      2Dh     ; '-'
E187: E6                                DB      0E6h
E188: 04                                DB      04h
E189: CA                                DB      0CAh
E18A: 85                                DB      85h
E18B: E1                                DB      0E1h
E18C: 79                                DB      79h     ; 'y'
E18D: E6                                DB      0E6h
E18E: 7F                                DB      7Fh
E18F: D3                                DB      0D3h
E190: 2C                                DB      2Ch     ; ','
E191: C9                                DB      0C9h
E192: DB                                DB      0DBh
E193: 2F                                DB      2Fh     ; '/'
E194: E6                                DB      0E6h
E195: 04                                DB      04h
E196: CA                                DB      0CAh
E197: 92                                DB      92h
E198: E1                                DB      0E1h
E199: 79                                DB      79h     ; 'y'
E19A: E6                                DB      0E6h
E19B: 7F                                DB      7Fh
E19C: D3                                DB      0D3h
E19D: 2E                                DB      2Eh     ; '.'
E19E: C9                                DB      0C9h
E19F: 3E                                DB      3Eh     ; '>'
E1A0: FF                                DB      0FFh
E1A1: C9                                DB      0C9h
E1A2: 3E                                DB      3Eh     ; '>'
E1A3: 1A                                DB      1Ah
E1A4: C9                                DB      0C9h
E1A5: C9                                DB      0C9h

                                        ; --- START PROC LE1A6 ---
E1A6: 01 00 00                   LE1A6: LD      BC,0000h
E1A9: 18 2D                             JR      LE1D8

                                        ; --- START PROC LE1AB ---
E1AB: 21 00 00                   LE1AB: LD      HL,0000h
E1AE: 79                                LD      A,C
E1AF: 32 96 E2                          LD      (LE296),A
E1B2: FE 04                             CP      04h
E1B4: D0                                RET     NC
E1B5: 7B                                LD      A,E
E1B6: 32 A6 E2                          LD      (LE2A6),A
E1B9: 11 97 E3                          LD      DE,0E397h
E1BC: 3A 96 E2                          LD      A,(LE296)
E1BF: 6F                                LD      L,A
E1C0: 29                                ADD     HL,HL
E1C1: 19                                ADD     HL,DE
E1C2: 5E                                LD      E,(HL)
E1C3: 23                                INC     HL
E1C4: 56                                LD      D,(HL)
E1C5: EB                                EX      DE,HL
E1C6: 22 99 E2                          LD      (LE299),HL
E1C9: 11 0C 00                          LD      DE,000Ch
E1CC: 19                                ADD     HL,DE
E1CD: 5E                                LD      E,(HL)
E1CE: 23                                INC     HL
E1CF: 56                                LD      D,(HL)
E1D0: ED 53 97 E2                       LD      (LE297),DE
E1D4: 2A 99 E2                          LD      HL,(LE299)
E1D7: C9                                RET

                                        ; --- START PROC LE1D8 ---
E1D8: ED 43 9C E2                LE1D8: LD      (LE29C),BC
E1DC: C9                                RET

                                        ; --- START PROC LE1DD ---
E1DD: 79                         LE1DD: LD      A,C
E1DE: 32 9E E2                          LD      (LE29E),A
E1E1: C9                                RET

                                        ; --- START PROC LE1E2 ---
E1E2: ED 43 9F E2                LE1E2: LD      (LE29F),BC
E1E6: C9                                RET

                                        ; --- START PROC LE1E7 ---
E1E7: ED 73 FD FD                LE1E7: LD      (0FDFDh),SP
E1EB: 31 FD FD                          LD      SP,0FDFDh
E1EE: DD 2A 97 E2                       LD      IX,(LE297)
E1F2: FD 21 96 E2                       LD      IY,0E296h
E1F6: CD 54 E4                          CALL    LE454
E1F9: ED 7B FD FD                       LD      SP,(0FDFDh)
E1FD: A7                                AND     A
E1FE: C8                                RET     Z
E1FF: 3E 01                             LD      A,01h
E201: C9                                RET

E202: 00                         LE202: DB      00h
E203: 9F                                DB      9Fh
E204: E1                                DB      0E1h
E205: 9F                                DB      9Fh
E206: E1                                DB      0E1h
E207: A2                                DB      0A2h
E208: E1                                DB      0E1h
E209: 85                                DB      85h
E20A: E1                                DB      0E1h
E20B: A5                                DB      0A5h
E20C: E1                                DB      0E1h
E20D: 9F                                DB      9Fh
E20E: E1                                DB      0E1h
E20F: 9F                                DB      9Fh
E210: E1                                DB      0E1h
E211: A2                                DB      0A2h
E212: E1                                DB      0E1h
E213: 92                                DB      92h
E214: E1                                DB      0E1h
E215: A5                                DB      0A5h
E216: E1                                DB      0E1h
E217: 9F                                DB      9Fh
E218: E1                                DB      0E1h
E219: 9F                                DB      9Fh
E21A: E1                                DB      0E1h
E21B: A2                                DB      0A2h
E21C: E1                                DB      0E1h
E21D: D8                                DB      0D8h
E21E: E8                                DB      0E8h
E21F: A5                                DB      0A5h
E220: E1                                DB      0E1h
E221: 9F                                DB      9Fh
E222: E1                                DB      0E1h
E223: 9F                                DB      9Fh
E224: E1                                DB      0E1h
E225: A2                                DB      0A2h
E226: E1                                DB      0E1h
E227: A5                                DB      0A5h
E228: E1                                DB      0E1h
E229: A5                                DB      0A5h
E22A: E1                                DB      0E1h

E22B: 02                                DB      02h
E22C: 25                                DB      25h     ; '%'
E22D: 07                                DB      07h
E22E: 0F                                DB      0Fh
E22F: 02                                DB      02h
E230: 27                                DB      27h     ; '''
E231: 07                                DB      07h
E232: 0F                                DB      0Fh
E233: 01                                DB      01h
E234: 37                                DB      37h     ; '7'
E235: E5                                DB      0E5h
E236: 09                                DB      09h
E237: 2F                                DB      2Fh     ; '/'
E238: 18                                DB      18h
E239: 01                                DB      01h
E23A: 00                                DB      00h
E23B: 03                                DB      03h
E23C: E1                                DB      0E1h
E23D: 04                                DB      04h
E23E: 44                                DB      44h     ; 'D'
E23F: 05                                DB      05h
E240: EA                                DB      0EAh
E241: 09                                DB      09h
E242: 2D                                DB      2Dh     ; '-'
E243: 18                                DB      18h
E244: 01                                DB      01h
E245: 00                                DB      00h
E246: 03                                DB      03h
E247: E1                                DB      0E1h
E248: 04                                DB      04h
E249: 45                                DB      45h     ; 'E'
E24A: 05                                DB      05h
E24B: AA                                DB      0AAh
E24C: 09                                DB      09h
E24D: 3B                                DB      3Bh     ; ';'
E24E: 18                                DB      18h
E24F: 01                                DB      01h
E250: 00                                DB      00h
E251: 03                                DB      03h
E252: E1                                DB      0E1h
E253: 04                                DB      04h
E254: 45                                DB      45h     ; 'E'
E255: 05                                DB      05h
E256: AA                                DB      0AAh
E257: 09                                DB      09h
E258: 39                                DB      39h     ; '9'
E259: 18                                DB      18h
E25A: 01                                DB      01h
E25B: 00                                DB      00h
E25C: 03                                DB      03h
E25D: E1                                DB      0E1h
E25E: 04                                DB      04h
E25F: 45                                DB      45h     ; 'E'
E260: 05                                DB      05h
E261: AA                                DB      0AAh
E262: 00                                DB      00h

                                        MESSAGE:

E263: 0D                                DB      0Dh
E264: 0A                                DB      0Ah
E265: 45 51 2D 34 20 45 51 55           DB      "EQ-4 EQUALIZER ROM loader rev 1.1A"
E26D: 41 4C 49 5A 45 52 20 52
E275: 4F 4D 20 6C 6F 61 64 65
E27D: 72 20 72 65 76 20 31 2E
E285: 31 41
E287: 0D                                DB      0Dh
E288: 0A                                DB      0Ah
E289: 30 36 2D 31 38 2D 38 33           DB      "06-18-83"
E291: 0D                                DB      0Dh
E292: 0A                                DB      0Ah
E293: 0D                                DB      0Dh
E294: 0A                                DB      0Ah
E295: 00                                DB      00h

E296: 00                         LE296: DB      00h
E297: 00                         LE297: DB      00h
E298: 00                                DB      00h
E299: 00                         LE299: DB      00h
E29A: 00                                DB      00h
E29B: 00                                DB      00h
E29C: 00                         LE29C: DB      00h
E29D: 00                                DB      00h
E29E: 00                         LE29E: DB      00h
E29F: 00                         LE29F: DB      00h
E2A0: 00                                DB      00h
E2A1: 00                                DB      00h
E2A2: 00                                DB      00h
E2A3: 00                                DB      00h
E2A4: 00                         LE2A4: DB      00h
E2A5: 00                         LE2A5: DB      00h
E2A6: 00                         LE2A6: DB      00h
E2A7: 00                         LE2A7: DB      00h
E2A8: 00                                DB      00h
E2A9: 00                         LE2A9: DB      00h
E2AA: 00                                DB      00h
E2AB: 00                         LE2AB: DB      00h
E2AC: 00                                DB      00h
E2AD: 00                         LE2AD: DB      00h
E2AE: 00                                DB      00h
E2AF: 00                         LE2AF: DB      00h
E2B0: 00                                DB      00h
E2B1: 00                         LE2B1: DB      00h
E2B2: 00                                DB      00h
E2B3: 00                         LE2B3: DB      00h
E2B4: 00                                DB      00h
E2B5: 01                                DB      01h
E2B6: 07                                DB      07h
E2B7: 0D                                DB      0Dh
E2B8: 13                                DB      13h
E2B9: 19                                DB      19h
E2BA: 05                                DB      05h
E2BB: 0B                                DB      0Bh
E2BC: 11                                DB      11h
E2BD: 17                                DB      17h
E2BE: 03                                DB      03h
E2BF: 09                                DB      09h
E2C0: 0F                                DB      0Fh
E2C1: 15                                DB      15h
E2C2: 02                                DB      02h
E2C3: 08                                DB      08h
E2C4: 0E                                DB      0Eh
E2C5: 14                                DB      14h
E2C6: 1A                                DB      1Ah
E2C7: 06                                DB      06h
E2C8: 0C                                DB      0Ch
E2C9: 12                                DB      12h
E2CA: 18                                DB      18h
E2CB: 04                                DB      04h
E2CC: 0A                                DB      0Ah
E2CD: 10                                DB      10h
E2CE: 16                                DB      16h
E2CF: 28                                DB      28h     ; '('
E2D0: 00                                DB      00h
E2D1: 04                                DB      04h
E2D2: 0F                                DB      0Fh
E2D3: 00                                DB      00h
E2D4: 8A                                DB      8Ah
E2D5: 01                                DB      01h
E2D6: 7F                                DB      7Fh
E2D7: 00                                DB      00h
E2D8: C0                                DB      0C0h
E2D9: 00                                DB      00h
E2DA: 20                                DB      20h     ; ' '
E2DB: 00                                DB      00h
E2DC: 02                                DB      02h
E2DD: 00                                DB      00h
E2DE: 03                                DB      03h
E2DF: 07                                DB      07h
E2E0: 00                                DB      00h
E2E1: FF                                DB      0FFh
E2E2: 50                                DB      50h     ; 'P'
E2E3: 00                                DB      00h
E2E4: 7F                                DB      7Fh
E2E5: 00                                DB      00h
E2E6: FF                                DB      0FFh
E2E7: 74                                DB      74h     ; 't'
E2E8: 00                                DB      00h
E2E9: 01                                DB      01h
E2EA: 00                                DB      00h
E2EB: 09                                DB      09h
E2EC: 40                                DB      40h     ; '@'
E2ED: 00                                DB      00h
E2EE: 00                                DB      00h
E2EF: 00                                DB      00h
E2F0: 00                                DB      00h
E2F1: 03                                DB      03h
E2F2: 05                                DB      05h
E2F3: 35                                DB      35h     ; '5'
E2F4: 00                                DB      00h
E2F5: 03                                DB      03h
E2F6: 03                                DB      03h
E2F7: 88                                DB      88h
E2F8: 80                                DB      80h
E2F9: 07                                DB      07h
E2FA: 00                                DB      00h
E2FB: 00                                DB      00h
E2FC: 00                                DB      00h
E2FD: 00                                DB      00h
E2FE: 00                                DB      00h
E2FF: 00                                DB      00h
E300: 00                                DB      00h
E301: 1A                                DB      1Ah
E302: 00                                DB      00h
E303: 03                                DB      03h
E304: 07                                DB      07h
E305: 00                                DB      00h
E306: F2                                DB      0F2h
E307: 00                                DB      00h
E308: 3F                                DB      3Fh     ; '?'
E309: 00                                DB      00h
E30A: C0                                DB      0C0h
E30B: 00                                DB      00h
E30C: 10                                DB      10h
E30D: 00                                DB      00h
E30E: 02                                DB      02h
E30F: 00                                DB      00h
E310: 00                                DB      00h
E311: 00                                DB      00h
E312: 00                                DB      00h
E313: 00                                DB      00h
E314: 4D                                DB      4Dh     ; 'M'
E315: 00                                DB      00h
E316: 7F                                DB      7Fh
E317: 00                                DB      00h
E318: FF                                DB      0FFh
E319: 1B                                DB      1Bh
E31A: 06                                DB      06h
E31B: 01                                DB      01h
E31C: 00                                DB      00h
E31D: 09                                DB      09h
E31E: 00                                DB      00h
E31F: 00                                DB      00h
E320: 00                                DB      00h
E321: 00                                DB      00h
E322: 00                                DB      00h
E323: 00                                DB      00h
E324: 1A                                DB      1Ah
E325: 07                                DB      07h
E326: 80                                DB      80h
E327: 03                                DB      03h
E328: 03                                DB      03h
E329: 0F                                DB      0Fh
E32A: FE                                DB      0FEh
E32B: 07                                DB      07h
E32C: 00                                DB      00h
E32D: 00                                DB      00h
E32E: 00                                DB      00h
E32F: 00                                DB      00h
E330: 00                                DB      00h
E331: 00                                DB      00h
E332: 00                                DB      00h
E333: 40                                DB      40h     ; '@'
E334: 00                                DB      00h
E335: 04                                DB      04h
E336: 0F                                DB      0Fh
E337: 00                                DB      00h
E338: 2B                                DB      2Bh     ; '+'
E339: 01                                DB      01h
E33A: FF                                DB      0FFh
E33B: 00                                DB      00h
E33C: F0                                DB      0F0h
E33D: 00                                DB      00h
E33E: 40                                DB      40h     ; '@'
E33F: 00                                DB      00h
E340: 02                                DB      02h
E341: 00                                DB      00h
E342: 03                                DB      03h
E343: 07                                DB      07h
E344: 00                                DB      00h
E345: 00                                DB      00h
E346: 4D                                DB      4Dh     ; 'M'
E347: 00                                DB      00h
E348: FF                                DB      0FFh
E349: 03                                DB      03h
E34A: FF                                DB      0FFh
E34B: 74                                DB      74h     ; 't'
E34C: 06                                DB      06h
E34D: 01                                DB      01h
E34E: 00                                DB      00h
E34F: 09                                DB      09h
E350: 40                                DB      40h     ; '@'
E351: 00                                DB      00h
E352: 00                                DB      00h
E353: 00                                DB      00h
E354: 00                                DB      00h
E355: 03                                DB      03h
E356: 08                                DB      08h
E357: 35                                DB      35h     ; '5'
E358: 00                                DB      00h
E359: 03                                DB      03h
E35A: 03                                DB      03h
E35B: 0F                                DB      0Fh
E35C: FE                                DB      0FEh
E35D: 07                                DB      07h
E35E: 00                                DB      00h
E35F: 00                                DB      00h
E360: 00                                DB      00h
E361: 00                                DB      00h
E362: 00                                DB      00h
E363: 00                                DB      00h
E364: 00                                DB      00h
E365: 00                                DB      00h
E366: 01                                DB      01h
E367: 05                                DB      05h
E368: 1F                                DB      1Fh
E369: 01                                DB      01h
E36A: BF                                DB      0BFh
E36B: 04                                DB      04h
E36C: FF                                DB      0FFh
E36D: 03                                DB      03h
E36E: FF                                DB      0FFh
E36F: 00                                DB      00h
E370: 00                                DB      00h
E371: 80                                DB      80h
E372: 01                                DB      01h
E373: 00                                DB      00h
E374: 03                                DB      03h
E375: 07                                DB      07h
E376: 00                                DB      00h
E377: 00                                DB      00h
E378: 99                                DB      99h
E379: 00                                DB      00h
E37A: FF                                DB      0FFh
E37B: 03                                DB      03h
E37C: 00                                DB      00h
E37D: 00                                DB      00h
E37E: 06                                DB      06h
E37F: 01                                DB      01h
E380: 00                                DB      00h
E381: 09                                DB      09h
E382: 00                                DB      00h
E383: 00                                DB      00h
E384: 00                                DB      00h
E385: 00                                DB      00h
E386: 00                                DB      00h
E387: 03                                DB      03h
E388: 20                                DB      20h     ; ' '
E389: 00                                DB      00h
E38A: 00                                DB      00h
E38B: 03                                DB      03h
E38C: 03                                DB      03h
E38D: 00                                DB      00h
E38E: 00                                DB      00h
E38F: 07                                DB      07h
E390: 00                                DB      00h
E391: 00                                DB      00h
E392: 00                                DB      00h
E393: 00                                DB      00h
E394: 00                                DB      00h
E395: 00                                DB      00h
E396: 00                                DB      00h
E397: B7                                DB      0B7h
E398: E3                                DB      0E3h
E399: C5                                DB      0C5h
E39A: E3                                DB      0E3h
E39B: D3                                DB      0D3h
E39C: E3                                DB      0E3h
E39D: E1                                DB      0E1h
E39E: E3                                DB      0E3h
E39F: 00                                DB      00h
E3A0: 00                                DB      00h
E3A1: 00                                DB      00h
E3A2: 00                                DB      00h
E3A3: 00                                DB      00h
E3A4: 00                                DB      00h
E3A5: 00                                DB      00h
E3A6: 00                                DB      00h
E3A7: 00                                DB      00h
E3A8: 00                                DB      00h
E3A9: 00                                DB      00h
E3AA: 00                                DB      00h
E3AB: 00                                DB      00h
E3AC: 00                                DB      00h
E3AD: 00                                DB      00h
E3AE: 00                                DB      00h
E3AF: 00                                DB      00h
E3B0: 00                                DB      00h
E3B1: 00                                DB      00h
E3B2: 00                                DB      00h
E3B3: 00                                DB      00h
E3B4: 00                                DB      00h
E3B5: 00                                DB      00h
E3B6: 00                                DB      00h
E3B7: 00                                DB      00h
E3B8: 00                                DB      00h
E3B9: 00                                DB      00h
E3BA: 00                                DB      00h
E3BB: 00                                DB      00h
E3BC: 00                                DB      00h
E3BD: 00                                DB      00h
E3BE: 00                                DB      00h
E3BF: 00                                DB      00h
E3C0: 00                                DB      00h
E3C1: 00                                DB      00h
E3C2: 00                                DB      00h
E3C3: CF                                DB      0CFh
E3C4: E2                                DB      0E2h
E3C5: B5                                DB      0B5h
E3C6: E2                                DB      0E2h
E3C7: 00                                DB      00h
E3C8: 00                                DB      00h
E3C9: 00                                DB      00h
E3CA: 00                                DB      00h
E3CB: 00                                DB      00h
E3CC: 00                                DB      00h
E3CD: 00                                DB      00h
E3CE: 00                                DB      00h
E3CF: 00                                DB      00h
E3D0: 00                                DB      00h
E3D1: 01                                DB      01h
E3D2: E3                                DB      0E3h
E3D3: 00                                DB      00h
E3D4: 00                                DB      00h
E3D5: 00                                DB      00h
E3D6: 00                                DB      00h
E3D7: 00                                DB      00h
E3D8: 00                                DB      00h
E3D9: 00                                DB      00h
E3DA: 00                                DB      00h
E3DB: 00                                DB      00h
E3DC: 00                                DB      00h
E3DD: 00                                DB      00h
E3DE: 00                                DB      00h
E3DF: 33                                DB      33h     ; '3'
E3E0: E3                                DB      0E3h
E3E1: 00                                DB      00h
E3E2: 00                                DB      00h
E3E3: 00                                DB      00h
E3E4: 00                                DB      00h
E3E5: 00                                DB      00h
E3E6: 00                                DB      00h
E3E7: 00                                DB      00h
E3E8: 00                                DB      00h
E3E9: 00                                DB      00h
E3EA: 00                                DB      00h
E3EB: 00                                DB      00h
E3EC: 00                                DB      00h
E3ED: 65                                DB      65h     ; 'e'
E3EE: E3                                DB      0E3h

                                        ; --- START PROC LE3EF ---
E3EF: 21 AC E6                   LE3EF: LD      HL,0E6ACh
E3F2: CD 91 EA                          CALL    LEA91
E3F5: 11 00 FD                          LD      DE,0FD00h
E3F8: 21 55 E6                          LD      HL,0E655h
E3FB: 01 34 00                          LD      BC,0034h
E3FE: ED B0                             LDIR
E400: ED 5E                             IM      2
E402: 3E FD                             LD      A,0FDh
E404: ED 47                             LD      I,A
E406: 21 C5 E6                          LD      HL,0E6C5h
E409: CD 91 EA                          CALL    LEA91
E40C: C9                                RET

                                        ; --- START PROC LE40D ---
E40D: CD FA E5                   LE40D: CALL    LE5FA
E410: CD F1 E5                          CALL    LE5F1
E413: DD 7E 17                          LD      A,(IX+17h)
E416: FE 00                             CP      00h
E418: C2 21 E4                          JP      NZ,LE421
E41B: CD 6B E4                          CALL    LE46B
E41E: C3 3C E4                          JP      LE43C

E421: 21 0A 00                   LE421: LD      HL,000Ah
E424: 22 1F E8                          LD      (LE81F),HL
E427: DD 7E 19                          LD      A,(IX+19h)
E42A: D3 23                             OUT     (23h),A ; '#'
E42C: 32 1E E8                          LD      (LE81E),A
E42F: DD E5                             PUSH    IX
E431: E1                                POP     HL
E432: 11 26 00                          LD      DE,0026h
E435: 19                                ADD     HL,DE
E436: CD D6 E5                          CALL    LE5D6
E439: CD AC E5                          CALL    LE5AC
                                        ; --- START PROC LE43C ---
E43C: C9                         LE43C: RET

E43D: DD                         LE43D: DB      0DDh
E43E: 7E                                DB      7Eh     ; '~'
E43F: 15                                DB      15h
E440: FD                                DB      0FDh
E441: 77                                DB      77h     ; 'w'
E442: 0B                                DB      0Bh
E443: DD                                DB      0DDh
E444: 7E                                DB      7Eh     ; '~'
E445: 16                                DB      16h
E446: FD                                DB      0FDh
E447: 77                                DB      77h     ; 'w'
E448: 0C                                DB      0Ch
E449: DD                                DB      0DDh
E44A: 7E                                DB      7Eh     ; '~'
E44B: 17                                DB      17h
E44C: FE                                DB      0FEh
E44D: 00                                DB      00h
E44E: CA                                DB      0CAh
E44F: D5                                DB      0D5h
E450: E6                                DB      0E6h
E451: C3                                DB      0C3h
E452: 88                                DB      88h
E453: E4                                DB      0E4h

                                        ; --- START PROC LE454 ---
E454: DD 7E 15                   LE454: LD      A,(IX+15h)
E457: FD 77 0B                          LD      (IY+0Bh),A
E45A: DD 7E 16                          LD      A,(IX+16h)
E45D: FD 77 0C                          LD      (IY+0Ch),A
E460: DD 7E 17                          LD      A,(IX+17h)
E463: FE 00                             CP      00h
E465: CA 0D E7                          JP      Z,LE70D
E468: C3 8C E4                          JP      LE48C

                                        ; --- START PROC LE46B ---
E46B: AF                         LE46B: XOR     A
E46C: D3 3D                             OUT     (3Dh),A ; '='
E46E: DB 3D                             IN      A,(3Dh) ; '='
E470: E6 10                             AND     10h
E472: CA 78 E4                          JP      Z,LE478
E475: 3E FF                             LD      A,0FFh
E477: C9                                RET

E478: CD 5F E7                   LE478: CALL    LE75F
E47B: 21 10 E8                          LD      HL,0E810h
E47E: CD 7A E7                          CALL    LE77A
E481: 21 16 E8                          LD      HL,0E816h
E484: CD 90 E7                          CALL    LE790
E487: C9                                RET

E488: 06                         LE488: DB      06h
E489: 05                                DB      05h
E48A: 18                                DB      18h
E48B: 02                                DB      02h

                                        ; --- START PROC LE48C ---
E48C: 06 06                      LE48C: LD      B,06h
E48E: DD 7E 1D                          LD      A,(IX+1Dh)
E491: E6 E0                             AND     0E0h
E493: B0                                OR      B
E494: DD 77 1D                          LD      (IX+1Dh),A
E497: FD 77 05                          LD      (IY+05h),A
E49A: DD 7E 11                          LD      A,(IX+11h)
E49D: DD 77 1E                          LD      (IX+1Eh),A
E4A0: FD 7E 06                          LD      A,(IY+06h)
E4A3: DD 77 1F                          LD      (IX+1Fh),A
E4A6: DD 36 20 00                       LD      (IX+20h),00h
E4AA: FD 7E 08                          LD      A,(IY+08h)
E4AD: DD 77 21                          LD      (IX+21h),A
E4B0: DD CB 19 4E                       BIT     1,(IX+19h)
E4B4: C2 DA E4                          JP      NZ,LE4DA
E4B7: 21 0A 00                          LD      HL,000Ah
E4BA: 22 1F E8                          LD      (LE81F),HL
E4BD: 3A 1E E8                          LD      A,(LE81E)
E4C0: CB 7F                             BIT     7,A
E4C2: CA DA E4                          JP      Z,LE4DA
E4C5: E6 7F                             AND     7Fh     ; ''
E4C7: D3 23                             OUT     (23h),A ; '#'
E4C9: 32 1E E8                          LD      (LE81E),A
E4CC: 16 02                             LD      D,02h
E4CE: 21 FF FF                   LE4CE: LD      HL,0FFFFh
E4D1: 2B                         LE4D1: DEC     HL
E4D2: 7C                                LD      A,H
E4D3: B5                                OR      L
E4D4: 20 FB                             JR      NZ,LE4D1
E4D6: 15                                DEC     D
E4D7: C2 CE E4                          JP      NZ,LE4CE
E4DA: 3A 1E E8                   LE4DA: LD      A,(LE81E)
E4DD: E6 F9                             AND     0F9h
E4DF: DD B6 19                          OR      (IX+19h)
E4E2: D3 23                             OUT     (23h),A ; '#'
E4E4: 32 1E E8                          LD      (LE81E),A
E4E7: 3A 8A E6                          LD      A,(LE68A)
E4EA: DD BE 28                          CP      (IX+28h)
E4ED: C2 F9 E4                          JP      NZ,LE4F9
E4F0: 3A 8B E6                          LD      A,(LE68B)
E4F3: DD BE 29                          CP      (IX+29h)
E4F6: CA 0F E5                          JP      Z,LE50F
E4F9: DD 7E 28                   LE4F9: LD      A,(IX+28h)
E4FC: 32 8A E6                          LD      (LE68A),A
E4FF: DD 7E 29                          LD      A,(IX+29h)
E502: 32 8B E6                          LD      (LE68B),A
E505: 11 26 00                          LD      DE,0026h
E508: DD E5                             PUSH    IX
E50A: E1                                POP     HL
E50B: 19                                ADD     HL,DE
E50C: CD D6 E5                          CALL    LE5D6
E50F: DD 7E 12                   LE50F: LD      A,(IX+12h)
E512: A7                                AND     A
E513: 28 0F                             JR      Z,LE524
E515: DD CB 1F 3E                       SRL     (IX+1Fh)
E519: D2 24 E5                          JP      NC,LE524
E51C: DD 36 20 01                       LD      (IX+20h),01h
E520: DD CB 1E D6                       SET     2,(IX+1Eh)
E524: 3E 05                      LE524: LD      A,05h
E526: 32 89 E6                          LD      (LE689),A
E529: CD 30 E6                   LE529: CALL    LE630
E52C: DD 7E 1F                          LD      A,(IX+1Fh)
E52F: DD BE 2E                          CP      (IX+2Eh)
E532: 28 07                             JR      Z,LE53B
E534: CD 67 E5                          CALL    LE567
E537: A7                                AND     A
E538: C2 56 E5                          JP      NZ,LE556
E53B: DD E5                      LE53B: PUSH    IX
E53D: E1                                POP     HL
E53E: 11 1C 00                          LD      DE,001Ch
E541: 19                                ADD     HL,DE
E542: CD D6 E5                          CALL    LE5D6
E545: FB                                EI
E546: DD E5                             PUSH    IX
E548: E1                                POP     HL
E549: 11 2A 00                          LD      DE,002Ah
E54C: 19                                ADD     HL,DE
E54D: CD E5 E5                          CALL    LE5E5
E550: 3E C0                             LD      A,0C0h
E552: DD A6 2B                          AND     (IX+2Bh)
E555: C8                                RET     Z
E556: CD AC E5                   LE556: CALL    LE5AC
E559: FE 00                             CP      00h
E55B: C2 64 E5                          JP      NZ,LE564
E55E: 21 89 E6                          LD      HL,0E689h
E561: 35                                DEC     (HL)
E562: 20 C5                             JR      NZ,LE529
E564: 3E FF                      LE564: LD      A,0FFh
E566: C9                                RET

                                        ; --- START PROC LE567 ---
E567: DD 7E 11                   LE567: LD      A,(IX+11h)
E56A: 32 8E E6                          LD      (LE68E),A
E56D: DD 7E 1F                          LD      A,(IX+1Fh)
E570: 32 8F E6                          LD      (LE68F),A
E573: 21 8C E6                          LD      HL,0E68Ch
E576: CD D6 E5                          CALL    LE5D6
E579: CD 27 E6                          CALL    LE627
E57C: 78                                LD      A,B
E57D: E6 C0                             AND     0C0h
E57F: C8                                RET     Z
E580: 3E FF                             LD      A,0FFh
E582: C9                                RET

E583: DD                         LE583: DB      0DDh
E584: 7E                                DB      7Eh     ; '~'
E585: 11                                DB      11h
E586: 32                                DB      32h     ; '2'
E587: 92                                DB      92h
E588: E6                                DB      0E6h
E589: DD                                DB      0DDh
E58A: 7E                                DB      7Eh     ; '~'
E58B: 1D                                DB      1Dh
E58C: E6                                DB      0E6h
E58D: 40                                DB      40h     ; '@'
E58E: F6                                DB      0F6h
E58F: 0A                                DB      0Ah
E590: 32                                DB      32h     ; '2'
E591: 91                                DB      91h
E592: E6                                DB      0E6h
E593: 21                                DB      21h     ; '!'
E594: 90                                DB      90h
E595: E6                                DB      0E6h
E596: CD                                DB      0CDh
E597: D6                                DB      0D6h
E598: E5                                DB      0E5h
E599: DD                                DB      0DDh
E59A: E5                                DB      0E5h
E59B: E1                                DB      0E1h
E59C: 11                                DB      11h
E59D: 1C                                DB      1Ch
E59E: 00                                DB      00h
E59F: 19                                DB      19h
E5A0: CD                                DB      0CDh
E5A1: E5                                DB      0E5h
E5A2: E5                                DB      0E5h
E5A3: 3E                                DB      3Eh     ; '>'
E5A4: C0                                DB      0C0h
E5A5: DD                                DB      0DDh
E5A6: A6                                DB      0A6h
E5A7: 2B                                DB      2Bh     ; '+'
E5A8: C8                                DB      0C8h
E5A9: 3E                                DB      3Eh     ; '>'
E5AA: FF                                DB      0FFh
E5AB: C9                                DB      0C9h

                                        ; --- START PROC LE5AC ---
E5AC: DD 7E 11                   LE5AC: LD      A,(IX+11h)
E5AF: E6 03                             AND     03h
E5B1: 32 95 E6                          LD      (LE695),A
E5B4: 21 93 E6                          LD      HL,0E693h
E5B7: CD D6 E5                          CALL    LE5D6
E5BA: CD 27 E6                          CALL    LE627
E5BD: 78                                LD      A,B
E5BE: E6 10                             AND     10h
E5C0: 28 09                             JR      Z,LE5CB
E5C2: 21 93 E6                          LD      HL,0E693h
E5C5: CD D6 E5                          CALL    LE5D6
E5C8: CD 27 E6                          CALL    LE627
E5CB: DD 36 2E 00                LE5CB: LD      (IX+2Eh),00h
E5CF: 78                                LD      A,B
E5D0: E6 D0                             AND     0D0h
E5D2: C8                                RET     Z
E5D3: 3E FF                             LD      A,0FFh
E5D5: C9                                RET

                                        ; --- START PROC LE5D6 ---
E5D6: CD F1 E5                   LE5D6: CALL    LE5F1
E5D9: 46                                LD      B,(HL)
E5DA: 0E 21                             LD      C,21h   ; '!'
E5DC: 23                                INC     HL
E5DD: CD 20 E6                   LE5DD: CALL    LE620
E5E0: ED A3                             OUTI
E5E2: 20 F9                             JR      NZ,LE5DD
E5E4: C9                                RET

                                        ; --- START PROC LE5E5 ---
E5E5: 46                         LE5E5: LD      B,(HL)
E5E6: 0E 21                             LD      C,21h   ; '!'
E5E8: 23                                INC     HL
E5E9: CD 20 E6                   LE5E9: CALL    LE620
E5EC: ED A2                             INI
E5EE: 20 F9                             JR      NZ,LE5E9
E5F0: C9                                RET

                                        ; --- START PROC LE5F1 ---
E5F1: CD 03 E6                   LE5F1: CALL    LE603
E5F4: 3E 80                             LD      A,80h
E5F6: B8                                CP      B
E5F7: C8                                RET     Z
E5F8: 18 F7                             JR      LE5F1

                                        ; --- START PROC LE5FA ---
E5FA: DB 20                      LE5FA: IN      A,(20h) ; ' '
E5FC: FE 80                             CP      80h
E5FE: C8                                RET     Z
E5FF: DB 21                             IN      A,(21h) ; '!'
E601: 18 F7                             JR      LE5FA

                                        ; --- START PROC LE603 ---
E603: CD 20 E6                   LE603: CALL    LE620
E606: 3E 08                             LD      A,08h
E608: D3 21                             OUT     (21h),A ; '!'
E60A: CD 20 E6                          CALL    LE620
E60D: DB 21                             IN      A,(21h) ; '!'
E60F: 47                                LD      B,A
E610: FE 80                             CP      80h
E612: C8                                RET     Z
E613: CD 20 E6                          CALL    LE620
E616: 78                                LD      A,B
E617: E6 E0                             AND     0E0h
E619: FE C0                             CP      0C0h
E61B: DB 21                             IN      A,(21h) ; '!'
E61D: C0                                RET     NZ
E61E: 18 E3                             JR      LE603

                                        ; --- START PROC LE620 ---
E620: DB 20                      LE620: IN      A,(20h) ; ' '
E622: E6 80                             AND     80h
E624: C0                                RET     NZ
E625: 18 F9                             JR      LE620

                                        ; --- START PROC LE627 ---
E627: CD 03 E6                   LE627: CALL    LE603
E62A: 78                                LD      A,B
E62B: E6 20                             AND     20h     ; ' '
E62D: 28 F8                             JR      Z,LE627
E62F: C9                                RET

                                        ; --- START PROC LE630 ---
E630: FD 7E 05                   LE630: LD      A,(IY+05h)
E633: 17                                RLA
E634: 17                                RLA
E635: E6 04                             AND     04h
E637: F6 01                             OR      01h
E639: 32 A7 E6                          LD      (LE6A7),A
E63C: FD 6E 0B                          LD      L,(IY+0Bh)
E63F: FD 66 0C                          LD      H,(IY+0Ch)
E642: 22 9F E6                          LD      (LE69F),HL
E645: FD 6E 09                          LD      L,(IY+09h)
E648: FD 66 0A                          LD      H,(IY+0Ah)
E64B: 22 9D E6                          LD      (LE69D),HL
E64E: 21 96 E6                          LD      HL,0E696h
E651: CD 91 EA                          CALL    LEA91
E654: C9                                RET

E655: 24                         LE655: DB      24h     ; '$'
E656: E8                                DB      0E8h
E657: 24                                DB      24h     ; '$'
E658: E8                                DB      0E8h
E659: 00                                DB      00h
E65A: 00                                DB      00h
E65B: 24                                DB      24h     ; '$'
E65C: E8                                DB      0E8h
E65D: 24                                DB      24h     ; '$'
E65E: E8                                DB      0E8h
E65F: 24                                DB      24h     ; '$'
E660: E8                                DB      0E8h
E661: 00                                DB      00h
E662: 00                                DB      00h
E663: 24                                DB      24h     ; '$'
E664: E8                                DB      0E8h
E665: 24                                DB      24h     ; '$'
E666: E8                                DB      0E8h
E667: 24                                DB      24h     ; '$'
E668: E8                                DB      0E8h
E669: 24                                DB      24h     ; '$'
E66A: E8                                DB      0E8h
E66B: 24                                DB      24h     ; '$'
E66C: E8                                DB      0E8h
E66D: 24                                DB      24h     ; '$'
E66E: E8                                DB      0E8h
E66F: 24                                DB      24h     ; '$'
E670: E8                                DB      0E8h
E671: 24                                DB      24h     ; '$'
E672: E8                                DB      0E8h
E673: 24                                DB      24h     ; '$'
E674: E8                                DB      0E8h
E675: 24                                DB      24h     ; '$'
E676: E8                                DB      0E8h
E677: 24                                DB      24h     ; '$'
E678: E8                                DB      0E8h
E679: 27                                DB      27h     ; '''
E67A: E8                                DB      0E8h
E67B: 24                                DB      24h     ; '$'
E67C: E8                                DB      0E8h
E67D: 3F                                DB      3Fh     ; '?'
E67E: E8                                DB      0E8h
E67F: 24                                DB      24h     ; '$'
E680: E8                                DB      0E8h
E681: 24                                DB      24h     ; '$'
E682: E8                                DB      0E8h
E683: 24                                DB      24h     ; '$'
E684: E8                                DB      0E8h
E685: 24                                DB      24h     ; '$'
E686: E8                                DB      0E8h
E687: 00                                DB      00h
E688: 00                                DB      00h
E689: 00                         LE689: DB      00h
E68A: 00                         LE68A: DB      00h
E68B: 00                         LE68B: DB      00h
E68C: 03                                DB      03h
E68D: 0F                                DB      0Fh
E68E: 00                         LE68E: DB      00h
E68F: 00                         LE68F: DB      00h
E690: 02                                DB      02h
E691: 00                                DB      00h
E692: 00                                DB      00h
E693: 02                                DB      02h
E694: 07                                DB      07h
E695: 00                         LE695: DB      00h
E696: 01                                DB      01h
E697: 33                                DB      33h     ; '3'
E698: 00                                DB      00h
E699: 10                                DB      10h
E69A: 32                                DB      32h     ; '2'
E69B: C3                                DB      0C3h
E69C: 79                                DB      79h     ; 'y'
E69D: 00                         LE69D: DB      00h
E69E: 00                                DB      00h
E69F: 00                         LE69F: DB      00h
E6A0: 00                                DB      00h
E6A1: 14                                DB      14h
E6A2: 28                                DB      28h     ; '('
E6A3: C5                                DB      0C5h
E6A4: 22                                DB      22h     ; '"'
E6A5: 8A                                DB      8Ah
E6A6: CF                                DB      0CFh
E6A7: 00                         LE6A7: DB      00h
E6A8: CF                                DB      0CFh
E6A9: AB                                DB      0ABh
E6AA: 87                                DB      87h
E6AB: 00                                DB      00h
E6AC: 16                                DB      16h
E6AD: 32                                DB      32h     ; '2'
E6AE: C3                                DB      0C3h
E6AF: C3                                DB      0C3h
E6B0: C3                                DB      0C3h
E6B1: C3                                DB      0C3h
E6B2: C3                                DB      0C3h
E6B3: C3                                DB      0C3h
E6B4: 79                                DB      79h     ; 'y'
E6B5: 00                                DB      00h
E6B6: 40                                DB      40h     ; '@'
E6B7: FF                                DB      0FFh
E6B8: 03                                DB      03h
E6B9: 14                                DB      14h
E6BA: 28                                DB      28h     ; '('
E6BB: D5                                DB      0D5h
E6BC: 22                                DB      22h     ; '"'
E6BD: 32                                DB      32h     ; '2'
E6BE: 20                                DB      20h     ; ' '
E6BF: 8A                                DB      8Ah
E6C0: CF                                DB      0CFh
E6C1: 05                                DB      05h
E6C2: CF                                DB      0CFh
E6C3: A3                                DB      0A3h
E6C4: 00                                DB      00h
E6C5: 03                                DB      03h
E6C6: 28                                DB      28h     ; '('
E6C7: 28                                DB      28h     ; '('
E6C8: A7                                DB      0A7h
E6C9: 7D                                DB      7Dh     ; '}'
E6CA: 01                                DB      01h
E6CB: 29                                DB      29h     ; ')'
E6CC: 43                                DB      43h     ; 'C'
E6CD: 01                                DB      01h
E6CE: 2A                                DB      2Ah     ; '*'
E6CF: 43                                DB      43h     ; 'C'
E6D0: 01                                DB      01h
E6D1: 2B                                DB      2Bh     ; '+'
E6D2: 43                                DB      43h     ; 'C'
E6D3: 00                                DB      00h
E6D4: 00                         LE6D4: DB      00h
E6D5: F3                                DB      0F3h
E6D6: 3A                                DB      3Ah     ; ':'
E6D7: A4                                DB      0A4h
E6D8: E2                                DB      0E2h
E6D9: 32                                DB      32h     ; '2'
E6DA: D4                                DB      0D4h
E6DB: E6                                DB      0E6h
E6DC: 3A                                DB      3Ah     ; ':'
E6DD: A5                                DB      0A5h
E6DE: E2                                DB      0E2h
E6DF: D3                                DB      0D3h
E6E0: 12                                DB      12h
E6E1: D3                                DB      0D3h
E6E2: 13                                DB      13h
E6E3: 32                                DB      32h     ; '2'
E6E4: A4                                DB      0A4h
E6E5: E2                                DB      0E2h
E6E6: FB                                DB      0FBh
E6E7: CD                                DB      0CDh
E6E8: 45                                DB      45h     ; 'E'
E6E9: E7                                DB      0E7h
E6EA: CD                                DB      0CDh
E6EB: 5F                                DB      5Fh     ; '_'
E6EC: E7                                DB      0E7h
E6ED: 21                                DB      21h     ; '!'
E6EE: 0A                                DB      0Ah
E6EF: E8                                DB      0E8h
E6F0: 36                                DB      36h     ; '6'
E6F1: 0A                                DB      0Ah
E6F2: CD                                DB      0CDh
E6F3: 7A                                DB      7Ah     ; 'z'
E6F4: E7                                DB      0E7h
E6F5: FD                                DB      0FDh
E6F6: 6E                                DB      6Eh     ; 'n'
E6F7: 09                                DB      09h
E6F8: FD                                DB      0FDh
E6F9: 66                                DB      66h     ; 'f'
E6FA: 0A                                DB      0Ah
E6FB: CD                                DB      0CDh
E6FC: 90                                DB      90h
E6FD: E7                                DB      0E7h
E6FE: F3                                DB      0F3h
E6FF: 47                                DB      47h     ; 'G'
E700: 3A                                DB      3Ah     ; ':'
E701: D4                                DB      0D4h
E702: E6                                DB      0E6h
E703: D3                                DB      0D3h
E704: 12                                DB      12h
E705: D3                                DB      0D3h
E706: 13                                DB      13h
E707: 32                                DB      32h     ; '2'
E708: A4                                DB      0A4h
E709: E2                                DB      0E2h
E70A: FB                                DB      0FBh
E70B: 78                                DB      78h     ; 'x'
E70C: C9                                DB      0C9h

                                        ; --- START PROC LE70D ---
E70D: F3                         LE70D: DI
E70E: 3A A4 E2                          LD      A,(LE2A4)
E711: 32 D4 E6                          LD      (LE6D4),A
E714: 3A A5 E2                          LD      A,(LE2A5)
E717: D3 12                             OUT     (12h),A
E719: D3 13                             OUT     (13h),A
E71B: 32 A4 E2                          LD      (LE2A4),A
E71E: FB                                EI
E71F: CD 45 E7                          CALL    LE745
E722: CD 5F E7                          CALL    LE75F
E725: 21 0A E8                          LD      HL,0E80Ah
E728: 36 08                             LD      (HL),08h
E72A: CD 7A E7                          CALL    LE77A
E72D: FD 6E 09                          LD      L,(IY+09h)
E730: FD 66 0A                          LD      H,(IY+0Ah)
E733: CD C7 E7                          CALL    LE7C7
E736: F3                                DI
E737: 47                                LD      B,A
E738: 3A D4 E6                          LD      A,(LE6D4)
E73B: D3 12                             OUT     (12h),A
E73D: D3 13                             OUT     (13h),A
E73F: 32 A4 E2                          LD      (LE2A4),A
E742: FB                                EI
E743: 78                                LD      A,B
E744: C9                                RET

                                        ; --- START PROC LE745 ---
E745: FD 7E 08                   LE745: LD      A,(IY+08h)
E748: 3D                                DEC     A
E749: 07                                RLCA
E74A: 07                                RLCA
E74B: E6 7C                             AND     7Ch     ; '|'
E74D: 67                                LD      H,A
E74E: FD 7E 06                          LD      A,(IY+06h)
E751: 0F                                RRCA
E752: 6F                                LD      L,A
E753: E6 80                             AND     80h
E755: B4                                OR      H
E756: 67                                LD      H,A
E757: 7D                                LD      A,L
E758: E6 7F                             AND     7Fh     ; ''
E75A: 6F                                LD      L,A
E75B: 22 0C E8                          LD      (LE80C),HL
E75E: C9                                RET

                                        ; --- START PROC LE75F ---
E75F: DB 3D                      LE75F: IN      A,(3Dh) ; '='
E761: E6 10                             AND     10h
E763: C2 5F E7                          JP      NZ,LE75F
E766: 3E 01                             LD      A,01h
E768: D3 3C                             OUT     (3Ch),A ; '<'
E76A: 3E 70                             LD      A,70h   ; 'p'
E76C: D3 3D                             OUT     (3Dh),A ; '='
E76E: DB 3D                      LE76E: IN      A,(3Dh) ; '='
E770: E6 10                             AND     10h
E772: CA 6E E7                          JP      Z,LE76E
E775: 3E E0                             LD      A,0E0h
E777: D3 3D                             OUT     (3Dh),A ; '='
E779: C9                                RET

                                        ; --- START PROC LE77A ---
E77A: 22 08 E8                   LE77A: LD      (LE808),HL
E77D: DB 3D                      LE77D: IN      A,(3Dh) ; '='
E77F: 4F                                LD      C,A
E780: EE BF                             XOR     0BFh
E782: CB 47                             BIT     0,A
E784: 20 F7                             JR      NZ,LE77D
E786: E6 0A                             AND     0Ah
E788: C0                                RET     NZ
E789: 7E                                LD      A,(HL)
E78A: D3 3C                             OUT     (3Ch),A ; '<'
E78C: 23                                INC     HL
E78D: C3 7D E7                          JP      LE77D

                                        ; --- START PROC LE790 ---
E790: 22 06 E8                   LE790: LD      (LE806),HL
E793: DB 3D                      LE793: IN      A,(3Dh) ; '='
E795: 4F                                LD      C,A
E796: E6 01                             AND     01h
E798: CA 93 E7                          JP      Z,LE793
E79B: 79                                LD      A,C
E79C: E6 08                             AND     08h
E79E: C2 A8 E7                          JP      NZ,LE7A8
E7A1: 7E                                LD      A,(HL)
E7A2: D3 3C                             OUT     (3Ch),A ; '<'
E7A4: 23                                INC     HL
E7A5: C3 93 E7                          JP      LE793

                                        ; --- START PROC LE7A8 ---
E7A8: DB 3C                      LE7A8: IN      A,(3Ch) ; '<'
E7AA: 4F                                LD      C,A
E7AB: DB 3D                      LE7AB: IN      A,(3Dh) ; '='
E7AD: 47                                LD      B,A
E7AE: E6 01                             AND     01h
E7B0: CA AB E7                          JP      Z,LE7AB
E7B3: DB 3C                             IN      A,(3Ch) ; '<'
E7B5: B7                                OR      A
E7B6: C2 C1 E7                          JP      NZ,LE7C1
E7B9: 79                                LD      A,C
E7BA: E6 1F                             AND     1Fh
E7BC: C2 C4 E7                          JP      NZ,LE7C4
E7BF: AF                                XOR     A
E7C0: C9                                RET

E7C1: 3E 01                      LE7C1: LD      A,01h
E7C3: C9                                RET

E7C4: 3E 02                      LE7C4: LD      A,02h
E7C6: C9                                RET

                                        ; --- START PROC LE7C7 ---
E7C7: 22 06 E8                   LE7C7: LD      (LE806),HL
E7CA: DB 3D                      LE7CA: IN      A,(3Dh) ; '='
E7CC: 4F                                LD      C,A
E7CD: E6 01                             AND     01h
E7CF: CA CA E7                          JP      Z,LE7CA
E7D2: 79                                LD      A,C
E7D3: E6 08                             AND     08h
E7D5: C2 A8 E7                          JP      NZ,LE7A8
E7D8: DB 3C                             IN      A,(3Ch) ; '<'
E7DA: 77                                LD      (HL),A
E7DB: 23                                INC     HL
E7DC: C3 CA E7                          JP      LE7CA

E7DF: 22                         LE7DF: DB      22h     ; '"'
E7E0: 06                                DB      06h
E7E1: E8                                DB      0E8h
E7E2: DB                                DB      0DBh
E7E3: 3D                                DB      3Dh     ; '='
E7E4: 4F                                DB      4Fh     ; 'O'
E7E5: E6                                DB      0E6h
E7E6: 01                                DB      01h
E7E7: CA                                DB      0CAh
E7E8: E2                                DB      0E2h
E7E9: E7                                DB      0E7h
E7EA: 79                                DB      79h     ; 'y'
E7EB: E6                                DB      0E6h
E7EC: 08                                DB      08h
E7ED: C2                                DB      0C2h
E7EE: A8                                DB      0A8h
E7EF: E7                                DB      0E7h
E7F0: C3                                DB      0C3h
E7F1: E2                                DB      0E2h
E7F2: E7                                DB      0E7h
E7F3: DB                                DB      0DBh
E7F4: 3D                                DB      3Dh     ; '='
E7F5: 4F                                DB      4Fh     ; 'O'
E7F6: E6                                DB      0E6h
E7F7: 01                                DB      01h
E7F8: CA                                DB      0CAh
E7F9: F3                                DB      0F3h
E7FA: E7                                DB      0E7h
E7FB: 79                                DB      79h     ; 'y'
E7FC: E6                                DB      0E6h
E7FD: 08                                DB      08h
E7FE: C2                                DB      0C2h
E7FF: A8                                DB      0A8h
E800: E7                                DB      0E7h
E801: DB                                DB      0DBh
E802: 3C                                DB      3Ch     ; '<'
E803: C3                                DB      0C3h
E804: F3                                DB      0F3h
E805: E7                                DB      0E7h
E806: 00                         LE806: DB      00h
E807: 00                                DB      00h
E808: 00                         LE808: DB      00h
E809: 00                                DB      00h
E80A: 00                                DB      00h
E80B: 00                                DB      00h
E80C: 00                         LE80C: DB      00h
E80D: 00                                DB      00h
E80E: 04                                DB      04h
E80F: 00                                DB      00h
E810: 0C                                DB      0Ch
E811: 00                                DB      00h
E812: 00                                DB      00h
E813: 00                                DB      00h
E814: 00                                DB      00h
E815: 00                                DB      00h
E816: 01                                DB      01h
E817: 32                                DB      32h     ; '2'
E818: 02                                DB      02h
E819: 01                                DB      01h
E81A: 32                                DB      32h     ; '2'
E81B: 01                                DB      01h
E81C: 32                                DB      32h     ; '2'
E81D: 0B                                DB      0Bh
E81E: 00                         LE81E: DB      00h
E81F: 00                         LE81F: DB      00h
E820: 00                                DB      00h
E821: 7D                                DB      7Dh     ; '}'
E822: 00                                DB      00h
E823: 00                                DB      00h
E824: FB                                DB      0FBh
E825: ED                                DB      0EDh
E826: 4D                                DB      4Dh     ; 'M'
E827: F5                                DB      0F5h
E828: 3A                                DB      3Ah     ; ':'
E829: 1E                                DB      1Eh
E82A: E8                                DB      0E8h
E82B: F6                                DB      0F6h
E82C: 01                                DB      01h
E82D: D3                                DB      0D3h
E82E: 23                                DB      23h     ; '#'
E82F: E6                                DB      0E6h
E830: FE                                DB      0FEh
E831: D3                                DB      0D3h
E832: 23                                DB      23h     ; '#'
E833: 3E                                DB      3Eh     ; '>'
E834: C3                                DB      0C3h
E835: D3                                DB      0D3h
E836: 32                                DB      32h     ; '2'
E837: AF                                DB      0AFh
E838: 32                                DB      32h     ; '2'
E839: 23                                DB      23h     ; '#'
E83A: E8                                DB      0E8h
E83B: F1                                DB      0F1h
E83C: FB                                DB      0FBh
E83D: ED                                DB      0EDh
E83E: 4D                                DB      4Dh     ; 'M'
E83F: ED                                DB      0EDh
E840: 73                                DB      73h     ; 's'
E841: FD                                DB      0FDh
E842: FB                                DB      0FBh
E843: 31                                DB      31h     ; '1'
E844: FD                                DB      0FDh
E845: FB                                DB      0FBh
E846: F5                                DB      0F5h
E847: E5                                DB      0E5h
E848: 2A                                DB      2Ah     ; '*'
E849: 21                                DB      21h     ; '!'
E84A: E8                                DB      0E8h
E84B: 2B                                DB      2Bh     ; '+'
E84C: 7C                                DB      7Ch     ; '|'
E84D: B5                                DB      0B5h
E84E: 22                                DB      22h     ; '"'
E84F: 21                                DB      21h     ; '!'
E850: E8                                DB      0E8h
E851: C2                                DB      0C2h
E852: B2                                DB      0B2h
E853: E8                                DB      0E8h
E854: 21                                DB      21h     ; '!'
E855: 7D                                DB      7Dh     ; '}'
E856: 00                                DB      00h
E857: 22                                DB      22h     ; '"'
E858: 21                                DB      21h     ; '!'
E859: E8                                DB      0E8h
E85A: 2A                                DB      2Ah     ; '*'
E85B: 1F                                DB      1Fh
E85C: E8                                DB      0E8h
E85D: 7C                                DB      7Ch     ; '|'
E85E: B5                                DB      0B5h
E85F: CA                                DB      0CAh
E860: 75                                DB      75h     ; 'u'
E861: E8                                DB      0E8h
E862: 2B                                DB      2Bh     ; '+'
E863: 7C                                DB      7Ch     ; '|'
E864: B5                                DB      0B5h
E865: C2                                DB      0C2h
E866: 72                                DB      72h     ; 'r'
E867: E8                                DB      0E8h
E868: 3A                                DB      3Ah     ; ':'
E869: 1E                                DB      1Eh
E86A: E8                                DB      0E8h
E86B: F6                                DB      0F6h
E86C: 80                                DB      80h
E86D: 32                                DB      32h     ; '2'
E86E: 1E                                DB      1Eh
E86F: E8                                DB      0E8h
E870: D3                                DB      0D3h
E871: 23                                DB      23h     ; '#'
E872: 22                                DB      22h     ; '"'
E873: 1F                                DB      1Fh
E874: E8                                DB      0E8h
E875: 3A                                DB      3Ah     ; ':'
E876: 5C                                DB      5Ch     ; '\'
E877: FE                                DB      0FEh
E878: C6                                DB      0C6h
E879: 01                                DB      01h
E87A: 27                                DB      27h     ; '''
E87B: 32                                DB      32h     ; '2'
E87C: 5C                                DB      5Ch     ; '\'
E87D: FE                                DB      0FEh
E87E: FE                                DB      0FEh
E87F: 60                                DB      60h     ; '`'
E880: C2                                DB      0C2h
E881: B2                                DB      0B2h
E882: E8                                DB      0E8h
E883: AF                                DB      0AFh
E884: 32                                DB      32h     ; '2'
E885: 5C                                DB      5Ch     ; '\'
E886: FE                                DB      0FEh
E887: 3A                                DB      3Ah     ; ':'
E888: 5B                                DB      5Bh     ; '['
E889: FE                                DB      0FEh
E88A: C6                                DB      0C6h
E88B: 01                                DB      01h
E88C: 27                                DB      27h     ; '''
E88D: 32                                DB      32h     ; '2'
E88E: 5B                                DB      5Bh     ; '['
E88F: FE                                DB      0FEh
E890: FE                                DB      0FEh
E891: 60                                DB      60h     ; '`'
E892: C2                                DB      0C2h
E893: B2                                DB      0B2h
E894: E8                                DB      0E8h
E895: AF                                DB      0AFh
E896: 32                                DB      32h     ; '2'
E897: 5B                                DB      5Bh     ; '['
E898: FE                                DB      0FEh
E899: 3A                                DB      3Ah     ; ':'
E89A: 5A                                DB      5Ah     ; 'Z'
E89B: FE                                DB      0FEh
E89C: C6                                DB      0C6h
E89D: 01                                DB      01h
E89E: 27                                DB      27h     ; '''
E89F: 32                                DB      32h     ; '2'
E8A0: 5A                                DB      5Ah     ; 'Z'
E8A1: FE                                DB      0FEh
E8A2: FE                                DB      0FEh
E8A3: 24                                DB      24h     ; '$'
E8A4: C2                                DB      0C2h
E8A5: B2                                DB      0B2h
E8A6: E8                                DB      0E8h
E8A7: AF                                DB      0AFh
E8A8: 32                                DB      32h     ; '2'
E8A9: 5A                                DB      5Ah     ; 'Z'
E8AA: FE                                DB      0FEh
E8AB: 2A                                DB      2Ah     ; '*'
E8AC: 58                                DB      58h     ; 'X'
E8AD: FE                                DB      0FEh
E8AE: 23                                DB      23h     ; '#'
E8AF: 22                                DB      22h     ; '"'
E8B0: 58                                DB      58h     ; 'X'
E8B1: FE                                DB      0FEh
E8B2: E1                                DB      0E1h
E8B3: F1                                DB      0F1h
E8B4: ED                                DB      0EDh
E8B5: 7B                                DB      7Bh     ; '{'
E8B6: FD                                DB      0FDh
E8B7: FB                                DB      0FBh
E8B8: FB                                DB      0FBh
E8B9: ED                                DB      0EDh
E8BA: 4D                                DB      4Dh     ; 'M'

                                        ; --- START PROC LE8BB ---
E8BB: 11 00 FF                   LE8BB: LD      DE,0FF00h
E8BE: 21 78 E9                          LD      HL,0E978h
E8C1: 01 B5 00                          LD      BC,00B5h
E8C4: ED B0                             LDIR
E8C6: 21 34 EA                          LD      HL,0EA34h
E8C9: CD 91 EA                          CALL    LEA91
E8CC: 3E 03                             LD      A,03h
E8CE: 32 56 EA                          LD      (LEA56),A
E8D1: 32 57 EA                          LD      (LEA57),A
E8D4: CD 72 E9                          CALL    LE972
E8D7: C9                                RET

E8D8: 2A                         LE8D8: DB      2Ah     ; '*'
E8D9: 53                                DB      53h     ; 'S'
E8DA: EA                                DB      0EAh
E8DB: E9                                DB      0E9h
E8DC: 79                                DB      79h     ; 'y'
E8DD: E6                                DB      0E6h
E8DE: 7F                                DB      7Fh
E8DF: 4F                                DB      4Fh     ; 'O'
E8E0: C5                                DB      0C5h
E8E1: 21                                DB      21h     ; '!'
E8E2: 2D                                DB      2Dh     ; '-'
E8E3: EA                                DB      0EAh
E8E4: CD                                DB      0CDh
E8E5: 56                                DB      56h     ; 'V'
E8E6: E9                                DB      0E9h
E8E7: C1                                DB      0C1h
E8E8: A7                                DB      0A7h
E8E9: CA                                DB      0CAh
E8EA: 6C                                DB      6Ch     ; 'l'
E8EB: E9                                DB      0E9h
E8EC: E9                                DB      0E9h
E8ED: 3A                                DB      3Ah     ; ':'
E8EE: 58                                DB      58h     ; 'X'
E8EF: EA                                DB      0EAh
E8F0: F5                                DB      0F5h
E8F1: FE                                DB      0FEh
E8F2: 17                                DB      17h
E8F3: CA                                DB      0CAh
E8F4: F9                                DB      0F9h
E8F5: E8                                DB      0E8h
E8F6: 3C                                DB      3Ch     ; '<'
E8F7: 18                                DB      18h
E8F8: 01                                DB      01h
E8F9: AF                                DB      0AFh
E8FA: 32                                DB      32h     ; '2'
E8FB: 58                                DB      58h     ; 'X'
E8FC: EA                                DB      0EAh
E8FD: D3                                DB      0D3h
E8FE: 4D                                DB      4Dh     ; 'M'
E8FF: F1                                DB      0F1h
E900: 21                                DB      21h     ; '!'
E901: 5C                                DB      5Ch     ; '\'
E902: EA                                DB      0EAh
E903: BE                                DB      0BEh
E904: C0                                DB      0C0h
E905: CD                                DB      0CDh
E906: 32                                DB      32h     ; '2'
E907: E9                                DB      0E9h
E908: C9                                DB      0C9h
E909: AF                                DB      0AFh
E90A: 32                                DB      32h     ; '2'
E90B: 5A                                DB      5Ah     ; 'Z'
E90C: EA                                DB      0EAh
E90D: D3                                DB      0D3h
E90E: 4C                                DB      4Ch     ; 'L'
E90F: C9                                DB      0C9h
E910: 3A                                DB      3Ah     ; ':'
E911: 5A                                DB      5Ah     ; 'Z'
E912: EA                                DB      0EAh
E913: FE                                DB      0FEh
E914: 00                                DB      00h
E915: C8                                DB      0C8h
E916: 3D                                DB      3Dh     ; '='
E917: 32                                DB      32h     ; '2'
E918: 5A                                DB      5Ah     ; 'Z'
E919: EA                                DB      0EAh
E91A: D3                                DB      0D3h
E91B: 4C                                DB      4Ch     ; 'L'
E91C: C9                                DB      0C9h
E91D: 2A                                DB      2Ah     ; '*'
E91E: 5A                                DB      5Ah     ; 'Z'
E91F: EA                                DB      0EAh
E920: E5                                DB      0E5h
E921: 21                                DB      21h     ; '!'
E922: 00                                DB      00h
E923: 00                                DB      00h
E924: 22                                DB      22h     ; '"'
E925: 5A                                DB      5Ah     ; 'Z'
E926: EA                                DB      0EAh
E927: 01                                DB      01h
E928: A0                                DB      0A0h
E929: 00                                DB      00h
E92A: CD                                DB      0CDh
E92B: 6F                                DB      6Fh     ; 'o'
E92C: E9                                DB      0E9h
E92D: E1                                DB      0E1h
E92E: 22                                DB      22h     ; '"'
E92F: 5A                                DB      5Ah     ; 'Z'
E930: EA                                DB      0EAh
E931: C9                                DB      0C9h
E932: 3A                                DB      3Ah     ; ':'
E933: 5C                                DB      5Ch     ; '\'
E934: EA                                DB      0EAh
E935: FE                                DB      0FEh
E936: 17                                DB      17h
E937: CA                                DB      0CAh
E938: 3D                                DB      3Dh     ; '='
E939: E9                                DB      0E9h
E93A: 3C                                DB      3Ch     ; '<'
E93B: 18                                DB      18h
E93C: 02                                DB      02h
E93D: 3E                                DB      3Eh     ; '>'
E93E: 00                                DB      00h
E93F: 32                                DB      32h     ; '2'
E940: 5C                                DB      5Ch     ; '\'
E941: EA                                DB      0EAh
E942: 2A                                DB      2Ah     ; '*'
E943: 58                                DB      58h     ; 'X'
E944: EA                                DB      0EAh
E945: E5                                DB      0E5h
E946: 2A                                DB      2Ah     ; '*'
E947: 5C                                DB      5Ch     ; '\'
E948: EA                                DB      0EAh
E949: 22                                DB      22h     ; '"'
E94A: 58                                DB      58h     ; 'X'
E94B: EA                                DB      0EAh
E94C: CD                                DB      0CDh
E94D: 1D                                DB      1Dh
E94E: E9                                DB      0E9h
E94F: E1                                DB      0E1h
E950: 22                                DB      22h     ; '"'
E951: 58                                DB      58h     ; 'X'
E952: EA                                DB      0EAh
E953: D3                                DB      0D3h
E954: 4B                                DB      4Bh     ; 'K'
E955: C9                                DB      0C9h
E956: 06                                DB      06h
E957: 00                                DB      00h
E958: 4E                                DB      4Eh     ; 'N'
E959: 23                                DB      23h     ; '#'
E95A: ED                                DB      0EDh
E95B: A1                                DB      0A1h
E95C: CA                                DB      0CAh
E95D: 65                                DB      65h     ; 'e'
E95E: E9                                DB      0E9h
E95F: E2                                DB      0E2h
E960: 6A                                DB      6Ah     ; 'j'
E961: E9                                DB      0E9h
E962: 23                                DB      23h     ; '#'
E963: 18                                DB      18h
E964: F4                                DB      0F4h
E965: 5E                                DB      5Eh     ; '^'
E966: 23                                DB      23h     ; '#'
E967: 56                                DB      56h     ; 'V'
E968: EB                                DB      0EBh
E969: C9                                DB      0C9h
E96A: AF                                DB      0AFh
E96B: C9                                DB      0C9h
E96C: C3                                DB      0C3h
E96D: 00                                DB      00h
E96E: FF                                DB      0FFh
E96F: C3                                DB      0C3h
E970: 31                                DB      31h     ; '1'
E971: FF                                DB      0FFh

                                        ; --- START PROC LE972 ---
E972: C3 5D FF                   LE972: JP      0FF5Dh

E975: C3                         LE975: DB      0C3h
E976: 9C                                DB      9Ch
E977: FF                                DB      0FFh
E978: 79                                DB      79h     ; 'y'
E979: FE                                DB      0FEh
E97A: 20                                DB      20h     ; ' '
E97B: D8                                DB      0D8h
E97C: FE                                DB      0FEh
E97D: 7F                                DB      7Fh
E97E: C8                                DB      0C8h
E97F: C5                                DB      0C5h
E980: CD                                DB      0CDh
E981: 75                                DB      75h     ; 'u'
E982: E9                                DB      0E9h
E983: C1                                DB      0C1h
E984: 3A                                DB      3Ah     ; ':'
E985: 57                                DB      57h     ; 'W'
E986: EA                                DB      0EAh
E987: 47                                DB      47h     ; 'G'
E988: 3A                                DB      3Ah     ; ':'
E989: A4                                DB      0A4h
E98A: E2                                DB      0E2h
E98B: F5                                DB      0F5h
E98C: 3E                                DB      3Eh     ; '>'
E98D: FF                                DB      0FFh
E98E: F3                                DB      0F3h
E98F: D3                                DB      0D3h
E990: 13                                DB      13h
E991: D3                                DB      0D3h
E992: 12                                DB      12h
E993: 71                                DB      71h     ; 'q'
E994: 23                                DB      23h     ; '#'
E995: 70                                DB      70h     ; 'p'
E996: F1                                DB      0F1h
E997: D3                                DB      0D3h
E998: 13                                DB      13h
E999: D3                                DB      0D3h
E99A: 12                                DB      12h
E99B: FB                                DB      0FBh
E99C: 3A                                DB      3Ah     ; ':'
E99D: 5A                                DB      5Ah     ; 'Z'
E99E: EA                                DB      0EAh
E99F: FE                                DB      0FEh
E9A0: 4F                                DB      4Fh     ; 'O'
E9A1: C8                                DB      0C8h
E9A2: 3C                                DB      3Ch     ; '<'
E9A3: 32                                DB      32h     ; '2'
E9A4: 5A                                DB      5Ah     ; 'Z'
E9A5: EA                                DB      0EAh
E9A6: D3                                DB      0D3h
E9A7: 4C                                DB      4Ch     ; 'L'
E9A8: C9                                DB      0C9h
E9A9: C5                                DB      0C5h
E9AA: CD                                DB      0CDh
E9AB: 75                                DB      75h     ; 'u'
E9AC: E9                                DB      0E9h
E9AD: C1                                DB      0C1h
E9AE: 3A                                DB      3Ah     ; ':'
E9AF: A4                                DB      0A4h
E9B0: E2                                DB      0E2h
E9B1: F5                                DB      0F5h
E9B2: 3A                                DB      3Ah     ; ':'
E9B3: 56                                DB      56h     ; 'V'
E9B4: EA                                DB      0EAh
E9B5: F5                                DB      0F5h
E9B6: 3E                                DB      3Eh     ; '>'
E9B7: FF                                DB      0FFh
E9B8: F3                                DB      0F3h
E9B9: D3                                DB      0D3h
E9BA: 13                                DB      13h
E9BB: D3                                DB      0D3h
E9BC: 12                                DB      12h
E9BD: F1                                DB      0F1h
E9BE: 36                                DB      36h     ; '6'
E9BF: 20                                DB      20h     ; ' '
E9C0: E5                                DB      0E5h
E9C1: 23                                DB      23h     ; '#'
E9C2: 77                                DB      77h     ; 'w'
E9C3: 23                                DB      23h     ; '#'
E9C4: EB                                DB      0EBh
E9C5: E1                                DB      0E1h
E9C6: 0B                                DB      0Bh
E9C7: 0B                                DB      0Bh
E9C8: 78                                DB      78h     ; 'x'
E9C9: B1                                DB      0B1h
E9CA: 28                                DB      28h     ; '('
E9CB: 02                                DB      02h
E9CC: ED                                DB      0EDh
E9CD: B0                                DB      0B0h
E9CE: F1                                DB      0F1h
E9CF: D3                                DB      0D3h
E9D0: 13                                DB      13h
E9D1: D3                                DB      0D3h
E9D2: 12                                DB      12h
E9D3: FB                                DB      0FBh
E9D4: C9                                DB      0C9h
E9D5: 21                                DB      21h     ; '!'
E9D6: 00                                DB      00h
E9D7: 20                                DB      20h     ; ' '
E9D8: 3A                                DB      3Ah     ; ':'
E9D9: A4                                DB      0A4h
E9DA: E2                                DB      0E2h
E9DB: F5                                DB      0F5h
E9DC: 3A                                DB      3Ah     ; ':'
E9DD: 56                                DB      56h     ; 'V'
E9DE: EA                                DB      0EAh
E9DF: F5                                DB      0F5h
E9E0: 3E                                DB      3Eh     ; '>'
E9E1: FF                                DB      0FFh
E9E2: F3                                DB      0F3h
E9E3: D3                                DB      0D3h
E9E4: 13                                DB      13h
E9E5: D3                                DB      0D3h
E9E6: 12                                DB      12h
E9E7: F1                                DB      0F1h
E9E8: 36                                DB      36h     ; '6'
E9E9: 20                                DB      20h     ; ' '
E9EA: E5                                DB      0E5h
E9EB: 23                                DB      23h     ; '#'
E9EC: 77                                DB      77h     ; 'w'
E9ED: 23                                DB      23h     ; '#'
E9EE: EB                                DB      0EBh
E9EF: E1                                DB      0E1h
E9F0: 01                                DB      01h
E9F1: 00                                DB      00h
E9F2: 0F                                DB      0Fh
E9F3: ED                                DB      0EDh
E9F4: B0                                DB      0B0h
E9F5: F1                                DB      0F1h
E9F6: D3                                DB      0D3h
E9F7: 13                                DB      13h
E9F8: D3                                DB      0D3h
E9F9: 12                                DB      12h
E9FA: FB                                DB      0FBh
E9FB: 21                                DB      21h     ; '!'
E9FC: 00                                DB      00h
E9FD: 00                                DB      00h
E9FE: 22                                DB      22h     ; '"'
E9FF: 58                                DB      58h     ; 'X'
EA00: EA                                DB      0EAh
EA01: 22                                DB      22h     ; '"'
EA02: 5A                                DB      5Ah     ; 'Z'
EA03: EA                                DB      0EAh
EA04: AF                                DB      0AFh
EA05: D3                                DB      0D3h
EA06: 4D                                DB      4Dh     ; 'M'
EA07: D3                                DB      0D3h
EA08: 4C                                DB      4Ch     ; 'L'
EA09: 3E                                DB      3Eh     ; '>'
EA0A: 17                                DB      17h
EA0B: D3                                DB      0D3h
EA0C: 46                                DB      46h     ; 'F'
EA0D: 21                                DB      21h     ; '!'
EA0E: 17                                DB      17h
EA0F: 00                                DB      00h
EA10: 22                                DB      22h     ; '"'
EA11: 5C                                DB      5Ch     ; '\'
EA12: EA                                DB      0EAh
EA13: C9                                DB      0C9h
EA14: 2A                                DB      2Ah     ; '*'
EA15: 58                                DB      58h     ; 'X'
EA16: EA                                DB      0EAh
EA17: 29                                DB      29h     ; ')'
EA18: 29                                DB      29h     ; ')'
EA19: 29                                DB      29h     ; ')'
EA1A: 29                                DB      29h     ; ')'
EA1B: E5                                DB      0E5h
EA1C: 29                                DB      29h     ; ')'
EA1D: 29                                DB      29h     ; ')'
EA1E: D1                                DB      0D1h
EA1F: 19                                DB      19h
EA20: 29                                DB      29h     ; ')'
EA21: E5                                DB      0E5h
EA22: 2A                                DB      2Ah     ; '*'
EA23: 5A                                DB      5Ah     ; 'Z'
EA24: EA                                DB      0EAh
EA25: 29                                DB      29h     ; ')'
EA26: D1                                DB      0D1h
EA27: 19                                DB      19h
EA28: 11                                DB      11h
EA29: 00                                DB      00h
EA2A: 20                                DB      20h     ; ' '
EA2B: 19                                DB      19h
EA2C: C9                                DB      0C9h
EA2D: 02                                DB      02h
EA2E: 0A                                DB      0Ah
EA2F: ED                                DB      0EDh
EA30: E8                                DB      0E8h
EA31: 0D                                DB      0Dh
EA32: 09                                DB      09h
EA33: E9                                DB      0E9h
EA34: 01                                DB      01h
EA35: 4E                                DB      4Eh     ; 'N'
EA36: 00                                DB      00h
EA37: 01                                DB      01h
EA38: 4A                                DB      4Ah     ; 'J'
EA39: 00                                DB      00h
EA3A: 01                                DB      01h
EA3B: 40                                DB      40h     ; '@'
EA3C: 65                                DB      65h     ; 'e'
EA3D: 01                                DB      01h
EA3E: 41                                DB      41h     ; 'A'
EA3F: 44                                DB      44h     ; 'D'
EA40: 01                                DB      01h
EA41: 42                                DB      42h     ; 'B'
EA42: 5D                                DB      5Dh     ; ']'
EA43: 01                                DB      01h
EA44: 43                                DB      43h     ; 'C'
EA45: 17                                DB      17h
EA46: 01                                DB      01h
EA47: 44                                DB      44h     ; 'D'
EA48: 33                                DB      33h     ; '3'
EA49: 01                                DB      01h
EA4A: 45                                DB      45h     ; 'E'
EA4B: 24                                DB      24h     ; '$'
EA4C: 01                                DB      01h
EA4D: 46                                DB      46h     ; 'F'
EA4E: 17                                DB      17h
EA4F: 01                                DB      01h
EA50: 4E                                DB      4Eh     ; 'N'
EA51: 00                                DB      00h
EA52: 00                                DB      00h
EA53: DC                                DB      0DCh
EA54: E8                                DB      0E8h
EA55: 00                                DB      00h
EA56: 00                         LEA56: DB      00h
EA57: 00                         LEA57: DB      00h
EA58: 00                                DB      00h
EA59: 00                                DB      00h
EA5A: 00                                DB      00h
EA5B: 00                                DB      00h
EA5C: 00                                DB      00h
EA5D: 00                                DB      00h
EA5E: 46                                DB      46h     ; 'F'
EA5F: 23                                DB      23h     ; '#'
EA60: 7E                                DB      7Eh     ; '~'
EA61: F5                                DB      0F5h
EA62: 0F                                DB      0Fh
EA63: 0F                                DB      0Fh
EA64: 0F                                DB      0Fh
EA65: 0F                                DB      0Fh
EA66: E6                                DB      0E6h
EA67: 0F                                DB      0Fh
EA68: CD                                DB      0CDh
EA69: 7C                                DB      7Ch     ; '|'
EA6A: EA                                DB      0EAh
EA6B: F1                                DB      0F1h
EA6C: E6                                DB      0E6h
EA6D: 0F                                DB      0Fh
EA6E: CD                                DB      0CDh
EA6F: 7C                                DB      7Ch     ; '|'
EA70: EA                                DB      0EAh
EA71: 0E                                DB      0Eh
EA72: 2C                                DB      2Ch     ; ','
EA73: CD                                DB      0CDh
EA74: 3F                                DB      3Fh     ; '?'
EA75: E1                                DB      0E1h
EA76: 23                                DB      23h     ; '#'
EA77: 10                                DB      10h
EA78: E7                                DB      0E7h
EA79: C3                                DB      0C3h
EA7A: 86                                DB      86h
EA7B: EA                                DB      0EAh
EA7C: C6                                DB      0C6h
EA7D: 90                                DB      90h
EA7E: 27                                DB      27h     ; '''
EA7F: CE                                DB      0CEh
EA80: 40                                DB      40h     ; '@'
EA81: 27                                DB      27h     ; '''
EA82: 4F                                DB      4Fh     ; 'O'
EA83: C3                                DB      0C3h
EA84: 3F                                DB      3Fh     ; '?'
EA85: E1                                DB      0E1h
EA86: 0E                                DB      0Eh
EA87: 0D                                DB      0Dh
EA88: CD                                DB      0CDh
EA89: 3F                                DB      3Fh     ; '?'
EA8A: E1                                DB      0E1h
EA8B: 0E                                DB      0Eh
EA8C: 0A                                DB      0Ah
EA8D: CD                                DB      0CDh
EA8E: 3F                                DB      3Fh     ; '?'
EA8F: E1                                DB      0E1h
EA90: C9                                DB      0C9h

                                        ; --- START PROC LEA91 ---
EA91: 7E                         LEA91: LD      A,(HL)
EA92: A7                                AND     A
EA93: C8                                RET     Z
EA94: 47                                LD      B,A
EA95: 23                                INC     HL
EA96: 4E                                LD      C,(HL)
EA97: 23                                INC     HL
EA98: ED B3                             OTIR
EA9A: 18 F5                             JR      LEA91

                                        ; prints zero terminated string pointed by HL
                                        PRNSTRZ:
EA9C: 7E                                LD      A,(HL)
EA9D: 23                                INC     HL
EA9E: A7                                AND     A
EA9F: C8                                RET     Z
EAA0: E5                                PUSH    HL
EAA1: 4F                                LD      C,A
EAA2: CD 3F E1                          CALL    CHROUT
EAA5: E1                                POP     HL
EAA6: 18 F4                             JR      PRNSTRZ

EAA8: 1A                         LEAA8: DB      1Ah
EAA9: 1A                                DB      1Ah
EAAA: 1A                                DB      1Ah
EAAB: 1A                                DB      1Ah
EAAC: 1A                                DB      1Ah
EAAD: 1A                                DB      1Ah
EAAE: 1A                                DB      1Ah
EAAF: 1A                                DB      1Ah
EAB0: 1A                                DB      1Ah
EAB1: 1A                                DB      1Ah
EAB2: 1A                                DB      1Ah
EAB3: 1A                                DB      1Ah
EAB4: 1A                                DB      1Ah
EAB5: 1A                                DB      1Ah
EAB6: 1A                                DB      1Ah
EAB7: 1A                                DB      1Ah
EAB8: 1A                                DB      1Ah
EAB9: 1A                                DB      1Ah
EABA: 1A                                DB      1Ah
EABB: 1A                                DB      1Ah
EABC: 1A                                DB      1Ah
EABD: 1A                                DB      1Ah
EABE: 1A                                DB      1Ah
EABF: 1A                                DB      1Ah
EAC0: 1A                                DB      1Ah
EAC1: 1A                                DB      1Ah
EAC2: 1A                                DB      1Ah
EAC3: 1A                                DB      1Ah
EAC4: 1A                                DB      1Ah
EAC5: 1A                                DB      1Ah
EAC6: 1A                                DB      1Ah
EAC7: 1A                                DB      1Ah
EAC8: 1A                                DB      1Ah
EAC9: 1A                                DB      1Ah
EACA: 1A                                DB      1Ah
EACB: 1A                                DB      1Ah
EACC: 1A                                DB      1Ah
EACD: 1A                                DB      1Ah
EACE: 1A                                DB      1Ah
EACF: 1A                                DB      1Ah
EAD0: 1A                                DB      1Ah
EAD1: 1A                                DB      1Ah
EAD2: 1A                                DB      1Ah
EAD3: 1A                                DB      1Ah
EAD4: 1A                                DB      1Ah
EAD5: 1A                                DB      1Ah
EAD6: 1A                                DB      1Ah
EAD7: 1A                                DB      1Ah
EAD8: 1A                                DB      1Ah
EAD9: 1A                                DB      1Ah
EADA: 1A                                DB      1Ah
EADB: 1A                                DB      1Ah
EADC: 1A                                DB      1Ah
EADD: 1A                                DB      1Ah
EADE: 1A                                DB      1Ah
EADF: 1A                                DB      1Ah
EAE0: 1A                                DB      1Ah
EAE1: 1A                                DB      1Ah
EAE2: 1A                                DB      1Ah
EAE3: 1A                                DB      1Ah
EAE4: 1A                                DB      1Ah
EAE5: 1A                                DB      1Ah
EAE6: 1A                                DB      1Ah
EAE7: 1A                                DB      1Ah
EAE8: 1A                                DB      1Ah
EAE9: 1A                                DB      1Ah
EAEA: 1A                                DB      1Ah
EAEB: 1A                                DB      1Ah
EAEC: 1A                                DB      1Ah
EAED: 1A                                DB      1Ah
EAEE: 1A                                DB      1Ah
EAEF: 1A                                DB      1Ah
EAF0: 1A                                DB      1Ah
EAF1: 1A                                DB      1Ah
EAF2: 1A                                DB      1Ah
EAF3: 1A                                DB      1Ah
EAF4: 1A                                DB      1Ah
EAF5: 1A                                DB      1Ah
EAF6: 1A                                DB      1Ah
EAF7: 1A                                DB      1Ah
EAF8: 1A                                DB      1Ah
EAF9: 1A                                DB      1Ah
EAFA: 1A                                DB      1Ah
EAFB: 1A                                DB      1Ah
EAFC: 1A                                DB      1Ah
EAFD: 1A                                DB      1Ah
EAFE: 1A                                DB      1Ah
EAFF: 1A                                DB      1Ah
EB00: 00                                DB      00h
EB01: 01                                DB      01h
EB02: 02                                DB      02h
EB03: 03                                DB      03h
EB04: 04                                DB      04h
EB05: 05                                DB      05h
EB06: 06                                DB      06h
EB07: 07                                DB      07h
EB08: 08                                DB      08h
EB09: 09                                DB      09h
EB0A: 0A                                DB      0Ah
EB0B: 0B                                DB      0Bh
EB0C: 0C                                DB      0Ch
EB0D: 0D                                DB      0Dh
EB0E: 0E                                DB      0Eh
EB0F: 0F                                DB      0Fh
EB10: 10                                DB      10h
EB11: 11                                DB      11h
EB12: 12                                DB      12h
EB13: 13                                DB      13h
EB14: 14                                DB      14h
EB15: 15                                DB      15h
EB16: 16                                DB      16h
EB17: 17                                DB      17h
EB18: 18                                DB      18h
EB19: 19                                DB      19h
EB1A: 1A                                DB      1Ah
EB1B: 1B                                DB      1Bh
EB1C: 1C                                DB      1Ch
EB1D: 1D                                DB      1Dh
EB1E: 1E                                DB      1Eh
EB1F: 1F                                DB      1Fh
EB20: 20                                DB      20h     ; ' '
EB21: 21                                DB      21h     ; '!'
EB22: 22                                DB      22h     ; '"'
EB23: 23 24 25 26 27 28 29 2A           DB      "#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~"
EB2B: 2B 2C 2D 2E 2F 30 31 32
EB33: 33 34 35 36 37 38 39 3A
EB3B: 3B 3C 3D 3E 3F 40 41 42
EB43: 43 44 45 46 47 48 49 4A
EB4B: 4B 4C 4D 4E 4F 50 51 52
EB53: 53 54 55 56 57 58 59 5A
EB5B: 5B 5C 5D 5E 5F 60 61 62
EB63: 63 64 65 66 67 68 69 6A
EB6B: 6B 6C 6D 6E 6F 70 71 72
EB73: 73 74 75 76 77 78 79 7A
EB7B: 7B 7C 7D 7E
EB7F: 7F                                DB      7Fh
EB80: 80                                DB      80h
EB81: 81                                DB      81h
EB82: 82                                DB      82h
EB83: 83                                DB      83h
EB84: 84                                DB      84h
EB85: 85                                DB      85h
EB86: 86                                DB      86h
EB87: 87                                DB      87h
EB88: 88                                DB      88h
EB89: 89                                DB      89h
EB8A: 8A                                DB      8Ah
EB8B: 8B                                DB      8Bh
EB8C: 8C                                DB      8Ch
EB8D: 8D                                DB      8Dh
EB8E: 8E                                DB      8Eh
EB8F: 8F                                DB      8Fh
EB90: 90                                DB      90h
EB91: 91                                DB      91h
EB92: 92                                DB      92h
EB93: 93                                DB      93h
EB94: 94                                DB      94h
EB95: 95                                DB      95h
EB96: 96                                DB      96h
EB97: 97                                DB      97h
EB98: 98                                DB      98h
EB99: 99                                DB      99h
EB9A: 9A                                DB      9Ah
EB9B: 9B                                DB      9Bh
EB9C: 9C                                DB      9Ch
EB9D: 9D                                DB      9Dh
EB9E: 9E                                DB      9Eh
EB9F: 9F                                DB      9Fh
EBA0: A0                                DB      0A0h
EBA1: A1                                DB      0A1h
EBA2: A2                                DB      0A2h
EBA3: A3                                DB      0A3h
EBA4: A4                                DB      0A4h
EBA5: A5                                DB      0A5h
EBA6: A6                                DB      0A6h
EBA7: A7                                DB      0A7h
EBA8: A8                                DB      0A8h
EBA9: A9                                DB      0A9h
EBAA: AA                                DB      0AAh
EBAB: AB                                DB      0ABh
EBAC: AC                                DB      0ACh
EBAD: AD                                DB      0ADh
EBAE: AE                                DB      0AEh
EBAF: AF                                DB      0AFh
EBB0: B0                                DB      0B0h
EBB1: B1                                DB      0B1h
EBB2: B2                                DB      0B2h
EBB3: B3                                DB      0B3h
EBB4: B4                                DB      0B4h
EBB5: B5                                DB      0B5h
EBB6: B6                                DB      0B6h
EBB7: B7                                DB      0B7h
EBB8: B8                                DB      0B8h
EBB9: B9                                DB      0B9h
EBBA: BA                                DB      0BAh
EBBB: BB                                DB      0BBh
EBBC: BC                                DB      0BCh
EBBD: BD                                DB      0BDh
EBBE: BE                                DB      0BEh
EBBF: BF                                DB      0BFh
EBC0: C0                                DB      0C0h
EBC1: C1                                DB      0C1h
EBC2: C2                                DB      0C2h
EBC3: C3                                DB      0C3h
EBC4: C4                                DB      0C4h
EBC5: C5                                DB      0C5h
EBC6: C6                                DB      0C6h
EBC7: C7                                DB      0C7h
EBC8: C8                                DB      0C8h
EBC9: C9                                DB      0C9h
EBCA: CA                                DB      0CAh
EBCB: CB                                DB      0CBh
EBCC: CC                                DB      0CCh
EBCD: CD                                DB      0CDh
EBCE: CE                                DB      0CEh
EBCF: CF                                DB      0CFh
EBD0: D0                                DB      0D0h
EBD1: D1                                DB      0D1h
EBD2: D2                                DB      0D2h
EBD3: D3                                DB      0D3h
EBD4: D4                                DB      0D4h
EBD5: D5                                DB      0D5h
EBD6: D6                                DB      0D6h
EBD7: D7                                DB      0D7h
EBD8: D8                                DB      0D8h
EBD9: D9                                DB      0D9h
EBDA: DA                                DB      0DAh
EBDB: DB                                DB      0DBh
EBDC: DC                                DB      0DCh
EBDD: DD                                DB      0DDh
EBDE: DE                                DB      0DEh
EBDF: DF                                DB      0DFh
EBE0: E0                                DB      0E0h
EBE1: E1                                DB      0E1h
EBE2: E2                                DB      0E2h
EBE3: E3                                DB      0E3h
EBE4: E4                                DB      0E4h
EBE5: E5                                DB      0E5h
EBE6: E6                                DB      0E6h
EBE7: E7                                DB      0E7h
EBE8: E8                                DB      0E8h
EBE9: E9                                DB      0E9h
EBEA: EA                                DB      0EAh
EBEB: EB                                DB      0EBh
EBEC: EC                                DB      0ECh
EBED: ED                                DB      0EDh
EBEE: EE                                DB      0EEh
EBEF: EF                                DB      0EFh
EBF0: F0                                DB      0F0h
EBF1: F1                                DB      0F1h
EBF2: F2                                DB      0F2h
EBF3: F3                                DB      0F3h
EBF4: F4                                DB      0F4h
EBF5: F5                                DB      0F5h
EBF6: F6                                DB      0F6h
EBF7: F7                                DB      0F7h
EBF8: F8                                DB      0F8h
EBF9: F9                                DB      0F9h
EBFA: FA                                DB      0FAh
EBFB: FB                                DB      0FBh
EBFC: FC                                DB      0FCh
EBFD: FD                                DB      0FDh
EBFE: FE                                DB      0FEh
EBFF: FF                                DB      0FFh
EC00: 00                                DB      00h
EC01: 01                                DB      01h
EC02: 02                                DB      02h
EC03: 03                                DB      03h
EC04: 04                                DB      04h
EC05: 05                                DB      05h
EC06: 06                                DB      06h
EC07: 07                                DB      07h
EC08: 08                                DB      08h
EC09: 09                                DB      09h
EC0A: 0A                                DB      0Ah
EC0B: 0B                                DB      0Bh
EC0C: 0C                                DB      0Ch
EC0D: 0D                                DB      0Dh
EC0E: 0E                                DB      0Eh
EC0F: 0F                                DB      0Fh
EC10: 10                                DB      10h
EC11: 11                                DB      11h
EC12: 12                                DB      12h
EC13: 13                                DB      13h
EC14: 14                                DB      14h
EC15: 15                                DB      15h
EC16: 16                                DB      16h
EC17: 17                                DB      17h
EC18: 18                                DB      18h
EC19: 19                                DB      19h
EC1A: 1A                                DB      1Ah
EC1B: 1B                                DB      1Bh
EC1C: 1C                                DB      1Ch
EC1D: 1D                                DB      1Dh
EC1E: 1E                                DB      1Eh
EC1F: 1F                                DB      1Fh
EC20: 20                                DB      20h     ; ' '
EC21: 21                                DB      21h     ; '!'
EC22: 22                                DB      22h     ; '"'
EC23: 23 24 25 26 27 28 29 2A           DB      "#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~"
EC2B: 2B 2C 2D 2E 2F 30 31 32
EC33: 33 34 35 36 37 38 39 3A
EC3B: 3B 3C 3D 3E 3F 40 41 42
EC43: 43 44 45 46 47 48 49 4A
EC4B: 4B 4C 4D 4E 4F 50 51 52
EC53: 53 54 55 56 57 58 59 5A
EC5B: 5B 5C 5D 5E 5F 60 61 62
EC63: 63 64 65 66 67 68 69 6A
EC6B: 6B 6C 6D 6E 6F 70 71 72
EC73: 73 74 75 76 77 78 79 7A
EC7B: 7B 7C 7D 7E
EC7F: 7F                                DB      7Fh
EC80: 80                                DB      80h
EC81: 81                                DB      81h
EC82: 82                                DB      82h
EC83: 83                                DB      83h
EC84: 84                                DB      84h
EC85: 85                                DB      85h
EC86: 86                                DB      86h
EC87: 87                                DB      87h
EC88: 88                                DB      88h
EC89: 89                                DB      89h
EC8A: 8A                                DB      8Ah
EC8B: 8B                                DB      8Bh
EC8C: 8C                                DB      8Ch
EC8D: 8D                                DB      8Dh
EC8E: 8E                                DB      8Eh
EC8F: 8F                                DB      8Fh
EC90: 90                                DB      90h
EC91: 91                                DB      91h
EC92: 92                                DB      92h
EC93: 93                                DB      93h
EC94: 94                                DB      94h
EC95: 95                                DB      95h
EC96: 96                                DB      96h
EC97: 97                                DB      97h
EC98: 98                                DB      98h
EC99: 99                                DB      99h
EC9A: 9A                                DB      9Ah
EC9B: 9B                                DB      9Bh
EC9C: 9C                                DB      9Ch
EC9D: 9D                                DB      9Dh
EC9E: 9E                                DB      9Eh
EC9F: 9F                                DB      9Fh
ECA0: A0                                DB      0A0h
ECA1: A1                                DB      0A1h
ECA2: A2                                DB      0A2h
ECA3: A3                                DB      0A3h
ECA4: A4                                DB      0A4h
ECA5: A5                                DB      0A5h
ECA6: A6                                DB      0A6h
ECA7: A7                                DB      0A7h
ECA8: A8                                DB      0A8h
ECA9: A9                                DB      0A9h
ECAA: AA                                DB      0AAh
ECAB: AB                                DB      0ABh
ECAC: AC                                DB      0ACh
ECAD: AD                                DB      0ADh
ECAE: AE                                DB      0AEh
ECAF: AF                                DB      0AFh
ECB0: B0                                DB      0B0h
ECB1: B1                                DB      0B1h
ECB2: B2                                DB      0B2h
ECB3: B3                                DB      0B3h
ECB4: B4                                DB      0B4h
ECB5: B5                                DB      0B5h
ECB6: B6                                DB      0B6h
ECB7: B7                                DB      0B7h
ECB8: B8                                DB      0B8h
ECB9: B9                                DB      0B9h
ECBA: BA                                DB      0BAh
ECBB: BB                                DB      0BBh
ECBC: BC                                DB      0BCh
ECBD: BD                                DB      0BDh
ECBE: BE                                DB      0BEh
ECBF: BF                                DB      0BFh
ECC0: C0                                DB      0C0h
ECC1: C1                                DB      0C1h
ECC2: C2                                DB      0C2h
ECC3: C3                                DB      0C3h
ECC4: C4                                DB      0C4h
ECC5: C5                                DB      0C5h
ECC6: C6                                DB      0C6h
ECC7: C7                                DB      0C7h
ECC8: C8                                DB      0C8h
ECC9: C9                                DB      0C9h
ECCA: CA                                DB      0CAh
ECCB: CB                                DB      0CBh
ECCC: CC                                DB      0CCh
ECCD: CD                                DB      0CDh
ECCE: CE                                DB      0CEh
ECCF: CF                                DB      0CFh
ECD0: D0                                DB      0D0h
ECD1: D1                                DB      0D1h
ECD2: D2                                DB      0D2h
ECD3: D3                                DB      0D3h
ECD4: D4                                DB      0D4h
ECD5: D5                                DB      0D5h
ECD6: D6                                DB      0D6h
ECD7: D7                                DB      0D7h
ECD8: D8                                DB      0D8h
ECD9: D9                                DB      0D9h
ECDA: DA                                DB      0DAh
ECDB: DB                                DB      0DBh
ECDC: DC                                DB      0DCh
ECDD: DD                                DB      0DDh
ECDE: DE                                DB      0DEh
ECDF: DF                                DB      0DFh
ECE0: E0                                DB      0E0h
ECE1: E1                                DB      0E1h
ECE2: E2                                DB      0E2h
ECE3: E3                                DB      0E3h
ECE4: E4                                DB      0E4h
ECE5: E5                                DB      0E5h
ECE6: E6                                DB      0E6h
ECE7: E7                                DB      0E7h
ECE8: E8                                DB      0E8h
ECE9: E9                                DB      0E9h
ECEA: EA                                DB      0EAh
ECEB: EB                                DB      0EBh
ECEC: EC                                DB      0ECh
ECED: ED                                DB      0EDh
ECEE: EE                                DB      0EEh
ECEF: EF                                DB      0EFh
ECF0: F0                                DB      0F0h
ECF1: F1                                DB      0F1h
ECF2: F2                                DB      0F2h
ECF3: F3                                DB      0F3h
ECF4: F4                                DB      0F4h
ECF5: F5                                DB      0F5h
ECF6: F6                                DB      0F6h
ECF7: F7                                DB      0F7h
ECF8: F8                                DB      0F8h
ECF9: F9                                DB      0F9h
ECFA: FA                                DB      0FAh
ECFB: FB                                DB      0FBh
ECFC: FC                                DB      0FCh
ECFD: FD                                DB      0FDh
ECFE: FE                                DB      0FEh
ECFF: FF                                DB      0FFh
ED00: 00                                DB      00h
ED01: 01                                DB      01h
ED02: 02                                DB      02h
ED03: 03                                DB      03h
ED04: 04                                DB      04h
ED05: 05                                DB      05h
ED06: 06                                DB      06h
ED07: 07                                DB      07h
ED08: 08                                DB      08h
ED09: 09                                DB      09h
ED0A: 0A                                DB      0Ah
ED0B: 0B                                DB      0Bh
ED0C: 0C                                DB      0Ch
ED0D: 0D                                DB      0Dh
ED0E: 0E                                DB      0Eh
ED0F: 0F                                DB      0Fh
ED10: 10                                DB      10h
ED11: 11                                DB      11h
ED12: 12                                DB      12h
ED13: 13                                DB      13h
ED14: 14                                DB      14h
ED15: 15                                DB      15h
ED16: 16                                DB      16h
ED17: 17                                DB      17h
ED18: 18                                DB      18h
ED19: 19                                DB      19h
ED1A: 1A                                DB      1Ah
ED1B: 1B                                DB      1Bh
ED1C: 1C                                DB      1Ch
ED1D: 1D                                DB      1Dh
ED1E: 1E                                DB      1Eh
ED1F: 1F                                DB      1Fh
ED20: 20                                DB      20h     ; ' '
ED21: 21                                DB      21h     ; '!'
ED22: 22                                DB      22h     ; '"'
ED23: 23 24 25 26 27 28 29 2A           DB      "#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~"
ED2B: 2B 2C 2D 2E 2F 30 31 32
ED33: 33 34 35 36 37 38 39 3A
ED3B: 3B 3C 3D 3E 3F 40 41 42
ED43: 43 44 45 46 47 48 49 4A
ED4B: 4B 4C 4D 4E 4F 50 51 52
ED53: 53 54 55 56 57 58 59 5A
ED5B: 5B 5C 5D 5E 5F 60 61 62
ED63: 63 64 65 66 67 68 69 6A
ED6B: 6B 6C 6D 6E 6F 70 71 72
ED73: 73 74 75 76 77 78 79 7A
ED7B: 7B 7C 7D 7E
ED7F: 7F                                DB      7Fh
ED80: 80                                DB      80h
ED81: 81                                DB      81h
ED82: 82                                DB      82h
ED83: 83                                DB      83h
ED84: 84                                DB      84h
ED85: 85                                DB      85h
ED86: 86                                DB      86h
ED87: 87                                DB      87h
ED88: 88                                DB      88h
ED89: 89                                DB      89h
ED8A: 8A                                DB      8Ah
ED8B: 8B                                DB      8Bh
ED8C: 8C                                DB      8Ch
ED8D: 8D                                DB      8Dh
ED8E: 8E                                DB      8Eh
ED8F: 8F                                DB      8Fh
ED90: 90                                DB      90h
ED91: 91                                DB      91h
ED92: 92                                DB      92h
ED93: 93                                DB      93h
ED94: 94                                DB      94h
ED95: 95                                DB      95h
ED96: 96                                DB      96h
ED97: 97                                DB      97h
ED98: 98                                DB      98h
ED99: 99                                DB      99h
ED9A: 9A                                DB      9Ah
ED9B: 9B                                DB      9Bh
ED9C: 9C                                DB      9Ch
ED9D: 9D                                DB      9Dh
ED9E: 9E                                DB      9Eh
ED9F: 9F                                DB      9Fh
EDA0: A0                                DB      0A0h
EDA1: A1                                DB      0A1h
EDA2: A2                                DB      0A2h
EDA3: A3                                DB      0A3h
EDA4: A4                                DB      0A4h
EDA5: A5                                DB      0A5h
EDA6: A6                                DB      0A6h
EDA7: A7                                DB      0A7h
EDA8: A8                                DB      0A8h
EDA9: A9                                DB      0A9h
EDAA: AA                                DB      0AAh
EDAB: AB                                DB      0ABh
EDAC: AC                                DB      0ACh
EDAD: AD                                DB      0ADh
EDAE: AE                                DB      0AEh
EDAF: AF                                DB      0AFh
EDB0: B0                                DB      0B0h
EDB1: B1                                DB      0B1h
EDB2: B2                                DB      0B2h
EDB3: B3                                DB      0B3h
EDB4: B4                                DB      0B4h
EDB5: B5                                DB      0B5h
EDB6: B6                                DB      0B6h
EDB7: B7                                DB      0B7h
EDB8: B8                                DB      0B8h
EDB9: B9                                DB      0B9h
EDBA: BA                                DB      0BAh
EDBB: BB                                DB      0BBh
EDBC: BC                                DB      0BCh
EDBD: BD                                DB      0BDh
EDBE: BE                                DB      0BEh
EDBF: BF                                DB      0BFh
EDC0: C0                                DB      0C0h
EDC1: C1                                DB      0C1h
EDC2: C2                                DB      0C2h
EDC3: C3                                DB      0C3h
EDC4: C4                                DB      0C4h
EDC5: C5                                DB      0C5h
EDC6: C6                                DB      0C6h
EDC7: C7                                DB      0C7h
EDC8: C8                                DB      0C8h
EDC9: C9                                DB      0C9h
EDCA: CA                                DB      0CAh
EDCB: CB                                DB      0CBh
EDCC: CC                                DB      0CCh
EDCD: CD                                DB      0CDh
EDCE: CE                                DB      0CEh
EDCF: CF                                DB      0CFh
EDD0: D0                                DB      0D0h
EDD1: D1                                DB      0D1h
EDD2: D2                                DB      0D2h
EDD3: D3                                DB      0D3h
EDD4: D4                                DB      0D4h
EDD5: D5                                DB      0D5h
EDD6: D6                                DB      0D6h
EDD7: D7                                DB      0D7h
EDD8: D8                                DB      0D8h
EDD9: D9                                DB      0D9h
EDDA: DA                                DB      0DAh
EDDB: DB                                DB      0DBh
EDDC: DC                                DB      0DCh
EDDD: DD                                DB      0DDh
EDDE: DE                                DB      0DEh
EDDF: DF                                DB      0DFh
EDE0: E0                                DB      0E0h
EDE1: E1                                DB      0E1h
EDE2: E2                                DB      0E2h
EDE3: E3                                DB      0E3h
EDE4: E4                                DB      0E4h
EDE5: E5                                DB      0E5h
EDE6: E6                                DB      0E6h
EDE7: E7                                DB      0E7h
EDE8: E8                                DB      0E8h
EDE9: E9                                DB      0E9h
EDEA: EA                                DB      0EAh
EDEB: EB                                DB      0EBh
EDEC: EC                                DB      0ECh
EDED: ED                                DB      0EDh
EDEE: EE                                DB      0EEh
EDEF: EF                                DB      0EFh
EDF0: F0                                DB      0F0h
EDF1: F1                                DB      0F1h
EDF2: F2                                DB      0F2h
EDF3: F3                                DB      0F3h
EDF4: F4                                DB      0F4h
EDF5: F5                                DB      0F5h
EDF6: F6                                DB      0F6h
EDF7: F7                                DB      0F7h
EDF8: F8                                DB      0F8h
EDF9: F9                                DB      0F9h
EDFA: FA                                DB      0FAh
EDFB: FB                                DB      0FBh
EDFC: FC                                DB      0FCh
EDFD: FD                                DB      0FDh
EDFE: FE                                DB      0FEh
EDFF: FF                                DB      0FFh
EE00: 00                                DB      00h
EE01: 01                                DB      01h
EE02: 02                                DB      02h
EE03: 03                                DB      03h
EE04: 04                                DB      04h
EE05: 05                                DB      05h
EE06: 06                                DB      06h
EE07: 07                                DB      07h
EE08: 08                                DB      08h
EE09: 09                                DB      09h
EE0A: 0A                                DB      0Ah
EE0B: 0B                                DB      0Bh
EE0C: 0C                                DB      0Ch
EE0D: 0D                                DB      0Dh
EE0E: 0E                                DB      0Eh
EE0F: 0F                                DB      0Fh
EE10: 10                                DB      10h
EE11: 11                                DB      11h
EE12: 12                                DB      12h
EE13: 13                                DB      13h
EE14: 14                                DB      14h
EE15: 15                                DB      15h
EE16: 16                                DB      16h
EE17: 17                                DB      17h
EE18: 18                                DB      18h
EE19: 19                                DB      19h
EE1A: 1A                                DB      1Ah
EE1B: 1B                                DB      1Bh
EE1C: 1C                                DB      1Ch
EE1D: 1D                                DB      1Dh
EE1E: 1E                                DB      1Eh
EE1F: 1F                                DB      1Fh
EE20: 20                                DB      20h     ; ' '
EE21: 21                                DB      21h     ; '!'
EE22: 22                                DB      22h     ; '"'
EE23: 23 24 25 26 27 28 29 2A           DB      "#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~"
EE2B: 2B 2C 2D 2E 2F 30 31 32
EE33: 33 34 35 36 37 38 39 3A
EE3B: 3B 3C 3D 3E 3F 40 41 42
EE43: 43 44 45 46 47 48 49 4A
EE4B: 4B 4C 4D 4E 4F 50 51 52
EE53: 53 54 55 56 57 58 59 5A
EE5B: 5B 5C 5D 5E 5F 60 61 62
EE63: 63 64 65 66 67 68 69 6A
EE6B: 6B 6C 6D 6E 6F 70 71 72
EE73: 73 74 75 76 77 78 79 7A
EE7B: 7B 7C 7D 7E
EE7F: 7F                                DB      7Fh
EE80: 80                                DB      80h
EE81: 81                                DB      81h
EE82: 82                                DB      82h
EE83: 83                                DB      83h
EE84: 84                                DB      84h
EE85: 85                                DB      85h
EE86: 86                                DB      86h
EE87: 87                                DB      87h
EE88: 88                                DB      88h
EE89: 89                                DB      89h
EE8A: 8A                                DB      8Ah
EE8B: 8B                                DB      8Bh
EE8C: 8C                                DB      8Ch
EE8D: 8D                                DB      8Dh
EE8E: 8E                                DB      8Eh
EE8F: 8F                                DB      8Fh
EE90: 90                                DB      90h
EE91: 91                                DB      91h
EE92: 92                                DB      92h
EE93: 93                                DB      93h
EE94: 94                                DB      94h
EE95: 95                                DB      95h
EE96: 96                                DB      96h
EE97: 97                                DB      97h
EE98: 98                                DB      98h
EE99: 99                                DB      99h
EE9A: 9A                                DB      9Ah
EE9B: 9B                                DB      9Bh
EE9C: 9C                                DB      9Ch
EE9D: 9D                                DB      9Dh
EE9E: 9E                                DB      9Eh
EE9F: 9F                                DB      9Fh
EEA0: A0                                DB      0A0h
EEA1: A1                                DB      0A1h
EEA2: A2                                DB      0A2h
EEA3: A3                                DB      0A3h
EEA4: A4                                DB      0A4h
EEA5: A5                                DB      0A5h
EEA6: A6                                DB      0A6h
EEA7: A7                                DB      0A7h
EEA8: A8                                DB      0A8h
EEA9: A9                                DB      0A9h
EEAA: AA                                DB      0AAh
EEAB: AB                                DB      0ABh
EEAC: AC                                DB      0ACh
EEAD: AD                                DB      0ADh
EEAE: AE                                DB      0AEh
EEAF: AF                                DB      0AFh
EEB0: B0                                DB      0B0h
EEB1: B1                                DB      0B1h
EEB2: B2                                DB      0B2h
EEB3: B3                                DB      0B3h
EEB4: B4                                DB      0B4h
EEB5: B5                                DB      0B5h
EEB6: B6                                DB      0B6h
EEB7: B7                                DB      0B7h
EEB8: B8                                DB      0B8h
EEB9: B9                                DB      0B9h
EEBA: BA                                DB      0BAh
EEBB: BB                                DB      0BBh
EEBC: BC                                DB      0BCh
EEBD: BD                                DB      0BDh
EEBE: BE                                DB      0BEh
EEBF: BF                                DB      0BFh
EEC0: C0                                DB      0C0h
EEC1: C1                                DB      0C1h
EEC2: C2                                DB      0C2h
EEC3: C3                                DB      0C3h
EEC4: C4                                DB      0C4h
EEC5: C5                                DB      0C5h
EEC6: C6                                DB      0C6h
EEC7: C7                                DB      0C7h
EEC8: C8                                DB      0C8h
EEC9: C9                                DB      0C9h
EECA: CA                                DB      0CAh
EECB: CB                                DB      0CBh
EECC: CC                                DB      0CCh
EECD: CD                                DB      0CDh
EECE: CE                                DB      0CEh
EECF: CF                                DB      0CFh
EED0: D0                                DB      0D0h
EED1: D1                                DB      0D1h
EED2: D2                                DB      0D2h
EED3: D3                                DB      0D3h
EED4: D4                                DB      0D4h
EED5: D5                                DB      0D5h
EED6: D6                                DB      0D6h
EED7: D7                                DB      0D7h
EED8: D8                                DB      0D8h
EED9: D9                                DB      0D9h
EEDA: DA                                DB      0DAh
EEDB: DB                                DB      0DBh
EEDC: DC                                DB      0DCh
EEDD: DD                                DB      0DDh
EEDE: DE                                DB      0DEh
EEDF: DF                                DB      0DFh
EEE0: E0                                DB      0E0h
EEE1: E1                                DB      0E1h
EEE2: E2                                DB      0E2h
EEE3: E3                                DB      0E3h
EEE4: E4                                DB      0E4h
EEE5: E5                                DB      0E5h
EEE6: E6                                DB      0E6h
EEE7: E7                                DB      0E7h
EEE8: E8                                DB      0E8h
EEE9: E9                                DB      0E9h
EEEA: EA                                DB      0EAh
EEEB: EB                                DB      0EBh
EEEC: EC                                DB      0ECh
EEED: ED                                DB      0EDh
EEEE: EE                                DB      0EEh
EEEF: EF                                DB      0EFh
EEF0: F0                                DB      0F0h
EEF1: F1                                DB      0F1h
EEF2: F2                                DB      0F2h
EEF3: F3                                DB      0F3h
EEF4: F4                                DB      0F4h
EEF5: F5                                DB      0F5h
EEF6: F6                                DB      0F6h
EEF7: F7                                DB      0F7h
EEF8: F8                                DB      0F8h
EEF9: F9                                DB      0F9h
EEFA: FA                                DB      0FAh
EEFB: FB                                DB      0FBh
EEFC: FC                                DB      0FCh
EEFD: FD                                DB      0FDh
EEFE: FE                                DB      0FEh
EEFF: FF                                DB      0FFh
EF00: 00                                DB      00h
EF01: 01                                DB      01h
EF02: 02                                DB      02h
EF03: 03                                DB      03h
EF04: 04                                DB      04h
EF05: 05                                DB      05h
EF06: 06                                DB      06h
EF07: 07                                DB      07h
EF08: 08                                DB      08h
EF09: 09                                DB      09h
EF0A: 0A                                DB      0Ah
EF0B: 0B                                DB      0Bh
EF0C: 0C                                DB      0Ch
EF0D: 0D                                DB      0Dh
EF0E: 0E                                DB      0Eh
EF0F: 0F                                DB      0Fh
EF10: 10                                DB      10h
EF11: 11                                DB      11h
EF12: 12                                DB      12h
EF13: 13                                DB      13h
EF14: 14                                DB      14h
EF15: 15                                DB      15h
EF16: 16                                DB      16h
EF17: 17                                DB      17h
EF18: 18                                DB      18h
EF19: 19                                DB      19h
EF1A: 1A                                DB      1Ah
EF1B: 1B                                DB      1Bh
EF1C: 1C                                DB      1Ch
EF1D: 1D                                DB      1Dh
EF1E: 1E                                DB      1Eh
EF1F: 1F                                DB      1Fh
EF20: 20                                DB      20h     ; ' '
EF21: 21                                DB      21h     ; '!'
EF22: 22                                DB      22h     ; '"'
EF23: 23 24 25 26 27 28 29 2A           DB      "#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~"
EF2B: 2B 2C 2D 2E 2F 30 31 32
EF33: 33 34 35 36 37 38 39 3A
EF3B: 3B 3C 3D 3E 3F 40 41 42
EF43: 43 44 45 46 47 48 49 4A
EF4B: 4B 4C 4D 4E 4F 50 51 52
EF53: 53 54 55 56 57 58 59 5A
EF5B: 5B 5C 5D 5E 5F 60 61 62
EF63: 63 64 65 66 67 68 69 6A
EF6B: 6B 6C 6D 6E 6F 70 71 72
EF73: 73 74 75 76 77 78 79 7A
EF7B: 7B 7C 7D 7E
EF7F: 7F                                DB      7Fh
EF80: 80                                DB      80h
EF81: 81                                DB      81h
EF82: 82                                DB      82h
EF83: 83                                DB      83h
EF84: 84                                DB      84h
EF85: 85                                DB      85h
EF86: 86                                DB      86h
EF87: 87                                DB      87h
EF88: 88                                DB      88h
EF89: 89                                DB      89h
EF8A: 8A                                DB      8Ah
EF8B: 8B                                DB      8Bh
EF8C: 8C                                DB      8Ch
EF8D: 8D                                DB      8Dh
EF8E: 8E                                DB      8Eh
EF8F: 8F                                DB      8Fh
EF90: 90                                DB      90h
EF91: 91                                DB      91h
EF92: 92                                DB      92h
EF93: 93                                DB      93h
EF94: 94                                DB      94h
EF95: 95                                DB      95h
EF96: 96                                DB      96h
EF97: 97                                DB      97h
EF98: 98                                DB      98h
EF99: 99                                DB      99h
EF9A: 9A                                DB      9Ah
EF9B: 9B                                DB      9Bh
EF9C: 9C                                DB      9Ch
EF9D: 9D                                DB      9Dh
EF9E: 9E                                DB      9Eh
EF9F: 9F                                DB      9Fh
EFA0: A0                                DB      0A0h
EFA1: A1                                DB      0A1h
EFA2: A2                                DB      0A2h
EFA3: A3                                DB      0A3h
EFA4: A4                                DB      0A4h
EFA5: A5                                DB      0A5h
EFA6: A6                                DB      0A6h
EFA7: A7                                DB      0A7h
EFA8: A8                                DB      0A8h
EFA9: A9                                DB      0A9h
EFAA: AA                                DB      0AAh
EFAB: AB                                DB      0ABh
EFAC: AC                                DB      0ACh
EFAD: AD                                DB      0ADh
EFAE: AE                                DB      0AEh
EFAF: AF                                DB      0AFh
EFB0: B0                                DB      0B0h
EFB1: B1                                DB      0B1h
EFB2: B2                                DB      0B2h
EFB3: B3                                DB      0B3h
EFB4: B4                                DB      0B4h
EFB5: B5                                DB      0B5h
EFB6: B6                                DB      0B6h
EFB7: B7                                DB      0B7h
EFB8: B8                                DB      0B8h
EFB9: B9                                DB      0B9h
EFBA: BA                                DB      0BAh
EFBB: BB                                DB      0BBh
EFBC: BC                                DB      0BCh
EFBD: BD                                DB      0BDh
EFBE: BE                                DB      0BEh
EFBF: BF                                DB      0BFh
EFC0: C0                                DB      0C0h
EFC1: C1                                DB      0C1h
EFC2: C2                                DB      0C2h
EFC3: C3                                DB      0C3h
EFC4: C4                                DB      0C4h
EFC5: C5                                DB      0C5h
EFC6: C6                                DB      0C6h
EFC7: C7                                DB      0C7h
EFC8: C8                                DB      0C8h
EFC9: C9                                DB      0C9h
EFCA: CA                                DB      0CAh
EFCB: CB                                DB      0CBh
EFCC: CC                                DB      0CCh
EFCD: CD                                DB      0CDh
EFCE: CE                                DB      0CEh
EFCF: CF                                DB      0CFh
EFD0: D0                                DB      0D0h
EFD1: D1                                DB      0D1h
EFD2: D2                                DB      0D2h
EFD3: D3                                DB      0D3h
EFD4: D4                                DB      0D4h
EFD5: D5                                DB      0D5h
EFD6: D6                                DB      0D6h
EFD7: D7                                DB      0D7h
EFD8: D8                                DB      0D8h
EFD9: D9                                DB      0D9h
EFDA: DA                                DB      0DAh
EFDB: DB                                DB      0DBh
EFDC: DC                                DB      0DCh
EFDD: DD                                DB      0DDh
EFDE: DE                                DB      0DEh
EFDF: DF                                DB      0DFh
EFE0: E0                                DB      0E0h
EFE1: E1                                DB      0E1h
EFE2: E2                                DB      0E2h
EFE3: E3                                DB      0E3h
EFE4: E4                                DB      0E4h
EFE5: E5                                DB      0E5h
EFE6: E6                                DB      0E6h
EFE7: E7                                DB      0E7h
EFE8: E8                                DB      0E8h
EFE9: E9                                DB      0E9h
EFEA: EA                                DB      0EAh
EFEB: EB                                DB      0EBh
EFEC: EC                                DB      0ECh
EFED: ED                                DB      0EDh
EFEE: EE                                DB      0EEh
EFEF: EF                                DB      0EFh
EFF0: F0                                DB      0F0h
EFF1: F1                                DB      0F1h
EFF2: F2                                DB      0F2h
EFF3: F3                                DB      0F3h
EFF4: F4                                DB      0F4h
EFF5: F5                                DB      0F5h
EFF6: F6                                DB      0F6h
EFF7: F7                                DB      0F7h
EFF8: F8                                DB      0F8h
EFF9: F9                                DB      0F9h
EFFA: FA                                DB      0FAh
EFFB: FB                                DB      0FBh
EFFC: FC                                DB      0FCh
EFFD: FD                                DB      0FDh
EFFE: FE                                DB      0FEh
EFFF: FF                                DB      0FFh

references to external address 0084h:
        E0B8 LD HL,(0084h)

references to external address 0086h:
        E0C1 LD HL,(0086h)

references to external address 0088h:
        E0C7 LD HL,(0088h)

references to external address 0F900h:
        E007 LD (0F900h),HL
        E00F CALL 0F900h

references to external address 0F902h:
        E00C LD (0F902h),A

references to external address 0FD7Eh:
        E13F LD (0FD7Eh),SP
        E143 LD SP,0FD7Eh
        E157 LD SP,(0FD7Eh)

references to external address 0FDFDh:
        E1E7 LD (0FDFDh),SP
        E1EB LD SP,0FDFDh
        E1F9 LD SP,(0FDFDh)

references to external address 0FE22h:
        E04E LD (0FE22h),HL

references to external address VIDEOPTR:
        E051 LD (VIDEOPTR),DE
        E14A LD HL,(VIDEOPTR)

references to external address 0FF5Dh:
        E972 JP 0FF5Dh

possible references to internal address E000:
        E016 LD DE,ENTRY

possible references to internal address E203:
        E178 LD DE,0E203h

possible references to internal address E22B:
        E055 LD HL,0E22Bh

possible references to internal address E263:
        E061 LD HL,0E263h

possible references to internal address E296:
        E1F2 LD IY,0E296h
        ----------
        E1AF LD (LE296),A
        E1BC LD A,(LE296)

possible references to internal address E2B1:
        E132 LD HL,0E2B1h
        ----------
        E0D6 LD (LE2B1),HL
        E0F2 LD BC,(LE2B1)

possible references to internal address E2CF:
        E0A9 LD DE,0E2CFh
        E0B1 LD IX,0E2CFh
        E0D9 LD HL,0E2CFh

possible references to internal address E397:
        E1B9 LD DE,0E397h

possible references to internal address E5E1:
        E004 LD HL,0E5E1h

possible references to internal address E655:
        E3F8 LD HL,0E655h

possible references to internal address E689:
        E55E LD HL,0E689h
        ----------
        E526 LD (LE689),A

possible references to internal address E68C:
        E573 LD HL,0E68Ch

possible references to internal address E693:
        E5B4 LD HL,0E693h
        E5C2 LD HL,0E693h

possible references to internal address E696:
        E64E LD HL,0E696h

possible references to internal address E6AC:
        E3EF LD HL,0E6ACh

possible references to internal address E6C5:
        E406 LD HL,0E6C5h

possible references to internal address E80A:
        E725 LD HL,0E80Ah

possible references to internal address E810:
        E47B LD HL,0E810h

possible references to internal address E816:
        E481 LD HL,0E816h

possible references to internal address E978:
        E8BE LD HL,0E978h

possible references to internal address EA34:
        E8C6 LD HL,0EA34h

possible references to external address 0000h:
        E0D3 LD HL,0000h
        E15D LD DE,0000h
        E1A6 LD BC,0000h
        E1AB LD HL,0000h

possible references to external address 0001h:
        E099 LD BC,0001h
        E0CD LD HL,0001h

possible references to external address 0006h:
        E14D LD DE,0006h

possible references to external address 000Ah:
        E421 LD HL,000Ah
        E4B7 LD HL,000Ah

possible references to external address 000Ch:
        E1C9 LD DE,000Ch

possible references to external address 001Ch:
        E53E LD DE,001Ch

possible references to external address 0026h:
        E432 LD DE,0026h
        E505 LD DE,0026h

possible references to external address 002Ah:
        E549 LD DE,002Ah

possible references to external address 0032h:
        E0AC LD BC,0032h

possible references to external address 0034h:
        E3FB LD BC,0034h

possible references to external address 0080h:
        E090 LD BC,0080h

possible references to external address 0090h:
        E0A6 LD HL,0090h

possible references to external address 00B5h:
        E8C1 LD BC,00B5h

possible references to external address 1000h:
        E019 LD BC,1000h
        E035 LD HL,1000h

possible references to external address 2000h:
        E032 LD DE,2000h
        E048 LD DE,2000h

possible references to external address 4000h:
        E03D LD DE,4000h
        E040 LD HL,4000h

possible references to external address 8000h:
        E04B LD HL,8000h

possible references to external address TMPSTACK:
        E001 LD SP,TMPSTACK

possible references to external address 0FCFFh:
        E022 LD SP,0FCFFh

possible references to external address 0FD00h:
        E3F5 LD DE,0FD00h

possible references to external address 0FD7Eh:
        E143 LD SP,0FD7Eh
        ----------
        E13F LD (0FD7Eh),SP
        E157 LD SP,(0FD7Eh)

possible references to external address 0FDFDh:
        E1EB LD SP,0FDFDh
        ----------
        E1E7 LD (0FDFDh),SP
        E1F9 LD SP,(0FDFDh)

possible references to external address 0FF00h:
        E8BB LD DE,0FF00h

possible references to external address 0FFEEh:
        E012 LD DE,0FFEEh

possible references to external address 0FFFFh:
        E4CE LD HL,0FFFFh

references to port 10h
        E02A OUT (10h),A

references to port 11h
        E02C OUT (11h),A

references to port 12h
        E026 OUT (12h),A
        E717 OUT (12h),A
        E73B OUT (12h),A

references to port 13h
        E028 OUT (13h),A
        E719 OUT (13h),A
        E73D OUT (13h),A

references to port 20h
        E5FA IN A,(20h)
        E620 IN A,(20h)

references to port 21h
        E5FF IN A,(21h)
        E60D IN A,(21h)
        E61B IN A,(21h)
        E608 OUT (21h),A

references to port 23h
        E42A OUT (23h),A
        E4C7 OUT (23h),A
        E4E2 OUT (23h),A

references to port 34h
        E02E IN A,(34h)
        E067 IN A,(34h)

references to port 3Ch
        E7A8 IN A,(3Ch)
        E7B3 IN A,(3Ch)
        E7D8 IN A,(3Ch)
        E768 OUT (3Ch),A
        E78A OUT (3Ch),A
        E7A2 OUT (3Ch),A

references to port 3Dh
        E46E IN A,(3Dh)
        E75F IN A,(3Dh)
        E76E IN A,(3Dh)
        E77D IN A,(3Dh)
        E793 IN A,(3Dh)
        E7AB IN A,(3Dh)
        E7CA IN A,(3Dh)
        E46C OUT (3Dh),A
        E76C OUT (3Dh),A
        E777 OUT (3Dh),A

Procedures (36):
  Proc  Length  References Dependants
  ENTRY  013F            0         12
  CHROUT  001D            1          1
  LE15C  0029            1          0
  LE1A6  0005            1          1
  LE1AB  002D            2          0
  LE1D8  0005            2          0
  LE1DD  0005            2          0
  LE1E2  0005            2          0
  LE1E7  001B            2          1
  LE3EF  001E            1          1
  LE40D  0030            2          6
  LE43C  0001            1          0
  LE454  0017            1          2
  LE46B  001D            1          3
  LE48C  00DB            1          5
  LE567  001C            1          2
  LE5AC  002A            2          2
  LE5D6  000F            6          2
  LE5E5  000C            1          1
  LE5F1  0009            3          1
  LE5FA  0009            2          0
  LE603  001D            3          1
  LE620  0007            6          0
  LE627  0009            4          1
  LE630  0025            1          1
  LE70D  0038            1          4
  LE745  001A            1          0
  LE75F  001B            3          0
  LE77A  0016            2          0
  LE790  0037            1          0
  LE7A8  001F            2          0
  LE7C7  0018            1          1
  LE8BB  001D            1          2
  LE972  0003            1          1
  LEA91  000B            6          0
  PRNSTRZ  000C            2          1

Call Graph:
ENTRY - Entry Point
  0F900h - External
  LEA91
  LE3EF
    LEA91
  LE8BB
    LEA91
    LE972
      0FF5Dh - External
  PRNSTRZ
    CHROUT
      LE15C
  LE1AB
  LE40D
    LE5FA
    LE5F1
      LE603
        LE620
    LE46B
      LE75F
      LE77A
      LE790
    LE43C
    LE5D6
      LE5F1
        LE603
          LE620
      LE620
    LE5AC
      LE5D6
        LE5F1
          LE603
            LE620
        LE620
      LE627
        LE603
          LE620
  LE1E2
  LE1A6
    LE1D8
  LE1DD
  LE1E7
    LE454
      LE70D
        LE745
        LE75F
        LE77A
        LE7C7
          LE7A8
      LE48C
        LE5D6
          LE5F1
            LE603
              LE620
          LE620
        LE630
          LEA91
        LE567
          LE5D6
            LE5F1
              LE603
                LE620
            LE620
          LE627
            LE603
              LE620
        LE5E5
          LE620
        LE5AC
          LE5D6
            LE5F1
              LE603
                LE620
            LE620
          LE627
            LE603
              LE620
  LE1D8
