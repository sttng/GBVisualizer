INCLUDE "hardware.inc"

current_music_pause EQU $90
music_pointer EQU $91
done_flag EQU $93
pcm_read_index EQU $94
pcm_write_index EQU $95
pcm_ram_start EQU $c000

SECTION "VBlank", ROM0[$40]
    jp VBlank

SECTION "OAMInt", ROM0[$48]
    jp OAMInt

SECTION "Timer", ROM0[$50]
Timer::
    ; Point HL to the write position
    ld h, pcm_ram_start / $100
    ldh a, [pcm_write_index]
    ld l, a

    ; Advance the index
    inc a
    ldh [pcm_write_index],  a

    ; Sum the four nibbles in registers PCM12 and PCM34
    ldh a, [rPCM12]
    ld b, a
    ldh a, [rPCM34]
    ld d, a
    and $F
    ld c, a

    ld a, d

    swap a
    and $F
    add c
    ld c, a

    ld a, b

    and $F
    add c
    ld c, a

    ld a, b

    swap b
    and $F
    add c

    ; Store result to hl
    ld [hl], a

    reti

SECTION "Header", ROM0[$100]

Start::
    di
    jp _Start

SECTION "Home", ROM0[$150]

_Start::
    ; Increase the CPU speed from 4MHz to 8MHz
    ld a, 1
    ldh [rKEY1], a
    stop

    ; Init the stack
    ld sp, $fffe

    ; Init buffer pointers
    xor a
    ldh [pcm_write_index], a
    ldh [pcm_read_index], a

    ; Other inits
    call LCDOff
    call LoadGraphics
	call LoadEquTiles
    call InitSound
    call LoadMap
    call LoadPalette
    call InitTimer

    ; Start Playing
    call InitMusic
    jp Main

WaitFrame::
    ldh a, [rLY]
    and a
    jr nz, WaitFrame
    ; Fall through

WaitVBlank::
    ldh a, [rLY]
    cp 145
    jr nz, WaitVBlank
    ret

LCDOff::
    call WaitVBlank
    ldh a, [rLCDC]
    and $7F
    ldh [rLCDC], a
    ret

LCDOn::
    di
    ldh a, [rLCDC]
    or $80
    ldh [rLCDC], a
    call WaitVBlank
    xor a
    ldh [rIF], a
    reti

InitTimer::
    ld a, $100-30 ; 262144 / 144*60 is about 30
    ldh [rTIMA], a
    ldh [rTMA], a
    ld a, 5 ; Enabled, 262144
    ldh [rTAC], a
    ret

InitSound::
    xor a
    ldh [rNR10], a
    ld a, $80
    ldh [rNR11], a
    ldh [rNR21], a
    ld a, $20
    ldh [rNR32], a

; Make the waveform square.
    ld a, $FF
    ld hl, $FF30

    ld b, $8
.loop
    ld [hli], a
    dec b
    jr nz, .loop

    xor a
    ld b, $8
.loop2
    ld [hli], a
    dec b
    jr nz, .loop2
    ret

LoadGraphics::
    ld de, Tiles
    ld hl, $8000
    ld bc, TilesEnd - Tiles
CopyTiles:
    ld a, [de]
    ld [hli], a
    inc de
    dec bc
    ld a, b
    or a, c
    jp nz, CopyTiles
    ret ;done

LoadEquTiles::
    ld de, EquTiles
    ld hl, $8800
    ld bc, EquTilesEnd - EquTiles
CopyEquTiles:
    ld a, [de]
    ld [hli], a
    inc de
    dec bc
    ld a, b
    or a, c
    jp nz, CopyEquTiles
    ret ;done

LoadPalette::
    ld hl, Colors
    ld a, $80
    ldh [rBGPI], a
    ld b, 64
.loop
    ld a, [hli]
    ldh [rBGPD], a
    dec b
    jr nz, .loop
    ret


LoadMap::
    ld de, Tilemap
    ld hl, $9800
    ld bc, TilemapEnd - Tilemap
CopyTilemap:
    ld a, [de]
    ld [hli], a
    inc de
    dec bc
    ld a, b
    or a, c
    jp nz, CopyTilemap
    ret ;done

Colors::
    dw $65C3
    dw $077F
    dw $5BDF
    dw $2108

Tiles::
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$0F,$00,$08,$00,$08,$00,$08,$00,$08,$00,$08
DB $00,$00,$00,$00,$00,$FF,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$FF,$00,$08,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$FF,$00,$10,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$FF,$00,$20,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$FF,$00,$40,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$FF,$00,$80,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$FF,$00,$01,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$FF,$00,$02,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$FF,$00,$04,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$F8,$00,$08,$00,$08,$00,$08,$00,$08,$00,$08
DB $00,$08,$00,$08,$00,$08,$00,$08,$00,$08,$00,$08,$00,$08,$00,$08
DB $00,$80,$00,$00,$00,$00,$00,$00,$00,$00,$00,$80,$00,$00,$00,$00
DB $00,$08,$00,$08,$00,$0C,$00,$08,$00,$08,$00,$08,$00,$08,$00,$08
DB $00,$00,$00,$00,$00,$80,$00,$00,$00,$00,$00,$00,$00,$00,$00,$80
DB $00,$08,$00,$08,$00,$18,$00,$08,$00,$08,$00,$08,$00,$08,$00,$08
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$80,$00,$00,$00,$00,$00,$00
DB $00,$08,$00,$0C,$00,$08,$00,$08,$00,$08,$00,$08,$00,$08,$00,$08
DB $00,$00,$00,$80,$00,$00,$00,$00,$00,$00,$00,$00,$00,$80,$00,$00
DB $00,$08,$00,$18,$00,$08,$00,$08,$00,$08,$00,$08,$00,$08,$00,$08
DB $00,$00,$00,$00,$00,$00,$00,$80,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$0C,$00,$08,$00,$08,$00,$08,$00,$08,$00,$08,$00,$08,$00,$08
DB $00,$18,$00,$08,$00,$08,$00,$08,$00,$08,$00,$08,$00,$08,$00,$08
DB $00,$08,$00,$08,$00,$08,$00,$08,$00,$08,$00,$08,$00,$08,$00,$0C
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$21
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$08
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$42
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$10
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$84
DB $00,$00,$00,$00,$00,$80,$00,$00,$00,$00,$00,$00,$00,$00,$00,$84
DB $00,$08,$00,$08,$00,$08,$00,$08,$00,$08,$00,$08,$00,$08,$00,$18
DB $00,$08,$00,$08,$00,$08,$00,$08,$00,$08,$00,$08,$00,$0C,$00,$08
DB $00,$08,$00,$08,$00,$08,$00,$08,$00,$08,$00,$08,$00,$18,$00,$08
DB $00,$08,$00,$08,$00,$08,$00,$08,$00,$08,$00,$0C,$00,$08,$00,$08
DB $00,$08,$00,$08,$00,$08,$00,$08,$00,$08,$00,$18,$00,$08,$00,$08
DB $00,$08,$00,$08,$00,$08,$00,$08,$00,$0C,$00,$08,$00,$08,$00,$08
DB $00,$08,$00,$08,$00,$08,$00,$08,$00,$18,$00,$08,$00,$08,$00,$08
DB $00,$08,$00,$08,$00,$08,$00,$08,$00,$0F,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$FF,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$08,$00,$FF,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$10,$00,$FF,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$20,$00,$FF,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$40,$00,$FF,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$80,$00,$FF,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$01,$00,$FF,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$02,$00,$FF,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$04,$00,$FF,$00,$00,$00,$00,$00,$00
DB $00,$08,$00,$08,$00,$08,$00,$08,$00,$F8,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$18,$18,$3C,$3C,$7E,$3C,$7E,$18,$3C,$00,$18,$FF,$FF
TilesEnd::

Tilemap::
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $01,$02,$03,$02,$04,$02,$05,$02,$06,$02,$07,$08,$02,$09,$02,$0A
DB $02,$03,$02,$0B,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $0C,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0D,$00,$00,$00,$00,$00
DB $00,$00,$00,$0C,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $0E,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0F,$00,$00,$00,$00,$00
DB $00,$00,$00,$10,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $0C,$00,$00,$00,$00,$00,$00,$00,$00,$00,$11,$00,$00,$00,$00,$00
DB $00,$00,$00,$0C,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $12,$00,$00,$00,$00,$00,$00,$00,$00,$00,$13,$00,$00,$00,$00,$00
DB $00,$00,$00,$14,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $0C,$00,$00,$00,$00,$00,$00,$00,$00,$00,$15,$00,$00,$00,$00,$00
DB $00,$00,$00,$0C,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $16,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0D,$00,$00,$00,$00,$00
DB $00,$00,$00,$17,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $18,$19,$1A,$1B,$1C,$1D,$19,$1A,$1B,$1C,$1E,$19,$1A,$1B,$1C,$1D
DB $19,$1A,$1B,$1F,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $0C,$00,$00,$00,$00,$00,$00,$00,$00,$00,$11,$00,$00,$00,$00,$00
DB $00,$00,$00,$0C,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $20,$00,$00,$00,$00,$00,$00,$00,$00,$00,$13,$00,$00,$00,$00,$00
DB $00,$00,$00,$21,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $0C,$00,$00,$00,$00,$00,$00,$00,$00,$00,$15,$00,$00,$00,$00,$00
DB $00,$00,$00,$0C,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $22,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0D,$00,$00,$00,$00,$00
DB $00,$00,$00,$23,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $0C,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0F,$00,$00,$00,$00,$00
DB $00,$00,$00,$0C,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $24,$00,$00,$00,$00,$00,$00,$00,$00,$00,$11,$00,$00,$00,$00,$00
DB $00,$00,$00,$25,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $0C,$00,$00,$00,$00,$00,$00,$00,$00,$00,$13,$00,$00,$00,$00,$00
DB $00,$00,$00,$0C,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $26,$27,$28,$27,$29,$27,$2A,$27,$2B,$27,$2C,$2D,$27,$2E,$27,$2F
DB $27,$28,$27,$30,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$31,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

TilemapEnd::

EquTiles:
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$FF,$FF
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$FF,$FF
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$FF,$FF,$00,$FF
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$FF,$FF,$00,$FF
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$FF,$FF,$00,$FF,$00,$FF
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$FF,$FF,$00,$FF,$00,$FF
DB $00,$00,$00,$00,$00,$00,$00,$00,$FF,$FF,$00,$FF,$00,$FF,$00,$FF
DB $00,$00,$00,$00,$00,$00,$00,$00,$FF,$FF,$00,$FF,$00,$FF,$00,$FF
DB $00,$00,$00,$00,$00,$00,$FF,$FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF
DB $00,$00,$00,$00,$00,$00,$FF,$FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF
DB $00,$00,$00,$00,$FF,$FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF
DB $00,$00,$00,$00,$FF,$FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF
DB $00,$00,$FF,$FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF
DB $00,$00,$FF,$FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF
DB $FF,$FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF
DB $FF,$FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF
EquTilesEnd:

VBlank::
    ; Sync pointers
    ldh a, [pcm_read_index]
    ldh [pcm_write_index], a
    call HandleMusic
    reti

OAMInt::
    ; Point HL to the read position
    ld h, pcm_ram_start / $100
    ldh a, [pcm_read_index]
    ld l, a

    ; Advance the index
    inc a
    ldh [pcm_read_index],  a

    ; Read
    ld a, [hl]

    ; Negate
    xor $FF
    inc a

    ; Update SCX
    ;ldh [rSCX], a

    ld b, a
    AND a, $0F ;get 1st nibble
    ld hl, $98A5 ;Which map tile to change
    add 128
    ld [hli], a

    ld a, b
    AND a, $F0
    srl a
    srl a
    srl a
    srl a    ;get 2nd nibble and shift
    ld hl, $98AE ;Which map tile to change
    add 128
    ld [hli], a


; 2nd PCM register for nibble 3 and nibble 4 ? need to check
    ; Point HL to the read position
    ld h, pcm_ram_start / $100
    ldh a, [pcm_read_index]
    ld l, a

    ; Advance the index
    inc a
    ldh [pcm_read_index],  a

    ; Read
    ld a, [hl]

    ; Negate
    xor $FF
    inc a

    ; Read
    ld a, [hl]

    ; Negate
    xor $FF
    inc a

    ; Update SCX
    ;ldh [rSCX], a

    ld b, a
    AND a, $0F ;get 1st nibble
    ld hl, $9985 ;Which map tile to change
    add 128
    ld [hli], a

    ld a, b
    AND a, $F0
    srl a
    srl a
    srl a
    srl a    ;get 2nd nibble and shift
    ld hl, $998E ;Which map tile to change
    add 128
    ld [hli], a



    ;sprite movement
    ld [_OAMRAM ], a  
    reti

InitMusic::


    ld hl, _OAMRAM
    ld a, 64 + 16
    ld [hli], a
    ld a, 16 + 8
    ld [hli], a
    ld a, $31
    ld [hli], a
    ld [hl], a






    xor a
    ldh [done_flag], a

    ; Init the music pointer
    ld a, Music >> 8
    ldh [music_pointer + 1], a
    ld a, Music & $FF
    ldh [music_pointer], a

    ; Clear pending interrupts
    ldh [rIF], a

    ; Enable interrupts
    ld a, 7
    ldh [rIE], a
    ld a, 32
    ldh [rSTAT], a
    call LCDOn
    ret

; The main loop. We only operate inside interrupts, we just constantly halt.
Main::
    halt
    jr Main

HandleMusic:
    ; The music system  is a simple array of  2-byte items.  The first byte is a
    ; pointer to HRAM (which includes our  sound registers) and second byte is a
    ; value that should be written into this pointer.
    ; We also define two variables in the HRAM:  current_music_pause, which will
    ; stop this loop when it's non-zero, and cause an n-frame pause in the music
    ; until the next byte is written.  The second variable, done_flag, reset the
    ; music pointer to loop the song.
    ; This function will never return without  writing to current_music_pause or
    ; the done_flag.
    ldh a, [music_pointer + 1]
    ld h, a
    ldh a, [music_pointer]
    ld l, a

.loop
    ldh a, [done_flag]
    and a
    jr nz, InitMusic
    ldh a, [current_music_pause]
    and a
    jr nz, .exit

    ld a, [hli]
    ld c, a
    ld a, [hli]
    ld [c], a
    jr .loop

.exit
    dec a
    ldh [current_music_pause], a
    ld a, h
    ldh [music_pointer + 1], a
    ld a, l
    ldh [music_pointer], a
    ret

Music::
INCBIN "music.gbm"
db done_flag, $01 ; loop