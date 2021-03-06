.segment "HEADER"

INES_MAPPER = 4 ; 4 = MMC3
INES_MIRROR = 1 ; 0 = horizontal mirroring, 1 = vertical mirroring
INES_SRAM   = 0 ; 1 = battery backed SRAM at $6000-7FFF

.byte 'N', 'E', 'S', $1A ; ID
.byte $02 ; 16k PRG chunk count
.byte $01 ; 8k CHR chunk count
.byte INES_MIRROR | (INES_SRAM << 1) | ((INES_MAPPER & $f) << 4)
.byte (INES_MAPPER & %11110000)
.byte $0, $0, $0, $0, $0, $0, $0, $0 ; padding

.segment "TILES"
.incbin "myrna.chr"

.segment "VECTORS"
.word nmi
.word reset
.word irq

.segment "CODE"
reset:
  sei       ; disable interrupts during game initialization
  lda #0
  sta $2000 ; disable NMI
  sta $2001 ; disable rendering
  sta $4015 ; disable APU sound
  sta $4010 ; disable DMC IRQ
  lda #$40
  sta $4017 ; disable APU IRQ
  cld       ; disable decimal mode
  ldx #$FF
  txs       ; initialize the stack
  ; wait for the first vblank
  bit $2002
  :
    bit $2002
    bpl :-
  ; reset all RAM
  lda #0
  ldx #0
  :
    sta $0000, X
    sta $0100, X
    sta $0200, X
    sta $0300, X
    sta $0400, X
    sta $0500, X
    sta $0600, X
    sta $0700, X
    inx
    bne :-
  ; place all sprites offscreen at Y=255
  lda #255
  ldx #0
  :
    sta oam, X
    inx
    inx
    inx
    inx
    bne :-
  ; wait for the second vblank
  :
    bit $2002
    bpl :-
  ; NES is initialized, ready to begin!
  ; enable the NMI for graphical updates, and jump to our main program
  lda #%10001000
  sta $2000

  ; clear the interrupt disable flag to enable irqs
  cli
  jmp main

;
; nmi routine
;

.segment "ZEROPAGE"
nmi_lock:       .res 1 ; prevents NMI re-entry
nmi_count:      .res 1 ; is incremented every NMI
nmi_ready:      .res 1 ; set to 1 to push a PPU frame update, 2 to turn rendering off next NMI
nmt_update_len: .res 1 ; number of bytes in nmt_update buffer
scroll_x:       .res 1 ; x scroll position
scroll_y:       .res 1 ; y scroll position
scroll_nmt:     .res 1 ; nametable select (0-3 = $2000,$2400,$2800,$2C00)
temp:           .res 1 ; temporary variable
background_pointer: .res 2

.segment "BSS"
nmt_update: .res 256 ; nametable update entry buffer for PPU update
palette:    .res 32  ; palette buffer for PPU update

.segment "OAM"
oam: .res 256        ; sprite OAM data to be uploaded by DMA

.segment "CODE"
nmi:
  ; save registers
  pha
  txa
  pha
  tya
  pha
  ; prevent NMI re-entry
  lda nmi_lock
  beq :+
    jmp @nmi_end
  :
  lda #1
  sta nmi_lock
  ; increment frame counter
  inc nmi_count
  ;
  lda nmi_ready
  bne :+ ; nmi_ready == 0 not ready to update PPU
   jmp @ppu_update_end
  :
  cmp #2 ; nmi_ready == 2 turns rendering off
  bne :+
    lda #%00000000
    sta $2001
    ldx #0
    stx nmi_ready
    jmp @ppu_update_end
  :
  ; sprite OAM DMA
  ldx #0
  stx $2003
  lda #>oam
  sta $4014
  ; palettes
  lda #%10001000
  sta $2000 ; set horizontal nametable increment
  lda $2002
  lda #$3F
  sta $2006
  stx $2006 ; set PPU address to $3F00
  ldx #0
  :
    lda palette, X
    sta $2007
    inx
    cpx #32
    bcc :-
@scroll:
  inc scroll_x
  clc

  ; when the scroll has exceeded a nametable's tiles, switch the nametable
  ; that is being scrolled
  ldx scroll_x
  cpx #0
  bne @NotScrolledAtNametableStart
    lda scroll_nmt
    cmp #0
    bne @DecScrollNmt
    inc scroll_nmt
    jmp @NotScrolledAtNametableStart
    @DecScrollNmt:
    dec scroll_nmt
  @NotScrolledAtNametableStart:
  lda scroll_nmt
  and #%00000011 ; keep only lowest 2 bits to prevent error
  ora #%10001000
  sta $2000

  lda scroll_x
  sta $2005
  lda scroll_y
  sta $2005
  ; enable rendering
  lda #%00011110
  sta $2001
  ; flag PPU update complete
  ldx #0
  stx nmi_ready

  ; Write to $E000 to acknowledge any currently pending interrupts
  lda #1
  sta $E000
  ; setup MMC3 to generate an IRQ after
  ; 120 scanlines
  lda #120
  sta $C000
  sta $C001
  ; Write to $E000 again to latch in the countdown value
  lda #1
  sta $E000
  ; Write to $E001 to enable the IRQ counter
  lda #1
  sta $E001
@ppu_update_end:
  ; if this engine had music/sound, this would be a good place to play it
  ; unlock re-entry flag
  lda #0
  sta nmi_lock
@nmi_end:
  ; restore registers and return
  pla
  tay
  pla
  tax
  pla
  rti

.segment "CODE"
irq:
  ; store a, x, and y register values onto the stack during IRQ
  pha
  txa
  pha
  tya
  pha
  lda scroll_nmt
  and #%00000011 ; keep only lowest 2 bits to prevent error
  ora #%10001000
  sta $2000
  lda #0
  sta $2005
  lda #0
  sta $2005
  ; restore a, x, and y register values
  pla
  tay
  pla
  tax
  pla
  rti

.segment "CODE"

; ppu_update: waits until next NMI, turns rendering on (if not already), uploads OAM, palette, and nametable update to PPU
ppu_update:
  lda #1
  sta nmi_ready
  :
    lda nmi_ready
    bne :-
  rts

.segment "ZEROPAGE"

.segment "RODATA"
example_palette:
.byte $21,$26,$37,$2d ; sky / airplane
.byte $21,$09,$27,$37 ; sand
.byte $21,$01,$11,$21
.byte $21,$00,$10,$30 ;
.byte $21,$27,$37,$3E ;
.byte $21,$14,$24,$34 ;
.byte $21,$1B,$2B,$3B ; sp2 teal
.byte $21,$12,$22,$32 ;

.segment "ZEROPAGE"

.segment "CODE"
; The main program, run after the NES is initialized
main:
  ; Write the palette to
  ldx #0
  :
    lda example_palette, X
    sta palette, X
    inx
    cpx #32
    bcc :-
  jsr setup_background
  jsr ppu_update
; The main loop
@GameLoop:
  lda #0
  jsr ppu_update
  jmp @GameLoop

; nametable data
nametable_1_background:
  ; tile data
  .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
  .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
  .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
  .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
  .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
  .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
  .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
  .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
  .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
  .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
  .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
  .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
  .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
  .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
  .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
  .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
  .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
  .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
  .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
  .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
  .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
  .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
  .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
  .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
  .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
  .byte $32, $33, $34, $35, $36, $32, $32, $33, $32, $34, $32, $35, $34, $33, $34, $33, $34, $32, $33, $34, $32, $33, $32, $33, $33, $34, $34, $35, $36, $36, $35, $36
  .byte $27, $28, $27, $27, $27, $2a, $27, $27, $27, $27, $27, $27, $28, $27, $27, $27, $27, $27, $2a, $27, $28, $27, $27, $27, $2c, $27, $27, $27, $2a, $27, $27, $27
  .byte $27, $27, $27, $27, $27, $38, $27, $27, $27, $2b, $27, $27, $27, $27, $2c, $27, $27, $2c, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $28, $27
  .byte $2b, $27, $27, $29, $27, $27, $2b, $27, $27, $27, $27, $27, $27, $27, $27, $27, $28, $27, $27, $27, $27, $2c, $27, $27, $2b, $27, $27, $27, $27, $27, $27, $27
  .byte $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27
  ; Attribute table data
  .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
  .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101

nametable_2_background:
; tile data
  .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
  .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
  .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
  .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
  .byte $00, $00, $00, $00, $61, $62, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
  .byte $00, $00, $00, $70, $71, $72, $17, $23, $1c, $18, $0b, $73, $0b, $1e, $73, $1e, $1c, $19, $19, $1d, $1e, $73, $10, $0f, $0c, $73, $02, $06, $73, $0a, $1a, $17
  .byte $00, $00, $00, $80, $81, $82, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
  .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
  .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
  .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
  .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
  .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
  .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
  .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
  .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
  .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
  .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
  .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
  .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
  .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
  .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
  .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
  .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
  .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
  .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
  .byte $32, $33, $34, $35, $36, $32, $32, $33, $32, $34, $32, $35, $34, $33, $34, $33, $34, $32, $33, $34, $32, $33, $32, $33, $33, $34, $34, $35, $36, $36, $35, $36
  .byte $27, $28, $27, $27, $27, $2a, $27, $27, $27, $27, $27, $27, $28, $27, $27, $27, $27, $27, $2a, $27, $28, $27, $27, $27, $2c, $27, $27, $27, $2a, $27, $27, $27
  .byte $27, $27, $27, $27, $27, $38, $27, $27, $27, $2b, $27, $27, $27, $27, $2c, $27, $27, $2c, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $28, $27
  .byte $2b, $27, $27, $29, $27, $27, $2b, $27, $27, $27, $27, $27, $27, $27, $27, $27, $28, $27, $27, $27, $27, $2c, $27, $27, $2b, $27, $27, $27, $27, $27, $27, $27
  .byte $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27, $27
  ; Attribute table data
  .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
  .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101

setup_background:
  lda $2002 ; reset PPU latch
  lda #$20 ; Write to the first nametable at #$2000
  sta $2006
  lda #$00
  sta $2006
  ; store the memory address of the background in two bytes
  ; at background_pointer and background_pointer + 1
  lda #<nametable_1_background
  sta background_pointer
  lda #>nametable_1_background
  sta background_pointer + 1

  ; Loop 1024 times to write the entire first nametable.

  ; y is a counter that increases with each loop iteration.
  ldy #0
  ; since the y register can only hold 256 values, we use
  ; a secondary counter, x, which counts how many times
  ; y has gone around the 0-256 cycle. The loop will complete
  ; after 4 cycles of x, 1024 interations.
  ldx #0
  @NametableLoop:
    lda (background_pointer), y ; Load the nametable data at the pointer
    sta $2007 ; Write it to the PPU
    iny
    cpy #0
    bne @NametableLoop
    inc background_pointer + 1 ; increase the high bit of the pointer
    inx
    cpx #4
    bne @NametableLoop

  ; same thing for second nametable
  lda #<nametable_2_background
  sta background_pointer
  lda #>nametable_2_background
  sta background_pointer + 1

  ldy #0
  ldx #0
  @NametableLoop2:
    lda (background_pointer), y ; Load the nametable data at the pointer
    sta $2007 ; Write it to the PPU
    iny
    cpy #0
    bne @NametableLoop2
    inc background_pointer + 1 ; increase the high bit of the pointer
    inx
    cpx #4
    bne @NametableLoop2
  rts