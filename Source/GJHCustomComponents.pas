unit GJHCustomComponents;

{
GJH Custom Components V1.00
Copyright (C) 2022-23 Gerald Holdsworth gerald@hollypops.co.uk

This source is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public Licence as published by the Free
Software Foundation; either version 3 of the Licence, or (at your option)
any later version.

This code is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE.  See the GNU General Public Licence for more
details.

A copy of the GNU General Public Licence is available on the World Wide Web
at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
to the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
Boston, MA 02110-1335, USA.
}

{$mode ObjFPC}{$H+}

interface

uses
 Classes, SysUtils, Graphics, ExtCtrls, Controls, Registry, StrUtils;

{$M+}

//Global constants
const
 csHorizontal = 0;
 csVertical   = 1;

//RISC OS style tick boxes - declarations ++++++++++++++++++++++++++++++++++++++
type
 TGJHTickBoxes = class(TGraphicControl)
 private
  FOnlyMouse,
  FTicked   : Boolean;
  FOn,
  FOff      : TPortableNetworkGraphic;
  FOnChange : TNotifyEvent;
  FCaption  : String;
  FColour   : TColor;
  procedure SetWidth(const LCaption: String);
  procedure SetTicked(const LTicked: Boolean);
 const
  FGap = 4;
 protected
  procedure Paint; override;
  procedure Click; override;
 published
  //Methods
  constructor Create(AOwner: TComponent); override;
  //Events
  property OnChange: TNotifyEvent read FOnChange  write FOnChange;
  //Properties
  property Caption:  String       read FCaption   write SetWidth;
  property Colour:   TColor       read FColour    write FColour    default clNone;
  property OnlyMouse:Boolean      read FOnlyMouse write FOnlyMouse default False;
  property Ticked:   Boolean      read FTicked    write SetTicked  default False;
 public
  destructor Destroy; override;
 end;

//Tick box - declarations ++++++++++++++++++++++++++++++++++++++++++++++++++++++
type
 TGJHTickBox = class(TGJHTickBoxes)
 private
  const
   FTickBoxOff: array[0..207] of Byte=(
$89,$50,$4E,$47,$0D,$0A,$1A,$0A,$00,$00,$00,$0D,$49,$48,$44,$52,$00,$00,$00,$2C,
$00,$00,$00,$2C,$08,$02,$00,$00,$00,$91,$E6,$CD,$56,$00,$00,$00,$06,$74,$52,$4E,
$53,$00,$FC,$00,$FC,$00,$FC,$EB,$B7,$8E,$4E,$00,$00,$00,$85,$49,$44,$41,$54,$78,
$9C,$ED,$CE,$31,$0A,$C3,$30,$10,$44,$51,$DF,$54,$A0,$5B,$08,$74,$0B,$81,$4E,$21,
$D0,$8D,$5C,$6E,$91,$66,$D3,$AC,$D5,$6C,$20,$76,$65,$12,$42,$4A,$A9,$F9,$BF,$19,
$A6,$7B,$DB,$6B,$75,$EE,$BE,$9D,$33,$C6,$38,$16,$65,$66,$31,$C6,$0B,$71,$9E,$E7,
$8A,$54,$35,$E7,$FC,$81,$78,$CC,$4D,$44,$6A,$AD,$3F,$10,$FB,$C4,$7A,$EF,$20,$40,
$80,$00,$01,$02,$04,$08,$10,$20,$40,$80,$00,$01,$02,$04,$08,$10,$20,$40,$FC,$47,
$88,$48,$9F,$58,$6B,$ED,$1B,$A1,$AA,$75,$7A,$A5,$94,$1B,$61,$66,$79,$51,$29,$A5,
$0B,$E1,$EE,$71,$69,$21,$84,$37,$74,$EA,$BD,$D6,$F8,$15,$FC,$2B,$00,$00,$00,$00,
$49,$45,$4E,$44,$AE,$42,$60,$82);
   FTickBoxOn: array[0..496] of Byte=(
$89,$50,$4E,$47,$0D,$0A,$1A,$0A,$00,$00,$00,$0D,$49,$48,$44,$52,$00,$00,$00,$2C,
$00,$00,$00,$2C,$08,$02,$00,$00,$00,$91,$E6,$CD,$56,$00,$00,$00,$06,$74,$52,$4E,
$53,$00,$F8,$00,$F8,$00,$F8,$1E,$53,$44,$4B,$00,$00,$01,$A6,$49,$44,$41,$54,$78,
$9C,$ED,$96,$B1,$8D,$83,$30,$18,$85,$BD,$00,$0B,$64,$00,$16,$C8,$02,$59,$20,$03,
$44,$BA,$9A,$06,$E9,$26,$88,$74,$19,$80,$48,$D7,$B2,$00,$0B,$64,$80,$2C,$10,$31,
$04,$91,$2B,$52,$58,$82,$C2,$12,$14,$F7,$12,$74,$E4,$97,$43,$08,$18,$DB,$34,$BC,
$26,$8E,$63,$8B,$0F,$3F,$FF,$EF,$0F,$DB,$ED,$76,$5F,$B3,$4A,$4A,$C9,$F0,$11,$86,
$E1,$F7,$4C,$12,$42,$54,$55,$75,$87,$C0,$97,$FD,$7E,$FF,$E3,$5C,$9C,$F3,$A2,$28,
$9E,$10,$98,$3A,$1E,$8F,$BF,$0E,$95,$A6,$69,$9E,$E7,$2A,$04,$7E,$B8,$38,$54,$96,
$65,$0B,$C4,$02,$31,$1F,$C4,$F9,$7C,$3E,$9D,$4E,$F3,$40,$44,$51,$B4,$5E,$AF,$57,
$AB,$15,$63,$2C,$08,$02,$D7,$10,$78,$F5,$CD,$66,$C3,$FE,$85,$71,$E7,$32,$8B,$10,
$20,$F0,$7D,$BF,$25,$C0,$18,$33,$AE,$21,$28,$81,$E7,$79,$9D,$B7,$C1,$2E,$04,$1A,
$32,$23,$4A,$92,$A4,$67,$B1,$15,$88,$38,$8E,$29,$C1,$E1,$70,$E8,$5F,$6F,$1E,$02,
$C6,$37,$85,$D0,$68,$BB,$DD,$7E,$DC,$62,$1E,$82,$1A,$81,$CA,$1C,$B2,$C5,$30,$04,
$22,$A1,$25,$C0,$79,$BC,$2B,$07,$8B,$10,$8A,$11,$B8,$19,$03,$37,$9A,$84,$A0,$46,
$74,$26,$A3,$75,$08,$5A,$11,$48,$88,$51,$7B,$CD,$40,$50,$23,$FA,$73,$C9,$22,$04,
$35,$E2,$63,$2A,$4C,$85,$40,$F0,$35,$0D,$89,$5E,$3A,$6A,$C4,$BB,$16,$65,$0C,$02,
$AF,$88,$A3,$56,$02,$00,$46,$B4,$93,$18,$0C,$AC,$49,$7D,$08,$3C,$98,$86,$71,$73,
$18,$B4,$53,$23,$24,$34,$08,$C6,$41,$28,$3D,$09,$4C,$38,$9B,$89,$46,$8C,$86,$A0,
$69,$A8,$48,$A3,$22,$34,$21,$20,$1A,$88,$54,$A3,$A2,$69,$2A,$84,$E2,$88,$5E,$34,
$4D,$85,$C0,$99,$BF,$42,$0C,$EF,$11,$66,$20,$2E,$2F,$35,$32,$E4,$EF,$82,$79,$08,
$7A,$3D,$B5,$83,$61,$2A,$C4,$E5,$91,$9B,$F1,$43,$46,$08,$34,$21,$8C,$6B,$81,$58,
$20,$86,$41,$A4,$69,$9A,$39,$D4,$F5,$7A,$55,$21,$38,$E7,$B9,$73,$DD,$6E,$B7,$27,
$84,$10,$A2,$98,$49,$65,$59,$DE,$21,$A4,$94,$D5,$AC,$AA,$EB,$FA,$0F,$5F,$A0,$16,
$44,$FF,$92,$F9,$9A,$00,$00,$00,$00,$49,$45,$4E,$44,$AE,$42,$60,$82);
 published
  constructor Create(AOwner: TComponent); override;
 public
  destructor Destroy; override;
end;

//Radio box - declarations +++++++++++++++++++++++++++++++++++++++++++++++++++++
type
 TGJHRadioBox = class(TGJHTickBoxes)
 private
  const
   FRadioOff: array[0..920] of Byte=(
$89,$50,$4E,$47,$0D,$0A,$1A,$0A,$00,$00,$00,$0D,$49,$48,$44,$52,$00,$00,$00,$2C,
$00,$00,$00,$2C,$08,$02,$00,$00,$00,$91,$E6,$CD,$56,$00,$00,$00,$06,$74,$52,$4E,
$53,$00,$00,$00,$00,$00,$00,$6E,$A6,$07,$91,$00,$00,$03,$4E,$49,$44,$41,$54,$78,
$9C,$ED,$97,$DD,$2B,$BB,$61,$18,$C7,$FD,$AD,$CA,$C1,$0E,$94,$03,$65,$72,$20,$8A,
$64,$91,$44,$14,$45,$14,$59,$4D,$D4,$48,$14,$79,$17,$79,$CB,$6B,$D8,$66,$86,$83,
$6D,$48,$4C,$44,$F1,$69,$57,$5D,$DD,$AD,$DF,$F3,$3C,$7B,$B6,$DB,$D1,$CF,$F7,$60,
$DD,$3D,$AD,$5D,$9F,$E7,$BA,$AF,$97,$EF,$2A,$2A,$FE,$F4,$27,$2B,$BA,$BF,$BF,$CF,
$66,$B3,$CF,$CF,$CF,$2F,$2F,$2F,$7C,$A6,$D3,$E9,$BB,$BB,$BB,$64,$32,$F9,$EB,$81,
$6F,$6F,$6F,$09,$F9,$F1,$F1,$F1,$9D,$D7,$D7,$D7,$D7,$FB,$FB,$7B,$2E,$97,$03,$E2,
$F1,$F1,$11,$0E,$C8,$52,$A9,$54,$22,$91,$B8,$BA,$BA,$8A,$C5,$62,$F6,$09,$9E,$9E,
$9E,$BE,$0D,$B9,$10,$5C,$5E,$5E,$9E,$9F,$9F,$9F,$9C,$9C,$F0,$69,$2D,$3C,$09,$D0,
$B7,$2F,$9E,$E0,$F0,$F0,$70,$77,$77,$77,$7F,$7F,$9F,$27,$E5,$12,$14,$24,$40,$08,
$08,$9F,$C9,$64,$88,$7A,$73,$73,$73,$7D,$7D,$9D,$C8,$2B,$1E,$8F,$73,$0B,$17,$17,
$17,$4A,$B0,$BD,$BD,$BD,$B1,$B1,$B1,$BE,$BE,$7E,$70,$70,$50,$3A,$41,$41,$02,$D0,
$DB,$DB,$1B,$05,$18,$77,$15,$AF,$4E,$02,$84,$60,$75,$75,$75,$69,$69,$69,$61,$61,
$61,$6D,$6D,$CD,$4E,$0E,$1E,$1E,$1E,$DC,$C3,$AB,$A8,$4A,$92,$A1,$04,$73,$73,$73,
$33,$33,$33,$60,$F9,$23,$A0,$0E,$CC,$F0,$A4,$84,$27,$45,$12,$A8,$4E,$4F,$4F,$81,
$10,$82,$E9,$E9,$E9,$A9,$A9,$29,$1F,$A5,$CA,$35,$9B,$17,$C1,$99,$27,$7E,$09,$44,
$54,$C9,$FC,$FC,$3C,$04,$91,$48,$24,$1C,$0E,$47,$A3,$D1,$62,$21,$A8,$79,$33,$0D,
$25,$E4,$C0,$D4,$D1,$D1,$91,$10,$8C,$8D,$8D,$8D,$8C,$8C,$90,$1B,$6F,$02,$9A,$AD,
$B4,$3A,$70,$D1,$F2,$F2,$B2,$10,$0C,$0D,$0D,$0D,$0C,$0C,$50,$2E,$1E,$10,$B4,$9F,
$D9,$0B,$E5,$13,$C4,$F3,$FD,$42,$26,$84,$A0,$BF,$BF,$9F,$E2,$F0,$80,$F8,$FC,$FC,
$D4,$79,$C0,$24,$B0,$02,$81,$F6,$F6,$F6,$84,$A0,$A7,$A7,$07,$1A,$37,$02,$0A,$50,
$09,$58,$13,$B6,$08,$44,$5C,$07,$04,$5D,$5D,$5D,$1D,$1D,$1D,$DE,$05,$21,$53,$99,
$61,$6C,$17,$82,$D6,$10,$82,$F6,$F6,$F6,$AD,$AD,$2D,$47,$08,$06,$94,$EE,$05,$8B,
$77,$21,$62,$80,$0A,$41,$6B,$6B,$EB,$EC,$EC,$AC,$23,$04,$57,$A0,$9B,$09,$73,$60,
$17,$82,$89,$29,$04,$CD,$CD,$CD,$A3,$A3,$A3,$8E,$10,$B9,$BC,$64,$37,$DA,$25,$40,
$C7,$C7,$C7,$42,$D0,$D8,$D8,$D8,$D9,$D9,$E9,$08,$F1,$FA,$FA,$AA,$DB,$D9,$3A,$04,
$53,$4B,$08,$1A,$1A,$1A,$42,$A1,$90,$23,$04,$8E,$4D,$FD,$81,$6C,$67,$8B,$DA,$DC,
$DC,$14,$82,$60,$30,$38,$38,$38,$E8,$08,$41,$6C,$75,$28,$76,$09,$E2,$F9,$C2,$14,
$82,$DA,$DA,$DA,$C9,$C9,$49,$47,$08,$4C,$8A,$7A,$24,$D6,$B1,$5D,$08,$26,$B7,$10,
$D4,$D4,$D4,$AC,$AC,$AC,$38,$42,$10,$5B,$5D,$1A,$1E,$C9,$2E,$44,$4B,$4B,$8B,$10,
$54,$57,$57,$3B,$12,$28,$87,$FA,$44,$16,$B1,$2D,$02,$4C,$9E,$12,$50,$19,$1E,$10,
$04,$56,$A7,$8A,$6C,$41,$B4,$B5,$B5,$09,$41,$20,$10,$E8,$EE,$EE,$F6,$80,$A0,$14,
$4C,$A7,$4A,$73,$97,$4F,$30,$31,$31,$A1,$04,$55,$55,$55,$B4,$89,$07,$04,$3A,$3B,
$3B,$33,$BD,$72,$99,$C5,$C1,$EF,$98,$04,$C3,$C3,$C3,$DE,$04,$92,$0C,$20,$D4,$2B,
$63,$94,$4B,$26,$E0,$7D,$A8,$47,$25,$A8,$AF,$AF,$2F,$8A,$40,$44,$59,$98,$5E,$99,
$03,$4F,$4A,$C8,$81,$49,$50,$59,$59,$C9,$8B,$F9,$80,$40,$24,$43,$DD,$BA,$78,$65,
$EE,$B2,$78,$82,$82,$3A,$80,$C0,$C3,$CB,$FC,$53,$B4,$09,$09,$10,$02,$75,$AA,$9C,
$31,$F2,$EE,$E1,$77,$76,$76,$CC,$5E,$10,$82,$BA,$BA,$3A,$DF,$04,$22,$06,$06,$85,
$69,$7A,$65,$F1,$89,$9C,$17,$17,$17,$09,$A6,$BD,$23,$35,$C4,$37,$D9,$4C,$3A,$0F,
$CA,$CA,$41,$81,$68,$57,$7E,$5D,$09,$C4,$27,$AA,$47,$D2,$ED,$AC,$7B,$C1,$24,$A0,
$12,$7D,$D7,$81,$93,$E8,$52,$EA,$C3,$2F,$01,$5F,$E6,$EE,$EC,$10,$A8,$C8,$F9,$F8,
$F8,$78,$6F,$6F,$AF,$3B,$01,$67,$2E,$A5,$A8,$3F,$39,$E5,$88,$52,$A0,$81,$E9,$97,
$BE,$BE,$BE,$A6,$A6,$26,$FA,$10,$32,$F8,$D8,$8D,$0C,$86,$DF,$8D,$FD,$A7,$FF,$44,
$3F,$1C,$20,$DD,$20,$E3,$04,$6A,$3B,$00,$00,$00,$00,$49,$45,$4E,$44,$AE,$42,$60,
$82);
   FRadioOn: array[0..1562] of Byte=(
$89,$50,$4E,$47,$0D,$0A,$1A,$0A,$00,$00,$00,$0D,$49,$48,$44,$52,$00,$00,$00,$2C,
$00,$00,$00,$2C,$08,$02,$00,$00,$00,$91,$E6,$CD,$56,$00,$00,$00,$06,$74,$52,$4E,
$53,$00,$00,$00,$00,$00,$00,$6E,$A6,$07,$91,$00,$00,$05,$D0,$49,$44,$41,$54,$78,
$9C,$ED,$97,$4D,$4B,$1B,$6B,$14,$C7,$FB,$31,$5C,$B8,$CC,$52,$28,$F3,$01,$A6,$9B,
$BA,$99,$5D,$56,$E3,$A6,$5D,$F8,$14,$17,$2E,$3A,$58,$2A,$68,$47,$A8,$54,$D0,$29,
$8A,$A5,$38,$7A,$EB,$55,$DB,$F1,$8D,$E8,$D5,$A6,$89,$DE,$68,$8C,$89,$56,$8D,$A9,
$1A,$95,$90,$1A,$53,$AB,$15,$F5,$F6,$65,$21,$C5,$72,$51,$A1,$A2,$E0,$FD,$4F,$1E,
$FB,$F8,$DC,$91,$4C,$4C,$9B,$BB,$BA,$1E,$0E,$92,$0C,$99,$79,$7E,$73,$5E,$FE,$E7,
$78,$ED,$DA,$95,$5D,$59,$4E,$6C,$66,$66,$C6,$E3,$F1,$74,$74,$74,$34,$34,$34,$3C,
$79,$F2,$A4,$A7,$A7,$67,$74,$74,$74,$7E,$7E,$FE,$3F,$3F,$78,$6E,$6E,$AE,$A9,$A9,
$A9,$BA,$BA,$FA,$EE,$DD,$BB,$65,$65,$65,$E5,$E5,$E5,$95,$95,$95,$55,$55,$55,$B8,
$52,$53,$53,$53,$57,$57,$57,$5F,$5F,$DF,$D8,$D8,$68,$18,$86,$D7,$EB,$8D,$C7,$E3,
$B9,$27,$E8,$EC,$EC,$2C,$29,$29,$29,$2D,$2D,$B5,$27,$78,$FA,$F4,$69,$73,$73,$73,
$6B,$6B,$6B,$7B,$7B,$FB,$EB,$D7,$AF,$73,$76,$FC,$9B,$37,$6F,$1E,$3C,$78,$90,$2D,
$01,$E2,$D1,$DD,$DD,$ED,$F3,$F9,$D6,$D6,$D6,$7E,$95,$00,$CF,$22,$84,$58,$08,$70,
$F0,$8B,$17,$2F,$02,$81,$C0,$F8,$F8,$78,$38,$1C,$9E,$9D,$9D,$0D,$85,$42,$F8,$EA,
$76,$BB,$F1,$7B,$46,$E0,$72,$B9,$06,$06,$06,$90,$9A,$68,$34,$FA,$F3,$04,$78,$5D,
$0B,$41,$6D,$6D,$AD,$DF,$EF,$5F,$B2,$B5,$E9,$E9,$E9,$BE,$BE,$3E,$4A,$00,$AC,$A1,
$A1,$A1,$91,$91,$11,$80,$E6,$26,$06,$28,$0B,$FB,$E3,$99,$E1,$D5,$69,$60,$28,$C1,
$D8,$D8,$58,$30,$18,$8C,$C5,$62,$D9,$11,$A0,$0E,$78,$82,$47,$8F,$1E,$21,$E0,$97,
$24,$60,$16,$89,$44,$C6,$52,$06,$02,$14,$29,$BE,$6E,$6C,$6C,$5C,$96,$20,$99,$4C,
$F2,$95,$08,$02,$34,$A7,$E5,$80,$96,$97,$2D,$15,$CF,$2B,$48,$07,$29,$32,$8A,$E0,
$F8,$70,$AF,$EB,$5E,$93,$A7,$C9,$F2,$B3,$C5,$C5,$C5,$89,$89,$09,$10,$A0,$74,$F0,
$62,$59,$04,$03,$E5,$CD,$67,$C1,$12,$03,$CF,$B8,$E7,$CE,$EF,$77,$C8,$00,$31,$DD,
$73,$EE,$B2,$5B,$36,$DD,$90,$83,$B3,$41,$FE,$F7,$0B,$0B,$0B,$94,$00,$39,$5A,$5E,
$5E,$BE,$54,$B3,$80,$DA,$A6,$0E,$F0,$F6,$C5,$5D,$C5,$8A,$5B,$51,$86,$53,$EE,$3F,
$77,$E2,$23,$F2,$B0,$0C,$97,$5E,$4A,$35,$9E,$1A,$0B,$07,$25,$80,$82,$AD,$AE,$AE,
$6E,$6E,$6E,$66,$80,$80,$0C,$F3,$BD,$60,$21,$20,$2E,$42,$09,$54,$9F,$AA,$06,$54,
$75,$F2,$CC,$95,$90,$02,$27,$01,$22,$FB,$65,$C9,$27,$C1,$2D,$1C,$8C,$00,$91,$F8,
$F0,$E1,$43,$06,$08,$B4,$25,$D3,$03,$3E,$11,$5D,$DE,$2E,$16,$03,$1C,$AF,$85,$34,
$6D,$2A,$E5,$11,$D3,$D5,$19,$55,$99,$52,$E0,$24,$4C,$E4,$90,$C9,$71,$E3,$D5,$8D,
$F1,$C8,$38,$0F,$41,$09,$50,$9B,$5B,$5B,$5B,$76,$04,$EF,$DE,$BD,$63,$04,$10,$41,
$FE,$55,$8A,$DA,$8A,$50,$04,$34,$06,$20,$D0,$C3,$BA,$3E,$9F,$F2,$65,$D3,$B5,$A8,
$A6,$CE,$A9,$4A,$24,$05,$31,$2D,$4B,$01,$49,$1C,$15,$0B,$FB,$0B,$F9,$27,$E0,$E1,
$94,$E0,$E3,$C7,$8F,$76,$10,$98,$8D,$4C,$95,$31,$18,$D9,$FD,$B5,$DD,$B5,$72,$AF,
$7C,$06,$81,$30,$4C,$69,$38,$DE,$88,$1A,$46,$CC,$30,$56,$0C,$3D,$AE,$6B,$31,$4D,
$5D,$52,$95,$A8,$42,$E6,$88,$1C,$91,$A5,$49,$13,$42,$F0,$09,$8F,$47,$1F,$B3,$87,
$24,$12,$09,$4A,$F0,$E5,$CB,$97,$9D,$9D,$9D,$B4,$10,$50,$3A,$36,$17,$A0,$36,$EC,
$FE,$D2,$B6,$52,$B9,$5F,$46,$0B,$A0,$00,$51,$01,$67,$10,$31,$63,$30,$39,$68,$42,
$24,$75,$2D,$A1,$A9,$71,$55,$89,$29,$64,$29,$05,$11,$96,$C4,$A0,$09,$51,$3C,$58,
$CC,$1E,$F2,$F6,$ED,$5B,$4A,$B0,$BB,$BB,$FB,$F9,$F3,$E7,$B4,$10,$2D,$2D,$2D,$6C,
$32,$A1,$4D,$D8,$FD,$CE,$DF,$9C,$FF,$82,$88,$68,$48,$01,$8E,$37,$7D,$CD,$D0,$37,
$74,$ED,$BD,$A6,$26,$55,$25,$A1,$90,$38,$91,$A3,$29,$88,$29,$51,$08,$08,$85,$83,
$85,$3C,$04,$25,$D8,$DB,$DB,$C3,$DF,$B4,$10,$68,$07,$36,$1B,$F9,$74,$4A,$CF,$A5,
$8B,$10,$C8,$02,$62,$00,$02,$7D,$5B,$D7,$36,$35,$75,$43,$55,$DE,$2B,$24,$49,$E4,
$98,$2C,$CD,$9F,$41,$5C,$7F,$79,$9D,$3D,$04,$4A,$45,$09,$FE,$4E,$59,$5A,$08,$88,
$23,$9B,$CE,$E9,$20,$D0,$8A,$E8,$05,$54,$22,$EA,$00,$59,$40,$0C,$4C,$82,$6D,$55,
$D9,$54,$E0,$F6,$10,$94,$E0,$F0,$F0,$70,$7F,$7F,$3F,$2D,$C4,$B3,$67,$CF,$D8,$7E,
$80,$B9,$C7,$A7,$43,$72,$49,$10,$44,$28,$92,$29,$09,$53,$0A,$7A,$01,$95,$88,$3A,
$40,$16,$CC,$18,$80,$E0,$2F,$85,$6C,$10,$39,$21,$4B,$CB,$12,$83,$E0,$D3,$81,$16,
$A5,$04,$DF,$BF,$7F,$B7,$8B,$04,$66,$3F,$DB,$50,$A0,$F9,$EC,$7E,$E8,$34,$85,$80,
$20,$42,$91,$4C,$49,$88,$28,$E8,$05,$54,$22,$EA,$00,$59,$30,$63,$00,$82,$F7,$32,
$85,$10,$23,$A2,$30,$29,$38,$FC,$8E,$5B,$AF,$6E,$F1,$10,$94,$E0,$E4,$E4,$E4,$EB,
$D7,$AF,$69,$21,$20,$F2,$6C,$47,$C2,$52,$C4,$EE,$7F,$D8,$FB,$50,$EC,$11,$A5,$41,
$C9,$14,$66,$BF,$0C,$31,$30,$7D,$8E,$A0,$17,$50,$89,$48,$81,$99,$85,$C4,$0F,$82,
$A8,$28,$84,$05,$0A,$51,$37,$76,$9E,$56,$B4,$06,$25,$38,$3D,$3D,$45,$85,$A6,$85,
$40,$2B,$B3,$2D,$0D,$AB,$11,$5F,$16,$37,$DB,$6E,$8A,$FD,$A2,$E4,$35,$25,$19,$9A,
$08,$45,$42,$2B,$9A,$1E,$95,$51,$04,$66,$1D,$20,$0B,$29,$82,$B3,$30,$04,$1D,$05,
$9E,$02,$FE,$09,$A8,$03,$4A,$00,$4B,$4B,$40,$0D,$5D,$CA,$F6,$44,$8C,$3E,$F6,$88,
$F6,$3F,$DB,$05,$97,$20,$BA,$45,$3A,$1A,$A0,$89,$50,$24,$B4,$A2,$E9,$F3,$67,$6E,
$1E,$4F,$63,$10,$74,$E4,$0F,$E7,$F7,$4D,$F7,$B1,$DB,$21,$97,$8C,$E0,$E8,$E8,$28,
$03,$04,$F4,$8A,$6D,$AA,$58,$8D,$F8,$57,$B9,$DD,$71,$9B,$72,$60,$2E,$40,$10,$4D,
$0F,$8A,$28,$40,$E6,$38,$9E,$12,$E4,$F9,$F2,$F8,$6A,$A0,$61,$38,$FD,$61,$76,$AD,
$41,$0D,$93,$86,$DF,$54,$F9,$F2,$84,$61,$5D,$00,$07,$1A,$4F,$F0,$08,$10,$44,$D3,
$03,$E7,$8E,$22,$80,$23,$06,$16,$82,$4F,$9F,$3E,$9D,$72,$86,$AF,$19,$20,$60,$D8,
$C6,$D8,$AE,$8C,$7F,$B3,$B0,$0D,$F0,$4F,$6C,$1D,$6D,$2D,$E8,$2D,$70,$F4,$3B,$1C,
$EE,$94,$0F,$9F,$7B,$9E,$27,$0F,$57,$F4,$90,$CE,$FF,$1E,$4D,$C1,$13,$7C,$FB,$F6,
$2D,$33,$01,$6C,$7D,$7D,$1D,$89,$60,$BB,$32,$98,$96,$2E,$58,$C5,$1F,$15,$CE,$1E,
$27,$68,$F2,$FB,$F3,$E1,$F8,$E0,$EC,$77,$DE,$F7,$DE,$B7,$FC,$0C,$02,$85,$B6,$64,
$04,$99,$AB,$81,$37,$2C,$20,$FC,$AE,$8C,$39,$82,$D5,$E8,$22,$8A,$BD,$51,$61,$C8,
$3A,$11,$BC,$61,$39,$66,$DB,$3A,$20,$20,$A0,$96,$BC,$D8,$9B,$A5,$0E,$B2,$48,$04,
$6F,$58,$40,$70,$36,$25,$E0,$37,$55,$98,$FD,$F1,$10,$1B,$BE,$17,$7E,$26,$11,$BC,
$61,$19,$C4,$91,$3C,$01,$DD,$13,$F1,$3F,$C1,$CA,$CA,$0A,$3E,$23,$E5,$F4,$60,$8C,
$69,$5C,$C7,$AA,$82,$A1,$C0,$F4,$E0,$97,$62,$60,$31,$2C,$86,$34,$00,$6C,$53,$65,
$5B,$1A,$DB,$0F,$D8,$64,$B2,$10,$20,$00,$59,$D7,$41,$3A,$C3,$92,$0E,$C9,$CB,$96,
$00,$D7,$B7,$B7,$B7,$73,$43,$C0,$A3,$20,$41,$19,$09,$8E,$8F,$8F,$71,$C5,$6E,$44,
$E5,$C4,$18,$07,$26,$F2,$C1,$C1,$01,$5D,$52,$90,$78,$5C,$C9,$FD,$AB,$5F,$D9,$FF,
$D3,$FE,$01,$3C,$70,$2C,$53,$4A,$59,$19,$5F,$00,$00,$00,$00,$49,$45,$4E,$44,$AE,
$42,$60,$82);
 protected
  procedure Click; override;
 published
  constructor Create(AOwner: TComponent); override;
 public
  destructor Destroy; override;
end;

//Coloured Slider - declarations +++++++++++++++++++++++++++++++++++++++++++++++
type TGJHSlider = class(TGraphicControl)
 private
  FColour     : TColor;
  FMouseIsDown,
  FShowValue,
  FHexValue,
  FGradient,
  FPointers,
  FFillSlider : Boolean;
  FPosition,
  FMax,
  FMin,
  FOrient,
  FStep       : Integer;
  FOnChange   : TNotifyEvent;
  FCaption    : String;
  procedure SetPosition(const LPosition: Integer);
  procedure SetStep(const LStep: Integer);
  procedure SetColour(const LColour: TColor);
  procedure SetMax(const LMax: Integer);
  procedure SetMin(const LMin: Integer);
  procedure SetShowValue(const LShowValue: Boolean);
  procedure SetHexValue(const LHexValue: Boolean);
  procedure SetOrient(const LOrient: Integer);
  procedure SetGradient(const LGradient: Boolean);
  procedure SetPointers(const LPointers: Boolean);
  procedure SetFillSlider(const LFillSlider: Boolean);
  function GetSliderEnd: Integer;
  function GetValue: String;
  function GetSliderStart: Integer;
  procedure FDown(Sender: TObject; {%H-}Button: TMouseButton;
                              {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
  procedure FMove(Sender: TObject; {%H-}Shift: TShiftState;  X,Y: Integer);
  procedure FUp(Sender: TObject; {%H-}Button: TMouseButton;
                              {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
 const
  FPointerLeft: array[0..333] of Byte = (
$89,$50,$4E,$47,$0D,$0A,$1A,$0A,$00,$00,$00,$0D,$49,$48,$44,$52,$00,$00,$00,$20,
$00,$00,$00,$20,$08,$02,$00,$00,$00,$FC,$18,$ED,$A3,$00,$00,$00,$06,$74,$52,$4E,
$53,$00,$00,$00,$00,$00,$00,$6E,$A6,$07,$91,$00,$00,$01,$03,$49,$44,$41,$54,$78,
$9C,$B5,$D6,$A1,$0E,$83,$30,$14,$85,$E1,$3D,$76,$5F,$03,$39,$81,$01,$87,$C5,$22,
$10,$58,$24,$06,$41,$08,$82,$10,$0C,$21,$88,$09,$42,$B6,$1B,$96,$35,$0C,$6E,$0B,
$2D,$A7,$47,$56,$FC,$9F,$81,$C2,$E3,$E1,$6C,$6D,$DB,$E6,$79,$EE,$2A,$FD,$5A,$27,
$84,$70,$92,$7E,$AF,$0B,$C3,$10,$09,$6C,$D3,$B4,$A2,$28,$C4,$3A,$7C,$9A,$B6,$2C,
$8B,$F8,$0D,$9C,$A6,$4D,$D3,$D4,$34,$CD,$5D,$40,$93,$AE,$D6,$D9,$03,$6C,$9A,$4E,
$64,$DA,$1E,$50,$A5,$E9,$BC,$3A,$CC,$0C,$30,$4A,$9B,$01,$6C,$7A,$9E,$E7,$AE,$EB,
$54,$E9,$AB,$80,$2A,$DD,$F7,$BD,$3E,$7D,$0E,$B0,$69,$7A,$B4,$2F,$A6,$75,$80,$2A,
$3D,$0C,$43,$5D,$D7,$D7,$EB,$0C,$00,$4C,$EF,$01,$36,$4D,$1B,$C7,$D1,$2E,$BD,$07,
$E8,$CA,$8E,$A2,$E8,$F9,$3F,$DF,$F7,$93,$24,$29,$CB,$12,$00,$7C,$A7,$62,$D2,$34,
$C5,$00,$1A,$26,$08,$82,$2C,$CB,$30,$00,$90,$39,$7F,$D1,$58,$86,$4E,$E8,$1C,$03,
$DC,$64,$CC,$2E,$3B,$0B,$C6,$E6,$BA,$66,$99,$38,$8E,$E9,$F3,$8B,$01,$8C,$18,$7B,
$40,$C3,$6C,$DF,$CD,$BB,$80,$8A,$91,$57,$00,$06,$D0,$30,$48,$80,$65,$A4,$01,$03,
$8E,$8C,$E7,$79,$78,$60,$C7,$B8,$02,$24,$43,$FF,$BF,$1F,$D5,$74,$FE,$49,$A5,$E3,
$08,$9D,$00,$00,$00,$00,$49,$45,$4E,$44,$AE,$42,$60,$82);
  FPointerDown: array[0..372] of Byte = (
$89,$50,$4E,$47,$0D,$0A,$1A,$0A,$00,$00,$00,$0D,$49,$48,$44,$52,$00,$00,$00,$20,
$00,$00,$00,$20,$08,$02,$00,$00,$00,$FC,$18,$ED,$A3,$00,$00,$00,$06,$74,$52,$4E,
$53,$00,$00,$00,$00,$00,$00,$6E,$A6,$07,$91,$00,$00,$01,$2A,$49,$44,$41,$54,$78,
$9C,$B5,$95,$B1,$0A,$83,$30,$14,$45,$FD,$EC,$4C,$6E,$FA,$01,$7E,$80,$9B,$93,$B8,
$8B,$88,$0A,$22,$28,$88,$E0,$20,$8A,$48,$47,$FF,$41,$41,$DA,$47,$5F,$6B,$DB,$84,
$DA,$C6,$BC,$9C,$29,$D3,$CD,$39,$83,$C6,$98,$E7,$79,$DB,$B6,$AB,$06,$60,$B6,$AA,
$2A,$A3,$EB,$BA,$2C,$CB,$96,$65,$A1,$5D,$07,$6F,$DB,$B6,$8B,$A2,$30,$80,$3C,$CF,
$A3,$28,$EA,$FB,$7E,$5D,$57,$F5,$69,$70,$0D,$82,$80,$31,$E6,$38,$8E,$81,$40,$44,
$74,$27,$8E,$E3,$69,$9A,$2E,$0A,$B4,$6D,$6B,$9A,$26,$BB,$F3,$D0,$47,$30,$02,$A8,
$EB,$5A,$E5,$02,$D7,$75,$71,$FD,$A5,$4F,$18,$01,$FA,$EC,$C9,$87,$3E,$55,$C4,$57,
$7D,$2E,$02,$18,$C7,$91,$58,$9F,$8B,$28,$CB,$52,$F6,$02,$B0,$3E,$D2,$57,$8C,$00,
$E5,$DF,$FA,$2A,$11,$7F,$E9,$8B,$11,$F0,$E9,$11,$EB,$73,$11,$70,$20,$D6,$17,$23,
$E0,$7C,$BC,$9E,$24,$89,$9C,$BE,$6C,$84,$65,$59,$72,$FA,$52,$11,$61,$18,$9E,$D1,
$FF,$3F,$E2,$A4,$BE,$18,$01,$5F,$29,$B1,$3E,$17,$91,$A6,$29,$B7,$3E,$0C,$C3,$FE,
$5B,$3E,$A3,$FF,$33,$C2,$F7,$7D,$55,$FD,$83,$08,$1A,$7D,$31,$A2,$69,$1A,$62,$7D,
$2E,$02,$DF,$22,$4A,$7D,$31,$02,$DE,$22,$CF,$F3,$28,$F5,$91,$F7,$88,$7D,$9D,$46,
$1F,$79,$8F,$A0,$D7,$47,$F6,$08,$7A,$7D,$64,$8F,$D0,$A2,$8F,$60,$84,$16,$7D,$04,
$23,$74,$E9,$23,$10,$A1,$4B,$1F,$81,$08,$59,$FD,$1B,$24,$50,$0F,$44,$C0,$FF,$1D,
$21,$00,$00,$00,$00,$49,$45,$4E,$44,$AE,$42,$60,$82);
  FPointerRight: array[0..339] of Byte = (
$89,$50,$4E,$47,$0D,$0A,$1A,$0A,$00,$00,$00,$0D,$49,$48,$44,$52,$00,$00,$00,$20,
$00,$00,$00,$20,$08,$02,$00,$00,$00,$FC,$18,$ED,$A3,$00,$00,$00,$06,$74,$52,$4E,
$53,$00,$00,$00,$00,$00,$00,$6E,$A6,$07,$91,$00,$00,$01,$09,$49,$44,$41,$54,$78,
$9C,$B5,$D5,$3F,$0E,$44,$40,$14,$C7,$71,$C7,$DE,$4A,$C7,$01,$1C,$C0,$05,$5C,$C0,
$05,$14,$4A,$9D,$C6,$46,$14,$0A,$8D,$88,$88,$88,$C8,$EC,$8B,$17,$B3,$FE,$C5,$98,
$79,$6F,$7E,$07,$F8,$7E,$EC,$AE,$99,$75,$DA,$B6,$AD,$AA,$CA,$B1,$B7,$79,$9E,$97,
$65,$E9,$FB,$DE,$16,$23,$84,$18,$C7,$51,$AC,$B3,$C2,$60,$1A,$3E,$87,$D8,$C6,$CC,
$5C,$01,$66,$46,$16,$21,$07,$51,$7E,$46,$B6,$BE,$EB,$20,$37,$0C,$03,$27,$73,$02,
$70,$75,$5D,$CB,$5F,$9E,$CA,$DC,$02,$9C,$CC,$03,$20,$19,$D2,$2B,$A0,$04,$70,$4D,
$D3,$18,$32,$2F,$01,$73,$46,$0B,$80,$95,$65,$09,$D7,$17,$DC,$2E,$6F,$19,$5D,$40,
$9B,$31,$03,$24,$D3,$75,$9D,$B8,$EC,$C0,$50,$00,$DC,$ED,$15,$F0,$67,$E8,$40,$9E,
$E7,$51,$14,$B9,$AE,$FB,$39,$2E,$08,$82,$24,$49,$48,$80,$22,$4D,$FC,$8A,$E2,$38,
$56,$A4,$8D,$01,$48,$7B,$9E,$A7,$4E,$1B,$00,$7A,$69,$2D,$00,$12,$10,$D2,$4B,$BF,
$04,$CC,$D3,$4A,$20,$CB,$32,$52,$FA,$01,$80,$74,$18,$86,$D4,$F4,$2D,$C0,$99,$3E,
$01,$78,$6A,$38,$D3,$7B,$00,$FE,$E8,$4F,$A7,$86,$21,$BD,$07,$F6,$CF,$CE,$96,$96,
$40,$51,$14,$56,$D2,$B8,$69,$9A,$7C,$DF,$B7,$92,$C6,$A5,$69,$6A,$2B,$BD,$EE,$07,
$71,$32,$5A,$AA,$42,$28,$E4,$A3,$00,$00,$00,$00,$49,$45,$4E,$44,$AE,$42,$60,$82);
  FPointerUp: array[0..385] of Byte = (
$89,$50,$4E,$47,$0D,$0A,$1A,$0A,$00,$00,$00,$0D,$49,$48,$44,$52,$00,$00,$00,$20,
$00,$00,$00,$20,$08,$02,$00,$00,$00,$FC,$18,$ED,$A3,$00,$00,$00,$06,$74,$52,$4E,
$53,$00,$00,$00,$00,$00,$00,$6E,$A6,$07,$91,$00,$00,$01,$37,$49,$44,$41,$54,$78,
$9C,$B5,$95,$B1,$AE,$45,$50,$10,$45,$EF,$67,$BF,$0F,$A0,$90,$F8,$01,$11,$8D,$52,
$23,$A2,$D0,$28,$15,$1A,$A5,$48,$14,$22,$0A,$85,$46,$21,$A2,$10,$71,$E7,$BE,$79,
$99,$1C,$92,$EB,$9D,$73,$8C,$5D,$69,$EC,$BD,$56,$42,$E6,$F5,$52,$49,$DF,$F7,$65,
$59,$2A,$BD,$A2,$96,$65,$59,$E2,$38,$7E,$AA,$1D,$F0,$F7,$7D,$77,$1C,$E7,$29,$09,
$C0,$C7,$81,$47,$24,$10,$1F,$07,$1E,$91,$40,$7C,$1A,$60,$96,$20,$7C,$1A,$60,$96,
$20,$FC,$6D,$DB,$68,$80,$4D,$42,$C4,$1F,$C7,$11,$7A,$99,$25,$44,$FC,$B6,$6D,$A1,
$94,$53,$E2,$84,$DF,$FC,$26,$49,$12,$36,$89,$13,$3E,$0E,$D4,$75,$ED,$79,$1E,$83,
$84,$88,$3F,$0C,$43,$23,$24,$4D,$53,$06,$09,$C2,$5F,$D7,$B5,$39,$86,$41,$E2,$02,
$9F,$47,$E2,$02,$9F,$24,$7C,$DF,$D7,$94,$F8,$17,$1F,$93,$65,$99,$A6,$04,$E1,$C3,
$C3,$B7,$76,$8C,$8E,$84,$88,$0F,$CF,$D7,$03,$3A,$12,$F2,$F8,$3A,$12,$4A,$F8,$98,
$3C,$CF,$15,$24,$54,$F1,$31,$41,$10,$48,$49,$88,$F8,$5D,$D7,$C9,$0F,$14,$45,$21,
$25,$41,$F8,$D3,$34,$C9,$B7,$CB,$4A,$68,$E3,$CB,$4A,$DC,$C1,$C7,$84,$61,$F8,$55,
$E2,$26,$3E,$46,$BC,$45,$67,$09,$C2,$A7,$AB,$A2,$17,$3A,$A8,$07,$09,$C2,$17,$AF,
$0A,$A7,$04,$E2,$57,$55,$65,$DB,$F6,$CF,$ED,$58,$96,$85,$A7,$E2,$4F,$02,$F0,$E7,
$79,$86,$8F,$EC,$7E,$B5,$18,$18,$70,$5D,$F7,$23,$01,$3F,$BA,$69,$9A,$BC,$ED,$18,
$C3,$30,$A2,$28,$7A,$03,$F3,$7A,$FF,$C9,$BE,$35,$86,$F3,$00,$00,$00,$00,$49,$45,
$4E,$44,$AE,$42,$60,$82);
  FGap = 4;
 protected
  procedure Paint; override;
 published
  //Methods
  constructor Create(AOwner: TComponent); override;
  //Events
  property OnChange   : TNotifyEvent read FOnChange    write FOnChange;
  //Properties
  property Caption    : string       read FCaption     write FCaption;
  property Colour     : TColor       read FColour      write SetColour      default clRed;
  property FillSlider : Boolean      read FFillSlider  write SetFillSlider  default False;
  property Gradient   : Boolean      read FGradient    write SetGradient    default False;
  property HexValue   : Boolean      read FHexValue    write SetHexValue    default False;
  property Max        : Integer      read FMax         write SetMax         default 100;
  property Min        : Integer      read FMin         write SetMin         default 0;
  property Orientation: Integer      read FOrient      write SetOrient      default csVertical;
  property Pointers   : Boolean      read FPointers    write SetPointers    default True;
  property Position   : Integer      read FPosition    write SetPosition    default 0; 
  property ShowValue  : Boolean      read FShowValue   write SetShowValue   default False;
  property Step       : Integer      read FStep        write SetStep        default 1;
 public
  destructor Destroy; override;
end;

//Registry Class - declarations ++++++++++++++++++++++++++++++++++++++++++++++++
type TGJHRegistry = class
 private
  FRegistry : TRegistry;
  FRegKey   : String;
  procedure OpenReg(key: String);
  function ExtractKey(var V: String):String;
 published
  //Methods
  constructor Create(LRegKey: String);
  function DeleteKey(key: String): Boolean;
  function DoesKeyExist(V: String):Boolean;
  procedure GetRegValA(V: String;var D: array of Byte);
  function GetRegValB(V: String;D: Boolean): Boolean;
  function GetRegValI(V: String;D: Cardinal): Cardinal;
  function GetRegValS(V: String;D: String): String;
  procedure SetRegValA(V: String;var D: array of Byte);
  procedure SetRegValB(V: String;D: Boolean);
  procedure SetRegValI(V: String;D: Cardinal);
  procedure SetRegValS(V: String;D: String);
  //Properties
  property Key : String read FRegKey;
 public
  destructor Destroy; override;
end;

procedure Register;

//Methods and functions ++++++++++++++++++++++++++++++++++++++++++++++++++++++++

implementation

{-------------------------------------------------------------------------------
Register all the components
-------------------------------------------------------------------------------}
procedure Register;
begin
 RegisterComponents('GJH Custom Components',[TGJHTickBox,
                                             TGJHRadioBox,
                                             TGJHTickBoxes,
                                             TGJHSlider]);
end;

//Tickbox parent Methods +++++++++++++++++++++++++++++++++++++++++++++++++++++++

{-------------------------------------------------------------------------------
Class creator - initialises the global variables
-------------------------------------------------------------------------------}
constructor TGJHTickBoxes.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 //Set the default variables
 FTicked:=False;
 FColour:=clNone;
 FOnlyMouse:=False;
 //Create the on and off graphics
 FOn:=TPortableNetworkGraphic.Create;
 FOff:=TPortableNetworkGraphic.Create;
end;

{-------------------------------------------------------------------------------
Class destructor - tidies up afterwards
-------------------------------------------------------------------------------}
destructor TGJHTickBoxes.Destroy;
begin
 inherited;
end;

{-------------------------------------------------------------------------------
Paint the control
-------------------------------------------------------------------------------}
procedure TGJHTickBoxes.Paint;
var
 Lgf: TPortableNetworkGraphic;
 R: TRect;
begin
 //Create a temporary graphic
 Lgf:=TPortableNetworkGraphic.Create;
 //Set it's dimensions
 Lgf.Width:=FOn.Width;
 Lgf.Height:=FOn.Height;
 //Colour the background, if one has been specified
 if FColour=clNone then
 begin
  Canvas.Brush.Style:=bsClear;
  Canvas.Pen.Style:=psClear;
 end
 else
 begin
  Canvas.Brush.Color:=FColour;
  Canvas.Brush.Style:=bsSolid;
  Canvas.Pen.Color:=FColour;
  Canvas.Pen.Style:=psSolid;
  Canvas.Rectangle(0,0,Width,Height);
 end;
 //Paint the appropriate graphic
 if FTicked then Lgf.Assign(FOn) else Lgf.Assign(FOff);
 //And paint it onto the control
 R.Top:=0;
 R.Left:=0;
 R.Width:=Height;
 R.Height:=Height;
 Canvas.StretchDraw(R,Lgf);
 Lgf.Free;
 //Write the text
 Canvas.TextOut(Height+Fgap,(Height-Canvas.TextHeight(Caption))div 2,FCaption);
end;

{-------------------------------------------------------------------------------
React to the click
-------------------------------------------------------------------------------}
procedure TGJHTickBoxes.Click;
begin
 FTicked:=not FTicked;
 Invalidate;//Force a redraw
 //Fire the OnChange event
 if Assigned(FOnChange) then FOnChange(Self as TObject);
 inherited Click;
end;

{-------------------------------------------------------------------------------
Caption has changed, so adjust the dimensions
-------------------------------------------------------------------------------}
procedure TGJHTickBoxes.SetWidth(const LCaption: String);
begin
 FCaption:=LCaption;
 Width:=Canvas.TextWidth(FCaption)+FGap+FOn.Width;
 Height:=Canvas.TextHeight(FCaption);
 Invalidate;//Force a redraw
end;

{-------------------------------------------------------------------------------
The ticked state has been changed
-------------------------------------------------------------------------------}
procedure TGJHTickBoxes.SetTicked(const LTicked: Boolean);
begin
 FTicked:=LTicked;
 Invalidate; //Force a redraw
 //Fire the OnChange event
 if not FOnlyMouse then //Unless we only reacting to a mouse click
  if Assigned(FOnChange) then FOnChange(Self as TObject);
end;

//Tickbox Methods ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

{-------------------------------------------------------------------------------
Class creator - initialises the local variables
-------------------------------------------------------------------------------}
constructor TGJHTickBox.Create(AOwner: TComponent);
var
 Lms: TMemoryStream;
begin
 inherited Create(AOwner);
 //Create the tick box graphics and assign them
 Lms:=TMemoryStream.Create;
 Lms.Write(FTickBoxOn[0],Length(FTickBoxOn));
 Lms.Position:=0;
 FOn.LoadFromStream(Lms);
 Lms.Clear;
 Lms.Write(FTickBoxOff[0],Length(FTickBoxOff));
 Lms.Position:=0;
 FOff.LoadFromStream(Lms);
 Lms.Free;
end;

{-------------------------------------------------------------------------------
Free up and tidy up
-------------------------------------------------------------------------------}
destructor TGJHTickBox.Destroy;
begin
 inherited;
end;

//Radio box Methods ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

{-------------------------------------------------------------------------------
Class creator - initialises the local variables
-------------------------------------------------------------------------------}
constructor TGJHRadioBox.Create(AOwner: TComponent);
var
 Lms: TMemoryStream;
begin
 inherited Create(AOwner);
 //Create the radio box graphics and assign them
 Lms:=TMemoryStream.Create;
 Lms.Write(FRadioOn[0],Length(FRadioOn));
 Lms.Position:=0;
 FOn.LoadFromStream(Lms);
 Lms.Clear;
 Lms.Write(FRadioOff[0],Length(FRadioOff));
 Lms.Position:=0;
 FOff.LoadFromStream(Lms);
 Lms.Free;
end;

{-------------------------------------------------------------------------------
Class destructor - tidy up
-------------------------------------------------------------------------------}
destructor TGJHRadioBox.Destroy;
begin
 inherited;
end;

{-------------------------------------------------------------------------------
React to the click
-------------------------------------------------------------------------------}
procedure TGJHRadioBox.Click;
var
 LParent: TComponent;
 Index: Integer;
begin
 inherited Click;
 //Force it to be set
 FTicked:=True;
 //Now we need to unset every other radio box control sibling
 if HasParent then
 begin
  //Get the parent control
  LParent:=GetParentComponent;
  //And iterate through it's children
  for Index:=0 to LParent.ComponentCount-1 do
   //Ignore ourself
   if(LParent.Components[Index]<>Self)
   and(LParent.Components[Index].ClassName=ClassName)then //But not other radios
    TGJHRadioBox(LParent.Components[Index]).Ticked:=False; //Unset them
 end;
 Invalidate; //Force a redraw
 //Fire the OnChange event
 if Assigned(FOnChange) then FOnChange(Self as TObject);
end;

//Slider methods +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

{-------------------------------------------------------------------------------
Class creator - initialises the local variables
-------------------------------------------------------------------------------}
constructor TGJHSlider.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 //Default values
 FMin:=0;
 FMax:=100;
 FPosition:=0;
 FColour:=$0000FF;
 FCaption:='';
 FShowValue:=False;
 FStep:=1;
 FHexValue:=False;
 FOrient:=csVertical;
 FGradient:=False;
 FPointers:=True;
 FFillSlider:=False;
 //We need to react to the MouseDown, MouseMove and MouseUp events
 OnMouseDown:=@FDown;
 OnMouseMove:=@FMove;
 OnMouseUp:=@FUp;
end;

{-------------------------------------------------------------------------------
Destructor - tidy up
-------------------------------------------------------------------------------}
destructor TGJHSlider.Destroy;
begin
 inherited;
end;

{-------------------------------------------------------------------------------
Paint the control
-------------------------------------------------------------------------------}
procedure TGJHSlider.Paint;
var
 LUnit     : Real;
 LSliderSize,
 LPosition,
 LY,Index,
 LTX,LX,LH : Integer;
 LCaption  : String;
 Lms       : TMemoryStream;
 Lpng      : TPortableNetworkGraphic;
 LR        : TRect;
 procedure GetGraphic(LGraphic: array of Byte);
 begin
  Lms.Clear;
  Lms.Write(LGraphic[0],Length(LGraphic));
  Lms.Position:=0;
  Lpng.LoadFromStream(Lms);
 end;
begin
 //Work out the position (centre of control)
 if FOrient=csVertical then
 begin
  LSliderSize:=(Width div 2)-FGap;
  LX:=(Width-LSliderSize)div 2;
 end
 else
 begin
  LSliderSize:=(Height div 2)-FGap;
  LX:=(Height-LSliderSize)div 2;
 end;
 //Work out where the top and bottom of the slider area
 if FOrient=csVertical then
 begin
  LY:=GetSliderStart;
  LH:=GetSliderEnd;
 end
 else
 begin
  LH:=GetSliderStart;
  LY:=GetSliderEnd;
 end;
 //Are we displaying the value?
 LCaption:=GetValue;
 //If so, then paint it
 Canvas.Brush.Style:=bsClear;
 if LCaption<>'' then
 begin
  Canvas.Font:=Font;
  if FOrient=csVertical then
  begin
   LTX:=(Width-Canvas.GetTextWidth(LCaption))div 2;
   Canvas.TextOut(LTX,LH,LCaption);
  end
  else
  begin
   LTX:=(Height-Canvas.GetTextHeight(LCaption))div 2;
   Canvas.TextOut(0,LTX,LCaption);
  end;
 end;
 //Are we displaying any caption?
 if FCaption<>'' then
 begin
  Canvas.Font:=Font;
  if FOrient=csVertical then
  begin
   LTX:=(Width-Canvas.GetTextWidth(FCaption))div 2;
   Canvas.TextOut(LTX,0,FCaption);
  end
  else
  begin
   LTX:=(Height-Canvas.GetTextHeight(FCaption))div 2;
   Canvas.TextOut(LY+FGap,LTX,FCaption);
  end;
 end;
 //And paint it
 Canvas.Brush.Style:=bsSolid;
 Canvas.Pen.Style:=psClear;
 //Gradient fill
 if FGradient then
 begin
  //Smallest unit for rectangles
  LUnit:=abs(LH-LY)/(FMax-FMin);
  for Index:=FMin+1 to FMax do
   if((Index<=FPosition)and(not FFillSlider))//Upto the position
   or(FFillSlider)then//Or the entire slider?
   begin
    Canvas.Brush.Color:=Round((FColour     AND$FF)*(Index/FMax))
                     OR Round((FColour>>8  AND$FF)*(Index/FMax))<<8
                     OR Round((FColour>>16 AND$FF)*(Index/FMax))<<16;
    if FOrient=csVertical then
     Canvas.Rectangle(LX,LH-Round(LUnit*(Index-1)),LX+LSliderSize,LH-Round(LUnit*Index))
    else
     Canvas.Rectangle(LH+Round(LUnit*(Index-1)),LX,LH+Round(LUnit*Index),LX+LSliderSize);
   end;
 end
 else //Solid fill
 begin
  //Work out where the filler starts and ends
  if not FFillSlider then
   LPosition:=LH-Round((FPosition/(FMax-FMin))*(LH-LY))
  else
   LPosition:=LY;
  Canvas.Brush.Color:=FColour;
  //Draw the rectangle
  if FOrient=csVertical then
   Canvas.Rectangle(LX,LPosition,LX+LSliderSize,LH)
  else
   Canvas.Rectangle(LPosition,LX,LH,LX+LSliderSize);
 end;
 //Draw the outline
 Canvas.Pen.Color:=$000000;
 Canvas.Pen.Style:=psSolid;
 Canvas.Brush.Style:=bsClear;
 if FOrient=csVertical then
  Canvas.Rectangle(LX,LY,LX+LSliderSize,LH)
 else
  Canvas.Rectangle(LH,LX,LY,LX+LSliderSize);
 //Draw the pointers
 if FPointers then
 begin
  //This is only calculated when partially filled
  LPosition:=LH-Round((FPosition/(FMax-FMin))*(LH-LY));
  //Create the containers
  Lms:=TMemoryStream.Create;
  Lpng:=TPortableNetworkGraphic.Create;
  if FOrient=csVertical then
  begin
   //Left
   GetGraphic(FPointerLeft);
   LR.Left:=LX+LSliderSize;
   LR.Top:=LPosition-(Width-LSliderSize)div 4;
   LR.Right:=Width;
   LR.Bottom:=LPosition+(Width-LSliderSize)div 4;
   Canvas.StretchDraw(LR,Lpng);
   //Right
   GetGraphic(FPointerRight);
   LR.Left:=0;
   LR.Top:=LPosition-(Width-LSliderSize)div 4;
   LR.Right:=LX;
   LR.Bottom:=LPosition+(Width-LSliderSize)div 4;
   Canvas.StretchDraw(LR,Lpng);
  end
  else
  begin
   //Up
   GetGraphic(FPointerUp);
   LR.Top:=LX+LSliderSize;
   LR.Left:=LPosition-(Height-LSliderSize)div 4;
   LR.Bottom:=Height;
   LR.Right:=LPosition+(Height-LSliderSize)div 4;
   Canvas.StretchDraw(LR,Lpng);
   //Down
   GetGraphic(FPointerDown);
   LR.Top:=0;
   LR.Left:=LPosition-(Height-LSliderSize)div 4;
   LR.Bottom:=LX;
   LR.Right:=LPosition+(Height-LSliderSize)div 4;
   Canvas.StretchDraw(LR,Lpng);
  end;
  Lpng.Free;
  Lms.Free;
 end;
end;

{-------------------------------------------------------------------------------
Position and/or step has been changed
-------------------------------------------------------------------------------}
procedure TGJHSlider.SetPosition(const LPosition: Integer);
begin
 //Ensure that they are valid
 if FStep<1 then FStep:=1;
 if FStep>FMax div 2 then FStep:=FMax div 2;
 if(LPosition>=FMin)and(LPosition<=FMax)then FPosition:=LPosition;
 FPosition:=(FPosition div FStep)*FStep;
 Invalidate; //Force a redraw
 //Fire the OnChange event
 if Assigned(FOnChange) then FOnChange(Self as TObject);
end;

{-------------------------------------------------------------------------------
Step has been changed
-------------------------------------------------------------------------------}
procedure TGJHSlider.SetStep(const LStep: Integer);
begin
 FStep:=LStep;
 //This is handled by the previous method
 SetPosition(FPosition);
end;

{-------------------------------------------------------------------------------
The colour has been changed
-------------------------------------------------------------------------------}
procedure TGJHSlider.SetColour(const LColour: TColor);
begin
 FColour:=LColour;
 Invalidate; //Force a redraw
end;

{-------------------------------------------------------------------------------
The max has been changed
-------------------------------------------------------------------------------}
procedure TGJHSlider.SetMax(const LMax: Integer);
begin
 //Ensure it is valid
 if LMax>FMin then FMax:=LMax;
 if FPosition>FMax then FPosition:=FMax;
 Invalidate; //Force a redraw
end;

{-------------------------------------------------------------------------------
The min has been changed
-------------------------------------------------------------------------------}
procedure TGJHSlider.SetMin(const LMin: Integer);
begin
 //Ensure it is valid
 if FMax>LMin then FMin:=LMin;
 if FPosition<FMin then FPosition:=FMin;
 Invalidate; //Force a redraw
end;

{-------------------------------------------------------------------------------
The show value boolean has been toggled
-------------------------------------------------------------------------------}
procedure TGJHSlider.SetShowValue(const LShowValue: Boolean);
begin
 FShowValue:=LShowValue;
 Invalidate; //Force a redraw
end;

{-------------------------------------------------------------------------------
The show as Hex has been toggled
-------------------------------------------------------------------------------}
procedure TGJHSlider.SetHexValue(const LHexValue: Boolean);
begin
 FHexValue:=LHexValue;
 Invalidate; //Force a redraw
end;

{-------------------------------------------------------------------------------
Orientation has changed
-------------------------------------------------------------------------------}
procedure TGJHSlider.SetOrient(const LOrient: Integer);
begin
 FOrient:=LOrient;
 Invalidate; //Force a redraw
end;

{-------------------------------------------------------------------------------
Gradient has changed
-------------------------------------------------------------------------------}
procedure TGJHSlider.SetGradient(const LGradient: Boolean);
begin
 FGradient:=LGradient;
 Invalidate; //Force a redraw
end;

{-------------------------------------------------------------------------------
Showing pointers has changed
-------------------------------------------------------------------------------}
procedure TGJHSlider.SetPointers(const LPointers: Boolean);
begin
 FPointers:=LPointers;
 Invalidate; //Force a redraw
end;

{-------------------------------------------------------------------------------
Fill the slider has changed
-------------------------------------------------------------------------------}
procedure TGJHSlider.SetFillSlider(const LFillSlider: Boolean);
begin
 FFillSlider:=LFillSlider;
 Invalidate; //Force a redraw
end;

{-------------------------------------------------------------------------------
React to the Mouse Down
-------------------------------------------------------------------------------}
procedure TGJHSlider.FDown(Sender: TObject; Button: TMouseButton;
 Shift: TShiftState; X, Y: Integer);
begin
 //Set the flag
 FMouseIsDown:=True;
 //And adjust the slider
 FMove(Sender,Shift,X,Y);
end;

{-------------------------------------------------------------------------------
React to the Mouse Move
-------------------------------------------------------------------------------}
procedure TGJHSlider.FMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
var
 Lposition,
 LH,LY     : Integer;
 Lpercent  : Real;
begin
 //Only if the mouse button is down
 if FMouseIsDown then
 begin
  //Work out position
  LH:=GetSliderEnd;
  LY:=GetSliderStart;
  if((FOrient=csVertical)and(Y>=LY)and(Y<=LH))
  or((FOrient=csHorizontal)and(X>=LY)and(X<=LH))then
  begin
   if FOrient=csVertical then Lposition:=LH-Y else Lposition:=X-LY;
   Lpercent:=Lposition/(LH-LY);
   SetPosition(Round((FMax-FMin)*Lpercent)+FMin);
   //Fire the OnChange event
   if Assigned(FOnChange) then FOnChange(Self as TObject);
  end;
 end;
end;

{-------------------------------------------------------------------------------
React to the Mouse Up
-------------------------------------------------------------------------------}
procedure TGJHSlider.FUp(Sender: TObject; Button: TMouseButton;
Shift: TShiftState; X, Y: Integer);
begin
 //Clear the flag
 FMouseIsDown:=False;
end;

{-------------------------------------------------------------------------------
Get the height of the slider, taking into account the text
-------------------------------------------------------------------------------}
function TGJHSlider.GetSliderEnd: Integer;
var
 LCaption : String;
begin
 //Default, if there is nothing to print
 if FOrient=csVertical then
 begin
  Result:=Height;
  //Compensate for the pointer overhang
  if FPointers then dec(Result,Width div 4);
 end
 else
 begin
  Result:=Width;
  //Compensate for the pointer overhang
  if FPointers then dec(Result,Height div 4);
 end;
 //Get the value
 LCaption:=GetValue;
 if LCaption<>'' then
 begin
  //Find out how high it will be
  Canvas.Font:=Font;
  if FOrient=csVertical then
   Result:=Height-Canvas.GetTextHeight(LCaption)
  else //Or wide
   Result:=Width-(Canvas.GetTextWidth(FCaption)+FGap);
 end;
end;

{-------------------------------------------------------------------------------
Returns a string representation of the value
-------------------------------------------------------------------------------}
function TGJHSlider.GetValue: String;
var
 L: Byte;
begin
 Result:='';
 //If we are showing a value
 if FShowValue then
  if FHexValue then //And it is in hex
  begin 
   //Work out how many digits are required
   L:=1;
   while IntToHex(FMax,L)[1]<>'0' do inc(L);
   Result:='0x'+IntToHex(FPosition,L-1)
  end
  else //Otherwise in decimal
  begin
   //Pad to how much?
   L:=Length(IntToStr(FMax));
   Result:=PadLeft(IntToStr(FPosition),L);
  end;
end;

{-------------------------------------------------------------------------------
Work out where the top of the slider is
-------------------------------------------------------------------------------}
function TGJHSlider.GetSliderStart: Integer;
var
 LCaption : String;
 LTemp: Integer;
begin
 //Default - if no text to print
 if FOrient=csVertical then
 begin
  Result:=0;
  //Compensate for the pointer overhang
  if FPointers then inc(Result,Width div 4);
 end
 else
 begin
  Result:=0;
  //Compensate for the pointer overhang
  if FPointers then inc(Result,Height div 4);
 end;
 if FCaption<>'' then
 begin
  Canvas.Font:=Font;
  if FOrient=csVertical then
   Result:=Canvas.GetTextHeight(FCaption)
  else
  begin
   //Get the maximum value
   LTemp:=FPosition;//Remember the previous setting
   FPosition:=FMax;//Set it to the max
   LCaption:=GetValue;//Get the value
   FPosition:=LTemp;//Reset it
   Result:=Canvas.GetTextWidth(LCaption)+FGap;
  end;
 end;
end;

//Registry Class methods +++++++++++++++++++++++++++++++++++++++++++++++++++++++

{-------------------------------------------------------------------------------
Class creator - initialises the global variables
-------------------------------------------------------------------------------}
constructor TGJHRegistry.Create(LRegKey: String);
begin
 inherited Create;
 FRegKey:=LRegKey;
end;

{-------------------------------------------------------------------------------
Class destructor
-------------------------------------------------------------------------------}
destructor TGJHRegistry.Destroy;
begin
 inherited;
end;

{-------------------------------------------------------------------------------
Open the registry key
-------------------------------------------------------------------------------}
procedure TGJHRegistry.OpenReg(key: String);
begin
 FRegistry:=TRegistry.Create;
 if key<>'' then key:='\'+key;
 FRegistry.OpenKey(FRegKey+key,true);
end;

{-------------------------------------------------------------------------------
Function to delete a key from the registry
-------------------------------------------------------------------------------}
function TGJHRegistry.DeleteKey(key: String): Boolean;
var
 x: Boolean;
begin
 x:=True;
 OpenReg(ExtractKey(key));
 if FRegistry.ValueExists(key) then x:=FRegistry.DeleteValue(key);
 FRegistry.Free;
 Result:=x;
end;

{-------------------------------------------------------------------------------
Function to read a string from the registry, or create it if it doesn't exist
-------------------------------------------------------------------------------}
function TGJHRegistry.GetRegValS(V: String;D: String): String;
var
 X: String;
begin
 OpenReg(ExtractKey(V));
 If FRegistry.ValueExists(V)then X:=FRegistry.ReadString(V)
 else begin X:=D;FRegistry.WriteString(V,X);end;
 FRegistry.Free;
 Result:=X;
end;

{-------------------------------------------------------------------------------
Function to read an array from the registry, or create it if it doesn't exist
-------------------------------------------------------------------------------}
procedure TGJHRegistry.GetRegValA(V: String;var D: array of Byte);
var
 s: Integer;
begin
 OpenReg(ExtractKey(V));
 If FRegistry.ValueExists(V)then
 begin
  s:=FRegistry.GetDataSize(V);
  FRegistry.ReadBinaryData(V,D,s);
 end
 else
 begin
  FRegistry.WriteBinaryData(V,D,SizeOf(D));
 end;
 FRegistry.Free;
end;

{-------------------------------------------------------------------------------
Function to read an integer from the registry, or create it if it doesn't exist
-------------------------------------------------------------------------------}
function TGJHRegistry.GetRegValI(V: String;D: Cardinal): Cardinal;
var
 X: Cardinal;
begin
 OpenReg(ExtractKey(V));
 If FRegistry.ValueExists(V)then X:=FRegistry.ReadInteger(V)
 else begin X:=D;FRegistry.WriteInteger(V,X);end;
 FRegistry.Free;
 Result:=X;
end;

{-------------------------------------------------------------------------------
Function to read a boolean from the registry, or create it if it doesn't exist
-------------------------------------------------------------------------------}
function TGJHRegistry.GetRegValB(V: String;D: Boolean): Boolean;
var
 X: Boolean;
begin
 OpenReg(ExtractKey(V));
 If FRegistry.ValueExists(V)then X:=FRegistry.ReadBool(V)
 else begin X:=D;FRegistry.WriteBool(V,X);end;
 FRegistry.Free;
 Result:=X;
end;

{-------------------------------------------------------------------------------
Does the specified key exist?
-------------------------------------------------------------------------------}
function TGJHRegistry.DoesKeyExist(V: String):Boolean;
begin
 OpenReg(ExtractKey(V));
 Result:=FRegistry.ValueExists(V);
 FRegistry.Free;
end;

{-------------------------------------------------------------------------------
Function to save a string to the registry
-------------------------------------------------------------------------------}
procedure TGJHRegistry.SetRegValS(V: String;D: String);
begin
 OpenReg(ExtractKey(V));
 FRegistry.WriteString(V,D);
 FRegistry.Free;
end;

{-------------------------------------------------------------------------------
Function to save an array to the registry
-------------------------------------------------------------------------------}
procedure TGJHRegistry.SetRegValA(V: String;var D: array of Byte);
begin
 OpenReg(ExtractKey(V));
 FRegistry.WriteBinaryData(V,D,SizeOf(D));
 FRegistry.Free;
end;

{-------------------------------------------------------------------------------
Function to save an integer to the registry
-------------------------------------------------------------------------------}
procedure TGJHRegistry.SetRegValI(V: String;D: Cardinal);
begin
 OpenReg(ExtractKey(V));
 FRegistry.WriteInteger(V,D);
 FRegistry.Free;
end;

{-------------------------------------------------------------------------------
Function to save a boolean to the registry
-------------------------------------------------------------------------------}
procedure TGJHRegistry.SetRegValB(V: String;D: Boolean);
begin
 OpenReg(ExtractKey(V));
 FRegistry.WriteBool(V,D);
 FRegistry.Free;
end;

{-------------------------------------------------------------------------------
Function to extract key part of string
-------------------------------------------------------------------------------}
function TGJHRegistry.ExtractKey(var V: String):String;
begin
 Result:='';
 if Pos('\',V)>0 then
 begin
  Result:=Copy(V,1,Pos('\',V)-1);
  V:=Copy(V,Pos('\',V)+1);
 end;
end;

end.
