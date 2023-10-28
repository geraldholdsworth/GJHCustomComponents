unit GJHCustomComponents;

{
GJH Custom Components V1.06
Copyright (C) 2023 Gerald Holdsworth gerald@hollypops.co.uk

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
 Classes, SysUtils, Graphics, ExtCtrls, Controls, Registry, StrUtils, Math,
 Forms;

{$M+}

//Global constants
const
 csHorizontal = 0;
 csVertical   = 1;
 csOutNone    = 0;
 csOutInner   = 1;
 csOutOuter   = 2;
 csOutBoth    = 3;
 GJHVersion   = '1.06';
 cmColBlack   = #$81#$00#$00#$00;
 cmColRed     = #$81#$00#$00#$FF;
 cmColGreen   = #$81#$00#$FF#$00;
 cmColDGreen  = #$81#$00#$77#$00;
 cmColYellow  = #$81#$00#$FF#$FF;
 cmColBlue    = #$81#$FF#$00#$00;
 cmColCyan    = #$81#$FF#$00#$FF;
 cmColMagenta = #$81#$FF#$FF#$00;
 cmColWhite   = #$81#$FF#$FF#$FF;
 cmResetCol   = #$82;
 cmHighBlack  = #$83#$00#$00#$00;
 cmHighRed    = #$83#$00#$00#$FF;
 cmHighGreen  = #$83#$00#$FF#$00;
 cmHighYellow = #$83#$00#$FF#$FF;
 cmHighBlue   = #$83#$FF#$00#$00;
 cmHighCyan   = #$83#$FF#$00#$FF;
 cmHighMagenta= #$83#$FF#$FF#$00;
 cmHighWhite  = #$83#$FF#$FF#$FF;
 cmResetHigh  = #$84;
 cmBold       = #$85#$01;
 cmItalic     = #$85#$02;
 cmBoldItalic = #$85#$03;
 cmStrike     = #$85#$04;
 cmBoldStrike = #$85#$05;
 cmItalicStrike= #$85#$06;
 cmBoldItalicStrike= #$85#$07;
 cmUnder      = #$85#$08;
 cmBoldUnder  = #$85#$09;
 cmItalicUnder= #$85#$0A;
 cmBoldItalicUnder= #$85#$0B;
 cmStrikeUnder= #$85#$0C;
 cmBoldStrikeUnder= #$85#$0D;
 cmItalicStrikeUnder= #$85#$0E;
 cmBoldItalicStrikeUnder= #$85#$0F;
 cmResetStyle = #$86;

//RISC OS style tick boxes - declarations ++++++++++++++++++++++++++++++++++++++
type
 TGJHTickBoxes = class(TGraphicControl)
 private
  FExclusive,
  FOnlyMouse,
  FTicked    : Boolean;
  FGroup     : Integer;
  FOn,
  FOff       : TPortableNetworkGraphic;
  FOnChange  : TNotifyEvent;
  FCaption   : String;
  FColour    : TColor;
  procedure SetWidth(const LCaption: String);
  procedure SetTicked(const LTicked: Boolean);
  procedure UnsetOthers;
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
{$INCLUDE 'TickBoxGraphics.pas'}
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
{$INCLUDE 'RadioBoxGraphics.pas'}
 protected
  // Protected Methods
 published
  constructor Create(AOwner: TComponent); override;
  property Group : Integer read FGroup write FGroup default 0;
 public
  destructor Destroy; override;
end;

//Coloured Slider - declarations +++++++++++++++++++++++++++++++++++++++++++++++
type TGJHSlider = class(TGraphicControl)
 private
  FBackColour,
  FColour      : TColor;
  F3DBorder,
  FTransparent,
  FMouseIsDown,
  FShowValue,
  FHexValue,
  FGradient,
  FPointers,
  FFillSlider  : Boolean;
  FOutline,
  FPosition,
  FMax,
  FMin,
  FOrient,
  FBorderSize,
  FStep        : Integer;
  FOnChange    : TNotifyEvent;
  FSuffix,
  FCaption     : String;
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
  procedure SetOutline(const LOutline: Integer);
  procedure SetSuffix(const LSuffix: String);
  procedure SetCaption(const LCaption: String);
  procedure SetBackColour(const LBackColour: TColor);
  procedure SetTransparent(const LTransparent: Boolean);
  procedure Set3DBorder(const L3DBorder: Boolean);
  function GetSliderEnd: Integer;
  function GetValue: String;
  function GetSliderStart: Integer;
  procedure FDown(Sender: TObject; {%H-}Button: TMouseButton;
                              {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
  procedure FMove(Sender: TObject; {%H-}Shift: TShiftState;  X,Y: Integer);
  procedure FUp(Sender: TObject; {%H-}Button: TMouseButton;
                              {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
 const
{$INCLUDE 'PointerGraphics.pas'}
  FGap = 4;
 protected
  procedure Paint; override;
 published
  //Methods
  constructor Create(AOwner: TComponent); override;
  //Events
  property OnChange   : TNotifyEvent read FOnChange    write FOnChange;
  //Properties
  property BackColour : TColor       read FBackColour  write SetBackColour  default $FFFFFF;
  property Border3D   : Boolean      read F3DBorder    write Set3DBorder    default False;
  property Caption    : string       read FCaption     write SetCaption;
  property Colour     : TColor       read FColour      write SetColour      default $0000FF;
  property FillSlider : Boolean      read FFillSlider  write SetFillSlider  default False;
  property Gradient   : Boolean      read FGradient    write SetGradient    default False;
  property HexValue   : Boolean      read FHexValue    write SetHexValue    default False;
  property Max        : Integer      read FMax         write SetMax         default 100;
  property Min        : Integer      read FMin         write SetMin         default 0;
  property Orientation: Integer      read FOrient      write SetOrient      default csVertical;
  property Outline    : Integer      read FOutline     write SetOutline     default csOutOuter;
  property Pointers   : Boolean      read FPointers    write SetPointers    default True;
  property Position   : Integer      read FPosition    write SetPosition    default 0;
  property ShowValue  : Boolean      read FShowValue   write SetShowValue   default False;
  property Step       : Integer      read FStep        write SetStep        default 1;
  property Suffix     : string       read FSuffix      write SetSuffix;
  property Transparent: Boolean      read FTransparent write SetTransparent default True;
 public
  destructor Destroy; override;
end;

//RISC OS Buttons - declarations +++++++++++++++++++++++++++++++++++++++++++++++
type TGJHButton = class(TGraphicControl)
 private
  FOnClick : TNotifyEvent;
  FPushed,
  FDefault : Boolean;
  FCaption : String;
  FModalResult: TModalResult;
  procedure FDown(Sender: TObject; {%H-}Button: TMouseButton;
                              {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
  procedure FUp(Sender: TObject; {%H-}Button: TMouseButton;
                              {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
 protected
  procedure Paint; override;
  procedure SetDefault(const LDefault: Boolean);
  procedure SetCaption(const LCaption: String);
  procedure SetDimensions;
  procedure SetModalResult(const LModalResult: TModalResult);
 published
  //Methods
  constructor Create(AOwner: TComponent); override;
  //Events
  property OnClick   : TNotifyEvent read FOnClick    write FOnClick;
  //Properties
  property Default : Boolean       read FDefault  write SetDefault  default False;
  property Caption : String        read FCaption  write SetCaption;
  property ModalResult: TModalResult read FModalResult write SetModalResult default mrNone;
 public
  destructor Destroy; override;
end;

//TColouredMemo class - declarations +++++++++++++++++++++++++++++++++++++++++++
type TColouredMemo = class(TScrollingWinControl)
 private
  type //Extended the TStringList so we can repaint the form when a line is added
   TExtStringList=class(TStringList)
    FColouredMemo : TColouredMemo;
    function Add(const S: string): Integer; override;
    function AddObject(const S: string; AObject: TObject): Integer; override;
    function Add(const Fmt : string; const Args : Array of const): Integer; overload;
    function AddObject(const Fmt: string; Args : Array of const; AObject: TObject): Integer; overload;
    function AddPair(const AName, AValue: string): TStrings; overload;
    function AddPair(const AName, AValue: string; AObject: TObject): TStrings; overload;
    procedure AddStrings(TheStrings: TStrings); override;
    procedure AddStrings(TheStrings: TStrings; ClearFirst : Boolean); overload;
    procedure AddStrings(const TheStrings: array of string); overload; override;
    procedure AddStrings(const TheStrings: array of string; ClearFirst : Boolean); overload;
    procedure SetStrings(TheStrings: TStrings); override;
    procedure SetStrings(TheStrings: array of string); override; overload;
    Procedure AddText(Const S : String); override;
    procedure AddCommaText(const S: String);
    procedure AddDelimitedText(const S: String; ADelimiter: char; AStrictDelimiter: Boolean);
    procedure AddDelimitedtext(const S: String); overload;
    procedure Append(const S: string);
    procedure Assign(Source: TPersistent); override;
   end;
 var
  FLines    : TExtStringList;
  FLineSpace: Cardinal;
  FIndent   : Cardinal;
  FContent  : TImage;
  FPlainText: TExtStringList;
  FTextWrap : Boolean;
 protected
  procedure Paint; override;
  procedure SetLines(const AValue: TExtStringList);
  procedure SetTextWrap(const AValue: Boolean);
 public
  constructor Create(AOwner: TComponent); override;
  destructor Destroy; override;
 published
  procedure Clear;
  property AutoScroll;
  property Indent    : Cardinal       read FIndent    write FIndent;
  property Lines     : TExtStringList read FLines     write SetLines;
  property LineSpace : Cardinal       read FLineSpace write FLineSpace;
  property PlainText : TExtStringList read FPlainText;
  property TextWrap  : Boolean        read FTextWrap  write SetTextWrap;
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
  function GetRegValB(V: String): Boolean; overload;
  function GetRegValI(V: String;D: Cardinal): Cardinal;
  function GetRegValI(V: String): Cardinal; overload;
  function GetRegValS(V: String;D: String): String;
  function GetRegValS(V: String): String; overload;
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
function ParseCSVLine(Line: String): TStringArray;

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
                                             TGJHSlider,
                                             TGJHButton,
                                             TColouredMemo]);
end;

{-------------------------------------------------------------------------------
Parse a CSV line into an array
-------------------------------------------------------------------------------}
function ParseCSVLine(Line: String): TStringArray;
var
 Index: Integer;
 PLine: PChar;
begin
 Result:=nil;
 //If not blank
 if Line<>'' then
 begin
  //Split it into an array
  Result:=Line.Split(',','"');//Split at commas, unless surrounded by quotes
  //Make sure it has at least a field
  if Length(Result)>0 then
  for Index:=0 to Length(Result)-1 do
  begin
   //Get each field
   PLine:=PChar(Result[Index]);
   //And remove any quotes (except those escaped)
   Result[Index]:=AnsiExtractQuotedStr(PLine,'"');
  end;
 end;
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
// Height:=Canvas.GetTextHeight(' ');
// Width:=Height+Canvas.GetTextWidth(' ')+4;
 //Create the on and off graphics
 FOn:=TPortableNetworkGraphic.Create;
 FOff:=TPortableNetworkGraphic.Create;
end;

{-------------------------------------------------------------------------------
Class destructor - tidies up afterwards
-------------------------------------------------------------------------------}
destructor TGJHTickBoxes.Destroy;
begin
 FOn.Free;
 FOff.Free;
 inherited;
end;

{-------------------------------------------------------------------------------
Paint the control
-------------------------------------------------------------------------------}
procedure TGJHTickBoxes.Paint;
var
 Lgf : TPortableNetworkGraphic;
 R   : TRect;
 Lcol: TColor;
begin
 //Control enabled?
 Lcol:=Font.Color;
 if not Enabled then Font.Color:=$8E8E8E;
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
 Canvas.TextOut(Height,(Height-Canvas.TextHeight(Caption))div 2,' '+FCaption);
 Font.Color:=LCol
end;

{-------------------------------------------------------------------------------
React to the click
-------------------------------------------------------------------------------}
procedure TGJHTickBoxes.Click;
begin
 if not FExclusive then FTicked:=not FTicked else FTicked:=True;
 Invalidate;//Force a redraw
 Update;
 UnsetOthers;
 //Fire the OnChange event
 if Assigned(FOnChange) then FOnChange(Self as TObject);
 inherited Click;
end;

{-------------------------------------------------------------------------------
If this is set, unset every other one
-------------------------------------------------------------------------------}
procedure TGJHTickBoxes.UnsetOthers;
var
 LParent: TComponent;
 LGroup,
 Index  : Integer;
begin
 if(FExclusive)and(FTicked)then
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
    begin
     if LParent.Components[Index] is TGJHRadioBox then
      LGroup:=TGJHRadioBox(LParent.Components[Index]).Group
     else LGroup:=FGroup;
     if LGroup=FGroup then //Ignore other groups
      TGJHTickBoxes(LParent.Components[Index]).Ticked:=False; //Unset them
    end;
  end;
end;

{-------------------------------------------------------------------------------
Caption has changed, so adjust the dimensions
-------------------------------------------------------------------------------}
procedure TGJHTickBoxes.SetWidth(const LCaption: String);
var Ltext: String;
begin
 FCaption:=LCaption;
 Ltext:=' '+FCaption;
 Height:=Canvas.TextHeight(Ltext);
 Width:=Canvas.TextWidth(Ltext)+Height+4;
 Invalidate;//Force a redraw
 Update;
end;

{-------------------------------------------------------------------------------
The ticked state has been changed
-------------------------------------------------------------------------------}
procedure TGJHTickBoxes.SetTicked(const LTicked: Boolean);
begin
 FTicked:=LTicked;
 Invalidate; //Force a redraw
 Update;
 UnsetOthers;
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
 FExclusive:=False;
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
 FExclusive:=True;
 FGroup:=0;
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
 FBackColour:=$FFFFFF;
 FColour:=$0000FF;
 FCaption:='';
 FSuffix:='';
 FShowValue:=False;
 FStep:=1;
 FHexValue:=False;
 FOrient:=csVertical;
 FGradient:=False;
 FPointers:=True;
 FFillSlider:=False;
 FOutline:=csOutOuter;
 FTransparent:=True;
 F3DBorder:=False;
 FBorderSize:=Round(ScreenInfo.PixelsPerInchX/96)<<1;//Scaled and to the nearest even number
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
 Lcol      : TColor;
 procedure GetGraphic(LGraphic: array of Byte);
 begin
  Lms.Clear;
  Lms.Write(LGraphic[0],Length(LGraphic));
  Lms.Position:=0;
  Lpng.LoadFromStream(Lms);
 end;
begin
 //Control enabled?
 Lcol:=Font.Color;
 if not Enabled then Font.Color:=$8E8E8E;
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
 //Fill in the background
 if not FTransparent then
 begin
  Canvas.Brush.Color:=FBackColour;
  Canvas.Pen.Style:=psClear;
  if FOrient=csVertical then
   Canvas.Rectangle(LX,LY,LX+LSliderSize,LH)
  else
   Canvas.Rectangle(LY,LX,LH,LX+LSliderSize);
 end;
 //Gradient fill
 if FGradient then
 begin
  Canvas.Pen.Style:=psSolid;
  //Smallest unit for rectangles
  LUnit:=abs(LH-LY)/(FMax-FMin);
  for Index:=1 to FMax-FMin do
   if((Index<=FPosition-FMin)and(not FFillSlider))//Upto the position
   or(FFillSlider)then//Or the entire slider?
   begin
    Canvas.Brush.Color:=Round((FColour     AND$FF)*(Index/(FMax-FMin)))
                     OR Round((FColour>>8  AND$FF)*(Index/(FMax-FMin)))<<8
                     OR Round((FColour>>16 AND$FF)*(Index/(FMax-FMin)))<<16;
    Canvas.Pen.Color:=Canvas.Brush.Color;
    if FOrient=csVertical then
     Canvas.Rectangle(LX,LH-Ceil(LUnit*(Index-1)),LX+LSliderSize,LH-Ceil(LUnit*Index))
    else
     Canvas.Rectangle(LH+Ceil(LUnit*(Index-1)),LX,LH+Ceil(LUnit*Index),LX+LSliderSize);
   end;
 end
 else //Solid fill
 begin
  //Work out where the filler starts and ends
  if not FFillSlider then
   LPosition:=LH-Round(((FPosition-FMin)/(FMax-FMin))*(LH-LY))
  else
   LPosition:=LY;
  Canvas.Brush.Color:=FColour;
  if(FOutline AND csOutInner)=csOutInner then
  begin
   Canvas.Pen.Style:=psSolid;
   Canvas.Pen.Color:=$000000;
  end
  else
   Canvas.Pen.Style:=psClear;
  //Draw the rectangle
  if FOrient=csVertical then
   Canvas.Rectangle(LX,LPosition,LX+LSliderSize,LH)
  else
   Canvas.Rectangle(LPosition,LX,LH,LX+LSliderSize);
 end;
 //Draw a little sliver of bar if at minimum, and no outer outline
 if(not FFillSlider)and(FPosition=FMin)and((FOutline AND csOutOuter)=0)then
  if FOrient=csVertical then
   Canvas.Rectangle(LX,LH,LX+LSliderSize,LH+1)
  else
   Canvas.Rectangle(LH,LX,LH+1,LX+LSliderSize);
 //Draw the outline
 if(FOutline AND csOutOuter)=csOutOuter then
 begin
  Canvas.Pen.Color:=$000000;
  Canvas.Pen.Style:=psSolid;
  Canvas.Brush.Style:=bsClear;
  if FOrient=csVertical then
   Canvas.Rectangle(LX,LY,LX+LSliderSize,LH)
  else
   Canvas.Rectangle(LH,LX,LY,LX+LSliderSize);
 end;
 //3D Border
 if F3DBorder then
 begin
  //Top
  Canvas.Brush.Style:=bsSolid;
  Canvas.Pen.Style:=psSolid;
  Canvas.Brush.Color:=$777777;
  Canvas.Pen.Color:=$777777;
  Canvas.Rectangle(0,0,Width,FBorderSize);
  //Left
  Canvas.Rectangle(0,0,FBorderSize,Height);
  //Bottom
  Canvas.Brush.Color:=$FFFFFF;
  Canvas.Pen.Color:=$FFFFFF;
  Canvas.Rectangle(FBorderSize>>1,Height-FBorderSize,Width,Height);
  Canvas.Rectangle(0,Height-FBorderSize>>1,Width,Height);
  //Right;
  Canvas.Rectangle(Width-FBorderSize,FBorderSize>>1,Width,Height);
  Canvas.Rectangle(Width-FBorderSize>>1,0,Width,Height);
 end;
 //Draw the pointers
 if FPointers then
 begin
  //This is only calculated when partially filled
  LPosition:=LH-Round(((FPosition-FMin)/(FMax-FMin))*(LH-LY));
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
 Font.Color:=Lcol;
end;

{-------------------------------------------------------------------------------
Position and/or step has been changed
-------------------------------------------------------------------------------}
procedure TGJHSlider.SetPosition(const LPosition: Integer);
var
 LOldPosition: Integer;
begin
 //Ensure that they are valid
 if FStep<1 then FStep:=1;
 if FStep>FMax div 2 then FStep:=FMax div 2;
 //Remember the old position
 LOldPosition:=FPosition;
 //Now change it
 if(LPosition>=FMin)and(LPosition<=FMax)then FPosition:=LPosition;
 FPosition:=(FPosition div FStep)*FStep;
 Invalidate; //Force a redraw
 Update;
 //Fire the OnChange event, if it has changed
 if FPosition<>LOldPosition then
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
 Update;
end;

{-------------------------------------------------------------------------------
The suffix has been changed
-------------------------------------------------------------------------------}
procedure TGJHSlider.SetSuffix(const LSuffix: String);
begin
 FSuffix:=LSuffix;
 Invalidate; //Force a redraw
 Update;
end;

{-------------------------------------------------------------------------------
The caption has been changed
-------------------------------------------------------------------------------}
procedure TGJHSlider.SetCaption(const LCaption: String);
begin
 FCaption:=LCaption;
 Invalidate; //Force a redraw
 Update;
end;

{-------------------------------------------------------------------------------
The background colour has been changed
-------------------------------------------------------------------------------}
procedure TGJHSlider.SetBackColour(const LBackColour: TColor);
begin
 FBackColour:=LBackColour;
 Invalidate; //Force a redraw
 Update;
end;

{-------------------------------------------------------------------------------
The transparent setting has been changed
-------------------------------------------------------------------------------}
procedure TGJHSlider.SetTransparent(const LTransparent: Boolean);
begin
 FTransparent:=LTransparent;
 Invalidate; //Force a redraw
 Update;
end;

{-------------------------------------------------------------------------------
The 3D Border setting has been changed
-------------------------------------------------------------------------------}
procedure TGJHSlider.Set3DBorder(const L3DBorder: Boolean);
begin
 F3DBorder:=L3DBorder;
 Invalidate; //Force a redraw
 Update;
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
 Update;
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
 Update;
end;

{-------------------------------------------------------------------------------
The show value boolean has been toggled
-------------------------------------------------------------------------------}
procedure TGJHSlider.SetShowValue(const LShowValue: Boolean);
begin
 FShowValue:=LShowValue;
 Invalidate; //Force a redraw
 Update;
end;

{-------------------------------------------------------------------------------
The show as Hex has been toggled
-------------------------------------------------------------------------------}
procedure TGJHSlider.SetHexValue(const LHexValue: Boolean);
begin
 FHexValue:=LHexValue;
 Invalidate; //Force a redraw
 Update;
end;

{-------------------------------------------------------------------------------
Orientation has changed
-------------------------------------------------------------------------------}
procedure TGJHSlider.SetOrient(const LOrient: Integer);
begin
 if(LOrient=csHorizontal)or(LOrient=csVertical)then
 begin
  FOrient:=LOrient;
  Invalidate; //Force a redraw
  Update;
 end;
end;

{-------------------------------------------------------------------------------
Gradient has changed
-------------------------------------------------------------------------------}
procedure TGJHSlider.SetGradient(const LGradient: Boolean);
begin
 FGradient:=LGradient;
 Invalidate; //Force a redraw
 Update;
end;

{-------------------------------------------------------------------------------
Showing pointers has changed
-------------------------------------------------------------------------------}
procedure TGJHSlider.SetPointers(const LPointers: Boolean);
begin
 FPointers:=LPointers;
 Invalidate; //Force a redraw
 Update;
end;

{-------------------------------------------------------------------------------
Fill the slider has changed
-------------------------------------------------------------------------------}
procedure TGJHSlider.SetFillSlider(const LFillSlider: Boolean);
begin
 FFillSlider:=LFillSlider;
 Invalidate; //Force a redraw
 Update;
end;

{-------------------------------------------------------------------------------
The outline setting has changed
-------------------------------------------------------------------------------}
procedure TGJHSlider.SetOutline(const LOutline: Integer);
begin
 if(LOutline>=csOutNone)and(LOutline<=csOutBoth)then
 begin
  FOutline:=LOutline;
  Invalidate; //Force a redraw
  Update;
 end;
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
  Result:=Height-(Width div 4)
 else
  Result:=Width-(Height div 4);
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
 begin
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
  if Result<>'' then Result:=Result+FSuffix;
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
  Result:=Width div 4
 else
  Result:=Height div 4;
 Canvas.Font:=Font;
 if(FCaption<>'')and(FOrient=csVertical)then
  Result:=Canvas.GetTextHeight(FCaption);
 if FOrient=csHorizontal then
 begin
  //Get the maximum value
  LTemp:=FPosition;//Remember the previous setting
  FPosition:=FMax;//Set it to the max
  LCaption:=GetValue;//Get the value
  FPosition:=LTemp;//Reset it
  Result:=Canvas.GetTextWidth(LCaption)+FGap;
 end;
end;

//Button Methods +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

{-------------------------------------------------------------------------------
Class creator - initialises the global variables
-------------------------------------------------------------------------------}
constructor TGJHButton.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 //Set the default variables
 FDefault:=False;
 FCaption:='';
 FPushed:=False;
 SetDimensions;
 FModalResult:=mrNone;
 //We need to react to the MouseDown, MouseMove and MouseUp events
 OnMouseDown:=@FDown;
 OnMouseUp:=@FUp;
end;

{-------------------------------------------------------------------------------
Class destructor - tidies up afterwards
-------------------------------------------------------------------------------}
destructor TGJHButton.Destroy;
begin
 inherited;
end;

{-------------------------------------------------------------------------------
Paint the control
-------------------------------------------------------------------------------}
procedure TGJHButton.Paint;
var
 LX,LY   : Integer;
 LCol    : TColor;
 Lsize   : Integer;
begin
 //Border size, 2px scaled and to the nearest even number
 Lsize:=Round(ScreenInfo.PixelsPerInchX/96)<<1;
 //Draw the button
 Canvas.Brush.Style:=bsSolid;
 Canvas.Pen.Style:=psClear;
 //Outer border, top and left
 if(not FDefault)and(not FPushed)then //Normal button, not pushed
  Canvas.Brush.Color:=$FFFFFF;
 if(FDefault)and(not Enabled)then //Default button, disabled
  Canvas.Brush.Color:=$BBBBBB;
 if((FDefault)and(Enabled))                     //Default button, enabled
 or((not FDefault)and(Enabled)and(FPushed))then //and normal button, pushed, enabled
  Canvas.Brush.Color:=$777777;
 Canvas.Rectangle(0,0,Width,Lsize);
 Canvas.Rectangle(0,0,Lsize,Height);
 //Outer border, bottom and right 
 if(not FDefault)and(not FPushed)and(Enabled)then //Normal button, not pushed, enabled
  Canvas.Brush.Color:=$777777;
 if(FDefault)                                   //Default button
 or((not FDefault)and(Enabled)and(FPushed))then //and normal button, pushed, enabled
  Canvas.Brush.Color:=$FFFFFF;
 if(not FDefault)and(not Enabled)then //Normal button, disabled
  Canvas.Brush.Color:=$BBBBBB;
 Canvas.Rectangle(Width-Lsize,Lsize,Width,Height);
 Canvas.Rectangle(Width-Lsize div 2,Lsize div 2,Width,Lsize);
 Canvas.Rectangle(Lsize div 2,Height-Lsize,Width,Height);
 Canvas.Rectangle(0,Height-Lsize div 2,Lsize div 2,Height);
 //Button surface (normal) and gap between inside and outside borders (default)
 if(FDefault)then Canvas.Brush.Color:=$BBEEEE;
 if(not FDefault)then Canvas.Brush.Color:=$DDDDDD;
 Canvas.Rectangle(Lsize,Lsize,Width-Lsize,Height-Lsize);
 //Default button, inside
 if(FDefault)and(not FPushed)then
 begin
  //Top and left inside border
  Canvas.Brush.Color:=$FFFFFF;
  Canvas.Rectangle(Lsize*2,Lsize*2,Width-Lsize*2,Lsize*3);
  Canvas.Rectangle(Lsize*2,Lsize*2,Lsize*3,Height-Lsize*2);
  //Bottom and right inside border
  if Enabled then Canvas.Brush.Color:=$777777 else Canvas.Brush.Color:=$BBBBBB;
  Canvas.Rectangle(Width-Lsize*3,Lsize*3,Width-Lsize*2,Height-Lsize*2);
  Canvas.Rectangle(Width-Lsize*2-Lsize div 2,Lsize*2+Lsize div 2,Width-Lsize*2,Lsize*3);
  Canvas.Rectangle(Lsize*2+Lsize div 2,Height-Lsize*3,Width-Lsize*3,Height-Lsize*2);
  Canvas.Rectangle(Lsize*2,Height-Lsize*2-Lsize div 2,Lsize*2+Lsize div 2,Height-Lsize*2);
  //Button surface
  Canvas.Brush.Color:=$DDDDDD;
  Canvas.Rectangle(Lsize*3,Lsize*3,Width-Lsize*3,Height-Lsize*3);
 end;
 //Write the text
 if FCaption<>'' then
 begin
  //Remember the current colour
  LCol:=Font.Color;
  //Change if disabled
  if not Enabled then Font.Color:=$8E8E8E;
  //Find the centred position
  LX:=(Width-Canvas.GetTextWidth(FCaption))div 2;
  LY:=(Height-Canvas.GetTextHeight(FCaption))div 2;
  //Write with transparent background
  Canvas.Brush.Style:=bsClear;
  Canvas.TextOut(LX,LY,FCaption);
  //Change the colour back
  Font.Color:=LCol;
 end;
end;

{-------------------------------------------------------------------------------
React to the mouse down
-------------------------------------------------------------------------------}
procedure TGJHButton.FDown(Sender: TObject; Button: TMouseButton;
                            Shift: TShiftState; X, Y: Integer);
begin
 FPushed:=True;
 Invalidate;
 Update;
end;

{-------------------------------------------------------------------------------
React to the mouse up
-------------------------------------------------------------------------------}
procedure TGJHButton.FUp(Sender: TObject; Button: TMouseButton;
                            Shift: TShiftState; X, Y: Integer);
var
 Lctrl: TWinControl;
begin
 FPushed:=False;
 Invalidate;
 Update;
 if Assigned(FOnClick) then FOnClick(Self as TObject);
 if HasParent then
 begin
  Lctrl:=Parent;
  repeat
   if Lctrl.HasParent then Lctrl:=Lctrl.Parent;
  until(Lctrl is TForm)or(not Lctrl.HasParent);
  if Lctrl is TForm then TForm(Lctrl).ModalResult:=FModalResult;
 end;
end;

{-------------------------------------------------------------------------------
The 'default' setting has changed
-------------------------------------------------------------------------------}
procedure TGJHButton.SetDefault(const LDefault: Boolean);
begin
 FDefault:=LDefault;
 SetDimensions;
 Invalidate;
 Update;
end;

{-------------------------------------------------------------------------------
The caption has changed
-------------------------------------------------------------------------------}
procedure TGJHButton.SetCaption(const LCaption: String);
begin
 FCaption:=LCaption;
 Invalidate;
 Update;
end;

{-------------------------------------------------------------------------------
Set the dimensions
-------------------------------------------------------------------------------}
procedure TGJHButton.SetDimensions;
var
 w,h: Integer;
begin
 if FDefault then w:=92 else w:=84; //Width
 if FDefault then h:=34 else h:=26; //Height
 Width:=Round(w*(ScreenInfo.PixelsPerInchX/96)); //Set width, scaled
 Height:=Round(h*(ScreenInfo.PixelsPerInchX/96));//Set height, scaled
end;

{-------------------------------------------------------------------------------
The modal result has changed
-------------------------------------------------------------------------------}
procedure TGJHButton.SetModalResult(const LModalResult: TModalResult);
begin
 FModalResult:=LModalResult;
end;

//ColouredMemo Methods +++++++++++++++++++++++++++++++++++++++++++++++++++++++++

{-------------------------------------------------------------------------------
These bunch of functions and procedures are just inherited from the base class.
They are so that the main component gets repainted when the list is updated.
-------------------------------------------------------------------------------}
function TColouredMemo.TExtStringList.Add(const S: string): Integer;
begin
 Result:=inherited Add(S);
 if Assigned(FColouredMemo) then FColouredMemo.Invalidate;
end;

function TColouredMemo.TExtStringList.AddObject(const S: string; AObject: TObject): Integer;
begin
 Result:=inherited AddObject(S,AObject);
 if Assigned(FColouredMemo) then FColouredMemo.Invalidate;
end;

function TColouredMemo.TExtStringList.Add(const Fmt : string; const Args : Array of const): Integer;
begin
 Result:=inherited Add(Fmt,Args);
 if Assigned(FColouredMemo) then FColouredMemo.Invalidate;
end;

function TColouredMemo.TExtStringList.AddObject(const Fmt: string; Args : Array of const; AObject: TObject): Integer;
begin
 Result:=inherited AddObject(Fmt,Args,AObject);
 if Assigned(FColouredMemo) then FColouredMemo.Invalidate;
end;

function TColouredMemo.TExtStringList.AddPair(const AName, AValue: string): TStrings;
begin
 Result:=inherited AddPair(AName,AValue);
 if Assigned(FColouredMemo) then FColouredMemo.Invalidate;
end;

function TColouredMemo.TExtStringList.AddPair(const AName, AValue: string; AObject: TObject): TStrings;
begin
 Result:=inherited AddPair(AName,AValue,AObject);
 if Assigned(FColouredMemo) then FColouredMemo.Invalidate;
end;

procedure TColouredMemo.TExtStringList.AddStrings(TheStrings: TStrings);
begin
 inherited AddStrings(TheStrings);
 if Assigned(FColouredMemo) then FColouredMemo.Invalidate;
end;

procedure TColouredMemo.TExtStringList.AddStrings(TheStrings: TStrings; ClearFirst : Boolean);
begin
 inherited AddStrings(TheStrings,ClearFirst);
 if Assigned(FColouredMemo) then FColouredMemo.Invalidate;
end;

procedure TColouredMemo.TExtStringList.AddStrings(const TheStrings: array of string);
begin
 inherited AddStrings(TheStrings);
 if Assigned(FColouredMemo) then FColouredMemo.Invalidate;
end;

procedure TColouredMemo.TExtStringList.AddStrings(const TheStrings: array of string; ClearFirst : Boolean);
begin
 inherited AddStrings(TheStrings,ClearFirst);
 if Assigned(FColouredMemo) then FColouredMemo.Invalidate;
end;

procedure TColouredMemo.TExtStringList.SetStrings(TheStrings: TStrings);
begin
 inherited SetStrings(TheStrings);
 if Assigned(FColouredMemo) then FColouredMemo.Invalidate;
end;

procedure TColouredMemo.TExtStringList.SetStrings(TheStrings: array of string);
begin
 inherited SetStrings(TheStrings);
 if Assigned(FColouredMemo) then FColouredMemo.Invalidate;
end;

Procedure TColouredMemo.TExtStringList.AddText(Const S : String);
begin
 inherited AddText(S);
 if Assigned(FColouredMemo) then FColouredMemo.Invalidate;
end;

procedure TColouredMemo.TExtStringList.AddCommaText(const S: String);
begin
 inherited AddCommaText(S);
 if Assigned(FColouredMemo) then FColouredMemo.Invalidate;
end;

procedure TColouredMemo.TExtStringList.AddDelimitedText(const S: String; ADelimiter: char; AStrictDelimiter: Boolean);
begin
 inherited AddDelimitedText(S,ADelimiter,AStrictDelimiter);
 if Assigned(FColouredMemo) then FColouredMemo.Invalidate;
end;

procedure TColouredMemo.TExtStringList.AddDelimitedtext(const S: String);
begin
 inherited AddDelimitedText(S);
 if Assigned(FColouredMemo) then FColouredMemo.Invalidate;
end;

procedure TColouredMemo.TExtStringList.Append(const S: string);
begin
 inherited Append(S);
 if Assigned(FColouredMemo) then FColouredMemo.Invalidate;
end;

procedure TColouredMemo.TExtStringList.Assign(Source: TPersistent);
begin
 inherited Assign(Source);
 if Assigned(FColouredMemo) then FColouredMemo.Invalidate;
end;

{-------------------------------------------------------------------------------
Constructor method for the coloured memo
-------------------------------------------------------------------------------}
constructor TColouredMemo.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 //Create the line container
 FLines:=TExtStringList.Create;
 FLines.FColouredMemo:=Self;
 //Create the plain text container
 FPlainText:=TExtStringList.Create;
 FPlainText.FColouredMemo:=Self;
 //Create the canvas
 FContent:=TImage.Create(Self);
 FContent.Parent:=Self;
 FContent.Top :=0;
 FContent.Left:=0;
 FContent.Picture.Bitmap.Width :=ClientWidth;
 FContent.Picture.Bitmap.Height:=ClientHeight;
 FContent.Width :=ClientWidth;
 FContent.Height:=ClientHeight;
 FContent.Visible:=True;
 //Defaults
 FLineSpace:=4;   //Space between lines, in pixels
 FIndent:=4;      //Indent in from the left, in pixels
 Color:=$FFFFFF;  //Default background colour
 TextWrap:=False; //Whether to wrap a line to the next
end;

{-------------------------------------------------------------------------------
Destructor method for the coloured memo
-------------------------------------------------------------------------------}
destructor TColouredMemo.Destroy;
begin
 FLines.Free;
 FPlainText.Free;
 FContent.Free;
 inherited Destroy;
end;

{-------------------------------------------------------------------------------
Repaint method - this is where most of the work is done
-------------------------------------------------------------------------------}
procedure TColouredMemo.Paint;
var
 LLine,
 LIndex,
 XPos,
 YPos,
 W,H    : Integer;
 LPart,
 LText  : String;
 LTemp,
 LRed,
 LGreen,
 LBlue  : Byte;
 //Function to remove control characters
 function RemCont(rcText: String): String;
 var rcIndex: Integer;
 begin
  Result:='';
  if Length(rcText)>1 then
  begin
   rcIndex:=1;
   while rcIndex<=Length(rcText) do
   begin
    //Only ASCII printable characters
    if(ord(rcText[rcIndex])>31)and(ord(rcText[rcIndex])<127)then
     Result:=Result+rcText[rcIndex];
    //Our special styles, so move along a few more characters
    if(ord(rcText[rcIndex])=$81)
    or(ord(rcText[rcIndex])=$83)then inc(rcIndex,3);
    if ord(rcText[rcIndex])=$85 then inc(rcIndex,1);
    //Next character
    inc(rcIndex);
   end;
  end;
 end;
 //Procedure to print the text
 procedure PrintText(ptText: String);
 begin
  FContent.Canvas.FillRect(XPos,YPos,
                           XPos+FContent.Canvas.TextWidth(ptText),
                           YPos+FContent.Canvas.TextHeight(ptText)+FLineSpace);
  FContent.Canvas.TextOut(XPos,YPos,ptText);
  inc(XPos,FContent.Canvas.TextWidth(ptText));
 end;
 //Procedure to wrap the text
 procedure WrapText(wtText: String);
 var
  LStart,
  LLength: Integer;
 begin
  //Only wrap if it is longer than the available width
  if(FTextWrap)and(FContent.Canvas.TextWidth(wtText)+XPos>W)then
  begin
   //Start at the beginning
   LStart:=1;
   //And continue until we are out of characters
   while LStart<Length(wtText) do
   begin
    //Length of remaining string (if LStart+LLength are bigger than the string
    //length, then it'll just go to the end of the string)
    LLength:=Length(wtText);
    //Reduce the length until it'll fit
    while(FContent.Canvas.TextWidth(Copy(wtText,LStart,LLength))+XPos>W)
      and(LLength>1)do dec(LLength);
    //Then print what we can
    PrintText(Copy(wtText,LStart,LLength));
    //And move the starting position along
    inc(LStart,LLength);
    //If there are more characters left, then do a CR and LF
    if LStart<=Length(wtText) then
    begin
     //Move the Y pointer downwards
     inc(YPos,FContent.Canvas.TextHeight(wtText)+FLineSpace);
     //Reset X pointer
     XPos:=FLineSpace;
    end;
   end;
   //The string is only a single character, but still won't fit, so CR LF
   if Length(wtText)=1 then
   begin
    //Move the Y pointer downwards
    inc(YPos,FContent.Canvas.TextHeight(wtText)+FLineSpace);
    //Reset X pointer
    XPos:=FLineSpace;
   end;
   //The starting point is the last character, so print and move along
   if LStart=Length(wtText) then PrintText(Copy(wtText,LStart));
  end
  else PrintText(wtText);//No text wrapping
 end;
begin
 //Set the font
 FContent.Canvas.Font:=Font;
 //Starting size
 W:=ClientWidth;
 H:=FLineSpace;
 //First pass, calculate then set the canvas size
 if FLines.Count>0 then
 begin
  FPlainText.Clear;
  for LLine:=0 to FLines.Count-1 do
  begin
   LText:='';
   if FLines[LLine]='' then LText:=' ' //blank line will give 0 height
   else LText:=RemCont(FLines[LLine]);//Remove control characters
   if FContent.Canvas.TextWidth(LText)+FIndent>W then
   begin
    if not FTextWrap then W:=FContent.Canvas.TextWidth(LText)+FIndent
    //Take account of any text wrapping
    else inc(H,(FContent.Canvas.TextHeight(LText)+FLineSpace)
              *((FContent.Canvas.TextWidth(LText)+FIndent)div W));
   end;
   //Increase the height by a line
   inc(H,FContent.Canvas.TextHeight(LText)+FLineSpace);
   FPlainText.Add(LText);
  end;
 end;
 //Height can't be smaller than the scroll height
 if H<ClientHeight then H:=ClientHeight;
 //Set the canvas size
 FContent.Picture.Bitmap.Width:=W;
 FContent.Picture.Bitmap.Height:=H;
 FContent.Width:=W;
 FContent.Height:=H;
 //Clear the background
 FContent.Canvas.Brush.Color:=Color;
 FContent.Canvas.Brush.Style:=bsSolid;
 FContent.Canvas.Pen.Color:=Color;
 FContent.Canvas.Pen.Style:=psSolid;
 FContent.Canvas.Rectangle(0,0,W,H);
 //Are there any lines entered?
 if FLines.Count>0 then
 begin
  //Start at the top
  YPos:=FLineSpace;
  //Work our way through the lines
  for LLine:=0 to FLines.Count-1 do
  begin
   //Get the current line
   LText:=FLines[LLine];
   //If it is empty, put a space in so we get a blank line
   if LText='' then LText:=' ';
   //Indent it
   XPos:=FIndent;
   //Clear the 'part of' string
   LPart:='';
   //Start at the beginning
   LIndex:=1;
   while LIndex<=Length(LText) do
   begin
    //Top bit set? Then this means a change of style or colour
    if(ord(LText[LIndex])and$80)=$80 then
    begin
     //Output what we currently have
     if LPart<>'' then WrapText(LPart);
     //New part of string
     LPart:='';
     case (ord(LText[LIndex])AND$0F) of
      $01,$03: //Change of colour : $81 and $83
      begin
       LTemp:=ord(LText[LIndex])AND$0F;
       //Get the Blue index
       inc(LIndex);
       if LIndex<=Length(LText) then LBlue :=ord(LText[LIndex]);
       //Get the Green index
       inc(LIndex);
       if LIndex<=Length(LText) then LGreen:=ord(LText[LIndex]);
       //Get the Red index
       inc(LIndex);
       if LIndex<=Length(LText) then LRed  :=ord(LText[LIndex]);
       //Set the colour
       if Ltemp=$01 then //Foreground (text)
        FContent.Canvas.Font.Color:=LRed+LGreen<<8+LBlue<<16;
       if Ltemp=$03 then //Background (highlight)
        FContent.Canvas.Brush.Color:=LRed+LGreen<<8+LBlue<<16;
      end;
      $05: //Change of style : $85
      begin
       inc(LIndex);
       if LIndex<=Length(LText) then
       begin
        //Bold
        if(ord(LText[LIndex])AND$01)=$01 then
         FContent.Canvas.Font.Style:=FContent.Canvas.Font.Style+[fsBold]
        else //No Bold
         FContent.Canvas.Font.Style:=FContent.Canvas.Font.Style-[fsBold];
        //Italic
        if(ord(LText[LIndex])AND$02)=$02 then
         FContent.Canvas.Font.Style:=FContent.Canvas.Font.Style+[fsItalic]
        else //No Italic
         FContent.Canvas.Font.Style:=FContent.Canvas.Font.Style-[fsItalic];
        //Strikeout
        if(ord(LText[LIndex])AND$04)=$04 then
         FContent.Canvas.Font.Style:=FContent.Canvas.Font.Style+[fsStrikeOut]
        else //No Strikeout
         FContent.Canvas.Font.Style:=FContent.Canvas.Font.Style-[fsStrikeOut];
        //Underline
        if(ord(LText[LIndex])AND$08)=$08 then
         FContent.Canvas.Font.Style:=FContent.Canvas.Font.Style+[fsUnderline]
        else //No Underline
         FContent.Canvas.Font.Style:=FContent.Canvas.Font.Style-[fsUnderline];
       end;
      end;
      $02: FContent.Canvas.Font.Color:=Font.Color;//Reset foreground colour : $82
      $04: FContent.Canvas.Brush.Color:=Color;//Reset background colour : $84
      $06: FContent.Canvas.Font.Style:=Font.Style;//Reset font style : $86
     end;
    end;
    //Valid ASCII character? add it to the 'part of' string
    if(ord(LText[LIndex])>31)and(ord(LText[LIndex])<127)then
     LPart:=LPart+LText[LIndex];
    //Next character
    inc(LIndex);
   end;
   //Anything left that hasn't been printed?
   if LPart<>'' then WrapText(LPart);
   //Move the Y pointer downwards
   inc(YPos,FContent.Canvas.TextHeight(LText)+FLineSpace);
  end;
 end;
end;

{-------------------------------------------------------------------------------
Clears the text
-------------------------------------------------------------------------------}
procedure TColouredMemo.Clear;
begin
 FLines.Clear;
end;

{-------------------------------------------------------------------------------
Inherits from the base class - unlikely this ever gets called
-------------------------------------------------------------------------------}
procedure TColouredMemo.SetLines(const AValue: TExtStringList);
begin
 if AValue<>nil then FLines.Assign(AValue);
 Invalidate;
end;

{-------------------------------------------------------------------------------
Sets the text wrap flag, and fires off the repaint
-------------------------------------------------------------------------------}
procedure TColouredMemo.SetTextWrap(const AValue: Boolean);
begin
 FTextWrap:=AValue;
 Invalidate;
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
function TGJHRegistry.GetRegValS(V: String): String;
var
 X: String;
begin
 OpenReg(ExtractKey(V));
 If FRegistry.ValueExists(V)then X:=FRegistry.ReadString(V) else X:='';
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
function TGJHRegistry.GetRegValI(V: String): Cardinal;
var
 X: Cardinal;
begin
 OpenReg(ExtractKey(V));
 If FRegistry.ValueExists(V)then X:=FRegistry.ReadInteger(V) else X:=0;
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
function TGJHRegistry.GetRegValB(V: String): Boolean;
var
 X: Boolean;
begin
 OpenReg(ExtractKey(V));
 If FRegistry.ValueExists(V)then X:=FRegistry.ReadBool(V) else X:=False;
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
