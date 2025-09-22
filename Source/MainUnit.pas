unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
 Classes, SysUtils, Forms, Controls, ExtCtrls, GJHCustomComponents, Dialogs,
 Graphics, StdCtrls, Buttons, ComCtrls;

 { TMainForm }

 type
 TMainForm = class(TForm)
  Label1:TLabel;
  Panel1:TPanel;
  Background: TImage;
  Texture: TImage;
  ColourMix: TShape;
  procedure CheckBox1Change(Sender: TObject);
  procedure FormActivate(Sender: TObject);
  procedure FormDeactivate(Sender: TObject);
  procedure FormPaint(Sender: TObject);
  procedure FormShow(Sender: TObject);
  procedure Image2Click(Sender: TObject);
  procedure Panel1Click(Sender: TObject);
  procedure Panel1Paint(Sender:TObject);
  procedure TileCanvas(c: TCanvas);
 public
  TickBox1    : TRISCOSTickBox;
  TickBox2    : TRISCOSTickBox;
  RedSlider   : TRISCOSSlider;
  GreenSlider : TRISCOSSlider;
  BlueSlider  : TRISCOSSlider;
  HSlider     : TRISCOSSlider;
  Fader       : TRISCOSSlider;
  RadioBox1   : TRISCOSRadioBox;
  RadioBox2   : TRISCOSRadioBox;
  RadioBox3   : TRISCOSRadioBox;
  DefaultBtn  : TRISCOSButton;
  NormalBtn   : TRISCOSButton;  
  DefaultBtn1 : TRISCOSButton;
  NormalBtn1  : TRISCOSButton;
 end;

var
 MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormShow(Sender: TObject);
begin
 Caption:='GJH Custom Components V'+GJHVersion;
 //RISC OS style tick box
 TickBox1:=TRISCOSTickBox.Create(MainForm as TControl);
 TickBox1.Parent:=MainForm as TWinControl;
 TickBox1.Visible:=True;
 TickBox1.Caption:='Display values in hex';
 TickBox1.Top:=Round(10*(PixelsPerInch/DesignTimePPI));
 TickBox1.Left:=Round(10*(PixelsPerInch/DesignTimePPI));
 TickBox1.Name:='ROTickbox';
 TickBox1.OnChange:=@CheckBox1Change;
 //RISC OS style tick box
 TickBox2:=TRISCOSTickBox.Create(MainForm as TControl);
 TickBox2.Parent:=MainForm as TWinControl;
 TickBox2.Visible:=True;
 TickBox2.Caption:='Display values in dec';
 TickBox2.Top:=Round(10*(PixelsPerInch/DesignTimePPI));
 TickBox2.Left:=TickBox1.Left+TickBox1.Width;
 TickBox2.Name:='ROTickbox2';
// TickBox2.OnChange:=@CheckBox1Change;
 //Move the colour box
 ColourMix.Top:=TickBox1.Top+TickBox1.Height+Round(4*(PixelsPerInch/DesignTimePPI));
 //Sliders - Red
 RedSlider:=TRISCOSSlider.Create(MainForm as TComponent);
 RedSlider.Parent:=MainForm as TWinControl;
 RedSlider.Visible:=True;
 RedSlider.Top:=ColourMix.Top+ColourMix.Height+Round(4*(PixelsPerInch/DesignTimePPI));
 RedSlider.Left:=Round(10*(PixelsPerInch/DesignTimePPI));
 RedSlider.Colour:=$0000FF;
 RedSlider.Max:=255;
 RedSlider.Position:=25;
 RedSlider.Width:=Round(40*(PixelsPerInch/DesignTimePPI));
 RedSlider.Height:=Height-RedSlider.Top-Round(8*(PixelsPerInch/DesignTimePPI));
 RedSlider.HexValue:=True;
 RedSlider.Caption:='Red';
 RedSlider.ShowValue:=True;
 RedSlider.Name:='RedSlider';
 RedSlider.OnChange:=@CheckBox1Change;
 //Sliders - Green
 GreenSlider:=TRISCOSSlider.Create(MainForm as TComponent);
 GreenSlider.Parent:=MainForm as TWinControl;
 GreenSlider.Visible:=True;
 GreenSlider.Top:=ColourMix.Top+ColourMix.Height+Round(4*(PixelsPerInch/DesignTimePPI));
 GreenSlider.Left:=RedSlider.Left+RedSlider.Width+Round(4*(PixelsPerInch/DesignTimePPI));
 GreenSlider.Colour:=$00FF00;
 GreenSlider.Max:=255;
 GreenSlider.Position:=128;
 GreenSlider.Width:=Round(30*(PixelsPerInch/DesignTimePPI));
 GreenSlider.Height:=RedSlider.Height;
 GreenSlider.Caption:='';
 GreenSlider.ShowValue:=False;
 GreenSlider.HexValue:=True;
 GreenSlider.Name:='GreenSlider';
 GreenSlider.OnChange:=@CheckBox1Change;
 GreenSlider.Transparent:=False;
 GreenSlider.Pointers:=False;
 GreenSlider.Border3D:=True;
 GreenSlider.Outline:=csOutNone;
 //Sliders - Blue
 BlueSlider:=TRISCOSSlider.Create(MainForm as TComponent);
 BlueSlider.Parent:=MainForm as TWinControl;
 BlueSlider.Visible:=True;
 BlueSlider.Top:=ColourMix.Top+ColourMix.Height+Round(4*(PixelsPerInch/DesignTimePPI));
 BlueSlider.Left:=GreenSlider.Left+GreenSlider.Width+Round(4*(PixelsPerInch/DesignTimePPI));
 BlueSlider.Colour:=$FF0000;
 BlueSlider.Max:=255;
 BlueSlider.Position:=255;
 BlueSlider.Width:=Round(40*(PixelsPerInch/DesignTimePPI));
 BlueSlider.Height:=RedSlider.Height;
 BlueSlider.Caption:='Blue';
 BlueSlider.ShowValue:=True;
 BlueSlider.HexValue:=True;
 BlueSlider.Name:='BlueSlider';
 BlueSlider.OnChange:=@CheckBox1Change;
 BlueSlider.Gradient:=True;
 //Sliders - Horizontal
 HSlider:=TRISCOSSlider.Create(MainForm as TComponent);
 HSlider.Parent:=MainForm as TWinControl;
 HSlider.Visible:=True;
 HSlider.Top:=ColourMix.Top;
 HSlider.Left:=ColourMix.Left+ColourMix.Width+Round(4*(PixelsPerInch/DesignTimePPI));
 HSlider.Colour:=$0000FF;
 HSlider.Max:=102;
 HSlider.Position:=40;
 HSlider.Min:=40;
 HSlider.Width:=Width-HSlider.Left-Round(8*(PixelsPerInch/DesignTimePPI));
 HSlider.Height:=Round(30*(PixelsPerInch/DesignTimePPI));
 HSlider.Caption:='';
 HSlider.ShowValue:=True;
 HSlider.HexValue:=False;
 HSlider.Name:='HorizSlider';
 HSlider.Orientation:=csHorizontal;
 HSlider.Pointers:=False;
 HSlider.Outline:=csOutInner;
 HSlider.Suffix:='K';
 //Sliders - Fader
 Fader:=TRISCOSSlider.Create(MainForm as TComponent);
 Fader.Parent:=MainForm as TWinControl;
 Fader.Visible:=True;
 Fader.Top:=Round(8*(PixelsPerInch/DesignTimePPI));
 Fader.Left:=Round(10*(PixelsPerInch/DesignTimePPI))+Panel1.Left+Panel1.Width;
 Fader.Colour:=$000000;
 Fader.Max:=600;
 Fader.Min:=-180;
 Fader.Position:=0;
 Fader.Pointers:=True;
 Fader.Faders:=True;
 Fader.FaderColour:=csRed;
 Fader.GradColour:=$FF0000;
 Fader.FillSlider:=True;
 Fader.Width:=Round(60*(PixelsPerInch/DesignTimePPI));
 Fader.Height:=Height-Fader.Top-Round(8*(PixelsPerInch/DesignTimePPI));
 Fader.HexValue:=False;
 Fader.ValueDiv:=10;
 Fader.Suffix:='dB';
 Fader.Caption:='Input #12';
 Fader.ShowValue:=True;
 Fader.Name:='Fader';
 Fader.Border3D:=True;
 Fader.Outline:=csOutNone;
 //Radio options
 RadioBox1:=TRISCOSRadioBox.Create(MainForm as TControl);
 RadioBox1.Parent:=MainForm as TWinControl;
 RadioBox1.Visible:=True;
 RadioBox1.Caption:='Red';
 RadioBox1.Top:=HSlider.Top+HSlider.Height+4;
 RadioBox1.Left:=HSlider.Left;
 RadioBox1.Name:='RORadioBox1';
 RadioBox1.Ticked:=True;
 //
 RadioBox2:=TRISCOSRadioBox.Create(MainForm as TControl);
 RadioBox2.Parent:=MainForm as TWinControl;
 RadioBox2.Visible:=True;
 RadioBox2.Caption:='Green';
 RadioBox2.Top:=RadioBox1.Top+RadioBox1.Height;
 RadioBox2.Left:=HSlider.Left;
 RadioBox2.Name:='RORadioBox2';
 //
 RadioBox3:=TRISCOSRadioBox.Create(MainForm as TControl);
 RadioBox3.Parent:=MainForm as TWinControl;
 RadioBox3.Visible:=True;
 RadioBox3.Caption:='Blue';
 RadioBox3.Top:=RadioBox2.Top+RadioBox2.Height;
 RadioBox3.Left:=HSlider.Left;
 RadioBox3.Name:='RORadioBox3';
 //
 DefaultBtn:=TRISCOSButton.Create(MainForm as TControl);
 DefaultBtn.Parent:=MainForm as TWinControl;
 DefaultBtn.Visible:=True;
 DefaultBtn.Default:=True;
 DefaultBtn.Caption:='OK';
 DefaultBtn.Top:=RadioBox3.Top+RadioBox3.Height+Round(4*(PixelsPerInch/DesignTimePPI));
 DefaultBtn.Left:=RadioBox3.Left;
 DefaultBtn.OnClick:=@Panel1Click;
 //
 NormalBtn:=TRISCOSButton.Create(MainForm as TControl);
 NormalBtn.Parent:=MainForm as TWinControl;
 NormalBtn.Visible:=True;
 NormalBtn.Default:=False;
 NormalBtn.Caption:='Cancel';
 NormalBtn.Top:=DefaultBtn.Top+Round(4*(PixelsPerInch/DesignTimePPI));
 NormalBtn.Left:=DefaultBtn.Left+DefaultBtn.Width+Round(8*(PixelsPerInch/DesignTimePPI));
 NormalBtn.OnClick:=@Image2Click;
 //
 DefaultBtn1:=TRISCOSButton.Create(MainForm as TControl);
 DefaultBtn1.Parent:=MainForm as TWinControl;
 DefaultBtn1.Visible:=True;
 DefaultBtn1.Default:=True;
 DefaultBtn1.Caption:='OK';
 DefaultBtn1.Top:=DefaultBtn.Top+DefaultBtn.Height+Round(4*(PixelsPerInch/DesignTimePPI));
 DefaultBtn1.Left:=RadioBox3.Left;
 DefaultBtn1.Enabled:=False;
 //
 NormalBtn1:=TRISCOSButton.Create(MainForm as TControl);
 NormalBtn1.Parent:=MainForm as TWinControl;
 NormalBtn1.Visible:=True;
 NormalBtn1.Default:=False;
 NormalBtn1.Caption:='Cancel';
 NormalBtn1.Top:=DefaultBtn1.Top+Round(4*(PixelsPerInch/DesignTimePPI));
 NormalBtn1.Left:=DefaultBtn1.Left+DefaultBtn1.Width+Round(8*(PixelsPerInch/DesignTimePPI));
 NormalBtn1.Enabled:=False;
 //
 CheckBox1Change(nil);
 Application.OnActivate:=@FormActivate;
 Application.OnDeactivate:=@FormDeActivate;
end;

procedure TMainForm.Image2Click(Sender: TObject);
begin
 //Caption:='Cancel Clicked';
end;

procedure TMainForm.Panel1Click(Sender: TObject);
begin
 //Caption:='OK Clicked';
end;

procedure TMainForm.Panel1Paint(Sender:TObject);
var
// b : TBrush;
 rc: TRect;
begin
 rc:=Rect(0,0,Panel1.Canvas.Width,Panel1.Canvas.Height);
{ b:=Tbrush.Create;
 b.Bitmap:=Background.Picture.Bitmap;
 Panel1.Canvas.Brush:=b;}
 Panel1.Canvas.StretchDraw(rc,Background.Picture.Graphic);
// b.Free;
end;

procedure TMainForm.CheckBox1Change(Sender: TObject);
begin
 RedSlider.HexValue   :=TickBox1.Ticked;
 GreenSlider.HexValue :=TickBox1.Ticked;
 BlueSlider.HexValue  :=TickBox1.Ticked;
 ColourMix.Brush.Color:=BlueSlider.Position<<16
                       +GreenSlider.Position<<8
                       +RedSlider.Position;
 TickBox2.Ticked:=not TickBox1.Ticked;
end;

procedure TMainForm.FormActivate(Sender: TObject);
begin
// Caption:='I have focus';
end;

procedure TMainForm.FormDeactivate(Sender: TObject);
begin
// Caption:='I do not have focus';
end;

procedure TMainForm.FormPaint(Sender: TObject);
begin
 TileCanvas(TForm(Sender).Canvas);
end;

procedure TMainForm.TileCanvas(c: TCanvas);
var
 b : TBrush;
 rc: TRect;
begin
 rc:=Rect(0,0,c.Width,c.Height);
 b:=Tbrush.Create;
 b.Bitmap:=Texture.Picture.Bitmap;
 c.Brush:=b;
 c.FillRect(rc);
 b.Free;
end;

end.
