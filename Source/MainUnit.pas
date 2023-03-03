unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
 Classes, SysUtils, Forms, Controls, ExtCtrls, GJHCustomComponents, Dialogs,
 Graphics, StdCtrls, Buttons;

 { TMainForm }

 type
 TMainForm = class(TForm)
  Title: TLabel;
  RCap: TImage;
  MidB: TImage;
  MidT: TImage;
  LCap: TImage;
  Texture: TImage;
  ColourMix: TShape;
  procedure CheckBox1Change(Sender: TObject);
  procedure FormActivate(Sender: TObject);
  procedure FormDeactivate(Sender: TObject);
  procedure FormPaint(Sender: TObject);
  procedure FormShow(Sender: TObject);
  procedure Image2Click(Sender: TObject);
  procedure Panel1Click(Sender: TObject);
  procedure TileCanvas(c: TCanvas);
 private

 public
  TickBox1    : TGJHTickBox;
  RedSlider   : TGJHSlider;
  GreenSlider : TGJHSlider;
  BlueSlider  : TGJHSlider;
  HSlider     : TGJHSlider;
  RadioBox1   : TGJHRadioBox;
  RadioBox2   : TGJHRadioBox;
  RadioBox3   : TGJHRadioBox;
  DefaultBtn  : TGJHButton;
  NormalBtn   : TGJHButton;  
  DefaultBtn1 : TGJHButton;
  NormalBtn1  : TGJHButton;
 end;

var
 MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormShow(Sender: TObject);
begin
 //RISC OS style tick box
 TickBox1:=TGJHTickBox.Create(MainForm as TControl);
 TickBox1.Parent:=MainForm as TWinControl;
 TickBox1.Visible:=True;
 TickBox1.Caption:='Display values in hex';
 TickBox1.Top:=Round(10*(PixelsPerInch/DesignTimePPI));
 TickBox1.Left:=Round(10*(PixelsPerInch/DesignTimePPI));
 TickBox1.Name:='ROTickbox';
 TickBox1.OnChange:=@CheckBox1Change;
 ColourMix.Top:=TickBox1.Top+TickBox1.Height+Round(4*(PixelsPerInch/DesignTimePPI));
 //Sliders - Red
 RedSlider:=TGJHSlider.Create(MainForm as TComponent);
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
 GreenSlider:=TGJHSlider.Create(MainForm as TComponent);
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
 BlueSlider:=TGJHSlider.Create(MainForm as TComponent);
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
 HSlider:=TGJHSlider.Create(MainForm as TComponent);
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
 //Radio options
 RadioBox1:=TGJHRadioBox.Create(MainForm as TControl);
 RadioBox1.Parent:=MainForm as TWinControl;
 RadioBox1.Visible:=True;
 RadioBox1.Caption:='Red';
 RadioBox1.Top:=HSlider.Top+HSlider.Height+4;
 RadioBox1.Left:=HSlider.Left;
 RadioBox1.Name:='RORadioBox1';
 RadioBox1.Ticked:=True;
 //
 RadioBox2:=TGJHRadioBox.Create(MainForm as TControl);
 RadioBox2.Parent:=MainForm as TWinControl;
 RadioBox2.Visible:=True;
 RadioBox2.Caption:='Green';
 RadioBox2.Top:=RadioBox1.Top+RadioBox1.Height;
 RadioBox2.Left:=HSlider.Left;
 RadioBox2.Name:='RORadioBox2';
 //
 RadioBox3:=TGJHRadioBox.Create(MainForm as TControl);
 RadioBox3.Parent:=MainForm as TWinControl;
 RadioBox3.Visible:=True;
 RadioBox3.Caption:='Blue';
 RadioBox3.Top:=RadioBox2.Top+RadioBox2.Height;
 RadioBox3.Left:=HSlider.Left;
 RadioBox3.Name:='RORadioBox3';
 //
 DefaultBtn:=TGJHButton.Create(MainForm as TControl);
 DefaultBtn.Parent:=MainForm as TWinControl;
 DefaultBtn.Visible:=True;
 DefaultBtn.Default:=True;
 DefaultBtn.Caption:='OK';
 DefaultBtn.Top:=RadioBox3.Top+RadioBox3.Height+Round(4*(PixelsPerInch/DesignTimePPI));
 DefaultBtn.Left:=RadioBox3.Left;
 DefaultBtn.OnClick:=@Panel1Click;
 //
 NormalBtn:=TGJHButton.Create(MainForm as TControl);
 NormalBtn.Parent:=MainForm as TWinControl;
 NormalBtn.Visible:=True;
 NormalBtn.Default:=False;
 NormalBtn.Caption:='Cancel';
 NormalBtn.Top:=DefaultBtn.Top+Round(4*(PixelsPerInch/DesignTimePPI));
 NormalBtn.Left:=DefaultBtn.Left+DefaultBtn.Width+Round(8*(PixelsPerInch/DesignTimePPI));
 NormalBtn.OnClick:=@Image2Click;
 //
 DefaultBtn1:=TGJHButton.Create(MainForm as TControl);
 DefaultBtn1.Parent:=MainForm as TWinControl;
 DefaultBtn1.Visible:=True;
 DefaultBtn1.Default:=True;
 DefaultBtn1.Caption:='OK';
 DefaultBtn1.Top:=DefaultBtn.Top+DefaultBtn.Height+Round(4*(PixelsPerInch/DesignTimePPI));
 DefaultBtn1.Left:=RadioBox3.Left;
 DefaultBtn1.Enabled:=False;
 //
 NormalBtn1:=TGJHButton.Create(MainForm as TControl);
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
 Caption:='Cancel Clicked';
end;

procedure TMainForm.Panel1Click(Sender: TObject);
begin
 Caption:='OK Clicked';
end;

procedure TMainForm.CheckBox1Change(Sender: TObject);
begin
 RedSlider.HexValue   :=TickBox1.Ticked;
 GreenSlider.HexValue :=TickBox1.Ticked;
 BlueSlider.HexValue  :=TickBox1.Ticked;
 ColourMix.Brush.Color:=BlueSlider.Position<<16
                       +GreenSlider.Position<<8
                       +RedSlider.Position;
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
