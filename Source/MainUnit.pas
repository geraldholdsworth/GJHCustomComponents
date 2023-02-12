unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
 Classes, SysUtils, Forms, Controls, ExtCtrls, GJHCustomComponents, Dialogs,
 Graphics;

 { TMainForm }

 type
 TMainForm = class(TForm)
  Texture: TImage;
  ColourMix: TShape;
  procedure CheckBox1Change(Sender: TObject);
  procedure FormPaint(Sender: TObject);
  procedure FormShow(Sender: TObject);
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
 TickBox1.Top:=10;
 TickBox1.Left:=10;
 TickBox1.Name:='ROTickbox';
 TickBox1.OnChange:=@CheckBox1Change;
 //Sliders - Red
 RedSlider:=TGJHSlider.Create(MainForm as TComponent);
 RedSlider.Parent:=MainForm as TWinControl;
 RedSlider.Visible:=True;
 RedSlider.Top:=50;
 RedSlider.Left:=10;
 RedSlider.Colour:=$0000FF;
 RedSlider.Max:=255;
 RedSlider.Position:=25;
 RedSlider.Width:=40;
 RedSlider.Height:=360;
 RedSlider.HexValue:=True;
 RedSlider.Caption:='Red';
 RedSlider.ShowValue:=True;
 RedSlider.Name:='RedSlider';
 RedSlider.OnChange:=@CheckBox1Change;
 //Sliders - Green
 GreenSlider:=TGJHSlider.Create(MainForm as TComponent);
 GreenSlider.Parent:=MainForm as TWinControl;
 GreenSlider.Visible:=True;
 GreenSlider.Top:=50;
 GreenSlider.Left:=50;
 GreenSlider.Colour:=$00FF00;
 GreenSlider.Max:=255;
 GreenSlider.Position:=128;
 GreenSlider.Width:=40;
 GreenSlider.Height:=360;
 GreenSlider.Caption:='Green';
 GreenSlider.ShowValue:=True;
 GreenSlider.HexValue:=True;
 GreenSlider.Name:='GreenSlider';
 GreenSlider.OnChange:=@CheckBox1Change;
 //Sliders - Blue
 BlueSlider:=TGJHSlider.Create(MainForm as TComponent);
 BlueSlider.Parent:=MainForm as TWinControl;
 BlueSlider.Visible:=True;
 BlueSlider.Top:=50;
 BlueSlider.Left:=90;
 BlueSlider.Colour:=$FF0000;
 BlueSlider.Max:=255;
 BlueSlider.Position:=255;
 BlueSlider.Width:=40;
 BlueSlider.Height:=360;
 BlueSlider.Caption:='Blue';
 BlueSlider.ShowValue:=True;
 BlueSlider.HexValue:=True;
 BlueSlider.Name:='BlueSlider';
 BlueSlider.OnChange:=@CheckBox1Change;
 //Sliders - Horizontal
 HSlider:=TGJHSlider.Create(MainForm as TComponent);
 HSlider.Parent:=MainForm as TWinControl;
 HSlider.Visible:=True;
 HSlider.Top:=30;
 HSlider.Left:=144;
 HSlider.Colour:=$00FFFF;
 HSlider.Max:=100;
 HSlider.Position:=50;
 HSlider.Width:=360;
 HSlider.Height:=40;
 HSlider.Caption:='Tester';
 HSlider.ShowValue:=True;
 HSlider.HexValue:=True;
 HSlider.Name:='HorizSlider';
 HSlider.Orientation:=csHorizontal;
 HSlider.Gradient:=True;
 HSlider.FillSlider:=True;
 //Radio options
 RadioBox1:=TGJHRadioBox.Create(MainForm as TControl);
 RadioBox1.Parent:=MainForm as TWinControl;
 RadioBox1.Visible:=True;
 RadioBox1.Caption:='Red';
 RadioBox1.Top:=80;
 RadioBox1.Left:=144;
 RadioBox1.Name:='RORadioBox1';
 RadioBox1.Ticked:=True;
 //
 RadioBox2:=TGJHRadioBox.Create(MainForm as TControl);
 RadioBox2.Parent:=MainForm as TWinControl;
 RadioBox2.Visible:=True;
 RadioBox2.Caption:='Green';
 RadioBox2.Top:=100;
 RadioBox2.Left:=144;
 RadioBox2.Name:='RORadioBox2';
 //
 RadioBox3:=TGJHRadioBox.Create(MainForm as TControl);
 RadioBox3.Parent:=MainForm as TWinControl;
 RadioBox3.Visible:=True;
 RadioBox3.Caption:='Blue';
 RadioBox3.Top:=120;
 RadioBox3.Left:=144;
 RadioBox3.Name:='RORadioBox3';
 //
 CheckBox1Change(nil);
end;

procedure TMainForm.CheckBox1Change(Sender: TObject);
begin
 RedSlider.HexValue  :=TickBox1.Ticked;
 GreenSlider.HexValue:=TickBox1.Ticked;
 BlueSlider.HexValue :=TickBox1.Ticked;
 HSlider.HexValue    :=TickBox1.Ticked;
 ColourMix.Brush.Color  :=BlueSlider.Position<<16
                      +GreenSlider.Position<<8
                      +RedSlider.Position;
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
