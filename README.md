# GJHCustomComponents
 A set of custom components and classes for Lazarus:<br>
* TGJHTickBox : RISC OS style tick box<br>
* TGJHRadioBox : RISC OS style radio (option) box<br>
* TGJHSlider : Coloured slider with title and value<br>
* TGJHButton : RISC OS style buttons<br>
* TGJHRegistry : Wrapper around the TRegistry class<br>
<br>
I have written these to serve my own purposes, but are here for others to use. I will likely add to them, and update them over time.<br>
<br>
Utilising these means not having to install packages and recompile Lazarus.<br>
<br>
<H2>Usage</H2>
<H3>TGJHTickBox and TGJHRadioBox</H3>
<B>Methods</B><br>
<I>Create(AOwner)</I><br>
Creates the control. Pass the owner control as the only parameter.<br><br>
<B>Events</B><br>
<I>OnChange</I><br>
Fires when the control changes state.<br><br>
<B>Properties</B><br>
In addtion to what is inherited from TGraphicControl, including Top, Left, Visible, Parent, Name and Font.<br>
<I>Caption</I>: String<br>
Text displayed next to the box.<br>
<I>Colour</I>: TColor<br>
Background colour. Default: clNone.<br>
<I>OnlyMouse</I>: Boolean<br>
Only fire the OnChange event when clicked on. Default: False.<br>
<I>Ticked</I>: Boolean<br>
Ticked or not. Default: False.<br><br>
<H3>TGJHSlider</H3>
<B>Methods</B><br>
<I>Create(AOwner)</I><br>
Creates the control. Pass the owner control as the only parameter.<br><br>
<B>Events</B><br>
<I>OnChange</I><br>
Fires when the position changes.<br><br>
<B>Properties</B><br>
In addtion to what is inherited from TGraphicControl, including Top, Left, Width, Height, Visible, Parent, Name and Font.<br>
<I>BackColour</I>: TColor<br>
Background colour of the actual slider (not the surrounding area). Has no effect if Transparent is True. Default: clWhite.<br>
<I>Border3D</I>: Boolean<br>
Display a RISC OS style 3D border around the entire control. Default: False.<br>
<I>BorderSize</I>: Integer<br>
Thickness of the 3D border (multiples of 2). Has no effect if Border3D is False. Default: 2.<br>
<I>Caption</I>: String<br>
Text displayed top or right of the slider.<br>
<I>Colour</I>: TColor<br>
Colour of the slider. Default: clRed.<br>
<I>FillSlider</I>: Boolean<br>
Whether to fill the slider or just to the position. Default: False.<br>
<I>Gradient</I>: Boolean<br>
Whether to fade the colour to black at lowest position or a solid colour. Default: False.<br>
<I>HexValue</I>: Boolean<br>
Display the position as hex. Default: False.<br>
<I>Max</I>: Integer<br>
Maximum the position can be. Default: 100.<br>
<I>Min</I>: Integer<br>
Minimum the position can be. Default: 0.<br>
<I>Orientation</I>: Integer<br>
Orientation of the slider - csVertical or csHorizontal. Default: csVertical.<br>
<I>Outline</I>: String<br>
Whether, and where, to display a 2D black border. Does not affect the 3D border settings. Default: csOutOuter.<br>
csOutNone : No borders<br>
csOutInner: Border around the actual slider position<br>
csOutOuter: Border around the actual slider entirity<br>
csOutBoth : Both of the above<br>
<I>Pointers</I>: Boolean<br>
Whether to show pointers either side of the slider. Default: True.<br>
<I>Position</I>: Integer<br>
Position of the slider (i.e., the value). Default: 0.<br>
<I>ShowValue</I>: Boolean<br>
Show the value on the bottom/left of the slider. Default: False.<br>
<I>Step</I>: Integer<br>
Step size between positions. Default: 1.<br><br>
<I>Suffix</I>: String<br>
Text displayed after the value, if shown.<br>
<I>Transparent</I>: Boolean<br>
Whether the actual slider is transparent or use BackColour. Default: True.<br>
<H3>TGJHRegistry</H3>
This is not a visual control.<br>
<B>Methods</B><br>
<I>Create(LKey: String)</I><br>
Creates the instance. Pass the registry key as the only parameter.<br>
<I>DeleteKey(LKey: String)</I><br>
Deletes the entry from the registry.<br>
<I>DoesKeyExist(V: String): Boolean</I><br>
Returns whether the registry entry V exists.<br>
<I>GetRegValA(V: String; var D: array of Byte)</I><br>
Gets an array of bytes from the registry entry held in V and stores it in D.<br>
<I>GetRegValB(V: String; D: Boolean): Boolean</I><br>
Gets a boolean from the registry entry held in V. If the entry does not exist it is created with a default value held in D.<br>
<I>GetRegValI(V: String; D: Integer): Integer</I><br>
Gets an integer from the registry entry held in V. If the entry does not exist it is created with a default value held in D.<br>
<I>GetRegValS(V: String; D: String): String</I><br>
Gets a string from the registry entry held in V. If the entry does not exist it is created with a default value held in D.<br>
<I>SetRegValA(V: String; var D: array of Byte)</I><br>
Sets an array of bytes held in D in the registry entry held in V.<br>
<I>SetRegValB(V: String; D: Boolean)</I><br>
Sets a boolean held in D in the registry entry held in V.<br>
<I>SetRegValI(V: String; D: Integer)</I><br>
Sets an integer held in D in the registry entry held in V.<br>
<I>SetRegValS(V: String; D: String)</I><br>
Sets a string held in D in the registry entry held in V.<br><br>
<B>Properties</B><br>
<I>Key</I>: String<br>
The registry key being used (read only).<br><br>
