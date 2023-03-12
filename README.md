# GJHCustomComponents
A set of custom components and classes for Lazarus:<br>
<UL>
<LI>TGJHTickBox : RISC OS style tick box</LI>
<LI>TGJHRadioBox : RISC OS style radio (option) box</LI>
<LI>TGJHSlider : Coloured slider with title and value</LI>
<LI>TGJHButton : RISC OS style buttons</LI>
<LI>TGJHRegistry : Wrapper around the TRegistry class</LI>
 </UL>
I have written these to serve my own purposes, but are here for others to use. I will likely add to them, and update them over time.<br>
Utilising these means not having to install packages and recompile Lazarus.<br>
<H2 style="color:#007700">Usage</H2>
<H3 style="color:#FF00FF">TGJHTickBox and TGJHRadioBox</H3>
<B style="color:#0000FF">Methods</B><br>
<B><I>Create(AOwner)</I></B><br>
Creates the control. Pass the owner control as the only parameter.<br><br>
<B style="color:#0000FF">Events</B><br>
<B><I>OnChange</I></B><br>
Fires when the control changes state.<br><br>
<B style="color:#0000FF">Properties</B><br>
In addtion to what is inherited from TGraphicControl, including Top, Left, Visible, Parent, Name and Font.<br>
<B><I>Caption</I></B>: String<br>
Text displayed next to the box.<br>
<B><I>Colour</I></B>: TColor<br>
Background colour. Default: clNone.<br>
<B><I>Group</I></B>: Integer<br>
(RadioBox only) Allows controls to be grouped together, and have several different groups of radio controls with the same parent.<br>
<B><I>OnlyMouse</I></B>: Boolean<br>
Only fire the OnChange event when clicked on. Default: False.<br>
<B><I>Ticked</I></B>: Boolean<br>
Ticked or not. Default: False.<br><br>
<H3 style="color:#FF00FF">TGJHSlider</H3>
<B style="color:#0000FF">Methods</B><br>
<B><I>Create(AOwner)</I></B><br>
Creates the control. Pass the owner control as the only parameter.<br><br>
<B style="color:#0000FF">Events</B><br>
<B><I>OnChange</I></B><br>
Fires when the position changes.<br><br>
<B style="color:#0000FF">Properties</B><br>
In addtion to what is inherited from TGraphicControl, including Top, Left, Width, Height, Visible, Parent, Name and Font.<br>
<B><I>BackColour</I></B>: TColor<br>
Background colour of the actual slider (not the surrounding area). Has no effect if Transparent is True. Default: clWhite.<br>
<B><I>Border3D</I></B>: Boolean<br>
Display a RISC OS style 3D border around the entire control. Default: False.<br>
<B><I>Caption</I></B>: String<br>
Text displayed top or right of the slider.<br>
<B><I>Colour</I></B>: TColor<br>
Colour of the slider. Default: clRed.<br>
<B><I>FillSlider</I></B>: Boolean<br>
Whether to fill the slider or just to the position. Default: False.<br>
<B><I>Gradient</I></B>: Boolean<br>
Whether to fade the colour to black at lowest position or a solid colour. Default: False.<br>
<B><I>HexValue</I></B>: Boolean<br>
Display the position as hex. Default: False.<br>
<B><I>Max</I></B>: Integer<br>
Maximum the position can be. Default: 100.<br>
<B><I>Min</I></B>: Integer<br>
Minimum the position can be. Default: 0.<br>
<B><I>Orientation</I></B>: Integer<br>
Orientation of the slider - csVertical or csHorizontal. Default: csVertical.<br>
<B><I>Outline</I></B>: String<br>
Whether, and where, to display a 2D black border. Does not affect the 3D border settings. Default: csOutOuter.<br>
<I>csOutNone</I> : No borders<br>
<I>csOutInner</I>: Border around the actual slider position<br>
<I>csOutOuter</I>: Border around the actual slider entirity<br>
<I>csOutBoth</I>: Both of the above<br>
<B><I>Pointers</I></B>: Boolean<br>
Whether to show pointers either side of the slider. Default: True.<br>
<B><I>Position</I></B>: Integer<br>
Position of the slider (i.e., the value). Default: 0.<br>
<B><I>ShowValue</I></B>: Boolean<br>
Show the value on the bottom/left of the slider. Default: False.<br>
<B><I>Step</I></B>: Integer<br>
Step size between positions. Default: 1.<br>
<B><I>Suffix</I></B>: String<br>
Text displayed after the value, if shown.<br>
<B><I>Transparent</I></B>: Boolean<br>
Whether the actual slider is transparent or use BackColour. Default: True.<br>
<H3 style="color:#FF00FF">TGJHButton</H3>
<B style="color:#0000FF">Methods</B><br>
<B><I>Create(AOwner)</I></B><br>
Creates the control. Pass the owner control as the only parameter.<br><br>
<B style="color:#0000FF">Events</B><br>
<B><I>OnClick</I></B><br>
Fires when the control is clicked on.<br><br>
<B style="color:#0000FF">Properties</B><br>
In addtion to what is inherited from TGraphicControl, including Top, Left, Visible, Parent, Name and Font.<br>
<B><I>Caption</I></B>: String<br>
Text displayed on the button.<br>
<B><I>Default</I></B>: Boolean<br>
Specifys that the button is a default button (has the yellow inset border). Default: False.<br>
<B><I>ModalResult</I></B>: TModalResult<br>
Modal result to be passed to the form. Default: mrNone.<br>
<H3 style="color:#FF00FF">TGJHRegistry</H3>
This is not a visual control.<br>
<B style="color:#0000FF">Methods</B><br>
<B><I>Create(LKey: String)</I></B><br>
Creates the instance. Pass the registry key as the only parameter.<br>
<B><I>DeleteKey(LKey: String)</I></B><br>
Deletes the entry from the registry.<br>
<B><I>DoesKeyExist(V: String): Boolean</I></B><br>
Returns whether the registry entry V exists.<br>
<B><I>GetRegValA(V: String; var D: array of Byte)</I></B><br>
Gets an array of bytes from the registry entry held in V and stores it in D.<br>
<B><I>GetRegValB(V: String; D: Boolean): Boolean</I></B><br>
Gets a boolean from the registry entry held in V. If the entry does not exist it is created with a default value held in D.<br>
<B><I>GetRegValI(V: String; D: Integer): Integer</I></B><br>
Gets an integer from the registry entry held in V. If the entry does not exist it is created with a default value held in D.<br>
<B><I>GetRegValS(V: String; D: String): String</I></B><br>
Gets a string from the registry entry held in V. If the entry does not exist it is created with a default value held in D.<br>
<B><I>SetRegValA(V: String; var D: array of Byte)</I></B><br>
Sets an array of bytes held in D in the registry entry held in V.<br>
<B><I>SetRegValB(V: String; D: Boolean)</I></B><br>
Sets a boolean held in D in the registry entry held in V.<br>
<B><I>SetRegValI(V: String; D: Integer)</I></B><br>
Sets an integer held in D in the registry entry held in V.<br>
<B><I>SetRegValS(V: String; D: String)</I></B><br>
Sets a string held in D in the registry entry held in V.<br><br>
<B style="color:#0000FF">Properties</B><br>
<B><I>Key</I></B>: String<br>
The registry key being used (read only).<br><br>