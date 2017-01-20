REM MacFractal (c) Jack Weber - October 1985
REM Plots fractal landscapes as a 3D grid by taking a random straight line
REM across the grid and raising (or lowering) all points to one side of the
REM line by one level step.
REM   Full hidden-line removal.
REM   Landscape may be flooded to any level.
REM   Adjustable vertical scale.
REM   Rotation in 90 degree steps in either direction.
REM
REM This program runs on a 128k or 512k Apple Macintosh under MS Basic.
REM Passing this program through the Basic Compressor utility gives a speed
REM increase of about 15%.
REM

GOSUB Initialise
REM Main program loop prints current generation number in its own window
REM then re-calculates the grid and, if necessary, draws it

mainloop:
FOR g%=gs% TO gen%
  WINDOW OUTPUT 3
  CLS
  CALL TEXTFONT(0)
  PRINT USING "#####";g%;
  GOSUB Calculate
  IF (g% MOD plot%=0) OR (g%=gen%) THEN GOSUB Plot
NEXT
REM When all iterations done, offer user choice to continue or stop there
IF gen%>32000 THEN GOSUB Endless
WINDOW 2,,(20,280)-(140,320),2
BUTTON 1,1," Finished",(10,2)-(110,18),3
BUTTON 2,1," Continue", (10,20)-(110,40),3
WHILE DIALOG(0)<>1: WEND
REM If "Finished" pressed hold display in endless loop
IF DIALOG(1)=1 THEN WINDOW CLOSE 2: GOSUB Endless
REM "Continue" pressed so increment generation counter and do 500 more gs%=gen$+1
gen%=gen%+500
WINDOW CLOSE 2
GOTO mainloop
END

Initialise:
  REM Open main output window
  WINDOW 1,"MacFractal",(5,40)-(508,335),1
  CLS
  DIM c%(30,30), d%(30,30), poly%(12)
  RANDOMIZE TIMER
  REM Start with magnification=1, 1st generation and no flooding
  magn!=1
  gs%=1
  flood%=0
  REM Set up pattern array for shading flood water
  FOR i%=0 TO 3
    patt%(i%)=21930
  NEXT
  REM Open a window and enter size of grid and number of generations to be done
  WINDOW 2,,(100,80)-(400,170),2
  CALL TEXTFONT(0)
  xin:
    CALL MOVETO(25,20)
    INPUT "Enter extent of X axis (max 30) - ", xmax%
    IF xmax%>30 OR xmax%<2 THEN BEEP: GOTO xin
  yin:
    CALL MOVETO(25,45)
    INPUT "Enter extent of Y axis (max 30) - ", ymax%
    IF ymax%>30 OR ymax%<2 THEN BEEP: GOTO yin
  genin:
    CALL MOVETO(25,70)
    INPUT "Enter number of iterations - ", gen%
    IF gen%>32000 OR gen%<1 THEN BEEP: GOTO genin
  REM Close that window and open new one to enter frequency of plots
  WINDOW CLOSE 2
  WINDOW 2,,(100,100)-(400,220),2
  CALL TEXTFONT(0)
  PRINT " How many iterations between plots?"
  BUTTON 1,1,"1",(20,25)-(65,50),3
  BUTTON 2,1,"10",(155,25)-(200,50),3
  BUTTON 3,1,"50",(20,55)-(70,80),3
  BUTTON 4,1,"100",(155,55)-(215,80),3
  BUTTON 5,1,"Draw final surface only",(20,85)-(220,110),3
  WHILE DIALOG(0)<> 1: WEND
  press%=DIALOG(l)
  plot%=1
  IF press%=2 THEN plot%=10
  IF press%=3 THEN plot%=50
  IF press%=4 THEN plot%=100
  IF press%=5 THEN plot%=gen%
  WINDOW CLOSE 2
  REM Calculate screen co-ordinates of grid origin according to size of grid
  REM but they could just as easily be fixed near the centre top of the screen
  xo%=334-2*xmax%
  yo%=80-2*ymax%
  REM Put plot manipulation options in menu bar
  MENU 6,0,1,"Landscape"
  MENU 6,1,1,"Rotate left"
  MENU 6,2,1,"Rotate right"
  MENU 6,3,1,"Magnify"
  MENU 6,4,1,"Flood"
  ON MENU GOSUB Menucheck
  REM Advise user if initial iterations are likely to take a long time
  IF plot%=1 AND (xmax%*ymax%)<400 GOTO ready
    WINDOW 2,,(150,100)-(350,190),4
    PRINT: PRINT " Performing initial iteration";
    IF plot%>1 THEN PRINT "s" ELSE PRINT"???"
    CALL TEXTFONT(0)
    PRINT: PRINT TAB(8);"Please wait"
  ready:
  REM Open small window to display generation number
  WINDOW 3,,(10,50)-(60,66),4
  MENU ON
RETURN

Calculate:
  MENU STOP
  ortho%=0
  REM Select two sides which fault line will cross
  side%(1)=INT(RND*4)
  side2:
    side%(2)=INT(RND*4)
    IF side%(2)=side%(1) GOTO side2
  REM Pos subroutines select random positions where line will cross the sides
  FOR i%= 1 TO 2
    ON side%(i%)+1 GOSUB Pos1,Pos2,Pos3,Pos4
  NEXT
  REM Set flag if line parallel to Y-axis to avoid division by zero
  IF xx%(1)=xx%(2) THEN ortho%=1 : GOTO skipover
  REM Equation of straight line is y=m*x+k. Calculate m and k
  m=(yy%(2)-yy%(1))/(xx%(2)-xx%(1))
  k=yy%(l)-m*xx%(1)
  skipover:
  REM Select whether points above line are to be raised or lowered
  q%=INT(RND*2): IF q%=0 THEN q%=-1
  REM Check all grid points. If above line raise (or lower)
  FOR x%=1 TO xmax%
    REM First find y co-ordinate of the line at that value of x
    yp%=INT(m*x%+k)
    FOR y%=l TO ymax%
      IF ortho%=0 AND y%>yp% THEN c%(x%,y%)=c%(x%,y%)+q%: GOTO nex
      IF ortho%=1 AND x%>xx%(1) THEN c%(x%,y%)=c%(x%,y%)+q%
      nex:
    NEXT
  NEXT
  MENU ON
RETURN

Pos1:
  xx%(i%)=1: yy%(i%)=INT(RND*ymax%)+1
RETURN

Pos2:
  xx%(i%)=INT(RND*xmax%)+1: yy%(i%)=ymax%
RETURN

Pos3:
  xx%(i%)=xmax%: yy%(i%)=INT(RND*ymax%)
RETURN

Pos4:
  xx%(i%)=INT(RND*xmax%): yy%(i%)=1
RETURN

Plot:
  MENU STOP
  WINDOW CLOSE 2
  WINDOW OUTPUT 1
  CLS
  REM Use separate plotting routines for flooding on or off
  IF flood%=0 THEN GOSUB Dryplot ELSE GOSUB Floodplot
  MENU ON
RETURN

Dryplot:
  REM Scan all grid points
  FOR x%= 1 TO xmax%
    FOR y%= 1 TO ymax%
      REM Calculate screen co-ordinates of first corner of grid square at (x%,y%)
      x1%=xo%+(y%-x%)*7
      y1%=yo%-INT(magn!*c%(x%,y%))+(x%+y%)*4
      REM Then calculate screen co-ordinates of second corner at (y%-1,y%)
      x2%=x1%+7
      y2%=yo%-INT(magn!*c%(x%-1,y%))+(x%-1+y%)*4
      REM Draw side of grid square parallel to X-axis
      REM First erase hidden-lines by drawing broad line in white
      CALL PENSIZE(1,30): CALL PENMODE(11)
      CALL MOVETO(x2%,y2%): CALL LINETO(x1%,y1%)
      REM Then draw new line
      IF x%>1 THEN LINE(x%,y1%)-(x2%,y2%)
      REM If end of row has been reached then second line riot needed so skip on
      IF y%=ymax% GOTO loopdry
      REM Draw side of grid square parallel to V-axis
      REM Calculate co-ordinates of 3rd corner at (x%,y%+1)
      y3%=yo%-INT (magn!*c%(x%,y%+1))+(x%+y%+1)*4
      REM Erase hidden-lines as before
      CALL LINETO(x2%,y3%)
      REM Draw new line
      LINE(x1%,y1%)-(x2%,y3%)
      loopdry:
    NEXT
  NEXT
  REM Draw base of grid
  CALL PENNORMAL
  REM First find altitudes of the three visible corners
  ca%=c%(xmax%,1): cb%=c%(xmax%,ymax%): cc%=c%(l,ymax%)
  REM Then draw three vertical lines down from these corners
  LINE(xo%+(1-xmax%)*7,yo%+(xmax%+1)*4-INT(magn!*ca%))-STEP(0,30+INT(magn!*ca%))
  LINE(xo%+(ymax%-xmax%)*7,yo%+(xmax%+ymax%)*4-INT(magn!*cb%))-
    STEP(0,30+INT(magn!*cb%))
  LINE(xo%+(ymax%-1)*7,yo%+(1+ymax%)*4-INT(magn!*cc%))-
    STEP(0,30+INT(magn!*cc%))
  REM Finally draw the two base lines
  LINE-STEP((1-xmax%)*7,(xmax%-1)*4)
  LINE-STEP((1-ymax%)*7,(1-ymax%)*4)
RETURN

Floodplot:
  FOR x%= 1 TO xmax%
    FOR y%=l TO ymax%
      REM Check level to see if this point is flooded.
      REM If it is, draw it at water level
      IF c%(x%,y%)<ml% THEN ca%=ml% ELSE ca%=c%(x%,y%)
      IF c%(x%-1,y%)<ml% THEN cb%=ml% ELSE cb%=c%(x%-1,y%)
      REM Calculate co-ordinates and plot as for dry
      x1%=xo%+(y%-x%)*7
      y1%=yo%-INT(magn!*ca%)+(x%+y%)*4
      x2%=x1%+7
      y2%=yo%-INT(magn!*cb%)+(x%-1+y%)*4
      CALL PENSIZE(1,30): CALL PENMODE(11)
      CALL MOVETO(x2%,y2%): CALL LINETO(xl%,yl%)
      IF x%>1 THEN LINE(xl%,y1%)-(x2%,y2%)
      IF y%=ymax% GOTO loopflood
      IF c%(x%,y%+l)<ml% THEN cc%=ml% ELSE cc%=c%(x%,y%+l)
      y3%=yo%-INT(magn!*cc%)+(x%+y%+1)*4
      CALL LINETO(x2%,y3%)
      LINE(x1%,y1%)-(x2%,y3%)
      loopflood:
      REM Shade any part of square which is flooded
      GOSUB Shade
    NEXT
  NEXT
  CALL PENNORMAL
  REM Draw base as for dry
  IF c%(xmax%,1)<ml% THEN ca%=ml% ELSE ca%=c%(xmax%,l)
  IF c%(xmax%,ymax%)<ml% THEN cb%=ml% ELSE cb%=c%(xmax%,ymax%)
  IF c%(l,ymax%)<ml% THEN cc%=ml% ELSE cc%=c%(l,ymax%)
  LINE(xo%+(1-xmax%)*7,yo%+(xmax%+l)*4-INT(magn!*ca%))-
    STEP(0,30+INT(magn!*ca%))
  LINE(xo%+(ymax%-xmax%)*7,yo%+(xmax%+ymax%)*4-INT(magn!*cb%))-
    STEP(0,30+INT(magn!*cb%))
  LINE(xo%+(ymax%-1)*7,yo%+(l+ymax%)*4-INT(magn!*cc%))-
    STEP(0,30+INT(magn!*cc%))
  LINE-STEP((1-xmax%)*7,(xmax%- 1)*4)
  LINE-STEP((1-ymax%)*7,(1-ymax%)*4)
RETURN

Menucheck: A menu selection has been detected - find what it is
  menunumber%=MENU(0)
  IF menunumber%<>6 THEN RETURN
  menuitem%=MENU(1)
  ON menuitem% GOSUB Rotateleft,Rotateright, Magnify, Flood
RETURN

Rotateleft:
  REM Rotate by reading c& array into dX array with axes interchanged
  FOR x%=1 TO xmax%
    FOR y%=l TO ymax%: d%(ymax%-y%+1,x%)=c%(x%,y%): NEXT
  NEXT
  REM Then return rotated data to c% array
  SWAP xmax%,ymax%
  FOR x%=0 TO xmax%
    FOR y%=1 TO ymax%: c%(x%,y%)=d%(x%,y%): NEXT
  NEXT
  GOSUB Plot
  MENU
RETURN

Rotateright:
  FOR x%=l TO xmax%
    FOR y%=1 TO ymax%: d%(y%,xmax%-x%+1)=c%(x%,y%): NEXT
  NEXT
  SWAP xmax%,ymax%
  FOR x%=0 TO xmax%
    FOR y%=0 TO ymax%: c%(x%,y%)=d%(x%,y%): NEXT
  NEXT
  GOSUB Plot
  MENU
RETURN

Magnify:
  REM Open window to enter desired vertical scale magnification
  WINDOW 2,,(80,100)-(420,220),2
  CALL TEXTFONT(0)
  PRINT SPC(4);"adjust vertical magnification as required"
  REM Draw magnification selection bar
  LINE(15,70)-(325,90),,b
  LINE(16,71)-(324,89),,b
  BUTTON 1,1,"OK",(240,25)-(290,55),1
  REM Put scale markings and labels onto selection bar
  FOR i%=20 TO 320 STEP 20: LINE(1%,90)-(i%,95): NEXT
  CALL TEXTFONT(1)
  CALL TEXTSIZE(9)
  FOR i%=20 TO 320 STEP 100
    CALL MOVETO(i%,108)
    PRINT .5*(1+(i%-20)/l00);
    LINE(i%,95)-(i%,100)
  NEXT
  REM Draw magnification pointer to show current magnification
  REM allow it to move with the mouse until the OK button is pressed
  LINE(20+(magn!-.5)*200,72)-STEP(0, 16)
  WHILE DIALOG(0)<> 1
    IF MOUSE(0)<>0 THEN GOSUB Mousemag
  WEND
  WINDOW CLOSE 2
  REM Re-draw grid with new magnification
  GOSUB Plot
  MENU
RETURN

Mousemag:
  REM Find position of mouse, if it is within selection bar then move pointer
  IF MOUSE(1)<15 OR MOUSE(1)>325 THEN RETURN
  IF MOUSE(2)<70 OR MOUSE(2)>90 THEN RETURN
  LINE(20+(magn!-.5)*200,72)-STEP(0,16),30
  magn!=.5+((MOUSE(1)-20)\20)/10
  LINE(20+(magn!-.5)*200,72)-STEP(0,16)
RETURN

Flood:
  REM Open a window
  WINDOW 2,,(80,100)-(420,220),2
  PRINT: PRINT: PRINT SPC(14);"Calculating levels"
  CALL TEXTFONT(0)
  PRINT: PRINT SPC(16);"Please wait"
  REM Find highest and lowest levels present within the array
  min%=10000
  max%=-10000
  FOR x%=1 TO xmax%
    FOR y%=l TO ymax%
      IF c%(x%,y%)<min% THEN min%=c%(x%,y%)
      IF c%(x%,y%)>max% THEN max%=c%(x%,y%)
    NEXT
  NEXT
  CLS
  CALL TEXTFONT(1)
  CALL TEXTSIZE(9)
  REM Draw flood level selection bar
  LINE(17,70)-(323,90),,b
  LINE(18,71)-(322,89),,b
  REM Put scale markings and labels on selection bar
  div%(max%-min%)\10: IF div%=0 THEN div%=l
  FOR i%=min% TO max%
    scalex%=20+(i%-min%)*300/(max%-min%)
    IF i% MOD div%=0 OR i%=max% THEN LINE(scalex%,90)-STEP(0,6)
    IF i% MOD (div%*5)=0 OR i%=min% OR i%=max% THEN
      CALL MOVETO(scalex%-10,110): PRINT i%;
  NEXT
  CALL TEXTSIZE(12)
  CALL TEXTFONT(0)
  CALL MOVETO(50,50): PRINT "Select required water level";
  BUTTON 1,1,"OK",(270,10)-(320,40),1
  level%=ml%-min%
  REM If Flood had been previously selected, the old level may now be outside
  REM the present level range - adjust if necessary
  IF flood%=0 OR level%<0 THEN level%=0
  IF level%>max%-min% THEN level%=max%-min%
  REM Draw pointer on selection bar to show current water level
  REM allow it to move with the mouse until the OK button is pressed
  LINE(20+level%*300/(max%-min%),72)-STEP(0,16)
  WHILE DIALOG(0)<>1
    IF MOUSE(0)<>0 THEN GOSUB Mouseflood
  WEND
  ml%=min%+level%
  REM Set flood flag if chosen flood level is not zero
  IF level%>0 THEN flood%=1
  WINDOW CLOSE 2
  REM Re-draw grid with new flood level
  GOSUB Plot
  MENU
RETURN

Mouseflood:
  REM Find position of mouse, if it is within selection bar then move pointer
  IF MOUSE(1)<20 OR MOUSE(1)>320 THEN RETURN
  IF MOUSE(2)<70 OR MOUSE(2)>90 THEN RETURN
  LINE(20+level%*300/(max%-min%),72)-STEP(0,16),30
  level%=(MOUSE(1)-20)/(300/(max%-min%))
  LINE(20+level%*300/(max%-min%),72)-STEP(0,16)
RETURN

Shade:
  IF x%=1 OR y%>=ymax% THEN RETURN
  REM Count number of corners in each grid square which are below flood level
  corner%=0
  maglev%=INT(magn!*ml%)
  IF c%(x%,y%)<=ml% THEN corner%=l
  IF c%(x%-1,y%)<=ml% THEN corner%=corner%+1
  IF c%(x%,y%+1)<=ml% THEN corner%=corner%+1
  IF c%(x%-1,y%+1)<=ml% THEN corner%=corner%+1
  REM No shading if less than 3 corners are below flood level
  IF corner%<3 THEN RETURN
  REM Prepare array for Toolbox FILLPOLY subroutine
  poly%(1)=yo%+(x%-1+y%)*4-maglev%
  poly%(2)=xo%+(y%-x%)*7
  poly%(3)=poly%(1)+8
  poly%(4)=poly%(2)+14
  REM If 3 corners wet shade triangle. If all 4 wet shade whole square
  IF corner%=3 THEN GOSUB Trishade ELSE GOSUB Squareshade
  CALL FILLPOLY(VARPTR(poly%(0)),VARPTR(patt%(O)))
RETURN

Trishade:
  poly%(0)=22
  REM Find which of 4 possible triangles needs shading
  IF c%(x%,y%)>ml% THEN GOSUB Tri1: RETURN
  IF c%(x%-1,y%)>ml% THEN GOSUB Tri2: RETURN
  IF c%(x%,y%+1)>ml% THEN GOSUB Tri3: RETURN
  GOSUB Tri4
RETURN

Tri1:
  poly%(5)=poly%(l)
  poly%(6)=poly%(2)+7
  poly%(7)=poly%(3)
  poly%(8)=poly%(6)
  poly%(9)=poly%(1)+4
  poly%(10)=poly%(4)
  poly%(2)=poly%(6)
RETURN

Tri2:
  poly%(5)=poly%(l)+4
  poly%(6)=poly%(2)
  poly%(7)=poly%(3)
  poly%(8)=poly%(2)+7
  poly%(9)=poly%(5)
  poly%(10)=poly%(4)
  poly%(l)=poly%(5)
RETURN

Tri3:
  poly%(5)=poly%(1)+4
  poly%(6)=poly%(2)
  poly%(7)=poly%(l)
  poly%(8)=poly%(2)+7
  poly%(9)=poly%(5)
  poly%(10)=poly%(4)
  poly%(3)=poly%(5)
RETURN

Tri4:
  poly%(5)=poly%(1)+4
  poly%(6)=poly%(2)
  poly%(7)=poly%(3)
  poly%(8)=poly%(2)+7
  poly%(9)=poly%(1)
  poly%(10)=poly%(a)
  poly%(4)=poly%(8)
RETURN

Squareshade:
  poly%(0)=26
  poly%(5)=poly%(1)+4
  poly%(6)=poly%(2)
  poly%(7)=poly%(3)
  poly%(8)=poly%(2)+7
  poly%(9)=poly%(5)
  poly%(1O)=poly%(4)
  poly%(11)=poly%(1)
  poly%(12)=poly%(8)
RETURN

Endless:
  WHILE 1<2: WEND
RETURN
