20 REM MacFractal (c) Jack Weber - October 1985
30 REM Plots fractal landscapes as a 3D grid by taking a random straight line
40 REM across the grid and raising (or lowering) all points to one side of the
50 REM line by one level step.
60 REM   Full hidden-line removal.
70 REM   Landscape may be flooded to any level.
80 REM   Adjustable vertical scale.
90 REM   Rotation in 90 degree steps in either direction.
100 REM
110 REM This program runs on a 128k or 512k Apple Macintosh under MS Basic.
120 REM Passing this program through the Basic Compressor utility gives a speed
130 REM increase of about 15%.
140 REM

160 GOSUB Initialise
170 REM Main program loop prints current generation number in its own window
180 REM then re-calculates the grid and, if necessary, draws it

200 mainloop:
210 FOR g%=gs% TO gen%
220   WINDOW OUTPUT 3
230   CLS
240   CALL TEXTFONT(0)
250   PRINT USING "#####";g%;
260   GOSUB Calculate
270   IF (g% MOD plot%=0) OR (g%=gen%) THEN GOSUB Plot
280 NEXT
290 REM When all iterations done, offer user choice to continue or stop there
300 IF gen%>32000 THEN GOSUB Endless
310 WINDOW 2,,(20,280)-(140,320),2
320 BUTTON 1,1," Finished",(10,2)-(110,18),3
330 BUTTON 2,1," Continue", (10,20)-(110,40),3
340 WHILE DIALOG(0)<>1: WEND
350 REM If "Finished" pressed hold display in endless loop
360 IF DIALOG(1)=1 THEN WINDOW CLOSE 2: GOSUB Endless
370 REM "Continue" pressed so increment generation counter and do 500 more gs%=gen$+1
380 gen%=gen%+500
390 WINDOW CLOSE 2
400 GOTO mainloop
410 END







490 Initialise:
500   REM Open main output window
510   WINDOW 1,"MacFractal",(5,40)-(508,335),1
520   CLS
530   DIM c%(30,30), d%(30,30), poly%(12)
540   RANDOMIZE TIMER
550   REM Start with magnification=1, 1st generation and no flooding
560   magn!=1
570   gs%=1
580   flood%=0
590   REM Set up pattern array for shading flood water
600   FOR i%=0 TO 3
610     patt%(i%)=21930
620   NEXT
630   REM Open a window and enter size of grid and number of generations to be done
640   WINDOW 2,,(100,80)-(400,170),2
650   CALL TEXTFONT(0)
660   xin:
670     CALL MOVETO(25,20)
680     INPUT "Enter extent of X axis (max 30) - ", xmax%
690     IF xmax%>30 OR xmax%<2 THEN BEEP: GOTO xin
700   yin:
710     CALL MOVETO(25,45)
720     INPUT "Enter extent of Y axis (max 30) - ", ymax%
730     IF ymax%>30 OR ymax%<2 THEN BEEP: GOTO yin
740   genin:
750     CALL MOVETO(25,70)
760     INPUT "Enter number of iterations - ", gen%
770     IF gen%>32000 OR gen%<1 THEN BEEP: GOTO genin
780   REM Close that window and open new one to enter frequency of plots
790   WINDOW CLOSE 2
800   WINDOW 2,,(100,100)-(400,220),2
810   CALL TEXTFONT(0)
820   PRINT " How many iterations between plots?"
830   BUTTON 1,1,"1",(20,25)-(65,50),3
840   BUTTON 2,1,"10",(155,25)-(200,50),3
850   BUTTON 3,1,"50",(20,55)-(70,80),3
860   BUTTON 4,1,"100",(155,55)-(215,80),3
870   BUTTON 5,1,"Draw final surface only",(20,85)-(220,110),3
880   WHILE DIALOG(0)<> 1: WEND
890   press%=DIALOG(l)
900   plot%=1
910   IF press%=2 THEN plot%=10
920   IF press%=3 THEN plot%=50
930   IF press%=4 THEN plot%=100
940   IF press%=5 THEN plot%=gen%
950   WINDOW CLOSE 2

970   REM Calculate screen co-ordinates of grid origin according to size of grid
980   REM but they could just as easily be fixed near the centre top of the screen
990   xo%=334-2*xmax%
1000   yo%=80-2*ymax%
1010   REM Put plot manipulation options in menu bar
1020   MENU 6,0,1,"Landscape"
1030   MENU 6,1,1,"Rotate left"
1040   MENU 6,2,1,"Rotate right"
1050   MENU 6,3,1,"Magnify"
1060   MENU 6,4,1,"Flood"
1070   ON MENU GOSUB Menucheck
1080   REM Advise user if initial iterations are likely to take a long time
1090   IF plot%=1 AND (xmax%*ymax%)<400 GOTO ready
1100     WINDOW 2,,(150,100)-(350,190),4
1110     PRINT: PRINT " Performing initial iteration";
1120     IF plot%>1 THEN PRINT "s" ELSE PRINT"???"
1130     CALL TEXTFONT(0)
1140     PRINT: PRINT TAB(8);"Please wait"
1150   ready:
1160   REM Open small window to display generation number
1170   WINDOW 3,,(10,50)-(60,66),4
1180   MENU ON
1190 RETURN

1210 Calculate:
1220   MENU STOP
1230   ortho%=0
1240   REM Select two sides which fault line will cross
1250   side%(1)=INT(RND*4)
1260   side2:
1270     side%(2)=INT(RND*4)
1280     IF side%(2)=side%(1) GOTO side2
1290   REM Pos subroutines select random positions where line will cross the sides
1300   FOR i%= 1 TO 2
1310     ON side%(i%)+1 GOSUB Pos1,Pos2,Pos3,Pos4
1320   NEXT
1330   REM Set flag if line parallel to Y-axis to avoid division by zero
1340   IF xx%(1)=xx%(2) THEN ortho%=1 : GOTO skipover
1350   REM Equation of straight line is y=m*x+k. Calculate m and k
1360   m=(yy%(2)-yy%(1))/(xx%(2)-xx%(1))
1370   k=yy%(l)-m*xx%(1)
1380   skipover:
1390   REM Select whether points above line are to be raised or lowered
1400   q%=INT(RND*2): IF q%=0 THEN q%=-1




1450   REM Check all grid points. If above line raise (or lower)
1460   FOR x%=1 TO xmax%
1470     REM First find y co-ordinate of the line at that value of x
1480     yp%=INT(m*x%+k)
1490     FOR y%=l TO ymax%
1500       IF ortho%=0 AND y%>yp% THEN c%(x%,y%)=c%(x%,y%)+q%: GOTO nex
1510       IF ortho%=1 AND x%>xx%(1) THEN c%(x%,y%)=c%(x%,y%)+q%
1520       nex:
1530     NEXT
1540   NEXT
1550   MENU ON
1560 RETURN

1580 Pos1:
1590   xx%(i%)=1: yy%(i%)=INT(RND*ymax%)+1
1600 RETURN

1620 Pos2:
1630   xx%(i%)=INT(RND*xmax%)+1: yy%(i%)=ymax%
1640 RETURN

1660 Pos3:
1670   xx%(i%)=xmax%: yy%(i%)=INT(RND*ymax%)
1680 RETURN

1700 Pos4:
1710   xx%(i%)=INT(RND*xmax%): yy%(i%)=1
1720 RETURN

1740 Plot:
1750   MENU STOP
1760   WINDOW CLOSE 2
1770   WINDOW OUTPUT 1
1780   CLS
1790   REM Use separate plotting routines for flooding on or off
1800   IF flood%=0 THEN GOSUB Dryplot ELSE GOSUB Floodplot
1810   MENU ON
1820 RETURN










1930 Dryplot:
1940   REM Scan all grid points
1950   FOR x%= 1 TO xmax%
1960     FOR y%= 1 TO ymax%
1970       REM Calculate screen co-ordinates of first corner of grid square at (x%,y%)
1980       x1%=xo%+(y%-x%)*7
1990       y1%=yo%-INT(magn!*c%(x%,y%))+(x%+y%)*4
2000       REM Then calculate screen co-ordinates of second corner at (y%-1,y%)
2010       x2%=x1%+7
2020       y2%=yo%-INT(magn!*c%(x%-1,y%))+(x%-1+y%)*4
2030       REM Draw side of grid square parallel to X-axis
2040       REM First erase hidden-lines by drawing broad line in white
2050       CALL PENSIZE(1,30): CALL PENMODE(11)
2060       CALL MOVETO(x2%,y2%): CALL LINETO(x1%,y1%)
2070       REM Then draw new line
2080       IF x%>1 THEN LINE(x%,y1%)-(x2%,y2%)
2090       REM If end of row has been reached then second line riot needed so skip on
2100       IF y%=ymax% GOTO loopdry
2110       REM Draw side of grid square parallel to V-axis
2120       REM Calculate co-ordinates of 3rd corner at (x%,y%+1)
2130       y3%=yo%-INT (magn!*c%(x%,y%+1))+(x%+y%+1)*4
2140       REM Erase hidden-lines as before
2150       CALL LINETO(x2%,y3%)
2160       REM Draw new line
2170       LINE(x1%,y1%)-(x2%,y3%)
2180       loopdry:
2190     NEXT
2200   NEXT
2210   REM Draw base of grid
2220   CALL PENNORMAL
2230   REM First find altitudes of the three visible corners
2240   ca%=c%(xmax%,1): cb%=c%(xmax%,ymax%): cc%=c%(l,ymax%)
2250   REM Then draw three vertical lines down from these corners
2260   LINE(xo%+(1-xmax%)*7,yo%+(xmax%+1)*4-INT(magn!*ca%))-STEP(0,30+INT(magn!*ca%))
2270   LINE(xo%+(ymax%-xmax%)*7,yo%+(xmax%+ymax%)*4-INT(magn!*cb%))-
2280     STEP(0,30+INT(magn!*cb%))
2290   LINE(xo%+(ymax%-1)*7,yo%+(1+ymax%)*4-INT(magn!*cc%))-
2300     STEP(0,30+INT(magn!*cc%))
2310   REM Finally draw the two base lines
2320   LINE-STEP((1-xmax%)*7,(xmax%-1)*4)
2330   LINE-STEP((1-ymax%)*7,(1-ymax%)*4)
2340 RETURN






2410 Floodplot:
2420   FOR x%= 1 TO xmax%
2430     FOR y%=l TO ymax%
2440       REM Check level to see if this point is flooded.
2450       REM If it is, draw it at water level
2460       IF c%(x%,y%)<ml% THEN ca%=ml% ELSE ca%=c%(x%,y%)
2470       IF c%(x%-1,y%)<ml% THEN cb%=ml% ELSE cb%=c%(x%-1,y%)
2480       REM Calculate co-ordinates and plot as for dry
2490       x1%=xo%+(y%-x%)*7
2500       y1%=yo%-INT(magn!*ca%)+(x%+y%)*4
2510       x2%=x1%+7
2520       y2%=yo%-INT(magn!*cb%)+(x%-1+y%)*4
2530       CALL PENSIZE(1,30): CALL PENMODE(11)
2540       CALL MOVETO(x2%,y2%): CALL LINETO(xl%,yl%)
2550       IF x%>1 THEN LINE(xl%,y1%)-(x2%,y2%)
2560       IF y%=ymax% GOTO loopflood
2570       IF c%(x%,y%+l)<ml% THEN cc%=ml% ELSE cc%=c%(x%,y%+l)
2580       y3%=yo%-INT(magn!*cc%)+(x%+y%+1)*4
2590       CALL LINETO(x2%,y3%)
2600       LINE(x1%,y1%)-(x2%,y3%)
2610       loopflood:
2620       REM Shade any part of square which is flooded
2630       GOSUB Shade
2640     NEXT
2650   NEXT
2660   CALL PENNORMAL
2670   REM Draw base as for dry
2680   IF c%(xmax%,1)<ml% THEN ca%=ml% ELSE ca%=c%(xmax%,l)
2690   IF c%(xmax%,ymax%)<ml% THEN cb%=ml% ELSE cb%=c%(xmax%,ymax%)
2700   IF c%(l,ymax%)<ml% THEN cc%=ml% ELSE cc%=c%(l,ymax%)
2710   LINE(xo%+(1-xmax%)*7,yo%+(xmax%+l)*4-INT(magn!*ca%))-
2720     STEP(0,30+INT(magn!*ca%))
2730   LINE(xo%+(ymax%-xmax%)*7,yo%+(xmax%+ymax%)*4-INT(magn!*cb%))-
2740     STEP(0,30+INT(magn!*cb%))
2750   LINE(xo%+(ymax%-1)*7,yo%+(l+ymax%)*4-INT(magn!*cc%))-
2760     STEP(0,30+INT(magn!*cc%))
2770   LINE-STEP((1-xmax%)*7,(xmax%- 1)*4)
2780   LINE-STEP((1-ymax%)*7,(1-ymax%)*4)
2790 RETURN

2810 Menucheck: A menu selection has been detected - find what it is
2820   menunumber%=MENU(0)
2830   IF menunumber%<>6 THEN RETURN
2840   menuitem%=MENU(1)
2850   ON menuitem% GOSUB Rotateleft,Rotateright, Magnify, Flood
2860 RETURN


2890 Rotateleft:
2900   REM Rotate by reading c& array into dX array with axes interchanged
2910   FOR x%=1 TO xmax%
2920     FOR y%=l TO ymax%: d%(ymax%-y%+1,x%)=c%(x%,y%): NEXT
2930   NEXT
2940   REM Then return rotated data to c% array
2950   SWAP xmax%,ymax%
2960   FOR x%=0 TO xmax%
2970     FOR y%=1 TO ymax%: c%(x%,y%)=d%(x%,y%): NEXT
2980   NEXT
2990   GOSUB Plot
3000   MENU
3010 RETURN

3030 Rotateright:
3040   FOR x%=l TO xmax%
3050     FOR y%=1 TO ymax%: d%(y%,xmax%-x%+1)=c%(x%,y%): NEXT
3060   NEXT
3070   SWAP xmax%,ymax%
3080   FOR x%=0 TO xmax%
3090     FOR y%=0 TO ymax%: c%(x%,y%)=d%(x%,y%): NEXT
3100   NEXT
3110   GOSUB Plot
3120   MENU
3130 RETURN

3150 Magnify:
3160   REM Open window to enter desired vertical scale magnification
3170   WINDOW 2,,(80,100)-(420,220),2
3180   CALL TEXTFONT(0)
3190   PRINT SPC(4);"adjust vertical magnification as required"
3200   REM Draw magnification selection bar
3210   LINE(15,70)-(325,90),,b
3220   LINE(16,71)-(324,89),,b
3230   BUTTON 1,1,"OK",(240,25)-(290,55),1
3240   REM Put scale markings and labels onto selection bar
3250   FOR i%=20 TO 320 STEP 20: LINE(1%,90)-(i%,95): NEXT
3260   CALL TEXTFONT(1)
3270   CALL TEXTSIZE(9)
3280   FOR i%=20 TO 320 STEP 100
3290     CALL MOVETO(i%,108)
3300     PRINT .5*(1+(i%-20)/l00);
3310     LINE(i%,95)-(i%,100)
3320   NEXT




3370   REM Draw magnification pointer to show current magnification
3380   REM allow it to move with the mouse until the OK button is pressed
3390   LINE(20+(magn!-.5)*200,72)-STEP(0, 16)
3400   WHILE DIALOG(0)<> 1
3410     IF MOUSE(0)<>0 THEN GOSUB Mousemag
3420   WEND
3430   WINDOW CLOSE 2
3440   REM Re-draw grid with new magnification
3450   GOSUB Plot
3460   MENU
3470 RETURN

3490 Mousemag:
3500   REM Find position of mouse, if it is within selection bar then move pointer
3510   IF MOUSE(1)<15 OR MOUSE(1)>325 THEN RETURN
3520   IF MOUSE(2)<70 OR MOUSE(2)>90 THEN RETURN
3530   LINE(20+(magn!-.5)*200,72)-STEP(0,16),30
3540   magn!=.5+((MOUSE(1)-20)\20)/10
3550   LINE(20+(magn!-.5)*200,72)-STEP(0,16)
3560 RETURN

3580 Flood:
3590   REM Open a window
3600   WINDOW 2,,(80,100)-(420,220),2
3610   PRINT: PRINT: PRINT SPC(14);"Calculating levels"
3620   CALL TEXTFONT(0)
3630   PRINT: PRINT SPC(16);"Please wait"
3640   REM Find highest and lowest levels present within the array
3650   min%=10000
3660   max%=-10000
3670   FOR x%=1 TO xmax%
3680     FOR y%=l TO ymax%
3690       IF c%(x%,y%)<min% THEN min%=c%(x%,y%)
3700       IF c%(x%,y%)>max% THEN max%=c%(x%,y%)
3710     NEXT
3720   NEXT
3730   CLS
3740   CALL TEXTFONT(1)
3750   CALL TEXTSIZE(9)
3760   REM Draw flood level selection bar
3770   LINE(17,70)-(323,90),,b
3780   LINE(18,71)-(322,89),,b
3790   REM Put scale markings and labels on selection bar
3800   div%(max%-min%)\10: IF div%=0 THEN div%=l




3850   FOR i%=min% TO max%
3860     scalex%=20+(i%-min%)*300/(max%-min%)
3870     IF i% MOD div%=0 OR i%=max% THEN LINE(scalex%,90)-STEP(0,6)
3880     IF i% MOD (div%*5)=0 OR i%=min% OR i%=max% THEN
3890       CALL MOVETO(scalex%-10,110): PRINT i%;
3900   NEXT
3910   CALL TEXTSIZE(12)
3920   CALL TEXTFONT(0)
3930   CALL MOVETO(50,50): PRINT "Select required water level";
3940   BUTTON 1,1,"OK",(270,10)-(320,40),1
3950   level%=ml%-min%
3960   REM If Flood had been previously selected, the old level may now be outside
3970   REM the present level range - adjust if necessary
3980   IF flood%=0 OR level%<0 THEN level%=0
3990   IF level%>max%-min% THEN level%=max%-min%
4000   REM Draw pointer on selection bar to show current water level
4010   REM allow it to move with the mouse until the OK button is pressed
4020   LINE(20+level%*300/(max%-min%),72)-STEP(0,16)
4030   WHILE DIALOG(0)<>1
4040     IF MOUSE(0)<>0 THEN GOSUB Mouseflood
4050   WEND
4060   ml%=min%+level%
4070   REM Set flood flag if chosen flood level is not zero
4080   IF level%>0 THEN flood%=1
4090   WINDOW CLOSE 2
4100   REM Re-draw grid with new flood level
4110   GOSUB Plot
4120   MENU
4130 RETURN

4150 Mouseflood:
4160   REM Find position of mouse, if it is within selection bar then move pointer
4170   IF MOUSE(1)<20 OR MOUSE(1)>320 THEN RETURN
4180   IF MOUSE(2)<70 OR MOUSE(2)>90 THEN RETURN
4190   LINE(20+level%*300/(max%-min%),72)-STEP(0,16),30
4200   level%=(MOUSE(1)-20)/(300/(max%-min%))
4210   LINE(20+level%*300/(max%-min%),72)-STEP(0,16)
4220 RETURN










4330 Shade:
4340   IF x%=1 OR y%>=ymax% THEN RETURN
4350   REM Count number of corners in each grid square which are below flood level
4360   corner%=0
4370   maglev%=INT(magn!*ml%)
4380   IF c%(x%,y%)<=ml% THEN corner%=l
4390   IF c%(x%-1,y%)<=ml% THEN corner%=corner%+1
4400   IF c%(x%,y%+1)<=ml% THEN corner%=corner%+1
4410   IF c%(x%-1,y%+1)<=ml% THEN corner%=corner%+1
4420   REM No shading if less than 3 corners are below flood level
4430   IF corner%<3 THEN RETURN
4440   REM Prepare array for Toolbox FILLPOLY subroutine
4450   poly%(1)=yo%+(x%-1+y%)*4-maglev%
4460   poly%(2)=xo%+(y%-x%)*7
4470   poly%(3)=poly%(1)+8
4480   poly%(4)=poly%(2)+14
4490   REM If 3 corners wet shade triangle. If all 4 wet shade whole square
4500   IF corner%=3 THEN GOSUB Trishade ELSE GOSUB Squareshade
4510   CALL FILLPOLY(VARPTR(poly%(0)),VARPTR(patt%(O)))
4520 RETURN

4540 Trishade:
4550   poly%(0)=22
4560   REM Find which of 4 possible triangles needs shading
4570   IF c%(x%,y%)>ml% THEN GOSUB Tri1: RETURN
4580   IF c%(x%-1,y%)>ml% THEN GOSUB Tri2: RETURN
4590   IF c%(x%,y%+1)>ml% THEN GOSUB Tri3: RETURN
4600   GOSUB Tri4
4610 RETURN

4630 Tri1:
4640   poly%(5)=poly%(l)
4650   poly%(6)=poly%(2)+7
4660   poly%(7)=poly%(3)
4670   poly%(8)=poly%(6)
4680   poly%(9)=poly%(1)+4
4690   poly%(10)=poly%(4)
4700   poly%(2)=poly%(6)
4710 RETURN









4810 Tri2:
4820   poly%(5)=poly%(l)+4
4830   poly%(6)=poly%(2)
4840   poly%(7)=poly%(3)
4850   poly%(8)=poly%(2)+7
4860   poly%(9)=poly%(5)
4870   poly%(10)=poly%(4)
4880   poly%(l)=poly%(5)
4890 RETURN

4910 Tri3:
4920   poly%(5)=poly%(1)+4
4930   poly%(6)=poly%(2)
4940   poly%(7)=poly%(l)
4950   poly%(8)=poly%(2)+7
4960   poly%(9)=poly%(5)
4970   poly%(10)=poly%(4)
4980   poly%(3)=poly%(5)
4990 RETURN

5010 Tri4:
5020   poly%(5)=poly%(1)+4
5030   poly%(6)=poly%(2)
5040   poly%(7)=poly%(3)
5050   poly%(8)=poly%(2)+7
5060   poly%(9)=poly%(1)
5070   poly%(10)=poly%(a)
5080   poly%(4)=poly%(8)
5090 RETURN

5110 Squareshade:
5120   poly%(0)=26
5130   poly%(5)=poly%(1)+4
5140   poly%(6)=poly%(2)
5150   poly%(7)=poly%(3)
5160   poly%(8)=poly%(2)+7
5170   poly%(9)=poly%(5)
5180   poly%(1O)=poly%(4)
5190   poly%(11)=poly%(1)
5200   poly%(12)=poly%(8)
5210 RETURN

5230 Endless:
5240   WHILE 1<2: WEND
5250 RETURN
