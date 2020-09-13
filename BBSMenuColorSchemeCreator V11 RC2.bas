DIM x AS INTEGER ' x is used in my program as a variable for the background color
DIM y AS INTEGER ' y is used as a variable for the foreground color
DIM i AS INTEGER ' i is used as a variable for the length of the line of display
DIM b AS STRING * 1
' above variables are used for the reading and writing of each character of info
' in each line of the ascii screens that are read into an array.

DIM lnumber AS STRING ' used as a variable for the line number of the ascii screen
DIM file AS STRING ' used as a variable for the filename entered in the program
DIM answer AS STRING ' used as a variable for the answer to a question
DIM asciidisp AS STRING ' ascii display variable used as the ascii display path
DIM ansidisp AS STRING ' ansi display variable used as the ansi path and filename
DIM wc8disp AS STRING 'Winserver 8 variable used as the wc8 BBS display path
DIM pcbdisp AS STRING 'Pc Board display variable used as the pcb display path
DIM asciipromptmenu AS STRING
DIM syndisp AS STRING 'Syncronet display variabe used as the Syncronet display path
DIM asciimenu AS STRING 'ascii menu variable used as the ascii menu path for input
DIM ansimenu AS STRING 'ansi menu variable used as the ansi path for output
DIM wc8menu AS STRING 'Winserver 8 variable used as the wc8 menu bbs path for output
DIM pcbmenu AS STRING 'PC Board variable used as the PcB BBS menu display for output
DIM synmenu AS STRING 'Syncronet variable used as the Syncronet Menu Display for output
DIM mysmenu AS STRING 'Mystic variable
DIM mysdisp AS STRING 'Mystic variable

'above variables are all string variables that are used for storing different information that in the
'Wildcat, Winserver, Ascii, and Ansi screens that are either read in , or written out to different filenames
'in my program. The *120, or *16 etc are just string lenghths.

DIM file1 AS STRING ' file1 is the ascii input file for the displays or menus that either other people or I create
DIM file2 AS STRING ' file2 is the ansi file created by he program from the ascii file above
DIM file3 AS STRING ' file3 is the Wildcator WInserver .BBS file that is created when the user selects wc8
DIM file4 AS STRING ' file4 is the PCBoard .BBS file created when the user selects pcb as a display
DIM file5 AS STRING ' file5 is reserved for Synchronet BBS when the user selects syncronet as a display
DIM file6 AS STRING ' file6 is reserved for Mystic BBS in the user selects mystic as a display
DIM file7 AS STRING ' file7 is reserved for the prompt files that follow any menu in any BBS

'ALL of the above are actual filenames without the drive letter and path for my program.


DIM horizontalines AS STRING
DIM verticalines AS STRING
DIM foregroundcoloransi AS STRING
DIM backgroundcoloransi AS STRING
DIM foregroundcolorbbs AS STRING
DIM backgroundcolorbbs AS STRING
DIM foregroundcolorpcb AS STRING
DIM backgroundcolorpcb AS STRING
DIM foregroundcolorsyncro AS STRING
DIM backgroundcolorsyncro AS STRING
DIM foregroundcolormystic AS STRING
DIM backgroundcolormystic AS STRING

DIM code0b AS STRING ' Code Setup for Black   Background
DIM code0f AS STRING ' Code Setup for Black   Foreground
DIM code1b AS STRING ' Code Setup for Blue    Background
DIM code1f AS STRING ' Code Setup for Blue    Foreground
DIM code2b AS STRING ' Code Setup for Green   Background
DIM code2f AS STRING ' Code Setup for Green   Foreground
DIM code3b AS STRING ' Code Setup for Cyan    Background
DIM code3f AS STRING ' Code Setup for Cyan    Foreground
DIM code4b AS STRING ' Code Setup for Red     Background
DIM code4f AS STRING ' Code Setup for Red     Foreground
DIM code5b AS STRING ' Code Setup for Magenta Background
DIM code5f AS STRING ' Code Setup for Magenta Foreground
DIM code6b AS STRING ' Code Setup for Brown   Background
DIM code6f AS STRING ' Code Setup for Yellow  Foreground
DIM code7b AS STRING ' Code Setup for White   Background
DIM code7f AS STRING ' Code Setup for White   Foreground
DIM corner1 AS STRING
DIM corner2 AS STRING
DIM corner3 AS STRING
DIM corner4 AS STRING
DIM connector1 AS STRING
DIM connector2 AS STRING
DIM connector3 AS STRING
DIM connector4 AS STRING
DIM lineconnector AS STRING
DIM menuchoice AS STRING
DIM flag AS STRING
DIM verticalline AS STRING
DIM display AS STRING
_FULLSCREEN
CLS
SCREEN 12
COLOR 12, 14
LOCATE 10, 24
PRINT "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
LOCATE 11, 24
PRINT "%                 BBS Screen Creator Written By Russ Campbell                  %"
LOCATE 12, 24
PRINT "%                                                                              %"
LOCATE 13, 24
PRINT "%                   Final Version 11 Release Candidate RC2                     %"
LOCATE 14, 24
PRINT "%                                                                              %"
LOCATE 15, 24
PRINT "                 All parts of the program are: fully functional                %"
LOCATE 16, 24
PRINT "%                                                                              %"
LOCATE 17, 24
PRINT "%             With fully customisable color sets added to the program.         %"
LOCATE 18, 24
PRINT "%                                                                              %"
LOCATE 19, 24
PRINT "%                    Press any key to continue.............                    %"
LOCATE 20, 24
PRINT "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
DO WHILE INKEY$ = ""
LOOP

_FULLSCREEN
COLOR 7, 0
setup:
CLS
PRINT
PRINT "BBS Menu Color Scheme Creator Version 10 Final Version RC1"
PRINT "Written by Russ Campbell Updated September 12 2020"
PRINT
PRINT "First of all design your menus in straight text no colors, etc"
PRINT "Do not put any background colors in either as the program will"
PRINT "Do that for you automatically. Use the following convention"
PRINT "that is laid out in the next display."
PRINT
PRINT "This program will then take your plain text menu and turn it into"
PRINT "a random colored display, methods will be taken to ensure that"
PRINT "the backgound and text colors do not print the same colors"
PRINT
PRINT "Take a look and see what you think, it will of course write"
PRINT "out your menus in .BBS and .ANS formats, kind of a neat feature."
PRINT
PRINT "Don't like what you get? Then run the program again and again"
PRINT "until you get what you like as a menu screen."
PRINT
PRINT "This is a complicated program, you don't have to use boxes"
PRINT "but for now that is the only shape that my program will"
PRINT "accept. I will print an example of a menu on the screen so"
PRINT "that you know exactly how to use my program with a text drawing."
PRINT ""
PRINT "Press any key to continue..."
DO WHILE INKEY$ = ""
LOOP
_FULLSCREEN
CLS
PRINT
PRINT " ^-------------------------------------------------+"
PRINT " !               Main Message Menu                 !"
PRINT " *-------------------&-----------------------------="
PRINT " !       $           !                             !"
PRINT " *-------------------#-----------------------------="
PRINT " ! [C] Check Your Mail   [S] Scan Messages         !"
PRINT " ! [E] Enter New Message [J] Join Conference       !"
PRINT " ! [F] FILE MENU         [U] Update Settings       !"
PRINT " ! [G] Goodbye           [H] Help Level            !"
PRINT " ! [Q] Quit to Main Menu [?] Command Help          !"
PRINT " ! [R] Read Messages                               !"
PRINT " ~-------------------------------------------------%"
PRINT
PRINT "Pay special attention to the symbols on each corner"
PRINT "They are all different, one for each corner, to work"
PRINT "properly with my program , they must be right. The "
PRINT "rest of the characters also must be right except for"
PRINT "whats in the menu. You can see the other characters"
PRINT "that I use lines and connectors, the program turns"
PRINT "into nice corners and borders, for double corners "
PRINT "and borders, well , you can figure that one out."
PRINT
PRINT "Press any key to continue..."
DO WHILE INKEY$ = ""
LOOP
CLS
PRINT
inputscreen1:
CLS
PRINT
PRINT "Enter your own colors [Y]es  or [N]o  [Defaults to [Y]es   : "
answer$ = INPUT$(1)
answer$ = UCASE$(answer$)
IF answer$ = "N" THEN GOTO inputscreen
CLS
PRINT
PRINT "This section will either load or create a new user data file with all used"
PRINT "variables saved in a filename that you entered. It will give you the option"
PRINT "of loading a saved file or creating a totally new filename. You do not have"
PRINT "to write over any existing files, and the program will be fool proof."
PRINT
PRINT "Load or [C]reate User Data file or [X]xit Program"
PRINT "[No extenders Please !!!!!! ] : "
answer$ = INPUT$(1)
database$ = UCASE$(answer$)
IF answer$ = "L" THEN GOTO filen
IF answer$ = "C" THEN GOTO back
IF answer$ = "X" THEN GOTO finish
filen:
PRINT
INPUT "Filename  [Please no extenders I will put them in ] : ", filename$
filename$ = "c:\display\data\" + filename$ + ".txt"
IF database$ = "L" THEN
    GOSUB load
END IF
back:
GOSUB background
PRINT
INPUT "Enter your background color for the outer lines : ", usercolorentry_g1$
INPUT "Enter your foreground color for the inner lines : ", usercolorentry_t1$
GOSUB background
PRINT "Enter your background color"
INPUT "for the inner '[' and outer ']' that surround commands : ", usercolorentry_g2$
PRINT "Enter your foreground color"
INPUT "for the inner '[' and outer ']' that surround commands : ", usercolorentry_t2$
PRINT
CLS
PRINT
PRINT "Do you wish to have multi colors randomly chosen for everything else"
PRINT "or user set colors for the entire area of the inner box that makes up the "
PRINT "rest of the menus. This can all be used in either displays or menus"
PRINT
PRINT "Choose either [R]andom or [U]ser Set Patterns : "
answer$ = INPUT$(1)
answer$ = UCASE$(answer$)
IF answer$ = "R" THEN GOTO inputscreen
CLS
PRINT
PRINT "Please enter 8 different color combinations for the display , be creative : "
PRINT "the program will pick randomly from those colors or will use them 1 at a time,"
PRINT "depending on what you choose, either you will use the colors one at a time or "
PRINT "you will use the same colors randomly. This can be fun to do and interesting."
PRINT "depending on what you choose."
PRINT
PRINT "Press any key...."
DO WHILE INKEY$ = ""
LOOP
GOSUB enteryourbackground
GOSUB whatyouget
GOSUB randomscreen
GOSUB create

PRINT "Press any key to continue...."
DO WHILE INKEY$ = ""
LOOP
inputscreen:

CLEAR
CLS
PRINT "Demo files that are available are: "
PRINT
PRINT "In the Menu selector"
PRINT
PRINT "    inet5     inet6     inet9"
PRINT "    msg1      msg5      msg6     msg9"
PRINT "    main1     main5     main6    main9"
PRINT "    file1     file5     file6    file9"
PRINT "    sysop8    sysop9" '                  display demo files that are available
PRINT
PRINT "In the Disp selector"
PRINT
PRINT "    prelog goodbye"
PRINT
PRINT "More are coming soon. Custom files are up to you to upload and then use"
PRINT
PRINT "Filename : [Do not put in the extender ]"
INPUT "           [Enter defaults to main5    ] : ", file ' ask user for filename
IF file = "a" OR file = "A" THEN GOTO inputscreen
IF file = "b" OR file = "B" THEN GOTO inputscreen
IF file = "c" OR file = "C" THEN GOTO inputscreen
IF file = "d" OR file = "D" THEN GOTO inputscreen
IF file = "e" OR file = "E" THEN GOTO inputscreen
displays:
CLS
PRINT "Output display [A] Ansi [B] Wildcat BBS  [C] PCboard BBS "
PRINT "Output Display [D] Syncronet BBS  [E] Mystic BBS : " 'which BBS display the user wants
display$ = INPUT$(1)
display$ = UCASE$(display$)
IF display$ = "A" THEN display = "ansi": GOTO menu
IF display$ = "B" THEN display = "wc8": GOTO menu ' select display coding by what the user types in
IF display$ = "C" THEN display = "pcb": GOTO menu
IF display$ = "D" THEN display = "syn": GOTO menu
IF display$ = "E" THEN display = "mys": GOTO menu
GOTO displays
startit:

selector:
IF pick = 1 THEN
    horizontalines = "double": verticalines$ = "double"
    corner1 = CHR$(201)
    corner2 = CHR$(187) ' corner character for new corners of new box output
    corner3 = CHR$(188)
    corner4 = CHR$(200)
    connector1 = CHR$(203)
    connector2 = CHR$(185) 'various connectors for lines in boxes
    connector3 = CHR$(202)

    connector4 = CHR$(204)
    lineconnector = CHR$(205) ' Horizontal line character for new box output
    verticalline = CHR$(186)
END IF
IF pick = 2 THEN
    horizontalines = "double": verticalines = "single"
    corner1 = CHR$(213)
    corner2 = CHR$(184) ' corner character for new corners of new box output
    corner3 = CHR$(190)
    corner4 = CHR$(212)
    connector1 = CHR$(209)
    connector2 = CHR$(181) 'various connectors for lines in boxes
    connector3 = CHR$(207)
    connector4 = CHR$(198)
    lineconnector = CHR$(205) ' Horizontal line character for new box output
    verticalline = CHR$(179)
END IF
IF pick = 3 THEN
    horizontalines = "single": verticalines = "double"
    corner1 = CHR$(214)
    corner2 = CHR$(183) ' corner character for new corners of new box output
    corner3 = CHR$(189)
    corner4 = CHR$(211)
    connector1 = CHR$(210)
    connector2 = CHR$(182) 'various connectors for lines in boxes
    connector3 = CHR$(208)
    connector4 = CHR$(199)
    lineconnector = CHR$(196) ' Horizontal line character for new box output
    verticalline = CHR$(186)

END IF
IF pick = 4 THEN
    horizontalines = "single": verticalines = "single"
    corner1 = CHR$(218)
    corner2 = CHR$(191) ' corner character for new corners of new box output
    corner3 = CHR$(217)
    corner4 = CHR$(192)
    connector1 = CHR$(194)
    connector2 = CHR$(180) 'various connectors for lines in boxes
    connector3 = CHR$(193)
    connector4 = CHR$(195)
    lineconnector = CHR$(196) ' Horizontal line character for new box output
    verticalline = CHR$(179)
END IF
RETURN




'----------------------------------------------------------------------------------------------------------------------------





menu:
PRINT "[M]enu file or [D]isplay file : "
answer$ = INPUT$(1)
answer = UCASE$(answer) ' ask the user what type of display they want, display file
menuchoice = answer ' or menu file , gives two choices
IF menuchoice = "M" OR menuchoice = "D" OR menuchoice = "" THEN GOTO fileit
GOTO menu
fileit:
IF file = "" AND menuchoice = "D" THEN ' default file for display files
    file = "goodbye"
END IF
IF file = "" AND menuchoice = "M" THEN ' default file for menu files
    file = "main5"
END IF



'-------------------------------------------------------------------------------------------------------------
' Start of actual program , continues until ended
'Start of menu selection decisions by the program
file1 = file + ".txt"
file2 = file + ".ans" ' adds file extension to each type of file
file3 = file + ".bbs"
file4 = file + ".bbs"
file5 = file + ".txt"
file6 = file + ".txt"
file7 = file + "a.txt"
asciidisp = "c:\display\bbsdispascii\" + file1

ansidisp = "c:\display\bbsdispansi\" + file2
wc8disp = "c:\display\bbsdispwildcat\" + file3
pcbdisp = "c:\display\bbsdisppcb\" + file4 ' all path, drive and filename variables
syndisp = "c:\display\bbsdispsyncro\" + file5
mysdisp = "c:\display\bbsdispmystic\" + file6
asciimenu = "c:\display\bbsmenuascii\" + file1
ansimenu = "c:\display\bbsmenuansi\" + file2 ' for various screen ouputs
wc8menu = "c:\display\bbsmenuWildcat\" + file3
pcbmenu = "c:\display\bbsmenupcb\" + file4
synmenu = "c:\display\bbsmenusyncro\" + file5
mysmenu = "C:\display\bbsmenumystic\" + file6
asciipromptmenu = "c:\display\bbsmenuascii\" + file7
ON ERROR GOTO errorhandle
IF menuchoice = "D" THEN
    contlne = 0
    GOSUB pick
    GOSUB selector
    OPEN asciidisp FOR INPUT AS #1

    IF display = "ansi" THEN
        OPEN ansidisp FOR OUTPUT AS #2
    END IF

    IF display = "wc8" THEN
        OPEN wc8disp FOR OUTPUT AS #2
    END IF

    IF display = "pcb" THEN
        OPEN pcbdisp FOR OUTPUT AS #2
    END IF
    IF display = "syn" THEN
        OPEN syndisp FOR OUTPUT AS #2
    END IF
    IF display = "mys" THEN
        OPEN mysdisp FOR OUTPUT AS #2
    END IF

END IF

IF menuchoice = "M" THEN

    contlne = 0
    GOSUB pick
    GOSUB selector
    OPEN asciimenu FOR INPUT AS #1
    OPEN asciipromptmenu FOR INPUT AS #3

    IF display = "ansi" THEN
        OPEN ansimenu FOR OUTPUT AS #2
    END IF

    IF display = "wc8" THEN
        OPEN wc8menu FOR OUTPUT AS #2
    END IF

    IF display = "pcb" THEN
        OPEN pcbmenu FOR OUTPUT AS #2
    END IF
    IF display = "syn" THEN
        OPEN synmenu FOR OUTPUT AS #2
    END IF
    IF display = "mys" THEN
        OPEN mysmenu FOR OUTPUT AS #2
    END IF
END IF ' end of menu selection decisions by the program.
IF display = "ansi" THEN
    ' Clear screen
    CLS
    ' Set background to black
    COLOR 0, 0
END IF
IF display = "wc8" THEN
    ' clear screen
    CLS
    PRINT #2, "@0" + "0@" + "@CLS@"
    ' set background to black
    COLOR 0, 0
END IF
IF display = "pcb" THEN
    ' clear screen
    CLS
    ' set background to black
    COLOR 0, 0
END IF
IF display = "syn" THEN
    ' clear screen
    CLS
    ' set background to black
    COLOR 0, 0
END IF
IF display = "mys" THEN
    ' clear screen
    CLS
    ' set background to black
    COLOR 0, 0
END IF
IF display = "ansi" OR display = "syn" THEN GOSUB setcolorsall8
IF display = "wc8" OR display = "pcb" OR display = "mys" THEN GOSUB setcolorsall16
DO UNTIL EOF(1)
    LINE INPUT #1, lnumber
    FOR i = 1 TO LEN(lnumber)
        b = MID$(lnumber, i, 1) ' Calculates the ascii value of every character in the line
        c$ = b
        IF display = "ansi" THEN
            GOSUB specialcharacters3
            GOSUB specialcharacters2

            GOSUB displ
            IF flag$ = "Y1" THEN
                GOSUB colorchange2
                cs = cs + 1
            ELSEIF flag$ = "Y2" THEN
                GOSUB colorchange2
                cs = cs + 1
            ELSEIF flag$ = "Y3" THEN
                GOSUB colorchange2
                cs = cs + 1

            ELSE
                IF cs > 0 THEN
                    PRINT #2, backgroundcolorbbs + foregroundcoloransi + c$;
                    cs = 0
                ELSE IF cs = 0 THEN
                        PRINT c$;
                        cs = 0
                    END IF
                END IF
            END IF
        END IF
        IF display = "wc8" THEN
            GOSUB specialcharacters3
            GOSUB specialcharacters2

            GOSUB displ
            IF flag$ = "Y1" THEN
                GOSUB colorchange2
                cs = cs + 1
            ELSEIF flag$ = "Y2" THEN
                GOSUB colorchange2
                cs = cs + 1
            ELSEIF flag$ = "Y3" THEN
                GOSUB colorchange2
                cs = cs + 1

            ELSE
                IF cs > 0 THEN
                    PRINT #2, backgroundcolorbbs + foregroundcoloransi + c$;
                    PRINT c$;
                    cs = 0
                ELSE IF cs = 0 THEN
                        PRINT #2, c$;
                        PRINT c$;
                        cs = 0
                    END IF
                END IF
            END IF
        END IF

        IF display = "pcb" THEN
            GOSUB specialcharacters3
            GOSUB specialcharacters2

            GOSUB displ
            IF flag$ = "Y1" THEN
                GOSUB colorchange2
                cs = cs + 1
            ELSEIF flag$ = "Y2" THEN
                GOSUB colorchange2
                cs = cs + 1
            ELSEIF flag$ = "Y3" THEN
                GOSUB colorchange2
                cs = cs + 1

            ELSE
                IF cs > 0 THEN
                    PRINT #2, backgroundcolorbbs + foregroundcoloransi + c$;
                    cs = 0
                ELSE IF cs = 0 THEN
                        PRINT c$;
                        cs = 0
                    END IF
                END IF
            END IF
        END IF
        IF display = "syn" THEN
            GOSUB specialcharacters3
            GOSUB specialcharacters2

            GOSUB displ
            IF flag$ = "Y1" THEN
                GOSUB colorchange2
                cs = cs + 1
            ELSEIF flag$ = "Y2" THEN
                GOSUB colorchange2
                cs = cs + 1
            ELSEIF flag$ = "Y3" THEN
                GOSUB colorchange2
                cs = cs + 1

            ELSE
                IF cs > 0 THEN
                    PRINT #2, backgroundcolorbbs + foregroundcoloransi + c$;
                    cs = 0
                ELSE IF cs = 0 THEN
                        PRINT c$;
                        cs = 0
                    END IF
                END IF
            END IF
        END IF
        IF display = "mys" THEN
            GOSUB specialcharacters3
            GOSUB specialcharacters2

            GOSUB displ
            IF flag$ = "Y1" THEN
                GOSUB colorchange2
                cs = cs + 1
            ELSEIF flag$ = "Y2" THEN
                GOSUB colorchange2
                cs = cs + 1
            ELSEIF flag$ = "Y3" THEN
                GOSUB colorchange2
                cs = cs + 1

            ELSE
                IF cs > 0 THEN
                    PRINT #2, backgroundcolorbbs + foregroundcoloransi + c$;
                    cs = 0
                ELSE IF cs = 0 THEN
                        PRINT c$;
                        cs = 0
                    END IF
                END IF
            END IF
        END IF

    NEXT i
    PRINT #2, ""
    PRINT
    IF display = "syn" OR display = "ansi" THEN GOSUB setcolorsg8
    IF display = "wc8" OR display = "pcb" OR display = "mys" THEN GOSUB setcolorsg16
LOOP

fini:
closeit:
CLOSE #1
PRINT #2, "@0" + "7@";
COLOR 7, 0
IF menuchoice = "M" THEN
    DO UNTIL EOF(3)
        LINE INPUT #3, lnumber
        PRINT #2, lnumber
        PRINT lnumber
    LOOP
ELSE
END IF
PRINT #2, "@0" + "7@"
CLOSE #2
CLOSE #3
COLOR 7, 0
PRINT
PRINT " Create another screen ? [Defaults to [Y]es : "
answer = INPUT$(1)
answer = UCASE$(answer)
IF answer = "N" THEN
    GOTO finish
ELSE
    contlne = 0
    CLEAR
    GOTO inputscreen1

END IF

finish:
CLS
PRINT "My program is now finished and I"
PRINT "am looking for suggestions on ways"
PRINT "To improve it. The program has been"
PRINT "tested with Winserver 8.0 but has"
PRINT "not been tested with any other BBS"
PRINT "programs at this moment."
PRINT
PRINT "By now you will have noticed custom"
PRINT "Color sets, if you have not tried "
PRINT "this out , then give it a try."
PRINT
PRINT "Program written by Russ Campbell"
PRINT "For more information on how I"
PRINT "wrote this program, contact me"
PRINT "on Facebook at many of the groups"
PRINT "I am in , or email me at"
PRINT "rcamp48@rogers.com"
PRINT
PRINT "Thank you for using BBS Menu Color Scheme Creator 11 RC2"
PRINT
END
pick:
RANDOMIZE TIMER
pick = INT(RND(1) * 4) + 1
RETURN
displ:
IF display = "ansi" THEN
    IF x = 0 THEN code0b = CHR$(27) + CHR$(91) + CHR$(52) + CHR$(48) + CHR$(109) ' Black     Background
    IF y = 0 THEN code0f = CHR$(27) + CHR$(91) + CHR$(51) + CHR$(48) + CHR$(109) ' Black     Foreground
    IF x = 1 THEN code1b = CHR$(27) + CHR$(91) + CHR$(52) + CHR$(52) + CHR$(109) ' Blue      Background
    IF y = 1 THEN code1f = CHR$(27) + CHR$(91) + CHR$(51) + CHR$(52) + CHR$(109) ' Blue      Foreground
    IF x = 2 THEN code2b = CHR$(27) + CHR$(91) + CHR$(52) + CHR$(50) + CHR$(109) ' Green     Background
    IF y = 2 THEN code2f = CHR$(27) + CHR$(91) + CHR$(51) + CHR$(50) + CHR$(109) ' Green     Foreground
    IF x = 3 THEN code3b = CHR$(27) + CHR$(91) + CHR$(52) + CHR$(54) + CHR$(109) ' Cyan      Background
    IF y = 3 THEN code3f = CHR$(27) + CHR$(91) + CHR$(51) + CHR$(54) + CHR$(109) ' Cyan      Foreground
    IF x = 4 THEN code4b = CHR$(27) + CHR$(91) + CHR$(52) + CHR$(49) + CHR$(109) ' Red       Background
    IF y = 4 THEN code4f = CHR$(27) + CHR$(91) + CHR$(51) + CHR$(49) + CHR$(109) ' Red       Foreground
    IF x = 5 THEN code5b = CHR$(27) + CHR$(91) + CHR$(52) + CHR$(53) + CHR$(109) ' Magenta   Background
    IF y = 5 THEN code5f = CHR$(27) + CHR$(91) + CHR$(51) + CHR$(53) + CHR$(109) ' Magenta   Foreground
    IF x = 6 THEN code6b = CHR$(27) + CHR$(91) + CHR$(52) + CHR$(51) + CHR$(109) ' Brown     Background
    IF y = 6 THEN code6f = CHR$(27) + CHR$(91) + CHR$(51) + CHR$(51) + CHR$(109) ' Brown     Foreground
    IF x = 7 THEN code7b = CHR$(27) + CHR$(91) + CHR$(52) + CHR$(55) + CHR$(109) ' White     Background
    IF y = 7 THEN code7f = CHR$(27) + CHR$(91) + CHR$(51) + CHR$(55) + CHR$(109) ' White     Foreground
    IF x = 0 THEN backgroundcoloransi = code0b
    IF x = 1 THEN backgroundcoloransi = code1b
    IF x = 2 THEN backgroundcoloransi = code2b
    IF x = 3 THEN backgroundcoloransi = code3b
    IF x = 4 THEN backgroundcoloransi = code4b
    IF x = 5 THEN backgroundcoloransi = code5b
    IF x = 6 THEN backgroundcoloransi = code6b
    IF x = 7 THEN backgroundcoloransi = code7b
    IF y = 0 THEN foregroundcoloransi = code0f
    IF y = 1 THEN foregroundcoloransi = code1f
    IF y = 2 THEN foregroundcoloransi = code2f
    IF y = 3 THEN foregroundcoloransi = code3f
    IF y = 4 THEN foregroundcoloransi = code4f
    IF y = 5 THEN foregroundcoloransi = code5f
    IF y = 6 THEN foregroundcoloransi = code6f
    IF y = 7 THEN foregroundcoloransi = code7f
END IF
IF display = "wc8" THEN
    IF x = 0 THEN backgroundcolorbbs = "@0"
    IF x = 1 THEN backgroundcolorbbs = "@1"
    IF x = 2 THEN backgroundcolorbbs = "@2"
    IF x = 3 THEN backgroundcolorbbs = "@3"
    IF x = 4 THEN backgroundcolorbbs = "@4"
    IF x = 5 THEN backgroundcolorbbs = "@5"
    IF x = 6 THEN backgroundcolorbbs = "@6"
    IF x = 7 THEN backgroundcolorbbs = "@7"
    IF x = 8 THEN backgroundcolorbbs = "@8"
    IF x = 9 THEN backgroundcolorbbs = "@9"
    IF x = 10 THEN backgroundcolorbbs = "@A"
    IF x = 11 THEN backgroundcolorbbs = "@B"
    IF x = 12 THEN backgroundcolorbbs = "@C"
    IF x = 13 THEN backgroundcolorbbs = "@D"
    IF x = 14 THEN backgroundcolorbbs = "@E"
    IF x = 15 THEN backgroundcolorbbs = "@F"
    IF y = 0 THEN foregroundcolorbbs = "0@"
    IF y = 1 THEN foregroundcolorbbs = "1@"
    IF y = 2 THEN foregroundcolorbbs = "2@"
    IF y = 3 THEN foregroundcolorbbs = "3@"
    IF y = 4 THEN foregroundcolorbbs = "4@"
    IF y = 5 THEN foregroundcolorbbs = "5@"
    IF y = 6 THEN foregroundcolorbbs = "6@"
    IF y = 7 THEN foregroundcolorbbs = "7@"
    IF y = 8 THEN foregroundcolorbbs = "8@"
    IF y = 9 THEN foregroundcolorbbs = "9@"
    IF y = 10 THEN foregroundcolorbbs = "A@"
    IF y = 11 THEN foregroundcolorbbs = "B@"
    IF y = 12 THEN foregroundcolorbbs = "C@"
    IF y = 13 THEN foregroundcolorbbs = "D@"
    IF y = 14 THEN foregroundcolorbbs = "E@"
    IF y = 15 THEN foregroundcolorbbs = "F@"
END IF
IF display = "pcb" THEN
    IF x = 0 THEN backgroundcolorpcb$ = "@X0"
    IF x = 1 THEN backgroundcolorpcb$ = "@X1"
    IF x = 2 THEN backgroundcolorpcb$ = "@X2"
    IF x = 3 THEN backgroundcolorpcb$ = "@X3"
    IF x = 4 THEN backgroundcolorpcb$ = "@X4"
    IF x = 5 THEN backgroundcolorpcb$ = "@X5"
    IF x = 6 THEN backgroundcolorpcb$ = "@X6"
    IF x = 7 THEN backgroundcolorpcb$ = "@X7"
    IF x = 8 THEN backgroundcolorpcb$ = "@X8"
    IF x = 9 THEN backgroundcolorpcb$ = "@X9"
    IF x = 10 THEN backgroundcolorpcb$ = "@XA"
    IF x = 11 THEN backgroundcolorpcb$ = "@XB"
    IF x = 12 THEN backgroundcolorpcb$ = "@XC"
    IF x = 13 THEN backgroundcolorpcb$ = "@XD"
    IF x = 14 THEN backgroundcolorpcb$ = "@XE"
    IF x = 15 THEN backgroundcolorpcb$ = "@XF"
    IF y = 0 THEN foregroundcolorpcb$ = "0@"
    IF y = 1 THEN foregroundcolorpcb$ = "1@"
    IF y = 2 THEN foregroundcolorpcb$ = "2@"
    IF y = 3 THEN foregroundcolorpcb$ = "3@"
    IF y = 4 THEN foregroundcolorpcb$ = "4@"
    IF y = 5 THEN foregroundcolorpcb$ = "5@"
    IF y = 6 THEN foregroundcolorpcb$ = "6@"
    IF y = 7 THEN foregroundcolorpcb$ = "7@"
    IF y = 8 THEN foregroundcolorpcb$ = "8@"
    IF y = 9 THEN foregroundcolorpcb$ = "9@"
    IF y = 10 THEN foregroundcolorpcb$ = "A@"
    IF y = 11 THEN foregroundcolorpcb$ = "B@"
    IF y = 12 THEN foregroundcolorpcb$ = "C@"
    IF y = 13 THEN foregroundcolorpcb$ = "D@"
    IF y = 14 THEN foregroundcolorpcb$ = "E@"
    IF y = 15 THEN foregroundcolorpcb$ = "F@"

END IF
IF display = "syn" THEN
    IF x = 0 THEN backgroundcolorsyncro$ = CHR$(1) + "0"
    IF x = 1 THEN backgroundcolorsyncro$ = CHR$(1) + "1"
    IF x = 2 THEN backgroundcolorsyncro$ = CHR$(1) + "2"
    IF x = 3 THEN backgroundcolorsyncro$ = CHR$(1) + "3"
    IF x = 4 THEN backgroundcolorsyncro$ = CHR$(1) + "4"
    IF x = 5 THEN backgroundcolorsyncro$ = CHR$(1) + "5"
    IF x = 6 THEN backgroundcolorsyncro$ = CHR$(1) + "6"
    IF x = 7 THEN backgroundcolorsyncro$ = CHR$(1) + "7"
    IF y = 0 THEN foregroundcolorsyncro$ = "k"
    IF y = 1 THEN foregroundcolorsyncro$ = "b"
    IF y = 2 THEN foregroundcolorsyncro$ = "g"
    IF y = 3 THEN foregroundcolorsyncro$ = "c"
    IF y = 4 THEN foregroundcolorsyncro$ = "r"
    IF y = 5 THEN foregroundcolorsyncro$ = "m"
    IF y = 6 THEN foregroundcolorsyncro$ = "y"
    IF y = 7 THEN foregroundcolorsyncro$ = "w"


END IF
IF display = "mys" THEN
    IF x = 0 THEN backgroundcolormystic$ = CHR$(254) + "16"
    IF x = 1 THEN backgroundcolormystic$ = CHR$(254) + "17"
    IF x = 2 THEN backgroundcolormystic$ = CHR$(254) + "18"
    IF x = 3 THEN backgroundcolormystic$ = CHR$(254) + "19"
    IF x = 4 THEN backgroundcolormystic$ = CHR$(254) + "20"
    IF x = 5 THEN backgroundcolormystic$ = CHR$(254) + "21"
    IF x = 6 THEN backgroundcolormystic$ = CHR$(254) + "22"
    IF x = 7 THEN backgroundcolormystic$ = CHR$(254) + "23"
    IF x = 8 THEN backgroundcolormystic$ = CHR$(254) + "24"
    IF x = 9 THEN backgroundcolormystic$ = CHR$(254) + "25"
    IF x = 10 THEN backgroundcolormystic$ = CHR$(254) + "26"
    IF x = 11 THEN backgroundcolormystic$ = CHR$(254) + "27"
    IF x = 12 THEN backgroundcolormystic$ = CHR$(254) + "28"
    IF x = 13 THEN backgroundcolormystic$ = CHR$(254) + "29"
    IF x = 14 THEN backgroundcolormystic$ = CHR$(254) + "30"
    IF x = 15 THEN backgroundcolormystic$ = CHR$(254) + "31"
    IF y = 0 THEN foregroundcolormystic$ = CHR$(254) + "00"
    IF y = 1 THEN foregroundcolormystic$ = CHR$(254) + "01"
    IF y = 2 THEN foregroundcolormystic$ = CHR$(254) + "02"
    IF y = 3 THEN foregroundcolormystic$ = CHR$(254) + "03"
    IF y = 4 THEN foregroundcolormystic$ = CHR$(254) + "04"
    IF y = 5 THEN foregroundcolormystic$ = CHR$(254) + "05"
    IF y = 6 THEN foregroundcolormystic$ = CHR$(254) + "06"
    IF y = 7 THEN foregroundcolormystic$ = CHR$(254) + "07"
    IF y = 8 THEN foregroundcolormystic$ = CHR$(254) + "08"
    IF y = 9 THEN foregroundcolormystic$ = CHR$(254) + "09"
    IF y = 10 THEN foregroundcolormystic$ = CHR$(254) + "10"
    IF y = 11 THEN foregroundcolormystic$ = CHR$(254) + "11"
    IF y = 12 THEN foregroundcolormystic$ = CHR$(254) + "12"
    IF y = 13 THEN foregroundcolormystic$ = CHR$(254) + "13"
    IF y = 14 THEN foregroundcolormystic$ = CHR$(254) + "14"
    IF y = 15 THEN foregroundcolormystic$ = CHR$(254) + "15"
END IF
RETURN
specialcharacters2:

IF b = CHR$(94) THEN c$ = corner1
IF b = CHR$(43) THEN c$ = corner2
IF b = CHR$(37) THEN c$ = corner3
IF b = CHR$(126) THEN c$ = corner4
IF b = CHR$(45) THEN
    c$ = lineconnector
END IF
IF b = CHR$(38) THEN
    c$ = connector1
END IF

IF b = CHR$(61) THEN
    c$ = connector2
END IF
IF b = CHR$(35) THEN
    c$ = connector3
END IF
IF b = CHR$(42) THEN
    c$ = connector4
END IF
IF b = CHR$(33) THEN
    c$ = verticalline
END IF
specialcharacters3:
IF b = CHR$(94) OR b = CHR$(43) OR b = CHR$(37) OR b = CHR$(126) OR b = CHR$(45) OR b = CHR$(38) OR b = CHR$(35) OR b = CHR$(42) OR b = CHR$(33) OR b = CHR$(61) THEN
    flag$ = "Y1"
ELSEIF b = CHR$(91) OR b = CHR$(93) THEN
    flag$ = "Y2"

ELSE
    flag$ = "Y3"
END IF

RETURN
colorchange:
IF flag$ = "Y1" THEN
    IF randome$ = "N" THEN
        g1 = usercolorentry_g1
        t1 = usercolorentry_t1
    ELSE
        GOSUB pick1
    END IF


    COLOR g1, t1
    x = g1
    y = t1

    GOSUB pick1
    COLOR g1, t1

    x = g1
    y = t1
ELSEIF flag$ = "Y2" THEN
    IF randome$ = "N" THEN
        g2 = usercolorentry_g2
        t2 = usercolorentry_t2
    ELSE
        GOSUB pick2
    END IF

    COLOR g2, y2
    x = g2
    y = y2
ELSEIF flag$ = "Y3" THEN
    IF randome$ = "N" THEN
        GOSUB randomscreen
        COLOR g, t
        x = g
        y = t
        c1 = x

    ELSE
        GOSUB pick3
        COLOR g, t
        x = g
        y = t
        c1 = x
    END IF
END IF
RETURN
colorchange2:
IF flag$ = "Y1" THEN
    IF randome$ = "N" THEN
        g1 = usercolorentry_g1
        t1 = usercolorentry_g1
    ELSE
        GOSUB pick4
    END IF

    COLOR g1, t1
    x = g1
    y = t1
ELSEIF flag$ = "Y2" THEN

    IF randome$ = "N" THEN
        g2 = usercolorentry_g2
        t2 = usercolorentry_g2
    ELSE
        GOSUB pick5
    END IF
    COLOR g2, t2


    x = g2
    y = t2
ELSEIF flag$ = "Y3" THEN
    IF multicolor$ = "N" THEN
        GOSUB randomscreen
        x = g
        y = t
        c1 = x
    ELSE

        GOSUB pick6
        COLOR g, t
        x = g
        y = t
        c1 = x
    END IF
END IF
RETURN

setcolorsall8:
RANDOMIZE TIMER
g1 = INT(RND(1) * 7) + 1
GOSUB pick4
g2 = INT(RND(1) * 7) + 1
IF g2 = g1 THEN GOTO setcolorsall8
GOSUB pick5
g = INT(RND(1) * 7) + 1
IF g = g2 OR g = g1 THEN GOTO setcolorsall8
IF g = c1 THEN GOTO setcolorsall8
IF g = c2 THEN GOTO setcolorsall8
IF g = 0 THEN GOTO setcolorsall8
GOSUB pick6

RETURN
setcolorsg8:
RANDOMIZE TIMER
g = INT(RND(1) * 7) + 1
IF g = c1 THEN GOTO setcolorsg8
IF g = c2 THEN GOTO setcolorsg8
IF g = 0 THEN GOTO setcolorsg8
GOSUB pick6

RETURN
setcolorsg16:
RANDOMIZE TIMER
g = INT(RND(1) * 7) + 1
IF g = c1 THEN GOTO setcolorsg16
IF g = c2 THEN GOTO setcolorsg16
IF g = 0 THEN GOTO setcolorsg16
GOSUB pick3
RETURN

setcolorsall16:
RANDOMIZE TIMER
g1 = INT(RND(1) * 7) + 1
GOSUB pick1
g2 = INT(RND(1) * 7) + 1
IF g2 = g1 THEN GOTO setcolorsall16
GOSUB pick2
g = INT(RND(1) * 7) + 1
IF g = g2 OR g = g1 THEN GOTO setcolorsall16
IF g = c1 THEN GOTO setcolorsall16
IF g = c2 THEN GOTO setcolorsall16
IF g = 0 THEN GOTO setcolorsall16
GOSUB pick3

RETURN

errorhandle:
CLS
CLOSE #1
PRINT
PRINT "Most likely there was a filename that you entered"
PRINT "That did not exist or you have not created the"
PRINT "prompt file for the Menu item desired. Or you picked"
PRINT "a Menu filename and selected a display output instead "
PRINT "a Menu display, the difference is that Menu output "
PRINT "files have a prompt file, display files do not. "
PRINT
PRINT "Your File "; file1; "Has Not Been Found .... Please Try Again."
PRINT "Press any key to contine ......"
DO WHILE INKEY$ = ""
LOOP
GOTO inputscreen
RETURN
pick1:
IF g1 = 0 THEN t1 = 7
IF g1 = 1 THEN t1 = 6
IF g1 = 2 THEN t1 = 5
IF g1 = 3 THEN t1 = 4
IF g1 = 4 THEN t1 = 3
IF g1 = 5 THEN t1 = 2
IF g1 = 6 THEN t1 = 1
IF g1 = 7 THEN t1 = 0
RETURN

pick2:
IF g2 = 0 THEN t2 = 7
IF g2 = 1 THEN t2 = 6
IF g2 = 2 THEN t2 = 5
IF g2 = 3 THEN t2 = 4
IF g2 = 4 THEN t2 = 3
IF g2 = 5 THEN t2 = 2
IF g2 = 6 THEN t2 = 1
IF g2 = 7 THEN t2 = 0

RETURN
pick3:
IF g = 1 THEN t = 6
IF g = 2 THEN t = 5
IF g = 3 THEN t = 4
IF g = 4 THEN t = 3
IF g = 5 THEN t = 2
IF g = 6 THEN t = 1
IF g = 7 THEN t = 0
c1 = g
c2 = g1
RETURN
pick4:
IF g1 = 0 THEN t1 = 7
IF g1 = 1 THEN t1 = 6
IF g1 = 2 THEN t1 = 5
IF g1 = 3 THEN t1 = 4
IF g1 = 4 THEN t1 = 3
IF g1 = 5 THEN t1 = 2
IF g1 = 6 THEN t1 = 1
IF g1 = 7 THEN t1 = 0
RETURN
pick5:
IF g2 = 0 THEN t2 = 7
IF g2 = 1 THEN t2 = 6
IF g2 = 2 THEN t2 = 5
IF g2 = 3 THEN t2 = 4
IF g2 = 4 THEN t2 = 3
IF g2 = 5 THEN t2 = 2
IF g2 = 6 THEN t2 = 1
IF g2 = 7 THEN t2 = 0
RETURN
pick6:

IF g = 1 THEN t = 6
IF g = 2 THEN t = 5
IF g = 3 THEN t = 4
IF g = 4 THEN t = 3
IF g = 5 THEN t = 2
IF g = 6 THEN t = 1
IF g = 7 THEN t = 0
c1 = g
c2 = g1
RETURN

background:
CLS
PRINT
PRINT "Please enter the color of your Background "
PRINT
PRINT "Black        is color code 0 "
PRINT "Blue         is color code 1 "
PRINT "Green        is color code 2 " ' ask for and show the various colors of
PRINT "Cyan         is color code 3 " ' background codes.
PRINT "Red          is color code 4 "
PRINT "Magenta      is color code 5 "
PRINT "Brown        is color code 6 "
PRINT "White        is color code 7 "
IF display = "syn" OR display = "ansi" THEN GOTO nex

PRINT "Black        is color code 8 "
PRINT "Blue         is color code 9 "
PRINT "Green        is color code 10"
PRINT "Cyan         is color code 11"
PRINT "Red          is color code 12"
PRINT "Magenta      is color code 13"
PRINT "Yellow       is color code 14"
PRINT "White        is color code 15"
PRINT
nex:
PRINT
PRINT "Don't worry, the program will translate from decimal to hex for this."
PRINT "Make your Background selection from [0] to [7] for Syncronet and Ansi."
PRINT "And [0] to [15] for everything else"
PRINT
RETURN
enteryourbackground:
etry1:
GOSUB background
INPUT "Color # 1 Background [From 0 to  7] : ", color1background
INPUT "Color # 1 Foreground [From 0 to 15] : ", color1foreground
PRINT
COLOR color1background, color1foreground
PRINT "[------------------]";: COLOR 7, 0: COLOR 7, 0
PRINT
PRINT "Is This acceptable [Y]es or [N]o : "
answer$ = INPUT$(1)
answer$ = UCASE$(answer$)
IF answer$ = "N" THEN GOTO etry1
etry2:
GOSUB background
INPUT "Color # 2 Background [From 0 to  7] : ", color2background
INPUT "Color # 2 Foreground [From 0 to 15] : ", color2foreground
PRINT
COLOR color2background, color2foreground
PRINT "[------------------]";: COLOR 7, 0: COLOR 7, 0
PRINT
PRINT "Is This acceptable [Y]es or [N]o : "
answer$ = INPUT$(1)
answer$ = UCASE$(answer$)
IF answer$ = "N" THEN GOTO etry2
etry3:
GOSUB background
INPUT "Color # 3 Background [From 0 to  7] : ", color3background
INPUT "Color # 3 Foreground [From 0 to 15] : ", color3foreground
PRINT
COLOR color3background, color3foreground
PRINT "[------------------]";: COLOR 7, 0: COLOR 7, 0
PRINT
PRINT "Is This acceptable [Y]es or [N]o : "
answer$ = INPUT$(1)
answer$ = UCASE$(answer$)
IF answer$ = "N" THEN GOTO etry3
etry4:
GOSUB background
INPUT "Color # 4 Background [From 0 to  7] : ", color4background
INPUT "Color # 4 Foreground [From 0 to 15] : ", color4foreground
PRINT
COLOR color4background, color4foreground
PRINT "[------------------]";: COLOR 7, 0: COLOR 7, 0
PRINT
PRINT "Is This acceptable [Y]es or [N]o : "
answer$ = INPUT$(1)
answer$ = UCASE$(answer$)
IF answer$ = "N" THEN GOTO etry4
etry5:
GOSUB background
INPUT "Color # 5 Background [From 0 to  7] : ", color5background
INPUT "Color # 5 Foreground [From 0 to 15] : ", color5foreground
PRINT
COLOR color5background, color5foreground
PRINT "[------------------]";: COLOR 7, 0: COLOR 7, 0
PRINT
PRINT "Is This acceptable [Y]es or [N]o : "
answer$ = INPUT$(1)
answer$ = UCASE$(answer$)
IF answer$ = "N" THEN GOTO etry5
etry6:
GOSUB background
INPUT "Color # 6 Background [From 0 to  7] : ", color6background
INPUT "Color # 6 Foreground [From 0 to 15] : ", color6foreground
PRINT
COLOR color6background, color6foreground
PRINT "[------------------]";: COLOR 7, 0: COLOR 7, 0
PRINT
PRINT "Is This acceptable [Y]es or [N]o : "
answer$ = INPUT$(1)
answer$ = UCASE$(answer$)
IF answer$ = "N" THEN GOTO etry6
etry7:
GOSUB background
INPUT "Color # 7 Background [From 0 to  7] : ", color7background
INPUT "Color # 7 Foreground [From 0 to 15] : ", color7foreground
PRINT
COLOR color7background, color7foreground
PRINT "[------------------]";: COLOR 7, 0: COLOR 7, 0
PRINT
PRINT "Is This acceptable [Y]es or [N]o : "
answer$ = INPUT$(1)
answer$ = UCASE$(answer$)
IF answer$ = "N" THEN GOTO etry7
etry8:
GOSUB background
INPUT "Color # 8 Background [From 0 to  7] : ", color8background
INPUT "Color # 8 Foreground [From 0 to 15] : ", color8foreground
PRINT
COLOR color8background, color8foreground
PRINT "[------------------]";: COLOR 7, 0: COLOR 7, 0
PRINT
PRINT "Is This acceptable [Y]es or [N]o : "
answer$ = INPUT$(1)
answer$ = UCASE$(answer$)
IF answer$ = "N" THEN GOTO etry8
RETURN
whatyouget:
CLS
PRINT
COLOR 7, 0
PRINT
PRINT "This is what you get for colors [Is it still acceptable] [Y] or [N] : "

COLOR usercolorentry_g1, usercolorentry_t1
PRINT "[------------------------------------------------------]"
COLOR color1background, color1foreground
PRINT "[This is line 1 of 8 printed to see what it looks like.]"
COLOR usercolorentry_g1, usercolorentry_t1

PRINT "[------------------------------------------------------]"
COLOR color2background, color2foreground
PRINT "[This is line 2 of 8 printed to see what it looks like.]"
COLOR usercolorentry_g1, usercolorentry_t1

PRINT "[------------------------------------------------------]"
COLOR color3background, color3foreground
PRINT "[This is line 3 of 8 printed to see what it looks like.]"
COLOR usercolorentry_g1, usercolorentry_t1

PRINT "[------------------------------------------------------]"
COLOR color4background, color4foreground
PRINT "[This is line 4 of 8 printed to see what it looks like.]"
COLOR usercolorentry_g1, usercolorentry_t1

PRINT "[------------------------------------------------------]"
COLOR color5background, color5foreground
PRINT "[This is line 5 of 8 printed to see what it looks like.]"
COLOR usercolorentry_g1, usercolorentry_t1

PRINT "[------------------------------------------------------]"
COLOR color6background, color6foreground

PRINT "[This is line 6 of 8 printed to see what it looks like.]"
COLOR usercolorentry_g1, usercolorentry_t1

PRINT "[------------------------------------------------------]"
COLOR color7background, color7foreground

PRINT "[This is line 7 of 8 printed to see what it looks like.]"
COLOR usercolorentry_g1, usercolorentry_t1

PRINT "[------------------------------------------------------]"
PRINT "[This is line 8 of 8 printed to see what it looks like.]"
COLOR usercolorentry_g1, usercolorentry_t1

PRINT "[------------------------------------------------------]"

COLOR 7, 0
PRINT
PRINT "Acceptable ???? [Y]es or [N]o : "
PRINT
answer$ = INPUT$(1)
answer$ = UCASE$(answer$)
IF answer$ = "N" THEN GOTO inputscreen1
GOSUB create
RETURN
randomscreen:
RANDOMIZE TIMER
randoms = INT(RND(1) * 7) + 1
IF randoms = 1 THEN
    g = color1background
    t = color1foreground
ELSEIF randoms = 2 THEN
    g = color2background
    t = color2foreground
ELSEIF randoms = 3 THEN
    g = color3background
    t = color3foreground
ELSEIF randoms = 4 THEN
    g = color4background
    t = color4foreground
ELSEIF randoms = 5 THEN
    g = color5background
    t = color5foreground
ELSEIF randoms = 6 THEN
    g = color6background
    t = color6foreground
ELSEIF randoms = 7 THEN
    g = color7background
    t = color7foreground
ELSEIF randoms = 0 THEN
    g = color8background
    t = color8foreground
END IF
RETURN
create:
CLS
PRINT
PRINT "Do you wish to save a configuration file for later on [Y] or [N] : "
answer$ = UCASE$(answer$)
IF answer$ = "N" THEN GOTO inputscreen
PRINT
PRINT "Saving data............"
PRINT
OPEN filename$ FOR OUTPUT AS #4
PRINT #4, usercolorentry_g1$
PRINT #4, usercolorentry_t1$
PRINT #4, usercolorentry_g2$
PRINT #4, usercolorentry_t2$
PRINT #4, color1background
PRINT #4, color1foreground
PRINT #4, color2background
PRINT #4, color2foreground
PRINT #4, color3background
PRINT #4, color3foreground
PRINT #4, color4background
PRINT #4, color4foreground
PRINT #4, color5background
PRINT #4, color5foreground
PRINT #4, color6background
PRINT #4, color6foreground
PRINT #4, color7background
PRINT #4, color7foreground
PRINT #4, color8background
PRINT #4, color8foreground
CLOSE #4
RETURN
load:
CLS
PRINT
PRINT "Do you wish to load a configuration file for later on [Y] or [N] : "
answer$ = UCASE$(answer$)
IF answer$ = "N" THEN GOTO inputscreen1
PRINT
PRINT "Saving data............"
PRINT
OPEN filename$ FOR INPUT AS #4
INPUT #4, usercolorentry_g1$
INPUT #4, usercolorentry_t1$
INPUT #4, usercolorentry_g2$
INPUT #4, usercolorentry_t2$
INPUT #4, color1background
INPUT #4, color1foreground
INPUT #4, color2background
INPUT #4, color2foreground
INPUT #4, color3background
INPUT #4, color3foreground
INPUT #4, color4background
INPUT #4, color4foreground
INPUT #4, color5background
INPUT #4, color5foreground
INPUT #4, color6background
INPUT #4, color6foreground
INPUT #4, color7background
INPUT #4, color7foreground
INPUT #4, color8background
INPUT #4, color8foreground
CLOSE #4
RETURN




