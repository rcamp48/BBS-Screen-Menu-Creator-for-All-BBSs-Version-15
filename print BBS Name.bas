DIM acdc$(132)


fixed:
_FULLSCREEN
CLS
OPEN "c:\display\bbsmenuascii\test.txt" FOR OUTPUT AS #2
OPEN "c:\display\bbsmenuascii\main6.txt" FOR INPUT AS #1
INPUT "Enter Your BBS name : [Max 30 Chars ] ", bbs$
DO UNTIL EOF(1)
    LINE INPUT #1, acdc$
    FOR i = 1 TO LEN(acdc$)
        a$ = MID$(acdc$, i, 1)
        IF a$ = CHR$(36) THEN
            PRINT #2, bbs$;
        ELSE
            PRINT #2, a$;

        END IF
    NEXT i
    PRINT #2, ""
LOOP
CLOSE #1
CLOSE #2

