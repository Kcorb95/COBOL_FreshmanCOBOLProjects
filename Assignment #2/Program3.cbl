       IDENTIFICATION DIVISION.
       PROGRAM-ID. ASSIGN3.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IN-FILE
               ASSIGN     TO   "INFILE.TXT"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUT-FILE
               ASSIGN     TO   "OUTFILE.TXT"
               ORGANIZATION IS SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD IN-FILE.
       01 IN-REC.
          05 NAME-IN                        PIC X(20).
          05 WEIGHT-IN                      PIC 9(3).
          05 HEIGHT-IN                      PIC 9(2).
          05 EYES-IN                        PIC 9(1).
          05 HAIR-IN                        PIC 9(1).
          05 SEX-IN                         PIC X(1).

       FD OUT-FILE.
       01 OUT-REC.
          05 NAME-OUT                       PIC X(26).
          05 SEX-OUT                        PIC X(7).
          05 WEIGHT-OUT                     PIC X(9).
          05 HEIGHT-OUT                     PIC X(7).
          05 EYES-OUT                       PIC X(8).
          05 HAIR-OUT                       PIC X(11).

       WORKING-STORAGE SECTION.

       01 TITLE-HDR.
          05 FILLER              PIC X(24).
          05 FILLER              PIC X(14) VALUE "-Model Report-".

       01 DATE-WK.
          05 YEAR-WK         PIC XXXX.
          05 MONTH-WK        PIC XX.
          05 DAY-WK          PIC XX.

       01 DATE-HDR.
          05 FILLER              PIC X(6)   VALUE "DATE: ".
          05 MONTH-HDR           PIC X(2).
          05 FILLER              PIC X      VALUE "/".
          05 DAY-HDR             PIC 9(2).
          05 FILLER              PIC X      VALUE "/".
          05 YEAR-HDR            PIC 9(4).
          05 FILLER              PIC X(43).
          05 FILLER              PIC X(3)   VALUE "Pg ".
          05 PAGE-HDR            PIC 9(2).

       01 COLUMN-HDR.
          05 FILLER              PIC X.
          05 NAME-HDR            PIC X(4)  VALUE "NAME".
          05 FILLER              PIC X(20).
          05 SEX-HDR             PIC X(3)  VALUE "SEX".
          05 FILLER              PIC X(3).
          05 WEIGHT-HDR          PIC X(6)  VALUE "WEIGHT".
          05 FILLER              PIC X(3).
          05 HEIGHT-HDR          PIC X(6)  VALUE "HEIGHT".
          05 FILLER              PIC X(3).
          05 EYECOLOR-HDR        PIC X(4)  VALUE "EYES".
          05 FILLER              PIC X(3).
          05 HAIRCOLOR-HDR       PIC X(4)  VALUE "HAIR".

       01 MALEMOD-FTR.
          05 FILLER         PIC X(15).
          05 MALE-FTR       PIC X(23)  VALUE "NUMBER OF MALE MODELS: ".
          05 MALECNT-FTR    PIC 9(2).

       01 FEMOD-FTR.
          05 FILLER       PIC X(15).
          05 FE-FTR       PIC X(25)  VALUE "NUMBER OF FEMALE MODELS: ".
          05 FECNT-FTR    PIC 9(2).

       01 NUMMOD-FTR.
          05 FILLER          PIC X(15).
          05 TOTALMOD-FTR    PIC X(14) VALUE "TOTAL MODELS: ".
          05 NUMOMOD-FTR     PIC 9(2).

       01 NUMBLND-FTR.
          05 FILLER          PIC X(15).
          05 TOTAL-BLONDE    PIC X(22) VALUE "TOTAL BLONDE MODELS: ".
          05 BLNDCNT-FTR     PIC 9(2).

       01 NUMBRWN-FTR.
          05 FILLER          PIC X(15).
          05 TOTAL-BROWN     PIC X(26)
                               VALUE "TOTAL BROWN EYED MODELS: ".
          05 BRWNCNT-FTR     PIC 9(2).

       01 FORMATING-WK.
          05 PAGENUM-WK          PIC 9(2).
          05 LINE-CNT            PIC 9(2).
          05 NUM-OMOD            PIC 9(2).
          05 MALE-CNT            PIC 9(2).
          05 FE-CNT              PIC 9(2).
          05 EYES-WK             PIC X(5).
          05 HAIR-WK             PIC X(6).
          05 BROWN-CNT           PIC 9(2).
          05 BLONDE-CNT          PIC 9(2).

       01 FOOTER-SET.
          05 FILLER              PIC X(10).
          05 TOTAL-FTR           PIC X(8)     VALUE "TOTALS: ".


       01 ATMR          PIC XXX VALUE "YES".

       PROCEDURE DIVISION.

       MOVE 0 TO PAGENUM-WK, MALE-CNT, FE-CNT, NUM-OMOD, BROWN-CNT,
       BLONDE-CNT.
       MOVE 59 TO LINE-CNT.

       100-MAIN-MODULE.
           OPEN INPUT IN-FILE
                OUTPUT OUT-FILE.
           PERFORM 105-MAKE-HEAD
           READ IN-FILE
               AT END MOVE "NO" TO ATMR.
           PERFORM 200-MOVER-JOB
               UNTIL ATMR = "NO".
           PERFORM 300-DONE-JOB.
           CLOSE IN-FILE, OUT-FILE.
           STOP RUN.

       105-MAKE-HEAD.
           PERFORM 110-DAY-JOB.
           WRITE OUT-REC FROM TITLE-HDR AFTER ADVANCING 2 LINES.
           WRITE OUT-REC FROM DATE-HDR AFTER ADVANCING 1 LINES.
           WRITE OUT-REC FROM COLUMN-HDR AFTER ADVANCING 3 LINES.
           MOVE 4 TO LINE-CNT.


       110-DAY-JOB.
           MOVE FUNCTION CURRENT-DATE TO DATE-WK.
           MOVE YEAR-WK TO YEAR-HDR.
           MOVE MONTH-WK TO MONTH-HDR.
           MOVE DAY-WK TO DAY-HDR.
           ADD 1 TO PAGENUM-WK.
           MOVE PAGENUM-WK TO PAGE-HDR.

       200-MOVER-JOB.
           IF LINE-CNT > 48 PERFORM 105-MAKE-HEAD.
           MOVE NAME-IN TO NAME-OUT.
           MOVE SEX-IN TO SEX-OUT.
           MOVE HEIGHT-IN TO HEIGHT-OUT.
           MOVE WEIGHT-IN TO WEIGHT-OUT.
           MOVE EYES-IN TO EYES-WK.
           PERFORM 210-EYE-JOB.
           MOVE EYES-WK TO EYES-OUT.
           MOVE HAIR-IN TO HAIR-WK
           PERFORM 215-HAIR-JOB.
           MOVE HAIR-WK TO HAIR-OUT.
           ADD 2 TO LINE-CNT.
           ADD 1 TO NUM-OMOD.
           IF SEX-OUT = "M" ADD 1 TO MALE-CNT ELSE ADD 1 TO FE-CNT.
           IF EYES-OUT = "BROWN" ADD 1 TO BROWN-CNT.
           IF HAIR-OUT = "BLONDE" ADD 1 TO BLONDE-CNT.
           WRITE OUT-REC AFTER ADVANCING 2 LINES.
           READ IN-FILE
               AT END MOVE "NO" TO ATMR.

       210-EYE-JOB.
           IF EYES-WK > 1
               IF EYES-WK = 2 MOVE "BROWN" TO EYES-WK
                                  ELSE MOVE "OTHER" TO EYES-WK
                                           ELSE MOVE "BLUE" TO EYES-WK.

       215-HAIR-JOB.
           IF HAIR-WK > 1
                IF HAIR-WK = 2 MOVE "BROWN" TO HAIR-WK
                                   ELSE MOVE "OTHER" TO HAIR-WK
                                        ELSE MOVE "BLONDE" TO HAIR-WK.
       300-DONE-JOB.
           MOVE MALE-CNT TO MALECNT-FTR.
           MOVE FE-CNT TO FECNT-FTR.
           MOVE NUM-OMOD TO NUMOMOD-FTR.
           MOVE BROWN-CNT TO BRWNCNT-FTR.
           MOVE BLONDE-CNT TO BLNDCNT-FTR.
           WRITE OUT-REC FROM FOOTER-SET AFTER ADVANCING 5 LINES.
           WRITE OUT-REC FROM NUMMOD-FTR AFTER ADVANCING 2 LINES.
           WRITE OUT-REC FROM MALEMOD-FTR AFTER ADVANCING 2 LINES.
           WRITE OUT-REC FROM FEMOD-FTR AFTER ADVANCING 2 LINES.
           WRITE OUT-REC FROM NUMBLND-FTR AFTER ADVANCING 2 LINES.
           WRITE OUT-REC FROM NUMBRWN-FTR AFTER ADVANCING 2 LINES.
