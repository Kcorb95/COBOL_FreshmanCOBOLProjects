       IDENTIFICATION DIVISION.
       PROGRAM-ID. ASSIGN4.
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
          05 TNUM-IN                        PIC X(2).
          05 ANUM-IN                        PIC X(2).
          05 DNUM-IN                        PIC X(2).
          05 FILLER                         PIC X(43).

       FD OUT-FILE.
       01 OUT-REC.
          05 FILLER              PIC X(70).

       WORKING-STORAGE SECTION.

       01 TITLE-HDR.
          05 FILLER              PIC X(24).
          05 FILLER              PIC X(17) VALUE "-EMPLOYEE REPORT-".

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

       01 FORMATING-WK.
          05 PAGENUM-WK          PIC 9(2).
          05 LINE-CNT            PIC 9(2).
          05 TOTAL-CNT           PIC 9(2).
          05 BAD-CNT             PIC 9(2).

       01 TERR-WK.
          05 FILLER                 PIC X(10).
          05 FILLER                 PIC X(10) VALUE "TERRITORY".
          05 FILLER                 PIC X(3).
          05 T-CNT            PIC X(2).
          05 FILLER                 PIC X(3).
          05 REASONT-WK             PIC X(13).

       01 AREA-WK.
          05 FILLER                 PIC X(10).
          05 FILLER                 PIC X(10) VALUE "AREA".
          05 FILLER                 PIC X(3).
          05 A-CNT            PIC X(2).
          05 FILLER                 PIC X(3).
          05 REASONA-WK             PIC X(13).


       01 DEP-WK.
          05 FILLER                 PIC X(10).
          05 FILLER                 PIC X(10) VALUE "DEPARTMENT".
          05 FILLER                 PIC X(3).
          05 D-CNT             PIC X(2).
          05 FILLER                 PIC X(3).
          05 REASOND-WK             PIC X(13).


       01 FTR-WK.
          05 FILLER         PIC X(15).
          05 FILLER         PIC X(14) VALUE "TOTAL RECORDS: ".
          05 TOTAL          PIC 9(2).

       01 GFTR-WK.
          05 FILLER         PIC X(15).
          05 FILLER         PIC X(22) VALUE "TOTAL CORRECT FEILDS: ".
          05 TOTAL-GOOD     PIC 9(3).

       01 BFTR-WK.
          05 FILLER         PIC X(15).
          05 FILLER         PIC X(21) VALUE "TOTAL BROKEN FEILDS: ".
          05 TOTAL-BAD      PIC 9(2).




       01 ATMR          PIC XXX VALUE "YES".

       PROCEDURE DIVISION.

       MOVE 0 TO PAGENUM-WK, LINE-CNT, TOTAL-CNT, A-CNT, BAD-CNT,
       T-CNT D-CNT, TOTAL, TOTAL-BAD, TOTAL-GOOD.
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
           WRITE OUT-REC FROM TITLE-HDR AFTER ADVANCING 6 LINES.
           WRITE OUT-REC FROM DATE-HDR AFTER ADVANCING 1 LINES.
           MOVE 2 TO LINE-CNT.


       110-DAY-JOB.
           MOVE FUNCTION CURRENT-DATE TO DATE-WK.
           MOVE YEAR-WK TO YEAR-HDR.
           MOVE MONTH-WK TO MONTH-HDR.
           MOVE DAY-WK TO DAY-HDR.
           ADD 1 TO PAGENUM-WK.
           MOVE PAGENUM-WK TO PAGE-HDR.

       200-MOVER-JOB.
           IF LINE-CNT > 48 PERFORM 105-MAKE-HEAD.

           MOVE TNUM-IN TO T-CNT.
           MOVE ANUM-IN TO A-CNT.
           MOVE DNUM-IN TO D-CNT.

          IF T-CNT IS NOT NUMERIC
                               MOVE "NOT NUMERIC" TO REASONT-WK,
                WRITE OUT-REC FROM TERR-WK AFTER ADVANCING 2 LINES,
                                                    ADD 01 TO BAD-CNT
            ELSE IF T-CNT > 03
                               MOVE "OUT OF BOUNDS" TO REASONT-WK,
                WRITE OUT-REC FROM TERR-WK AFTER ADVANCING 2 LINES,
                                                    ADD 01 TO BAD-CNT.


          IF A-CNT IS NOT NUMERIC
                               MOVE "NOT NUMERIC" TO REASONA-WK,
                WRITE OUT-REC FROM AREA-WK AFTER ADVANCING 2 LINES,
                                                    ADD 01 TO BAD-CNT
            ELSE IF A-CNT > 03
                               MOVE "OUT OF BOUNDS" TO REASONA-WK,
                WRITE OUT-REC FROM AREA-WK AFTER ADVANCING 2 LINES,
                                                    ADD 01 TO BAD-CNT.


          IF D-CNT IS NOT NUMERIC
                               MOVE "NOT NUMERIC" TO REASOND-WK,
                WRITE OUT-REC FROM DEP-WK AFTER ADVANCING 2 LINES,
                                                    ADD 01 TO BAD-CNT
            ELSE IF D-CNT > 10
                               MOVE "OUT OF BOUNDS" TO REASOND-WK,
                WRITE OUT-REC FROM DEP-WK AFTER ADVANCING 2 LINES,
                                                    ADD 01 TO BAD-CNT.


           ADD 1 TO TOTAL-CNT.
           READ IN-FILE
               AT END MOVE "NO" TO ATMR.

       300-DONE-JOB.
       COMPUTE TOTAL-GOOD = TOTAL-CNT * 3 - BAD-CNT.
       MOVE TOTAL-CNT TO TOTAL.
       MOVE BAD-CNT TO TOTAL-BAD.

       WRITE OUT-REC FROM FTR-WK AFTER ADVANCING 3 LINES.
       WRITE OUT-REC FROM GFTR-WK AFTER ADVANCING 1 LINES.
       WRITE OUT-REC FROM BFTR-WK AFTER ADVANCING 1 LINES.
