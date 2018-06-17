       IDENTIFICATION DIVISION.
       PROGRAM-ID. ASSIGN5.
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
          05 INLINE           PIC X(80).

       FD OUT-FILE.
       01 OUT-REC.
          05 FILLER              PIC X(80).

       WORKING-STORAGE SECTION.

       01 TITLE-HDR.
          05 FILLER         PIC X(20).
          05 FILLER         PIC X(19) VALUE "REGISTRATION REPORT".
          05 FILLER         PIC X(2).
          05 MONTH-HDR      PIC X(2).
          05 FILLER         PIC X      VALUE "/".
          05 DAY-HDR        PIC 9(2).
          05 FILLER         PIC X      VALUE "/".
          05 YEAR-HDR       PIC 9(4).
          05 FILLER         PIC X(1).
          05 FILLER         PIC X(5)   VALUE "PAGE ".
          05 PAGE-HDR       PIC 9(2).

       01 COLLUMN-HDR.
          05 FILLER         PIC X(2).
          05 VCLASS-HDR     PIC X(13) VALUE "VEHICLE CLASS".
          05 FILLER         PIC X(2).
          05 NAME-HDR       PIC X(4)  VALUE "NAME".
          05 FILLER         PIC X(19).
          05 WEIGHT-HDR     PIC X(6)  VALUE "WEIGHT".
          05 FILLER         PIC X(5).
          05 FEE            PIC X(3)  VALUE "FEE".

       01 DATE-WK.
          05 YEAR-WK         PIC XXXX.
          05 MONTH-WK        PIC XX.
          05 DAY-WK          PIC XX.

       01 FORMATING-WK.
          05 PAGENUM-WK          PIC 9(2).
          05 LINE-CNT            PIC 9(2).
          05 INPUT-SW            PIC 9.
          05 HOWMANY             PIC 9(2) VALUE ZEROS.
          05 SUB                 PIC 99 VALUE 01.
          05 BAD-CNT             PIC 9 VALUE 0.
          05 RATE-WS             PIC 9V99.

       01 TABLE-DB.
          05 TABLE-INFO       OCCURS 15 TIMES INDEXED BY X1.
               10 VCLASS-TABLE      PIC XX.
               10 REGRATE           PIC 9V99.

       01 TRANSACTION-INFO.
               10 VCLASS            PIC XX.
               10 OWNER             PIC X(20).
               10 WOV               PIC 9(5).

       01 TRANSACTION-OUT.
          05 FILLER                 PIC X(7) VALUE SPACES.
          05 VCLASS-OUT             PIC XX.
          05 FILLER                 PIC X(7) VALUE SPACES.
          05 NAME-OUT               PIC X(20).
          05 FILLER                 PIC X(5) VALUE SPACES.
          05 WEIGHT-OUT             PIC 9(5).
          05 FILLER                 PIC X(5) VALUE SPACES.
          05 REGFEE                 PIC $$$$99.99.

       01 ATMR          PIC XXX VALUE "YES".

       PROCEDURE DIVISION.

       MOVE 0 TO PAGENUM-WK, LINE-CNT.
       MOVE 59 TO LINE-CNT.

       100-MAIN-MODULE.
           OPEN INPUT IN-FILE
                OUTPUT OUT-FILE.
           PERFORM 105-MAKE-HEAD.
           PERFORM 200-MOVER-JOB.
           CLOSE IN-FILE, OUT-FILE.
           STOP RUN.

       200-MOVER-JOB.
           IF LINE-CNT > 48 PERFORM 105-MAKE-HEAD.
           PERFORM TABLE-JOB VARYING SUB FROM 1 BY 1
                       UNTIL SUB > 15.
           PERFORM TRANSACTION-JOB UNTIL ATMR = "NO".
           ADD 1 TO LINE-CNT.

       105-MAKE-HEAD.
           PERFORM 110-DAY-JOB.
           WRITE OUT-REC FROM TITLE-HDR AFTER ADVANCING 6 LINES.
           WRITE OUT-REC FROM COLLUMN-HDR AFTER ADVANCING 2 LINES.
           MOVE 9 TO LINE-CNT.

       110-DAY-JOB.
           MOVE FUNCTION CURRENT-DATE TO DATE-WK.
           MOVE YEAR-WK TO YEAR-HDR.
           MOVE MONTH-WK TO MONTH-HDR.
           MOVE DAY-WK TO DAY-HDR.
           ADD 1 TO PAGENUM-WK.
           MOVE PAGENUM-WK TO PAGE-HDR.

       TABLE-JOB.
           READ IN-FILE INTO TABLE-INFO (SUB)
               AT END MOVE "NO" TO ATMR.

       TRANSACTION-JOB.
           MOVE 1 TO SUB.
           MOVE SPACES TO TRANSACTION-OUT.
           MOVE 0 TO BAD-CNT.
           READ IN-FILE INTO TRANSACTION-INFO AT END MOVE "NO" TO ATMR.
           PERFORM REGFEE-JOB.
           INSPECT OWNER TALLYING BAD-CNT FOR ALL "BAD".
           MOVE VCLASS TO VCLASS-OUT.
           MOVE OWNER TO NAME-OUT.
           MOVE WOV TO WEIGHT-OUT.
           IF BAD-CNT = 0
           WRITE OUT-REC FROM TRANSACTION-OUT AFTER ADVANCING 1 LINES.
           ADD 1 TO SUB.

       REGFEE-JOB.
           SET X1 TO 1.
           SEARCH TABLE-INFO VARYING X1
           AT END DISPLAY "VEHICLE CLASS "VCLASS" WAS NOT FOUND"
           WHEN VCLASS-TABLE (X1) = VCLASS
                COMPUTE REGFEE = WOV * REGRATE (X1).

