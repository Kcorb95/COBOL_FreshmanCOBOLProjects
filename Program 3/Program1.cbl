       ID DIVISION.
       PROGRAM-ID. TEST.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 MYCOUNT                       PIC 99.
       PROCEDURE DIVISION.
           MOVE 0 TO MYCOUNT.
           PERFORM D-PAR UNTIL MYCOUNT > 15.
           STOP RUN.
       D-PAR.
           DISPLAY MYCOUNT.
           COMPUTE MYCOUNT = MYCOUNT + 2.



