       IDENTIFICATION DIVISION.

       PROGRAM-ID.    FNLMAIN1.
       AUTHOR.        YUNUS EMRE AKTAS.

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OUT-FILE   ASSIGN OUTFILE
                             STATUS OUT-ST.
           SELECT INP-FILE   ASSIGN INPFILE
                             STATUS INP-ST.
       DATA DIVISION.
       FILE SECTION.
       FD  OUT-FILE RECORDING MODE F.
       01  OUT-REC.
           03 OUT-FINAL      PIC X(140).

       FD  INP-FILE RECORDING MODE F.
       01  INP-REC.
           05 INP-OPRT       PIC X(01).
           05 INP-UID        PIC 9(05).

       WORKING-STORAGE SECTION.
       01  WS-WORK-AREA.
           05 WS-SUBPROG            PIC X(8) VALUE 'FINALSUB'.
           05 OUT-ST                PIC 9(2).
              88 OUT-SUCCESS               VALUE 00.
           05 INP-ST                PIC 9(2).
              88 INP-EOF                   VALUE 10.
              88 INP-SUCCESS               VALUE 00.
           05 WS-OPT-TYPE           PIC 9(1).
              88 OPT-VALID                 VALUE 1 THRU 4.
           05 WS-SUB-AREA.
              07 WS-SUB-FUNC        PIC 9(1).
                 88 WS-READ            VALUE 1.
                 88 WS-WRITE           VALUE 2.
                 88 WS-DELETE          VALUE 3.
                 88 WS-UPDATE          VALUE 4.
              07 WS-SUB-UID         PIC 9(5).
              07 WS-SUB-RC          PIC 9(2).
              07 WS-COMMENT-1       PIC X(09).
              07 WS-COMMENT-2       PIC X(95).
              07 WS-FNAME-FROM      PIC X(15).
              07 WS-FNAME-TO        PIC X(15).
              07 WS-LNAME-FROM      PIC X(15).
              07 WS-LNAME-TO        PIC X(15).
           05 WS-SUB-DATA           PIC X(140).
       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM H100-OPEN-FILES
           PERFORM H200-PROCESS UNTIL INP-EOF
           PERFORM H999-PROGRAM-EXIT.

       H100-OPEN-FILES.
           OPEN INPUT INP-FILE
           IF NOT INP-SUCCESS
               DISPLAY 'INPUT DOES NOT OPENED'
               PERFORM H999-PROGRAM-EXIT
           END-IF.
           OPEN OUTPUT OUT-FILE.
           IF NOT OUT-SUCCESS
               DISPLAY 'OUTFILE DOES NOT OPENED'
               PERFORM H999-PROGRAM-EXIT
           END-IF.
           READ INP-FILE.
           IF NOT INP-SUCCESS
                DISPLAY 'INPUT DOES NOT OPENED'
                PERFORM H999-PROGRAM-EXIT
           END-IF.
       H100-END. EXIT.

       H200-PROCESS.
           MOVE SPACES TO WS-COMMENT-1
           MOVE SPACES TO WS-COMMENT-2
           MOVE SPACES TO WS-FNAME-FROM
           MOVE SPACES TO WS-FNAME-TO
           MOVE SPACES TO WS-LNAME-FROM
           MOVE SPACES TO WS-LNAME-TO
           EVALUATE INP-OPRT
                WHEN 'R'
                     SET WS-READ TO TRUE
                WHEN 'W'
                     SET WS-WRITE TO TRUE
                WHEN 'D'
                     SET WS-DELETE TO TRUE
                WHEN 'U'
                     SET WS-UPDATE TO TRUE
                WHEN OTHER
                     MOVE 0 TO WS-SUB-FUNC
           END-EVALUATE.
           MOVE WS-SUB-FUNC TO WS-OPT-TYPE.
           IF OPT-VALID
                MOVE INP-UID TO WS-SUB-UID
                MOVE SPACES  TO WS-SUB-DATA
                MOVE ZEROS   TO WS-SUB-RC
                CALL WS-SUBPROG USING WS-SUB-AREA
                PERFORM H300-WRITE-OUTFILE
                MOVE WS-SUB-DATA TO OUT-FINAL
                WRITE OUT-REC
           END-IF.   
           MOVE SPACES TO WS-SUB-DATA
           READ INP-FILE.
       H200-END. EXIT.

       H300-WRITE-OUTFILE.
           STRING WS-SUB-UID DELIMITED BY SIZE
                  WS-COMMENT-1 DELIMITED BY SIZE
                  WS-SUB-RC DELIMITED BY SIZE
                  '-' DELIMITED BY SIZE
                  WS-COMMENT-2 DELIMITED BY SIZE
                  INTO WS-SUB-DATA.
       H300-END. EXIT.       

       H999-PROGRAM-EXIT.
           CLOSE INP-FILE.
           CLOSE OUT-FILE.
           DISPLAY 'PROGRAM IS TERMINATED'
           STOP RUN.
       H999-END. EXIT.
