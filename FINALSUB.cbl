       IDENTIFICATION DIVISION.
       PROGRAM-ID.  FINALSUB.
       AUTHOR. YUNUS EMRE AKTAS.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IDX-FILE ASSIGN TO IDXFILE
                           STATUS ST-IDXFILE
                           ORGANIZATION INDEXED
                           ACCESS RANDOM
                           RECORD KEY IDX-KEY.
       DATA DIVISION.
       FILE SECTION.
       FD IDX-FILE.
       01 IDX-REC.
           03 IDX-KEY.
                05 IDX-ID             PIC S9(5) COMP-3.
           03 IDX-EXC                 PIC S9(3) COMP.
           03 IDX-NAME                PIC X(15).
           03 IDX-SURNAME             PIC X(15).
           03 IDX-DATE                PIC S9(7) COMP-3.
           03 IDX-BALANCE             PIC S9(15) COMP-3.
       WORKING-STORAGE SECTION.
       01 WS-WORK-AREA.
           03 ST-IDXFILE              PIC 9(02).
              88 ST-IDXFILE-OK          VALUE 00 97.
              88 ST-IDXFILE-EOF         VALUE 10.
           03 WS-ID                   PIC S9(05) COMP-3.
           03 WS-INDEX-0              PIC S9(02).
           03 WS-INDEX-1              PIC S9(02).
       LINKAGE SECTION.
       01 WS-SUB-AREA.
              07 WS-SUB-FUNC          PIC 9(01).
                 88 WS-SUB-READ            VALUE 1.
                 88 WS-SUB-WRITE           VALUE 2.
                 88 WS-SUB-DELETE          VALUE 3.
                 88 WS-SUB-UPDATE          VALUE 4.
              07 WS-SUB-UID           PIC 9(05).
              07 WS-SUB-RC            PIC 9(02).
              07 WS-COMMENT-1         PIC X(09).
              07 WS-COMMENT-2         PIC X(95).
              07 WS-FNAME-FROM        PIC X(15).
              07 WS-FNAME-TO          PIC X(15).
              07 WS-LNAME-FROM        PIC X(15).
              07 WS-LNAME-TO          PIC X(15).              
       PROCEDURE DIVISION USING WS-SUB-AREA.
       0000-MAIN.
           PERFORM H100-OPEN-FILES
           PERFORM H200-PROCESS.
           PERFORM H999-PROGRAM-EXIT.
       H100-END. EXIT.

       H100-OPEN-FILES.
           OPEN I-O IDX-FILE.
           IF NOT ST-IDXFILE-OK
              DISPLAY "IDX-FILE OPEN ERROR: " ST-IDXFILE
              PERFORM H999-PROGRAM-EXIT
           END-IF.
           MOVE ST-IDXFILE TO WS-SUB-RC.
       H100-END. EXIT.

       H200-PROCESS.
           MOVE WS-SUB-UID TO IDX-ID
           READ IDX-FILE KEY IDX-KEY
           INVALID KEY
              IF WS-SUB-WRITE THEN PERFORM H220-WRITE
              ELSE
              PERFORM H800-INVALID-KEY
              END-IF
           NOT INVALID KEY
           IF WS-SUB-READ THEN PERFORM H210-READ
           END-IF
           IF WS-SUB-DELETE THEN PERFORM H230-DELETE
           END-IF
           IF WS-SUB-UPDATE THEN PERFORM H240-UPDATE
           END-IF
           IF WS-SUB-WRITE THEN PERFORM H800-INVALID-KEY
           END-IF
           END-READ.
       H200-END. EXIT.

       H210-READ.
           MOVE "-read-rc:" TO WS-COMMENT-1
           MOVE "KAYIT BULUNDU." TO WS-COMMENT-2.
       H210-END. EXIT.

       H220-WRITE.
           MOVE 948 TO  IDX-EXC.
           MOVE "YUNUS EMRE     " TO IDX-NAME.
           MOVE "AKTAS          " TO IDX-SURNAME.
           MOVE 19971001 TO IDX-DATE.
           MOVE ZEROS TO IDX-BALANCE.
           MOVE WS-SUB-UID TO IDX-ID.
           WRITE IDX-REC.
           MOVE "-writ-rc:" TO WS-COMMENT-1
           MOVE "KAYIT EKLENDI." TO WS-COMMENT-2.
       H220-END. EXIT.

       H230-DELETE.
           DELETE IDX-FILE.
           MOVE "-delt-rc:" TO WS-COMMENT-1
           MOVE "KAYIT SILINDI." TO WS-COMMENT-2.
       H230-END. EXIT.

       H240-UPDATE.
             MOVE SPACES TO WS-FNAME-TO.
             MOVE 1 TO WS-INDEX-1.
             MOVE 0 TO WS-INDEX-0.
             MOVE IDX-NAME TO WS-FNAME-FROM
             MOVE IDX-SURNAME TO WS-LNAME-FROM
             PERFORM VARYING WS-INDEX-0 FROM 1 BY 1
                UNTIL WS-INDEX-0 > LENGTH OF WS-FNAME-FROM
                IF WS-FNAME-FROM (WS-INDEX-0:1) = ' '
                   CONTINUE
                ELSE
                    MOVE WS-FNAME-FROM (WS-INDEX-0:1) TO
                                    WS-FNAME-TO (WS-INDEX-1:1)
                    ADD 1 TO WS-INDEX-1
                END-IF
           END-PERFORM.
           MOVE WS-LNAME-FROM TO WS-LNAME-TO.
           INSPECT WS-LNAME-TO REPLACING ALL 'E' BY 'I'.
           INSPECT WS-LNAME-TO REPLACING ALL 'A' BY 'E'.
           MOVE WS-LNAME-TO TO IDX-SURNAME. 
           MOVE WS-FNAME-TO TO IDX-NAME.
           REWRITE IDX-REC.
           MOVE "-updt-rc:" TO WS-COMMENT-1
              STRING "KAYIT GUNCELLENDI." DELIMITED BY SIZE
                     WS-FNAME-FROM DELIMITED BY SIZE
                     ">" DELIMITED BY SIZE
                     WS-FNAME-TO DELIMITED BY SIZE
                     "|" DELIMITED BY SIZE
                     WS-LNAME-FROM DELIMITED BY SIZE
                     ">" DELIMITED BY SIZE
                     WS-LNAME-TO DELIMITED BY SIZE
                     INTO WS-COMMENT-2.
       H240-END. EXIT.

       H800-INVALID-KEY.
           IF WS-SUB-READ
              MOVE "-read-rc:" TO WS-COMMENT-1
              MOVE "KAYIT BULUNAMADI." TO WS-COMMENT-2
           END-IF.
           IF WS-SUB-WRITE
              MOVE "-writ-rc:" TO WS-COMMENT-1
              MOVE "KAYIT EKLENEMEDI" TO WS-COMMENT-2
           END-IF.
           IF WS-SUB-DELETE
              MOVE "-delt-rc:" TO WS-COMMENT-1
              MOVE "KAYIT BULUNAMADI." TO WS-COMMENT-2
           END-IF.
           IF WS-SUB-UPDATE
              MOVE "-updt-rc:" TO WS-COMMENT-1
              MOVE "KAYIT BULUNAMADI." TO WS-COMMENT-2
           END-IF.
       H800-END. EXIT.

       H999-PROGRAM-EXIT.
           CLOSE IDX-FILE.
           GOBACK.
       H999-END. EXIT.
