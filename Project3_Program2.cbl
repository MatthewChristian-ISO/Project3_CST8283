      ******************************************************************
      * Author: Josiah Basilio & Matthew Christian
      * Date: April 17, 2025
      * Purpose: Project 3
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Project3_Program2.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STOCK-FILE-IN ASSIGN TO "../STOCKS.TXT"
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT PORTFOLIO-FILE-IN ASSIGN TO "../PORTFOLIO.DAT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS PORTFOLIO-SYM.

       DATA DIVISION.
       FILE SECTION.
       FD STOCK-FILE-IN.
       01 STOCK-RECORD.
           05 STOCK-SYM PIC X(7).
           05 STOCK-NAME PIC X(25).
           05 CLOSING-PRICE PIC 9(4)V99.

       FD PORTFOLIO-FILE-IN.
       01 PORTFOLIO-RECORD.
           05 PORTFOLIO-SYM PIC X(7).
           05 NUM-SHARES PIC 9(5).
           05 AVG-COST PIC 9(4)V99.

       WORKING-STORAGE SECTION.
      *Delcaring variables
       01 CONTROL-FIELDS.
           05 FOUND-FLAG PIC A.
           05 PORT-FOUND-FLAG PIC A.
           05 EOF-FLAG PIC A.
           05 SUB-1 PIC 9(2).
           05 STOCK-SYM-WS PIC X(7).
           05 STOCK-NAME-WS PIC X(25).
           05 CLOSING-PRICE-WS PIC 9(4)V99.
           05 NUM-SHARES-PURCHASE-WS PIC 9(3).
           05 STOCK-TABLE OCCURS 20 TIMES.
               10 STOCK-SYM-TBL PIC X(7).
               10 STOCK-NAME-TBL PIC X(25).
               10 CLOSING-PRICE-TBL PIC 9(4)V99.
           05 PORTFOLIO-REC-WS.
               10 PORTFOLIO-SYM-WS PIC X(7).
               10 NUM-SHARES-WS PIC 9(5).
               10 AVG-COST-WS PIC 9(4)V99.

       SCREEN SECTION.
       01 STOCK-BUY-SCREEN.
           05 VALUE "BUY STOCK" LINE 1 COL 35 BLANK SCREEN.
           05 VALUE "STOCK SYMBOL" LINE 3 COL 10.
           05 STOCK-SYM-IN LINE 3 COL 25
               PIC X(7) TO STOCK-SYM-WS.

       01 STOCK-CONFIRM-SCREEN.
           05 VALUE "CONFIRM STOCK" LINE 1 COL 35 BLANK SCREEN.
           05 VALUE "STOCK SYMBOL" LINE 3 COL 10.
           05 STOCK-SYM-IN LINE 3 COL 25
               PIC X(7) FROM STOCK-SYM-WS.
           05 VALUE "STOCK NAME" LINE 5 COL 10.
           05 STOCK-NAME-IN LINE 5 COL 25
               PIC X(25) FROM STOCK-NAME-WS.
           05 VALUE "STOCK PRICE" LINE 7 COL 10.
           05 STOCK-PRICE-IN LINE 7 COL 25
               PIC 9(4)V99 FROM CLOSING-PRICE-WS.
           05 VALUE "# SHARES" LINE 9 COL 10.
           05 STOCK-NUM-IN LINE 9 COL 25
               PIC 9(4)V99 TO NUM-SHARES-PURCHASE-WS.


       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM 201-INITALIZE.
           PERFORM 202-SEARCH-STOCK.
           PERFORM 203-BUY-STOCK.
           PERFORM 205-TERMINATE.
            STOP RUN.


       201-INITALIZE.
           PERFORM 301-OPEN-FILES.
           PERFORM 302-LOAD-STOCK-TABLE VARYING SUB-1 FROM 1 BY 1
               UNTIL SUB-1 > 20 OR EOF-FLAG = "Y".


       202-SEARCH-STOCK.
           ACCEPT STOCK-BUY-SCREEN.
           PERFORM 305-SEARCH-STOCK-TABLE VARYING SUB-1 FROM 1 BY 1
               UNTIL SUB-1 > 20 OR FOUND-FLAG = "Y".
           IF FOUND-FLAG = "Y"
               ACCEPT STOCK-CONFIRM-SCREEN
           ELSE
               DISPLAY "STOCK NOT FOUND"
           END-IF.


       203-BUY-STOCK.
           PERFORM 303-SEARCH-PORTFOLIO.
           IF PORT-FOUND-FLAG = "Y"
               PERFORM 304-UPDATE-PORTFOLIO
           ELSE
               PERFORM 306-ADD-TO-PORTFOLIO
           END-IF.

       205-TERMINATE.
           CLOSE PORTFOLIO-FILE-IN STOCK-FILE-IN.


      *Open files
       301-OPEN-FILES.
           OPEN INPUT STOCK-FILE-IN
           OPEN I-O PORTFOLIO-FILE-IN.

      *Load table
       302-LOAD-STOCK-TABLE.
           READ STOCK-FILE-IN AT END MOVE "Y" TO EOF-FLAG
               NOT AT END
                   MOVE STOCK-RECORD TO STOCK-TABLE(SUB-1).

      *Write header
       303-SEARCH-PORTFOLIO.
           MOVE STOCK-SYM-WS TO PORTFOLIO-SYM.
           READ PORTFOLIO-FILE-IN
               INVALID KEY
                   MOVE "N" TO PORT-FOUND-FLAG
               NOT INVALID KEY
                   MOVE "Y" TO PORT-FOUND-FLAG.
                   MOVE PORTFOLIO-RECORD TO PORTFOLIO-REC-WS.



      *Read records for portfolio
       304-UPDATE-PORTFOLIO.
           COMPUTE AVG-COST = (NUM-SHARES-WS*AVG-COST-WS +
               NUM-SHARES-PURCHASE-WS*CLOSING-PRICE-WS) /
                   (NUM-SHARES-WS + NUM-SHARES-PURCHASE-WS).
           ADD NUM-SHARES-PURCHASE-WS TO NUM-SHARES.
           MOVE STOCK-SYM-WS TO PORTFOLIO-SYM.
           REWRITE PORTFOLIO-RECORD
               INVALID KEY
                   DISPLAY "INVALID KEY PURCHASE INCOMPLETE"
               NOT INVALID KEY
                   DISPLAY "STOCK PURCHASE COMPLETE".

      *Search stock table
       305-SEARCH-STOCK-TABLE.
           IF STOCK-SYM-TBL(SUB-1) = STOCK-SYM-WS
               MOVE "Y" TO FOUND-FLAG
               MOVE STOCK-NAME-TBL(SUB-1) TO STOCK-NAME-WS
               MOVE CLOSING-PRICE-TBL(SUB-1) TO CLOSING-PRICE-WS.

      *Calculate value
       306-ADD-TO-PORTFOLIO.
           MOVE STOCK-SYM-WS TO PORTFOLIO-SYM.
           MOVE NUM-SHARES-PURCHASE-WS TO NUM-SHARES.
           MOVE CLOSING-PRICE-WS TO AVG-COST.
           WRITE PORTFOLIO-RECORD
               INVALID KEY
                   DISPLAY "INVALID KEY PURCHASE INCOMPLETE"
               NOT INVALID KEY
                   DISPLAY "STOCK PURCHASE COMPLETE".

      *Ends the program
       END PROGRAM Project3_Program2.
