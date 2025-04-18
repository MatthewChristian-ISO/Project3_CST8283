      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCPORTFOLIO.
       DATA DIVISION.
       LINKAGE SECTION.
       01 COST-BASE-CALC-LS PIC 9(5)V99.
       01 MARKET-VALUE-CALC-LS PIC 9(5)V99.
       01 GAIN-LOSS-CALC-LS PIC S9(6)V99.
       01 NUM-SHARES-LS PIC 9(5).
       01 AVG-COST-LS PIC 9(4)V99.
       01 CLOSING-PRICE-FROM-TBL-LS PIC 9(4)V99.
       PROCEDURE DIVISION USING COST-BASE-CALC-LS, MARKET-VALUE-CALC-LS,
           GAIN-LOSS-CALC-LS, NUM-SHARES-LS, AVG-COST-LS,
           CLOSING-PRICE-FROM-TBL-LS.
       MAIN-PROCEDURE.
           COMPUTE COST-BASE-CALC-LS = NUM-SHARES-LS * AVG-COST-LS.

           COMPUTE MARKET-VALUE-CALC-LS =
               NUM-SHARES-LS * CLOSING-PRICE-FROM-TBL-LS.

           COMPUTE GAIN-LOSS-CALC-LS =
               MARKET-VALUE-CALC-LS - COST-BASE-CALC-LS.
           EXIT PROGRAM.
       END PROGRAM CALCPORTFOLIO.
