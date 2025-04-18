      ******************************************************************
      * Author(s): Josiah Bailio, Matthew Christian
      * Date: April 17, 2025
      * Purpose: Creates an indexed sequential file from a line
      * sequential file of Investment records.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROJECT3_PROGRAM1.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *>   Assign a name to a known file to be read from the system.
           SELECT PORTFOLIO-FILE-IN ASSIGN TO "../PORTFOLIO.TXT"
      *>       Set the file's organization to line sequential.
               ORGANIZATION IS LINE SEQUENTIAL.

      *>   Assign a name to a file to be written to the system.
           SELECT PORTFOLIO-FILE-OUT ASSIGN TO "../PORTFOLIO.DAT"
      *>       Set the file's organization to indexed.
               ORGANIZATION IS INDEXED
      *>       Set the file's access mode to sequential.
               ACCESS MODE IS SEQUENTIAL
      *>       Set the file's record/primary key to a defined record
      *>       field.
               RECORD KEY IS PORTFOLIO-STOCK-SYM-OUT.

       DATA DIVISION.
       FILE SECTION.
      *>  Line Sequential File which stores portfolio records.
       FD PORTFOLIO-FILE-IN.
      *>  Portfolio record for the Line Sequential File.
       01 PORTFOLIO-RECORD-IN.
      *>   Stock symbol.
           05 PORTFOLIO-STOCK-SYM-IN PIC X(7).
      *>   Number of shares of this stock.
           05 NUM-SHARES-IN PIC 9(5).
      *>   Average cost per share of this stock.
           05 AVG-COST-PER-SHARE-IN PIC 9(4)V99.

      *>  Indexed Sequential File to store portfolio records.
       FD PORTFOLIO-FILE-OUT.
      *>  Portfolio record for the Indexed Sequential File.
       01 PORTFOLIO-RECORD-OUT.
      *>   Stock symbol.
           05 PORTFOLIO-STOCK-SYM-OUT PIC X(7).
      *>   Number of shares of this stock.
           05 NUM-SHARES-OUT PIC 9(5).
      *>   Average cost per share of this stock.
           05 AVG-COST-PER-SHARE-OUT PIC 9(4)V99.

       WORKING-STORAGE SECTION.
       01 CONTROL-FIELDS.
      *>   Character to indicate the end of a file.
           05 EOF-FLAG PIC A VALUE "N".
               88 NEOF VALUE "N".
               88 EOF VALUE "Y".
      *>      05 EOF-FLAG PIC A.

      *>   Number of records that have been read from a file.
           05 READ-CTR PIC 9(2).
      *>   Number of records that have been written to a file.
           05 WRITE-CTR PIC 9(2).

       PROCEDURE DIVISION.
       100-MAIN-PROCEDURE.
           PERFORM 201-INITIALIZE.
           PERFORM 202-WRITE-ONE-PORTFOLIO-RECORD UNTIL EOF.
           PERFORM 203-TERMINATE.
           STOP RUN.

       201-INITIALIZE.
           PERFORM 301-OPEN-FILES.
           PERFORM 302-READ-PORTFOLIO-RECORD.

       202-WRITE-ONE-PORTFOLIO-RECORD.
           PERFORM 303-WRITE-PORTFOLIO-RECORD.
           PERFORM 302-READ-PORTFOLIO-RECORD.

       203-TERMINATE.
           PERFORM 304-CLOSE-FILES.
           PERFORM 305-DISPLAY-COUNTERS.

       301-OPEN-FILES.
      *>   Open the Line Sequential File for input and the Indexed
      *>   Sequential File for output.
           OPEN INPUT PORTFOLIO-FILE-IN
                OUTPUT PORTFOLIO-FILE-OUT.

       302-READ-PORTFOLIO-RECORD.
      *>   Read one record of the Line Sequential File.
           READ PORTFOLIO-FILE-IN
      *>       If we're at the end of the file:
               AT END
      *>           Set the END OF FILE flag to Yes.
                   MOVE "Y" TO EOF-FLAG
      *>       If we're not at the end of the file:
               NOT AT END
      *>           Move the read record of the Line Sequential File
      *>           to the record for the Indexed Sequential File.
                   MOVE PORTFOLIO-RECORD-IN TO PORTFOLIO-RECORD-OUT
      *>           Increment the counter for records read.
                   ADD 1 TO READ-CTR
      *>   End the read operation.
           END-READ.

       303-WRITE-PORTFOLIO-RECORD.
      *>   Write the moved record for the Indexed Sequential File to the
      *>   Indexed Sequential File.
           WRITE PORTFOLIO-RECORD-OUT
      *>       If the record/primary key for this record is invalid:
               INVALID KEY
      *>           Output an error message indicating that the key is
      *>           invalid and the record was not written to the file.
                   DISPLAY "ERROR! INVALID KEY - RECORD NOT WRITTEN ",
                           "TO FILE."
      *>       If the record/primary key for this record is valid:
               NOT INVALID KEY
      *>           Ouput a message indicating that the record was
      *>           written to the file.
                   DISPLAY "RECORD WRITTEN TO FILE."
      *>           Increment the counter for records written.
                   ADD 1 TO WRITE-CTR
      *>   End the write operation.
           END-WRITE.

       304-CLOSE-FILES.
      *>   Close the Line Sequnetial and Indexed Sequential Files which
      *>   store portfolio records.
           CLOSE PORTFOLIO-FILE-IN PORTFOLIO-FILE-OUT.

       305-DISPLAY-COUNTERS.
      *>   Output the number of records that were read from the Line
      *>   Sequential File.
           DISPLAY "RECORDS READ: ", READ-CTR.
      *>   Output the number of records that were written to the
      *>   Indexed Sequential File.
           DISPLAY "RECORDS WRITTEN: ", WRITE-CTR.

      *>  End of the program.
       END PROGRAM PROJECT3_PROGRAM1.
