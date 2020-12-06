      *==============================================================
      *COURSE:CST8283 BUSINESS PROGRAMMING
      *PROFESSOR:MEL SANSCHAGRIN
      *GROUP MEMBERS: WEI YU
      *DESCRIPTION:  
      *==============================================================*  
       IDENTIFICATION DIVISION. 
       PROGRAM-ID. UPDATE-STUD-FILE.
       AUTHOR. WEI YU.
       DATE-WRITTEN. 02-DEC-2020.
       DATE-COMPILED. 02-DEC-2020.

    
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STUDENT-FILE
               ASSIGN TO "D:\Cobol\STUFILE_IN.txt"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS STUDENT-NUMBER
               FILE STATUS IS STATUS-FIELD.


       DATA DIVISION.
       FILE SECTION.
       FD STUDENT-FILE.
       01 STUDENT-RECORD.
           05  STUDENT-NUMBER      PIC 9(6).
           05  TUITION-OWNED       PIC 9(5)V99.
           05  STUDENT-NAME        PIC X(40).
           05  PROGRAM-OF-STUDY    PIC X(5).
           05  COURSE-CODE-1       PIC X(7).
           05  COURSE-AVERAGE-1    PIC 9(3).
           05  COURSE-CODE-2       PIC X(7).
           05  COURSE-AVERAGE-2    PIC 9(3).
           05  COURSE-CODE-3       PIC X(7).
           05  COURSE-AVERAGE-3    PIC 9(3). 
           05  COURSE-CODE-4       PIC X(7).
           05  COURSE-AVERAGE-4    PIC 9(3).          
           05  COURSE-CODE-5       PIC X(7).
           05  COURSE-AVERAGE-5    PIC 9(3).
 
       WORKING-STORAGE SECTION. 
      *File status field
       01  STATUS-FIELD.
           05 FILE-STATUS           PIC X(2).
           
       01 FLAG-FIELDS.
           05 TRANS-FLAG            PIC X(1) VALUE 'Y'.
           05 FOUND-FLAG            PIC X(1).
           05 EOF-FLAG              PIC X(1).

      *User Input Definition
       01  USER-INPUT.
           05 CHOICE                PIC X(1).
           05 INVALID-STUD-NUMBER   PIC X(1).
           05 INVALID-TUIT-OWNED    PIC X(1).

       01 RECORD-FROM-FILE.
           05 STUD-NUM-WS          PIC 9(6).
           05 TUIT-OWNED-WS        PIC 9(5)V99. 
    
      *Transaction Data Definition
       01  DATA-FROM-SCREEN.
           05 STUD-NUM-IN-WS                  PIC 9(6).
           05 TUIT-OWNED-IN-WS                PIC 9(5)V99.

       01 STUDENT-RECORD-IN.
          05  STUDENT-NUMBER-WS      PIC 9(6).
          05  TUITION-OWNED-WS       PIC 9(5)V99.
          05  STUDENT-NAME-WS        PIC X(40).
          05  PROGRAM-OF-STUDY-WS    PIC X(5).
          05  COURSE-CODE-1-WS       PIC X(7).
          05  COURSE-AVERAGE-1-WS    PIC 9(3).
          05  COURSE-CODE-2-WS       PIC X(7).
          05  COURSE-AVERAGE-2-WS    PIC 9(3).
          05  COURSE-CODE-3-WS       PIC X(7).
          05  COURSE-AVERAGE-3-WS    PIC 9(3). 
          05  COURSE-CODE-4-WS       PIC X(7).
          05  COURSE-AVERAGE-4-WS    PIC 9(3).          
          05  COURSE-CODE-5-WS       PIC X(7).
          05  COURSE-AVERAGE-5-WS    PIC 9(3).
 

       SCREEN SECTION.
      *Prompt the user to enter choice
       01  STUD-INFO-ENTRY-SCREEN.
           05  VALUE   "Transaction Screen" LINE 5 COLUMN 16.
           05  VALUE   "Transaction to enter? (Y/N)"
                                            LINE 8 COLUMN 16.
           05  CHOICE-IN                    LINE 9 COLUMN 16
                                          PIC X(1) TO CHOICE.

       01  STUD-NUM-INPUT-SCREEN.                                                    
           05  VALUE "Student number: "     LINE 12 COLUMN 16.
           05  STUD-NUM-INPUT               LINE 13 COLUMN 16
                                  PIC 9(6) TO STUD-NUM-IN-WS.

       01  TUIT-OWNED-INPUT-SCREEN.
           05  VALUE "Tuition owned: "      LINE 17 COLUMN 16.
           05  TUIT-OWNED-INPUT             LINE 18 COLUMN 16
                              PIC 9(5)V99 TO TUIT-OWNED-IN-WS
                              BLANK WHEN ZERO.
        
       01 REC-NOT-FOUND-SCREEN.
           05 VALUE "Record not found"     LINE 19 COLUMN 16.    
       
           
      *Display error message
       01  DISPLAY-ERROR-SCREEN.
           05  VALUE "Invalid input"        LINE 20 COLUMN 16.
        
      *Operation success message
       01 UPDATE-SUCCESS-SCREEN.
           05  VALUE "Successfully updated" LINE 21 COLUMN 16.    


       
       PROCEDURE DIVISION.
      *Online Student File Record update
       100-UPDATE-STUDENT-FILE.
           PERFORM 200-INIT-UPDATE-STUD-FILE.
           PERFORM 201-CREATE-UPDATE-STUD-FILE
                   UNTIL CHOICE = "N".
           PERFORM 202-TERMINATE-UPDATE-STUD-FILE.
           STOP RUN.

      *Initialize the student file
       200-INIT-UPDATE-STUD-FILE.
           PERFORM 300-OPEN-STUD-FILE.
           PERFORM 301-READ-FILE-IN.
           PERFORM 302-READ-USER-CHOICE.

      *Update the student file 
       201-CREATE-UPDATE-STUD-FILE.
           PERFORM 303-ACCEPT-STUD-DATA-ENTRY.
           PERFORM 304-UPDATE-STUD-DATA.
           PERFORM 305-REWRITE-STUD-DATA.
           PERFORM 302-READ-USER-CHOICE.
         
      *Close all the files 
       202-TERMINATE-UPDATE-STUD-FILE.
           CLOSE STUDENT-FILE.
        
      *Open the student file 
       300-OPEN-STUD-FILE.
           OPEN I-O STUDENT-FILE.

    
       301-READ-FILE-IN.
           READ STUDENT-FILE 
               RECORD KEY IS STUDENT-NUMBER.

          
      *Display prompt for the user input  
       302-READ-USER-CHOICE.
           DISPLAY STUD-INFO-ENTRY-SCREEN.
           ACCEPT  STUD-INFO-ENTRY-SCREEN.
           IF CHOICE = "Y" DISPLAY STUD-NUM-INPUT-SCREEN.        
             
      *Accept data from user input
       303-ACCEPT-STUD-DATA-ENTRY.
           DISPLAY STUD-NUM-INPUT-SCREEN.  
           ACCEPT  STUD-NUM-INPUT-SCREEN.  
           DISPLAY TUIT-OWNED-INPUT-SCREEN.
           ACCEPT  TUIT-OWNED-INPUT-SCREEN.

       304-UPDATE-STUD-DATA.
           MOVE STUD-NUM-IN-WS TO STUDENT-NUMBER.
           READ STUDENT-FILE 
           RECORD KEY IS STUDENT-NUMBER.
           MOVE TUIT-OWNED-IN-WS TO TUITION-OWNED-WS.
         
       305-REWRITE-STUD-DATA.
           DISPLAY STUDENT-RECORD.
           MOVE STUDENT-RECORD-IN TO STUDENT-RECORD.
           REWRITE STUDENT-RECORD FROM STUDENT-RECORD-IN.
           DISPLAY STUDENT-RECORD.
           
       END PROGRAM UPDATE-STUD-FILE.





