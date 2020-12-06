      *==============================================================
      *COURSE:CST8283 BUSINESS PROGRAMMING
      *PROFESSOR:MEL SANSCHAGRIN
      *GROUP MEMBERS: WEI YU
      *DESCRIPTION:      
      *THIS PROGRAM READ STUDENT RECORDS AND COURSE RECORDS FROM 
      *EXTERNALE FILES STUFILE.TXT AND PROGRAM.TXT, CALCULATE THE 
      *STUDENT AGERAGE, AND PRODUCE A STUDENT REPORT INTO A FILE
      * NAMED STURPT.TXT      
      *==============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CAL-AVERAGE.
       AUTHOR. WEI YU.
       DATE-WRITTEN. 03-DEC-2020.
       DATE-COMPILED. 03-DEC-2020.
 
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STUDENT-FILE-IN 
               ASSIGN TO "D:\COBOL\STUFILE.TXT"
                   ORGANIZATION IS LINE SEQUENTIAL.
           SELECT PROGRAM-FILE-IN 
               ASSIGN TO "D:\COBOL\PROGRAM.TXT"
                   ORGANIZATION IS LINE SEQUENTIAL.   
           SELECT STUDENT-REPORT
               ASSIGN TO "D:\COBOL\STURPT.TXT"
                   ORGANIZATION IS LINE SEQUENTIAL.
      *=================================================================
      *STUDENT-FILE-IN AND PROGRAM-FILE-IN ARE THE INPUT FILES.
      *DURING INITIAZATION STUDENT-REPORT WILL BE POPULATED WITH THE 
      *DATA COLLECTED IN THE WORKING STORAGE RECORD IN THE 
      *STUDENT-REPORT-WS AND COLUMN-HEADER-WS
      *=================================================================
       DATA DIVISION.
       FILE SECTION.
       FD STUDENT-FILE-IN.
       01  STUDENT-RECORD.
           05  STUDENT-NUMBER      PIC 9(6).
           05  TUITION-OWNED       PIC 9(4)V99.
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

       FD PROGRAM-FILE-IN.
       01 PROGRAM-RECORD.
           05 PROGRAM-CODE     PIC X(5).
           05 PROGRAM-NAME     PIC X(20).

       FD STUDENT-REPORT.
       01  STUDENT-REPORT-RECORD-OUT   PIC X(83).

      *================================================================
      *STUDENT-REPORT-WS IS FOR RECORD FORMATED DATA  
      *COLUMN-HEADER-WS IS FOR OUTPUT RECORD HEADER
      *=================================================================
       WORKING-STORAGE SECTION.
      *REPORT OUTPUT DATA FIELD
       01  STUDENT-REPORT-WS.
           05  STUDENT-NAME-OUT-WS      PIC X(22).
           05  FILLER                   PIC X(2)     VALUE SPACES.
           05  STUDENT-AVERAGE-OUT-WS   PIC ZZ9.
           05  FILLER                   PIC X(11)    VALUE SPACES. 
           05  PROGRAM-NAME-OUT-WS      PIC X(19).     
           05  FILLER                   PIC X(4)     VALUE SPACES. 
           05  TUITION-OWNED-OUT-WS     PIC Z,ZZ9.99.
           
      *PROGRAM TABLE 
       COPY "D:\COBOL\PROJECT3\PRG-TBL-STRUCTURE.CBL".

      *REPORT PAGE HEADING 
       01  PAGE-HEADER-WS.
           05  FILLER PIC X(21)  VALUE   "STUDENT RECORD REPORT".

      *REPORT COLUNM HEADING 
       01  COLUMN-HEADER-WS.
           05  FILLER  PIC X(4)  VALUE  "NAME".
           05  FILLER  PIC X(20) VALUE  SPACES.
           05  FILLER  PIC X(7)  VALUE "AVERAGE".   
           05  FILLER  PIC X(7)  VALUE  SPACES.           
           05  FILLER  PIC X(7)  VALUE  "PROGRAM".
           05  FILLER  PIC X(16) VALUE  SPACES.  
           05  FILLER  PIC X(12) VALUE  "TUITION OWED".

       01  AUDIT-TRAILERS-WS.
           05  FILLER                 PIC X(6)  VALUE "READ: ".
           05  RECORDS-IN-COUNTER-WS  PIC 9(3).           
           05  FILLER                 PIC X(10)  VALUE " WRITTEN: ".               
           05  RECORDS-OUT-COUNTER-WS PIC 9(3).
           05  FILLER                 PIC X(6)   VALUE SPACES.
           05  FILLER                 PIC X(13)  VALUE "AUTHOR:WEI YU". 
           05  FILLER                 PIC X(3)   VALUE SPACES.
           05  FILLER                 PIC X(13)  VALUE "VINCENT WANG".
       
       01  CALCULATION-FIELD-WS.
           05  STUDENT-AVERAGE-WS     PIC 9(3).
           05  TOTAL-GRADE-WS         PIC 9(3)    VALUE ZERO.

     
       01  FLAGS-WORKING-FIELDS.
           05  EOF-FLAG                PIC X(3)    VALUE "NO ".
           05  EOF-TBL-FLAG            PIC X(3)    VALUE "NO ".
           05  FOUND-FLAG              PIC X(3)    VALUE "NO ".
           05  TBL-SUB                 PIC 9(2)    VALUE 1.
       
       PROCEDURE DIVISION.
      *CREATE THE STUDENT REPORT 
       100-CREATE-STUDENT-REPORT.
           PERFORM 201-INITIATE-CREATE-STUDENT-REPORT.
           PERFORM 202-CREATE-STUDEN-REPORT-RECORD
                   UNTIL EOF-FLAG = "YES".
           PERFORM 203-TERMINATE-CREATE-STUDENT-REPORT.
           STOP RUN.

       201-INITIATE-CREATE-STUDENT-REPORT.
           PERFORM 301-OPEN-STUDENT-PROGRAM-FILES.
           PERFORM 302-INIT-READ-WRITE-COUNTERS.   
           PERFORM 303-PRODUCE-PROGRAM-TBL 
                       VARYING TBL-SUB FROM 1 BY 1
                       UNTIL TBL-SUB > 20 OR EOF-TBL-FLAG = "YES".
           PERFORM 306-WRITE-REPORT-COLUMN-HEADERS. 
           PERFORM 304-READ-STUDENT-PROGRAM-RECORDS.
        
       202-CREATE-STUDEN-REPORT-RECORD.
           PERFORM 305-CALCULATE-STUDENT-AVERAGE. 
           PERFORM 307-WRITE-STUDENT-REPORT-RECORD. 
           PERFORM 304-READ-STUDENT-PROGRAM-RECORDS. 

       203-TERMINATE-CREATE-STUDENT-REPORT.
           PERFORM 308-DISPLAY-RECORD-COUNTERS.
           PERFORM 309-CLOSE-FILES.

       
       301-OPEN-STUDENT-PROGRAM-FILES.
           OPEN INPUT  STUDENT-FILE-IN
                       PROGRAM-FILE-IN
                OUTPUT STUDENT-REPORT.

       302-INIT-READ-WRITE-COUNTERS.
           INITIALIZE  RECORDS-IN-COUNTER-WS
                       RECORDS-OUT-COUNTER-WS.

       303-PRODUCE-PROGRAM-TBL.
           READ PROGRAM-FILE-IN INTO PROGRAM-TBL-RTN(TBL-SUB)
                  AT END MOVE "YES" TO EOF-TBL-FLAG.

       304-READ-STUDENT-PROGRAM-RECORDS.
           READ STUDENT-FILE-IN 
                AT END MOVE "YES" TO EOF-FLAG
                   NOT AT END ADD 1 TO RECORDS-IN-COUNTER-WS.    

       305-CALCULATE-STUDENT-AVERAGE. 
       CALL 'CALC-AVERAGE' USING COURSE-AVERAGE-1 COURSE-AVERAGE-2      
           COURSE-AVERAGE-3 COURSE-AVERAGE-4 COURSE-AVERAGE-5
           STUDENT-AVERAGE-WS TOTAL-GRADE-WS.
           
           
       306-WRITE-REPORT-COLUMN-HEADERS. 
           WRITE STUDENT-REPORT-RECORD-OUT FROM PAGE-HEADER-WS. 
           WRITE STUDENT-REPORT-RECORD-OUT FROM COLUMN-HEADER-WS.

       307-WRITE-STUDENT-REPORT-RECORD.   
           MOVE STUDENT-NAME TO STUDENT-NAME-OUT-WS.   
           MOVE STUDENT-AVERAGE-WS TO STUDENT-AVERAGE-OUT-WS. 
           MOVE SPACES TO PROGRAM-NAME-OUT-WS.
           PERFORM 402-SEARCH-RTN
               VARYING TBL-SUB FROM 1 BY 1 UNTIL TBL-SUB > 20.   
           MOVE TUITION-OWNED TO TUITION-OWNED-OUT-WS.
           WRITE STUDENT-REPORT-RECORD-OUT FROM STUDENT-REPORT-WS 
               AFTER ADVANCING 1 LINES.
           ADD 1 TO RECORDS-OUT-COUNTER-WS.
           

       402-SEARCH-RTN.
       IF PROGRAM-OF-STUDY = PROGRAM-CODE-TBL-WS(TBL-SUB)
           MOVE PROGRAM-NAME-TBL-WS(TBL-SUB) 
                TO PROGRAM-NAME-OUT-WS 
                      DISPLAY "PROGRAM FOUND"
       END-IF.

       308-DISPLAY-RECORD-COUNTERS.
           WRITE STUDENT-REPORT-RECORD-OUT FROM AUDIT-TRAILERS-WS
               AFTER ADVANCING 2 LINE.

       309-CLOSE-FILES.
           CLOSE STUDENT-FILE-IN
                 PROGRAM-FILE-IN
                 STUDENT-REPORT.
           DISPLAY "GENERATE STUDENT REPORT SUCCESSFULLY".

       END PROGRAM CAL-AVERAGE.









