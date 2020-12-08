      *========================================================
      *COURSE:CST8283 BUSINESS PROGRAMMING
      *PROFESSOR:MEL SANSCHAGRIN
      *GROUP MEMBERS: WEI YU, WEIXIN WANG, ZIYIN YAN, CHUN XIA LI, 
      *DING SUN, JINGSHAN GUAN                                         
      *DESCRIPTION:
      *THIS PROGRAM CONVERTS THE STUDENT FILE INTO A INDEXED SEQUENTIAL 
      *FILE.
      *===============================================================  
       
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INDEX_CONVERSION.
       AUTHOR. WEI YU, WEIXIN WANG, ZIYIN YAN, CHUN XIA LI, DING SUN,
               JINGSHAN GUAN.
       DATE-WRITTEN. 02-DEC-2020.
       DATE-COMPILED. O7-DEC-2020.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STUDENT-FILE-IN
               ASSIGN TO "D:\Cobol\STUFILE3.TXT"
                   ORGANIZATION IS LINE SEQUENTIAL.
               
           SELECT INDEXED-STUDENT-FILE
               ASSIGN TO "D:\Cobol\STUFILE_IN.TXT"
                   ORGANIZATION IS INDEXED
                       ACCESS MODE IS SEQUENTIAL
                           RECORD KEY IS STUD-NUM-OUT
                               FILE STATUS IS STATUS-FIELD.
                           
       DATA DIVISION.
       FILE SECTION.
       FD STUDENT-FILE-IN.
       01  STUDENT-RECORD-IN.
           05  STUDENT-NUMBER      PIC 9(6).
           05  TUITION-OWED        PIC 9(4)V99.
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

       FD INDEXED-STUDENT-FILE.
       01  STUDENT-RECORD-OUT.
           05  STUD-NUM-OUT             PIC 9(6).
           05  TUITION-OWED-OUT         PIC 9(4)V99.
           05  STUD-NAME-OUT            PIC X(40).
           05  PROGRAM-OF-STUDY-OUT     PIC X(5).
           05  COURSE-CODE-1-OUT        PIC X(7).
           05  COURSE-AVERAGE-1-OUT     PIC 9(3).
           05  COURSE-CODE-2-OUT        PIC X(7).
           05  COURSE-AVERAGE-2-OUT     PIC 9(3).
           05  COURSE-CODE-3-OUT        PIC X(7).
           05  COURSE-AVERAGE-3-OUT     PIC 9(3).
           05  COURSE-CODE-4-OUT        PIC X(7).
           05  COURSE-AVERAGE-4-OUT     PIC 9(3).
           05  COURSE-CODE-5-OUT        PIC X(7).
           05  COURSE-AVERAGE-5-OUT     PIC 9(3).

       WORKING-STORAGE SECTION.
       
      *Control areas
       01 FLAGS-WORKING-FIELDS.
           05  EOF-FLAG                PIC X(3).
           05  STATUS-FIELD            PIC X(2).
           05  VALID-FLAG              PIC X(3).

       PROCEDURE DIVISION.
           
      *Convert student file into Indexed Sequential Files
       100-CREATE-INDEXED-STUD-FILE.
           PERFORM 201-INITIALIZE-CREATE-FILE.
           PERFORM 202-CREATE-STUD-RECORDS UNTIL EOF-FLAG = "YES".
           PERFORM 203-TERMINATE-PROGRAM.
           STOP RUN.

      *Initialize the Student File Convert
       201-INITIALIZE-CREATE-FILE.
           PERFORM 300-OPEN-STUD-FILE.
           PERFORM 301-INIT-FLAGS-WORKING-FIELDS.
           PERFORM 302-READ-STUD-FILE-IN.

      *Convert student files
       202-CREATE-STUD-RECORDS.
           PERFORM 303-WRITE-INDEXED-STUD-RECORD.
           PERFORM 302-READ-STUD-FILE-IN.

      *Open the Student File
       300-OPEN-STUD-FILE.
           OPEN INPUT STUDENT-FILE-IN
               OUTPUT INDEXED-STUDENT-FILE.

      *Initialize FLAGS-WORKING-FIELDS.
       301-INIT-FLAGS-WORKING-FIELDS.
           INITIALIZE FLAGS-WORKING-FIELDS.

      *Read in the Student File
       302-READ-STUD-FILE-IN.
           READ STUDENT-FILE-IN
               AT END MOVE "YES" TO EOF-FLAG.

      *Write Student File Out Record
       303-WRITE-INDEXED-STUD-RECORD.
           WRITE STUDENT-RECORD-OUT FROM STUDENT-RECORD-IN
               INVALID KEY MOVE "NO" TO VALID-FLAG
               NOT INVALID KEY MOVE "YES" TO VALID-FLAG 
                               DISPLAY STUDENT-RECORD-OUT
           END-WRITE.
      
      *Terminate the conversion of Student File
       203-TERMINATE-PROGRAM.
           CLOSE STUDENT-FILE-IN
                   INDEXED-STUDENT-FILE.

       END PROGRAM INDEX_CONVERSION.










