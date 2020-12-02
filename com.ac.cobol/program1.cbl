IDENTIFICATION DIVISION.
        PROGRAM-ID. INDEX_CONVERSION.
        AUTHOR. WEI YU .
        DATE-WRITTEN. 02-DEC-2020.
        DATE-COMPILED. O2-DEC-2020.

        *=================================================================
        *THIS PROGRAM READ STUDENT RECORDS AND COURSE RECORDS FROM
        *EXTERNALE FILES STUFILE.txt AND PROGRAM.txt, CALCULATE THE
        *STUDENT AGERAGE, AND PRODUCE A STUDENT REPORT INTO A FILE
        * NAMED STURPT.txt, THEN CONVERT THE SEQUENTIAL FILE INTO A
        *INDEXED SEQUENTIAL FILE
        *=================================================================

        ENVIRONMENT DIVISION.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
        SELECT STUDENT-FILE-IN
        ASSIGN TO "D:\Cobol\STUFILE.txt"
        ORGANIZATION IS LINE SEQUENTIAL.
        SELECT STUDENT-FILE-OUT
        ASSIGN TO "D:\Cobol\STUFILE_1.txt"
        ORGANIZATION IS INDEXED
        ACCESS MODE IS SEQUENTIAL
        RECORD KEY IS STUD-NUM-OUT
        FILE STATUS IS STUD-FILE-STAT.

        *=================================================================
        *STUDENT-FILE-IN AND PROGRAM-FILE-IN ARE THE INPUT FILES.
        *DURING INITIAZATION STUDENT-REPORT WILL BE POPULATED WITH THE
        *DATA COLLECTED IN THE WORKING STORAGE RECORD IN THE
        *STUDENT-REPORT-WS AND COLUMN-HEADER-WS
        *=================================================================
        DATA DIVISION.
        FILE SECTION.
        FD STUDENT-FILE-IN.
        01  STUDENT-RECORD-IN.
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

        FD STUDENT-FILE-OUT.
        01  STUDENT-RECORD-OUT.
        05  STUD-NUM-OUT             PIC 9(6).
        05  TUITION-OWNED-OUT        PIC 9(4)V99.
        05  STUDENT-NAME-OUT         PIC X(40).
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

        *=================================================================
        *STUDENT-REPORT-WS IS FOR RECORD FORMATED DATA
        *COLUMN-HEADER-WS IS FOR OUTPUT RECORD HEADER
        *=================================================================
        WORKING-STORAGE SECTION.

        *Control areas
        01 FLAGS-WORKING-FIELDS.
        05  EOF-FLAG                PIC X(3)    VALUE "NO ".
        05  STUD-FILE-STAT          PIC X(2).

        PROCEDURE DIVISION.
        *Convert student file and program file
        *into Indexed Sequential Files
        100-CONVERT-STUD-PROG-FILE.
        PERFORM 200-CONVERT-STUD-FILE.
        STOP RUN.

        *Convert student files
        200-CONVERT-STUD-FILE.
        PERFORM 300-INIT-STUD-CONVERT.
        PERFORM 301-PRODUCE-STUD-CONVERT UNTIL EOF-FLAG = "YES".
        PERFORM 302-TERMINATE-STUD-CONVERT.

        *Initialize the Student File Convert
        300-INIT-STUD-CONVERT.
        PERFORM 500-OPEN-STUD-FILE.
        PERFORM 501-INIT-FLAGS-WORKING-FIELDS.
        PERFORM 502-READ-STUD-FILE-IN.

        *Open the Student File
        500-OPEN-STUD-FILE.
        OPEN INPUT STUDENT-FILE-IN.

        *Initialize FLAGS-WORKING-FIELDS.
        501-INIT-FLAGS-WORKING-FIELDS.
        INITIALIZE FLAGS-WORKING-FIELDS.

        *Read in the Student File
        502-READ-STUD-FILE-IN.
        READ STUDENT-FILE-IN
        AT END MOVE "YES" TO EOF-FLAG.
        DISPLAY "READ SUCESSFULLY".

        *Produce Student File Conversion
        301-PRODUCE-STUD-CONVERT.
        PERFORM 503-WRITE-STUD-RECORD.
        PERFORM 502-READ-STUD-FILE-IN.

        *Write Student File Out Record
        503-WRITE-STUD-RECORD.
        WRITE STUDENT-RECORD-OUT FROM STUDENT-RECORD-IN
        INVALID KEY DISPLAY "Error: " STUDENT-RECORD-IN.
        DISPLAY "WRITE SUCESSFULLY".

        *Terminate the conversion of Student File
        302-TERMINATE-STUD-CONVERT.
        CLOSE STUDENT-FILE-IN
        STUDENT-FILE-OUT.

        END PROGRAM INDEX_CONVERSION.










