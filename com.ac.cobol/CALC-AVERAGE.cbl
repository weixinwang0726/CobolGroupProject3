      *========================================================
      *COURSE:CST8283 BUSINESS PROGRAMMING
      *PROFESSOR:MEL SANSCHAGRIN
      *GROUP MEMBERS: WEI YU, WEIXIN WANG, ZIYIN YAN, CHUN XIA LI, 
      *DING SUN, JINGSHAN GUAN                                         
      *DESCRIPTION:
      *THIS PROGRAM CALCULATES STUDENT GRADE AVARAGE
      *===============================================================  
                                                                        
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALC-AVERAGE.
       AUTHOR.WEI YU.
       DATE-WRITTEN. 03-DEC-2020.
       DATE-COMPILED. 03-DEC-2020.
       
       DATA DIVISION.
       LINKAGE SECTION.
       01 COURSE-AVG-1    PIC 9(3).
       01 COURSE-AVG-2    PIC 9(3).
       01 COURSE-AVG-3    PIC 9(3).
       01 COURSE-AVG-4    PIC 9(3).
       01 COURSE-AVG-5    PIC 9(3).
       01 STUD-AVG-WS  PIC 9(3).
       01 TOTAL-GRADE-WS      PIC 9(3)    VALUE ZERO.
       
       
       PROCEDURE DIVISION
           USING COURSE-AVG-1 COURSE-AVG-2 COURSE-AVG-3
                   COURSE-AVG-4 COURSE-AVG-5 STUD-AVG-WS
                   TOTAL-GRADE-WS.
                   
           ADD COURSE-AVG-1 COURSE-AVG-2 COURSE-AVG-3 COURSE-AVG-4
                 COURSE-AVG-5 GIVING TOTAL-GRADE-WS.
           COMPUTE STUD-AVG-WS ROUNDED = TOTAL-GRADE-WS/5.
           
           