      * cobc -x EduBingo.cbl -o EduBingo
      * ./EduBingo 
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EDU-BINGO.

      *******FILES USED FOR PROCESSING INPUT AND GENERATING OUTPUT****** 
       ENVIRONMENT DIVISION. 
       INPUT-OUTPUT SECTION. 
       FILE-CONTROL. 

      *************FILE DEFINITION FOR ALL THE FILES NEEDED*************    
       DATA DIVISION.

      *******************VARIABLES USED ON OUR PROGRAM****************** 
       WORKING-STORAGE SECTION.
       01 USER-EMAIL         PIC X(50).
       01 USER-PASSWORD      PIC X(20).
       01 LOGGED-IN          PIC 9 VALUE 0.
       01 TASKS-COMPLETED    PIC 9(2) VALUE 0.
       01 TOTAL-TASKS        PIC 9(2) VALUE 5.
       01 PROGRESS-PERCENT   PIC 9(3).
       01 USER-CHOICE        PIC 9 VALUE 0.

       01 TASK-LIST.
           05 TASK-STATUS    PIC X(1) OCCURS 5.
           88 COMPLETED      VALUE "Y".
           88 PENDING        VALUE "N".

      ********************MAIN PROCESS OF THE PROGRAM******************* 
       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
      *    Here medyo magulo pa ung main since nag e-experiment pa aq hehe 
           PERFORM VARYING USER-CHOICE FROM 1 BY 1 UNTIL USER-CHOICE > 5
               MOVE "N" TO TASK-STATUS(USER-CHOICE)
           END-PERFORM.
           
           DISPLAY "========================================"
           DISPLAY "=         Welcome to EduBingo!         ="
           DISPLAY "========================================"
           DISPLAY " "
           
           PERFORM LOGIN-PROCESS.

           IF LOGGED-IN = 1
              
              PERFORM DISPLAY-TASKS
              PERFORM TASK-SELECTION
           ELSE
              DISPLAY "Invalid login. Program will exit."
              STOP RUN
           END-IF.
           
           PERFORM DISPLAY-TASKS. 
           PERFORM SHOW-PROGRESS.
           STOP RUN.

       LOGIN-PROCESS.
           DISPLAY "Enter your email: " WITH NO ADVANCING.
           ACCEPT USER-EMAIL.
           DISPLAY "Enter your password: " WITH NO ADVANCING.
           ACCEPT USER-PASSWORD.

           IF USER-EMAIL = "student@example.com" AND
              USER-PASSWORD = "password123"
              MOVE 1 TO LOGGED-IN
           ELSE
              MOVE 0 TO LOGGED-IN
           END-IF. 

       DISPLAY-TASKS.
           DISPLAY "Bingo Tasks:"
           PERFORM VARYING USER-CHOICE FROM 1 BY 1 UNTIL USER-CHOICE > 5
              IF TASK-STATUS(USER-CHOICE) = "N"
                 DISPLAY USER-CHOICE ":" " [ ] Task " USER-CHOICE
              ELSE
                 DISPLAY USER-CHOICE ":" " [X] Task " USER-CHOICE
              END-IF
           END-PERFORM.

       TASK-SELECTION.
           DISPLAY "Enter the task number you completed (1-5), or 0 to"& 
           " finish: "
           ACCEPT USER-CHOICE.

           PERFORM UNTIL USER-CHOICE = 0
              IF TASK-STATUS(USER-CHOICE) = "N"
                 MOVE "Y" TO TASK-STATUS(USER-CHOICE)
                 ADD 1 TO TASKS-COMPLETED
                 DISPLAY "Task " USER-CHOICE " marked as completed!"
              ELSE
                 DISPLAY "Task already completed!"
              END-IF

              DISPLAY "Enter another task number (1-5), or 0 to"       &
              "finish: "
              ACCEPT USER-CHOICE
           END-PERFORM.

       SHOW-PROGRESS.
           COMPUTE PROGRESS-PERCENT = (TASKS-COMPLETED / TOTAL-TASKS) *
           100
           DISPLAY "You have completed " TASKS-COMPLETED " out of "   
           TOTAL-TASKS " tasks."
           DISPLAY "Progress: " PROGRESS-PERCENT "%.".        

       

       

           