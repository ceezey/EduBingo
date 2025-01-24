      *******************THE METADATA FOR OUR PROGRAM*******************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EDU-BINGO.
       AUTHOR. GROUP 1.
       DATE-WRITTEN. 12/13/2024.
       SECURITY. AUTHORIZED PERSONNEL ONLY.
       REMARKS. PROGRAM IS CURRENTLY IN TESTING AND MAY ENCOUNTER BUGS.

      *******FILES USED FOR PROCESSING INPUT AND GENERATING OUTPUT******
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCOUNT-FILE ASSIGN TO "ACCOUNT-FILE.dat"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT MOOD-FILE ASSIGN TO "MOOD-FILE.dat"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT TASKS-FILE ASSIGN TO "TASKS-FILE.dat"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT BADGES-FILE ASSIGN TO "BADGES-FILE.dat"
               ORGANIZATION IS LINE SEQUENTIAL.
       
      *************FILE DEFINITION FOR ALL THE FILES NEEDED*************
       DATA DIVISION.
       FILE SECTION.
       FD ACCOUNT-FILE.
       01 ACCOUNT-FILE-RECORD.
           05 USER-NAME                      PIC X(20).
           05 USER-PASSWORD                  PIC X(20).
           05 USER-ACCOUNT-CREATED           PIC X(20).
       FD MOOD-FILE.
       01 MOOD-FILE-RECORD.
           05 MOOD-USER                      PIC X(20).
           05 MOOD-DATE                      PIC X(20).
           05 MOOD-STATUS                    PIC 9.
       FD TASKS-FILE.
       01 TASKS-FILE-RECORD.
           05 TASKS-USER                     PIC X(20).
           05 TASKS-DATE                     PIC X(20).
           05 TASKS-DESCRIPTION              PIC X(30).
           05 TASKS-STATUS                   PIC 9.
       FD BADGES-FILE.
       01 BADGES-FILE-RECORD.
           05 BADGE-USER                     PIC X(20).
           05 USER-CURRENT-BADGE             PIC 9.

      *******************VARIABLES USED ON OUR PROGRAM******************
       WORKING-STORAGE SECTION.
       01 USER-CHOICE               PIC X(1).
       01 USER-NAME-INPUT           PIC X(20).
       01 USER-PASSWORD-INPUT       PIC X(20).
       01 CURRENT-SESSION           PIC X(20).
       01 CURRENT-PASSWORD          PIC X(20).
       01 NEW-PASSWORD-INPUT        PIC X(20).
       01 ACCOUNT-FOUND             PIC X(1) VALUE 'N'.
       01 USER-DATE-INPUT           PIC X(20).
       01 USER-TASK-DESCRIPTION     PIC X(30).
       01 USER-TASK-STATUS          PIC X(1).
       01 USER-TASK-DATE            PIC X(20).
       01 COUNT-DUE-TODAY           PIC 9 VALUE 0.
       01 EOF                       PIC X VALUE 'N'.
       01 RAW-DATE                  PIC 9(8).
       01 TODAY-MONTH               PIC 99.    
       01 TODAY-DAY                 PIC 99.   
       01 TODAY-YEAR                PIC 9(4).  
       01 TODAY-DATE                PIC X(10).
       01 MOOD-STATUS-INPUT         PIC 9.
       01 MOOD-FOUND                PIC X(1) VALUE 'N'.
       01 USER-UPDATE-DESCRIPTION   PIC X(1) VALUE 'N'.
       01 USER-UPDATE-STATUS        PIC X(1) VALUE 'N'.
       01 USER-UPDATE-DATE          PIC X(1) VALUE 'N'.
       01 USER-CHANGES-MADE         PIC X(1) VALUE 'N'.
       01 TASKS-FOUND               PIC X(1) VALUE 'N'.
       01 LOG-HEADER-DISPLAYED      PIC X(1) VALUE 'N'.
       01 MOOD-LOG                  PIC X(20).
       01 BADGE                     PIC 9(1) VALUE 0.
       01 RECORD-FOUND              PIC X VALUE 'N'.
       01 BADGE-FOUND               PIC X VALUE 'N'.
       01 BADGE-TYPE                PIC 9.
       01 USER-CONFIRM-FLAG         PIC X(1).
       01 CONFIRM-PASSWORD-INPUT    PIC X(20).
       01 COMPARE-DATE              PIC X(20).
       01 RETRIEVE-PASS             PIC X(20).

      ********************MAIN PROCESS OF THE PROGRAM*******************
       PROCEDURE DIVISION.
       MAIN.
           ACCEPT RAW-DATE FROM DATE YYYYMMDD
           MOVE RAW-DATE(5:2) TO TODAY-MONTH
           MOVE RAW-DATE(7:2) TO TODAY-DAY
           MOVE RAW-DATE(1:4) TO TODAY-YEAR

           STRING TODAY-MONTH DELIMITED BY SIZE
                  "/" DELIMITED BY SIZE
                  TODAY-DAY DELIMITED BY SIZE
                  "/" DELIMITED BY SIZE
                  TODAY-YEAR DELIMITED BY SIZE
                  INTO TODAY-DATE
           DISPLAY " "
           DISPLAY ".------..------..------..------..------..------.".
           DISPLAY "|E.--. ||D.--. ||U.--. ||X.--. ||X.--. ||X.--. |".
           DISPLAY "| :/\: || :/\: || (\/) || :(): || (\/) || :(): |".
           DISPLAY "| (__) || (__) || :\/: || ()() || :\/: || ()() |".
           DISPLAY "| '--'X|| '--'B|| '--'I|| '--'N|| '--'G|| '--'O|".
           DISPLAY "'------''------''------''------''------''------'".
           DISPLAY "================================================"
           DISPLAY "|| Empower Your Education, One Card at a Time!||".
           DISPLAY "================================================"
           DISPLAY " ".
           DISPLAY "(\_/)  Welcome to Edubingo!".
           DISPLAY "(o.o)  Hi! Meet Mimi, your study buddy!".
           DISPLAY "(> <) ".
           DISPLAY " ".

           PERFORM ACCOUNT-MENU.

           STOP RUN.


       ACCOUNT-MENU.
           DISPLAY "> What would you like to do?".
           DISPLAY "  [1] Register".
           DISPLAY "  [2] Login".
           DISPLAY "  [3] Forgot Password".
           DISPLAY "  [4] Exit".
           DISPLAY " ".
           DISPLAY "> Enter your choice: " WITH NO ADVANCING.
           ACCEPT USER-CHOICE.
           
           EVALUATE USER-CHOICE
               WHEN '1'
                   PERFORM SIGN-UP
               WHEN '2'
                   PERFORM LOGIN
               WHEN '3'
                   PERFORM FORGOT-PASSWORD
               WHEN '4'
                   DISPLAY " "
                   DISPLAY "(\_/)"
                   DISPLAY "(o.o)  See you next time! Keep safe :)"
                   DISPLAY "(> <) "
                   DISPLAY " "
                   MOVE " " TO CURRENT-SESSION
                   STOP RUN
               WHEN OTHER
                   DISPLAY " "
                   DISPLAY "(\_/)"
                   DISPLAY "(o.o)  Oops, only pick among the choices."
                   DISPLAY "(> <) "
                   DISPLAY " "
                   PERFORM ACCOUNT-MENU
           END-EVALUATE.
       
       DASHBOARD.
           DISPLAY " "
           DISPLAY "                               .------."
           DISPLAY "         _  _  __  __ __ ___   |I.--. |"
           DISPLAY "        | || |/__\|  V  | __|  | :/\: |"
           DISPLAY "        | >< | \/ | \_/ | _|   | (__) |"
           DISPLAY "        |_||_|\__/|_| |_|___|  | '--'I|"
           DISPLAY "                               '------'"
           DISPLAY "==================================================="
           DISPLAY "|[1] TASKS |[2] STATUS |[3] SUPPORT |[4] SETTINGS |"
           DISPLAY"=================================================== "
           PERFORM BADGE-LEVEL-I
           PERFORM REMINDER
           PERFORM DISPLAY-TODO-TASKS
           PERFORM DISPLAY-ONGOING-TASKS
           PERFORM DISPLAY-DONE-TASKS
           DISPLAY " "
           DISPLAY "(\_/)"
           DISPLAY "(o.o)  What would you like to do? :)"
           DISPLAY "(> <) "
           DISPLAY " "

           PERFORM GET-VALID-ACTION-DASHBOARD.
       
       UPDATE-TASK.                                                        
           DISPLAY " "
           DISPLAY "(\_/)  Update your task!"
           DISPLAY "(o.o)  Here are your tasks, " CURRENT-SESSION
           DISPLAY "(> <)  "
           PERFORM DISPLAY-TODO-TASKS
           PERFORM DISPLAY-ONGOING-TASKS
           PERFORM DISPLAY-DONE-TASKS

           OPEN I-O TASKS-FILE
           MOVE 'N' TO EOF
           MOVE 'N' TO USER-CHANGES-MADE
           
           DISPLAY " "
           DISPLAY "> Enter the task description to update: " 
           WITH NO ADVANCING
           ACCEPT USER-TASK-DESCRIPTION

           PERFORM UNTIL EOF = 'Y'
           READ TASKS-FILE INTO TASKS-FILE-RECORD
                AT END
                    MOVE 'Y' TO EOF
                NOT AT END
                    IF TASKS-USER = CURRENT-SESSION 
                    AND TASKS-DESCRIPTION = USER-TASK-DESCRIPTION
                        DISPLAY " "
                        DISPLAY "(\_/)  You selected the task: " 
                        TASKS-DESCRIPTION
                        EVALUATE TASKS-STATUS
                            WHEN 1
                            DISPLAY 
                            "(o.o)  It's current status is: TO-DO" 
                            WHEN 2
                            DISPLAY
                            "(o.o)  It's current status is: ONGOING" 
                            WHEN 3
                            DISPLAY
                            "(o.o)  It's current status is: DONE" 
                        END-EVALUATE
                        
                        DISPLAY "(> <)  Due date is: " TASKS-DATE
                        DISPLAY " "

                        DISPLAY 
                  "> Do you want to update the task description (Y/N)? "
                        WITH NO ADVANCING
                        ACCEPT USER-UPDATE-DESCRIPTION

                        IF USER-UPDATE-DESCRIPTION = 'Y'
                            DISPLAY "Enter the new task description: "
                            WITH NO ADVANCING
                            ACCEPT USER-TASK-DESCRIPTION
                            MOVE USER-TASK-DESCRIPTION 
                            TO TASKS-DESCRIPTION
                            MOVE 'Y' TO USER-CHANGES-MADE
                            DISPLAY "Task description updated."
                        END-IF
                        
                        DISPLAY
                       "> Do you want to update the task status (Y/N)? "
                        WITH NO ADVANCING
                        ACCEPT USER-UPDATE-STATUS

                        IF USER-UPDATE-STATUS = 'Y'
                            DISPLAY 
                  "Update the status [1] TO-DO, [2] ONGOING, [3] DONE: " 
                            WITH NO ADVANCING
                            ACCEPT USER-TASK-STATUS
                            MOVE USER-TASK-STATUS TO TASKS-STATUS
                            MOVE 'Y' TO USER-CHANGES-MADE
                            EVALUATE USER-TASK-STATUS
                            WHEN '1'
                                MOVE 1 TO TASKS-STATUS
                                DISPLAY 
                                "Status updated to TO-DO."
                            WHEN '2'
                                MOVE 2 TO TASKS-STATUS
                                DISPLAY 
                                "Status updated to ONGOING."
                            WHEN '3'
                                MOVE 3 TO TASKS-STATUS
                                DISPLAY 
                                "Status updated to DONE."
                            WHEN OTHER
                                DISPLAY 
                                "Invalid status selection, try again."
                                CONTINUE
                        END-EVALUATE
                        END-IF

                        DISPLAY 
                        "> Do you want to update the due date (Y/N)? "
                        WITH NO ADVANCING
                        ACCEPT USER-UPDATE-DATE
      
                        IF USER-UPDATE-DATE = 'Y'
                            DISPLAY
                            "> Enter the new due date (MM/DD/YYYY): " 
                            WITH NO ADVANCING
                            ACCEPT USER-TASK-DATE
                            MOVE USER-TASK-DATE TO TASKS-DATE
                            MOVE 'Y' TO USER-CHANGES-MADE
                            DISPLAY "Due date updated."
                        END-IF
                        
                        REWRITE TASKS-FILE-RECORD
                        MOVE 'Y' TO EOF 
                    ELSE
                        MOVE 'X' TO USER-CHANGES-MADE
                    END-IF
           END-READ
           END-PERFORM
           CLOSE TASKS-FILE

           DISPLAY " "
           IF USER-CHANGES-MADE = 'Y'
               DISPLAY "(\_/)  All done! Your task has been updated."
               DISPLAY "(o.o)  Keep up the good work!"
               DISPLAY "(> <)  "
               DISPLAY " "
               PERFORM BADGE-LEVEL-III
               PERFORM DASHBOARD
           ELSE IF USER-CHANGES-MADE = 'X'
               DISPLAY "(\_/)  Task not found! Please try again."
               DISPLAY "(o.o)  Make sure to enter the correct task."
               DISPLAY "(> <)  "
               DISPLAY " "
               PERFORM DASHBOARD
           ELSE
               DISPLAY "(\_/)  No changes were made to your task."
               DISPLAY "(o.o)  Maybe next time!"
               DISPLAY "(> <)  "
               DISPLAY " "
               PERFORM DASHBOARD
           END-IF.
       
       DISPLAY-TODO-TASKS.
           OPEN INPUT TASKS-FILE
           MOVE 'N' TO EOF
           MOVE 'N' TO TASKS-FOUND

           DISPLAY " "
           DISPLAY "___ _     _  _ "
           DISPLAY " | / \---| \/ \"
           DISPLAY " | \_/   |_/\_/"
           DISPLAY " "
           MOVE CURRENT-SESSION TO TASKS-USER

           PERFORM UNTIL EOF = 'Y'
               READ TASKS-FILE INTO TASKS-FILE-RECORD
                   AT END
                       MOVE 'Y' TO EOF
                   NOT AT END
                       IF TASKS-USER = CURRENT-SESSION 
                       AND TASKS-STATUS = 1
                           DISPLAY"> "TASKS-DESCRIPTION"Due " TASKS-DATE
                           DISPLAY " "
                           MOVE 'Y' TO TASKS-FOUND
                       END-IF
               END-READ
           END-PERFORM

           IF TASKS-FOUND = 'N'
               DISPLAY "              -- No tasks to do --"
           END-IF

           CLOSE TASKS-FILE.
       
       DISPLAY-ONGOING-TASKS.
           OPEN INPUT TASKS-FILE
           MOVE 'N' TO EOF
           MOVE 'N' TO TASKS-FOUND
           DISPLAY " "
           DISPLAY " _     __ _ ___    __"
           DISPLAY "/ \|\|/__/ \ | |\|/__"
           DISPLAY "\_/| |\_|\_/_|_| |\_|"
           DISPLAY " "

           PERFORM UNTIL EOF = 'Y'
               READ TASKS-FILE INTO TASKS-FILE-RECORD
                   AT END
                       MOVE 'Y' TO EOF
                   NOT AT END
                       IF TASKS-USER = CURRENT-SESSION 
                       AND TASKS-STATUS = 2
                           DISPLAY"> "TASKS-DESCRIPTION"Due " TASKS-DATE
                           DISPLAY " "
                           MOVE 'Y' TO TASKS-FOUND
                       END-IF
               END-READ
           END-PERFORM

           IF TASKS-FOUND = 'N'
               DISPLAY "              -- No tasks to do --"
           END-IF
      
           CLOSE TASKS-FILE.
       
       DISPLAY-DONE-TASKS.
           OPEN INPUT TASKS-FILE
           MOVE 'N' TO EOF
           MOVE 'N' TO TASKS-FOUND

           DISPLAY " "
           DISPLAY " _  _     __"
           DISPLAY "| \/ \|\||_ "
           DISPLAY "|_/\_/| ||__"
           DISPLAY " "
           PERFORM UNTIL EOF = 'Y'
               READ TASKS-FILE INTO TASKS-FILE-RECORD
                   AT END
                       MOVE 'Y' TO EOF
                   NOT AT END
                       IF TASKS-USER = CURRENT-SESSION 
                       AND TASKS-STATUS = 3
                           DISPLAY"> "TASKS-DESCRIPTION"Due " TASKS-DATE
                           DISPLAY " "
                           MOVE 'Y' TO TASKS-FOUND
                       END-IF
               END-READ
           END-PERFORM

           CLOSE TASKS-FILE

           IF TASKS-FOUND = 'N'
               DISPLAY "              -- No tasks to do --"
           ELSE 
               PERFORM BADGE-LEVEL-IV
           END-IF.

       REMINDER.
           OPEN INPUT TASKS-FILE
           OPEN INPUT ACCOUNT-FILE
           
           MOVE ZERO TO COUNT-DUE-TODAY
           MOVE CURRENT-SESSION TO TASKS-USER

           MOVE 'N' TO EOF

           PERFORM UNTIL EOF = 'Y'
               READ TASKS-FILE INTO TASKS-FILE-RECORD
                   AT END
                       MOVE 'Y' TO EOF
                   NOT AT END
                       IF TASKS-USER = CURRENT-SESSION
                          AND (TASKS-STATUS = 1 OR TASKS-STATUS = 2)
                          AND TASKS-DATE = TODAY-DATE
                          ADD 1 TO COUNT-DUE-TODAY
                       END-IF
               END-READ
           END-PERFORM

           IF COUNT-DUE-TODAY > 0
               DISPLAY " "
               DISPLAY 
               "(\_/)  REMINDERS!"
               DISPLAY 
               "(o.o)  You have " COUNT-DUE-TODAY " task(s) due today!"
               DISPLAY "(> <)  Stay on track and complete them on time."
           ELSE
               DISPLAY " "
               DISPLAY "(\_/)  No tasks due today! Enjoy your day."
               DISPLAY 
               "(o.o)  Keep up the good work and stay motivated."
               DISPLAY "(> <)  "
           END-IF

           CLOSE TASKS-FILE
           CLOSE ACCOUNT-FILE.

       GET-VALID-ACTION-DASHBOARD.
           DISPLAY "> Enter your choice: " WITH NO ADVANCING.
           ACCEPT USER-CHOICE
           
           EVALUATE USER-CHOICE
               WHEN '1'
                   PERFORM TASKS
               WHEN '2'
                   PERFORM STAT
               WHEN '3'
                   PERFORM SUPPORT
               WHEN '4'
                   PERFORM SETTINGS
               WHEN OTHER
                   DISPLAY " "
                   DISPLAY 
                   "(\_/)  Oops! Please select only between 1 and 4."
                   DISPLAY 
                   "(o.o)  Let's try again. :)"
                   DISPLAY "(> <) "
                   DISPLAY " "
                   PERFORM GET-VALID-ACTION-DASHBOARD
           END-EVALUATE.
       
       LOGIN.
           DISPLAY " "
           DISPLAY "(\_/)  Login to your account!"
           DISPLAY "(o.o)  Start your study session with Mimi."
           DISPLAY "(> <)  "
           DISPLAY " ".
           DISPLAY "> Enter your username: " WITH NO ADVANCING.
           ACCEPT USER-NAME-INPUT.
           DISPLAY "> Enter your password: " WITH NO ADVANCING.
           ACCEPT USER-PASSWORD-INPUT.

           OPEN I-O ACCOUNT-FILE.

           MOVE 'N' TO ACCOUNT-FOUND.
           MOVE 'N' TO EOF.

           PERFORM VALIDATE-USER-ACCOUNTS UNTIL EOF = 'Y' 
           OR ACCOUNT-FOUND = 'Y'.

           IF ACCOUNT-FOUND = 'Y'
               MOVE USER-NAME-INPUT TO CURRENT-SESSION
               DISPLAY " "
               DISPLAY 
               "(\_/)  Welcome! " CURRENT-SESSION
               DISPLAY 
               "(o.o)  You have successfully logged in."
               DISPLAY 
               "(> <)  Enjoy your study session."
               DISPLAY " "
               PERFORM MOOD-TRACKER
           ELSE
               DISPLAY " "
               DISPLAY 
               "(\_/)  Invalid username or password!"
               DISPLAY 
               "(o.o)  Please check your credentials and try again."
               DISPLAY 
               "(> <)  If you don't have an account, please sign up."
               DISPLAY " "
               CLOSE ACCOUNT-FILE
               PERFORM ACCOUNT-MENU
           END-IF.

           CLOSE ACCOUNT-FILE.

       VALIDATE-USER-ACCOUNTS.
           READ ACCOUNT-FILE INTO ACCOUNT-FILE-RECORD
               AT END
                   MOVE 'Y' TO EOF
               NOT AT END
                   IF USER-NAME-INPUT = USER-NAME 
                   AND USER-PASSWORD-INPUT = USER-PASSWORD
                       MOVE 'Y' TO ACCOUNT-FOUND
                   END-IF
               END-READ.
       
       FORGOT-PASSWORD.
           DISPLAY " "
           DISPLAY "(\_/)  Forgot your password?"
           DISPLAY "(o.o)  No worries! We got you covered."
           DISPLAY "(> <)  "
           DISPLAY " ".
           DISPLAY "> Enter your username: " WITH NO ADVANCING
           ACCEPT USER-NAME-INPUT
       
           MOVE 'N' TO EOF.

           OPEN INPUT ACCOUNT-FILE

           PERFORM CHECK-EXISTING-ACCOUNTS UNTIL EOF = 'Y'

           IF ACCOUNT-FOUND = 'Y'
                DISPLAY " "
                DISPLAY "(\_/)  Account found! Verification required."
                DISPLAY 
                "(o.o)  When is your account created? (MM/DD/YYYY)"
                DISPLAY 
                "(> <)  "
                DISPLAY " "
                DISPLAY "> Enter the date (MM/DD/YYYY): "
                WITH NO ADVANCING
                ACCEPT USER-DATE-INPUT
                IF USER-DATE-INPUT = COMPARE-DATE
                    DISPLAY " "
                    DISPLAY 
                    "(\_/)  Account verified!"
                    DISPLAY 
                    "(o.o)  Here's your password: " RETRIEVE-PASS
                    DISPLAY "(> <)  "
                    DISPLAY " "
                    MOVE 'N' TO ACCOUNT-FOUND
                    CLOSE ACCOUNT-FILE
                    PERFORM ACCOUNT-MENU
                ELSE
                    DISPLAY " "
                    DISPLAY 
                    "(\_/)  Oops, you entered the wrong date."
                    DISPLAY 
                    "(o.o)  Make sure to enter the correct date"
                    DISPLAY "(> <)  and follow the format (MM/DD/YYYY)"
                    DISPLAY " "
                    CLOSE ACCOUNT-FILE
                    MOVE 'N' TO ACCOUNT-FOUND
                    PERFORM ACCOUNT-MENU
                END-IF
           ELSE
                DISPLAY " "
                DISPLAY "(\_/)  Account not found!"
                DISPLAY "(o.o)  Please check your username."
                DISPLAY "(> <)  "
                DISPLAY " "
                MOVE 'N' TO ACCOUNT-FOUND
                CLOSE ACCOUNT-FILE
                PERFORM ACCOUNT-MENU
           END-IF
           
           MOVE 'N' TO ACCOUNT-FOUND
           CLOSE ACCOUNT-FILE.
       
       SIGN-UP.
           DISPLAY " "
           DISPLAY "(\_/)  Create your very own account!"
           DISPLAY "(o.o)  Make sure to remember your credentials."
           DISPLAY "(> <)  "
           DISPLAY " ".
           DISPLAY "> Create your username: " WITH NO ADVANCING.
           ACCEPT USER-NAME-INPUT.
           
           OPEN INPUT ACCOUNT-FILE.
           
           MOVE 'N' TO ACCOUNT-FOUND.
           MOVE 'N' TO EOF.
           
           PERFORM CHECK-EXISTING-ACCOUNTS UNTIL EOF = 'Y'
           
           IF ACCOUNT-FOUND = 'Y'
               DISPLAY " "
               DISPLAY "(\_/)  Account already exists!"
               DISPLAY "(o.o)  The username is already taken."
               DISPLAY "(> <)  Please choose a different username."
               CLOSE ACCOUNT-FILE
               PERFORM ACCOUNT-MENU
           ELSE
               CLOSE ACCOUNT-FILE
               DISPLAY "> Create your password: " WITH NO ADVANCING
               ACCEPT USER-PASSWORD-INPUT

               ACCEPT RAW-DATE FROM DATE YYYYMMDD
               MOVE RAW-DATE(5:2) TO TODAY-MONTH
               MOVE RAW-DATE(7:2) TO TODAY-DAY
               MOVE RAW-DATE(1:4) TO TODAY-YEAR
      
               STRING TODAY-MONTH DELIMITED BY SIZE
                      "/" DELIMITED BY SIZE
                      TODAY-DAY DELIMITED BY SIZE
                      "/" DELIMITED BY SIZE
                      TODAY-YEAR DELIMITED BY SIZE
                      INTO TODAY-DATE
                       
               OPEN EXTEND ACCOUNT-FILE
        
               MOVE USER-NAME-INPUT TO USER-NAME
               MOVE USER-PASSWORD-INPUT TO USER-PASSWORD
               MOVE TODAY-DATE TO USER-ACCOUNT-CREATED
               WRITE ACCOUNT-FILE-RECORD
           
               DISPLAY " "
               DISPLAY 
               "(\_/)  Account created successfully!"
               DISPLAY 
               "(o.o)  You can now log in with your credentials."
               DISPLAY 
               "(> <) "
               DISPLAY " "
               CLOSE ACCOUNT-FILE
               PERFORM ACCOUNT-MENU
           END-IF.

           CLOSE ACCOUNT-FILE.
           
       CHECK-EXISTING-ACCOUNTS.
           READ ACCOUNT-FILE INTO ACCOUNT-FILE-RECORD
               AT END
                   MOVE 'Y' TO EOF
               NOT AT END
                   IF USER-NAME-INPUT = USER-NAME
                   MOVE USER-ACCOUNT-CREATED to COMPARE-DATE
                   MOVE USER-PASSWORD TO RETRIEVE-PASS
                   MOVE 'Y' TO ACCOUNT-FOUND
                   END-IF
               END-READ.
       
       MOOD-TRACKER.
           CLOSE ACCOUNT-FILE
           
           OPEN INPUT MOOD-FILE

           MOVE 'N' TO MOOD-FOUND
           MOVE 'N' TO EOF

           PERFORM CHECK-MOOD-RECORDS UNTIL EOF ='Y'
           CLOSE MOOD-FILE

           IF MOOD-FOUND = 'Y'
               PERFORM DASHBOARD
           ELSE 
               DISPLAY " "
               DISPLAY "TODAY'S DATE IS:  " TODAY-DATE
               DISPLAY " "
               DISPLAY "(\_/)  How is the rate of your mood today? :)"
               DISPLAY "(o.o)  [1] Happy  [2] Sad  [3] Tired  [4] Angry"
               DISPLAY "(> <)"
               DISPLAY " "
               PERFORM GET-VALID-MOOD

               OPEN EXTEND MOOD-FILE
               MOVE TODAY-DATE TO MOOD-DATE
               MOVE MOOD-STATUS-INPUT TO MOOD-STATUS
               MOVE CURRENT-SESSION TO MOOD-USER
               WRITE MOOD-FILE-RECORD
               CLOSE MOOD-FILE
                EVALUATE MOOD-STATUS-INPUT
                   WHEN '1'
                    DISPLAY 
                    "(\_/)  We're glad you're feeling happy!"
                    DISPLAY 
                    "(o.o)  Spread your joy to others today."
                    DISPLAY 
                    "(> <)  Keep that positive vibe going!"
                   WHEN '2'
                    DISPLAY 
                    "(\_/)  It's okay to feel sad sometimes."
                    DISPLAY 
                    "(o.o)  Remember, tough times don’t last."
                    DISPLAY 
                    "(> <)  Take a deep breath, and keep going."
                   WHEN '3'
                    DISPLAY 
                    "(\_/)  Feeling tired? Here mwah-mwah. :)"
                    DISPLAY 
                    "(o.o)  Take a moment to rest and recharge."
                    DISPLAY 
                    "(> <)  You’ve got this, one step at a time!"
                   WHEN '4'
                    DISPLAY 
                    "(\_/)  Anger is a natural emotion."
                    DISPLAY 
                    "(o.o)  Try channeling it into something creative!"
                    DISPLAY 
                    "(> <)  Remember, you’re in control."
                   WHEN OTHER
                    DISPLAY 
                    "(\_/)  Thank you for sharing your mood!"
                    DISPLAY 
                    "(o.o)  Stay strong and take care!"
                    DISPLAY 
                    "(> <)  Better days are ahead!"
               END-EVALUATE

               PERFORM DASHBOARD
           END-IF. 
       
       GET-VALID-MOOD.
           DISPLAY "> Enter your mood rate: " WITH NO ADVANCING
           ACCEPT MOOD-STATUS-INPUT

           IF MOOD-STATUS-INPUT = '1' OR MOOD-STATUS-INPUT = '2' 
           OR MOOD-STATUS-INPUT = '3' OR MOOD-STATUS-INPUT = '4'
               DISPLAY " "
           ELSE
               DISPLAY 
               "(\_/)  Oops! Please enter a number between 1 and 4."
               DISPLAY 
               "(o.o)  How is the rate of your mood today? :)"
               DISPLAY 
               "(> <)  [1] Happy  [2] Sad  [3] Tired  [4] Angry"
               PERFORM GET-VALID-MOOD
           END-IF.
       
       CHECK-MOOD-RECORDS.
           READ MOOD-FILE INTO MOOD-FILE-RECORD
               AT END
                   MOVE 'Y' TO EOF
               NOT AT END
                   IF MOOD-USER = CURRENT-SESSION 
                   AND MOOD-DATE = TODAY-DATE
                       MOVE 'Y' TO MOOD-FOUND
                   END-IF
           END-READ.
       
       SETTINGS.
           DISPLAY " "
           DISPLAY "                                        .--------."
           DISPLAY "    __  ___  ___  ___  _  _  _  __  __  |IV.--.  |"
           DISPLAY "   / _|| __||_ _||_ _|| || \| |/ _|/ _  |  :/\:  |"
           DISPLAY "   \_ \| _|  | |  | | | || \\ ( |_n\_ \ |  (__)  |"
           DISPLAY "   |__/|___| |_|  |_| |_||_|\_|\__/|__/ |  '--'IV|"
           DISPLAY "                                        '--------'"
           DISPLAY "==================================================="
           DISPLAY "|[1] YOUR ACCOUNT       | [2] LOGOUT              |"
           DISPLAY"==================================================="
           DISPLAY " "
           DISPLAY "(\_/)"
           DISPLAY "(o.o)  What would you like to do? :)"
           DISPLAY "(> <) "
           DISPLAY " "
           DISPLAY "                                      ___________"
           DISPLAY "                                     || BACK (0)||"
           DISPLAY "                                     ||_________||"
           DISPLAY "                                     |/_________\|".
           PERFORM GET-VALID-ACTION-SETTINGS.
       
       GET-VALID-ACTION-SETTINGS.
           DISPLAY "> Enter your choice: " WITH NO ADVANCING.
           ACCEPT USER-CHOICE
           
           EVALUATE USER-CHOICE
               WHEN '0'
                   PERFORM DASHBOARD
               WHEN '1'
                   OPEN INPUT ACCOUNT-FILE
                   MOVE 'N' TO ACCOUNT-FOUND

                   PERFORM GET-PASSWORD-FOR-USER

                   CLOSE ACCOUNT-FILE
                   DISPLAY " "
                   DISPLAY "(\_/)  Here's your account information:"
                   DISPLAY "(o.o)  Username: "CURRENT-SESSION
                   DISPLAY "(> <)  Password: "CURRENT-PASSWORD
                   DISPLAY " "
                   DISPLAY 
                   "> Would you like to change your password? (Y/N): "
                   WITH NO ADVANCING
                   ACCEPT USER-CONFIRM-FLAG

                   IF USER-CONFIRM-FLAG = 'Y'
                       DISPLAY " "
                       DISPLAY 
                       "(\_/)  Change your password!"
                       DISPLAY 
                       "(o.o)  Make sure to remember your new password."
                       DISPLAY 
                       "(> <)  "
                       DISPLAY " "
                       DISPLAY "> Enter your new password: "
                       WITH NO ADVANCING
                       ACCEPT NEW-PASSWORD-INPUT
                       DISPLAY " "
                       DISPLAY "> Re-enter your new password: " 
                       WITH NO ADVANCING
                       ACCEPT CONFIRM-PASSWORD-INPUT

                       IF NEW-PASSWORD-INPUT = CONFIRM-PASSWORD-INPUT
                       OPEN I-O ACCOUNT-FILE

                       MOVE 'N' TO EOF

                       PERFORM UNTIL EOF = 'Y'
                           READ ACCOUNT-FILE INTO ACCOUNT-FILE-RECORD
                               AT END
                                   MOVE 'Y' TO EOF
                               NOT AT END
                                   IF USER-NAME = CURRENT-SESSION
                                       MOVE NEW-PASSWORD-INPUT 
                                       TO USER-PASSWORD
                                       REWRITE ACCOUNT-FILE-RECORD
                                       DISPLAY " "
                                       DISPLAY 
                                "(\_/)  Password updated successfully!"
                                       DISPLAY 
                    "(o.o)  You can now log in with your new password."
                                       DISPLAY "(> <)  "
                                       CLOSE ACCOUNT-FILE
                                       PERFORM SETTINGS
                                   END-IF
                           END-READ
                       END-PERFORM
                       CLOSE ACCOUNT-FILE
                       ELSE
                           DISPLAY " "
                           DISPLAY "(\_/)  Passwords do not match!"
                           DISPLAY "(o.o)  Please try again."
                           DISPLAY "(> <)  "
                           PERFORM SETTINGS
                       END-IF
                  END-IF
                  PERFORM GET-VALID-ACTION-SETTINGS
               WHEN '2'
                   MOVE SPACES TO CURRENT-SESSION
                   PERFORM MAIN
                   PERFORM ACCOUNT-MENU
               WHEN OTHER
                   DISPLAY " "
                   DISPLAY 
                   "(\_/)  Oops! Please select only between 1 and 2."
                   DISPLAY 
                   "(o.o)  Let's try again. :)"
                   DISPLAY "(> <) "
                   DISPLAY " "
                   PERFORM GET-VALID-ACTION-SETTINGS
           END-EVALUATE.

       GET-PASSWORD-FOR-USER.
           READ ACCOUNT-FILE INTO ACCOUNT-FILE-RECORD
               AT END
                   MOVE 'Y' TO EOF
               NOT AT END
                   IF USER-NAME = CURRENT-SESSION 
                       MOVE USER-PASSWORD TO CURRENT-PASSWORD
                   END-IF
           END-READ.

       SUPPORT.
           DISPLAY " "
           DISPLAY "                                     .----------."
           DISPLAY "       __  _ _  ___ ___  _  ___ ___  |III.--.   |"
           DISPLAY "      / _|| | || o \ o \/ \| o \_ _| |   :/\:   |"
           DISPLAY "      \_ \| U ||  _/  _( o )   /| |  |   (__)   |"
           DISPLAY "      |__/|___||_| |_|  \_/|_|\\|_|  |   '--'III|"
           DISPLAY "                                     '----------'"
           DISPLAY "==================================================="
           DISPLAY "|[1] FAQs              | [2] CONTACTS             |"
           DISPLAY"==================================================="
           DISPLAY " "
           DISPLAY "(\_/)"
           DISPLAY "(o.o)  What would you like to do? :)"
           DISPLAY "(> <) "
           DISPLAY " "
           DISPLAY "                                      ___________"
           DISPLAY "                                     || BACK (0)||"
           DISPLAY "                                     ||_________||"
           DISPLAY "                                     |/_________\|".
           PERFORM GET-VALID-ACTION-SUPPORT.
       
       GET-VALID-ACTION-SUPPORT.
           DISPLAY "> Enter your choice: " WITH NO ADVANCING.
           ACCEPT USER-CHOICE
           
           EVALUATE USER-CHOICE
               WHEN '0'
                   PERFORM DASHBOARD
               WHEN '1'
                   PERFORM FAQS
                   PERFORM GET-VALID-ACTION-SUPPORT
               WHEN '2'
                   PERFORM CONTACTS
                   PERFORM GET-VALID-ACTION-SUPPORT
               WHEN OTHER
                   DISPLAY " "
                   DISPLAY 
                   "(\_/)  Oops! Please select only between 1 and 2."
                   DISPLAY 
                   "(o.o)  Let's try again. :)"
                   DISPLAY "(> <) "
                   DISPLAY " "
                   PERFORM GET-VALID-ACTION-SUPPORT
           END-EVALUATE.
       
       CONTACTS.
           DISPLAY" "
           DISPLAY"   ___   _     ___      _____  ____   __    _    "
           DISPLAY"  / / \ | | | | |_)      | |  | |_   / /\  | |\/|"
           DISPLAY"  \_\_/ \_\_/ |_| \      |_|  |_|__ /_/--\ |_|  |"
           DISPLAY" "
           DISPLAY "PROJECT MANAGER           Makaila Nelrei C. Indaya"
           DISPLAY "TECHNICAL DESIGNER        Lancilot Jannse S. Tibay"
           DISPLAY "TECHNICAL DESIGNER        Denisse M. Victoria"
           DISPLAY "DEVELOPER                 Cian Jake F. Francisco"
           DISPLAY "DEVELOPER                 Eliniel V. Recio"
           DISPLAY "SYSTEM ANALYST            Raphael Andrei G. Latoy"
           DISPLAY "SYSTEM ANALYST            Cherry Mae O. Dampios"
           DISPLAY "BUSINESS ANALYST          Nadya Sofia G. Bernabe"
           DISPLAY "BUSINESS ANALYST          Eloisa Marie O. Afable"
           DISPLAY "TECHNICAL WRITER          Gene Margarett A. Sy"
           DISPLAY "TECHNICAL WRITER          Makaila Nelrei C. Indaya"
           DISPLAY " "
           DISPLAY "Professor: Ms. Angie Payne"
           DISPLAY "Subject:   Computer Programming 3 (INTE 201)"
           DISPLAY " "
           DISPLAY "   _     _   "
           DISPLAY "  (c).-.(c)                                        "
           DISPLAY "   / ._. \           Empower Your Education        "
           DISPLAY " __\( Y )/__           One Card at a Time!         "
           DISPLAY "(_.-/'-'\\-._)                                     "
           DISPLAY "   ||   ||              Merry Christmas &          "
           DISPLAY " _.' `-' '._             Happy New Year!           "    
           DISPLAY "(.-./`-'\.-.)                                      "
           DISPLAY " `-'     `-'                 © 2024              "
           DISPLAY " "
           DISPLAY "                                      ___________"
           DISPLAY "                                     || BACK (0)||"
           DISPLAY "                                     ||_________||"
           DISPLAY "                                     |/_________\|".
       
       FAQS.
           DISPLAY "                                                  "
           DISPLAY " ____    ______  _____                             "
           DISPLAY "/\  _`\ /\  _  \/\  __`\                           "
           DISPLAY "\ \ \L\_\ \ \L\ \ \ \/\ \    ____                  "
           DISPLAY " \ \  _\/\ \  __ \ \ \ \ \  /',__\                 "
           DISPLAY "  \ \ \/  \ \ \/\ \ \ \\'\\/\__, `\                "
           DISPLAY "   \ \_\   \ \_\ \_\ \___\_\/\____/                "
           DISPLAY "    \/_/    \/_/\/_/\/__//_/\/___/                 "
           DISPLAY " "
           DISPLAY "1. What is this program about? "
           DISPLAY "   > Edubingo is a study companion program         "
           DISPLAY "     designed to help students track their tasks   "
           DISPLAY "     and their moods. Also to make COBOL fun and   "
           DISPLAY "     and interactive as it always deals with       "
           DISPLAY "     with busineses serious so we add some features"
           DISPLAY "     to make it fun".
           DISPLAY " "
           DISPLAY "2. Is there are tutorial for this program?         "
           DISPLAY "   > Of course, because we care about our users. <3"
           DISPLAY "     You may access the manual using this link:    "
           DISPLAY "            https://shorturl.at/2lyV7             "
           DISPLAY "                                                   "
           DISPLAY "     Rest assured this link is safe and secured.   "
           DISPLAY " "
           DISPLAY "                                      ___________ "
           DISPLAY "                                     || BACK (0)||"
           DISPLAY "                                     ||_________||"
           DISPLAY "                                     |/_________\|".

       TASKS.
           DISPLAY " "
           DISPLAY "                                      .--------."
           DISPLAY "      _________   _____ __ ______     |I .--.  |"
           DISPLAY "     /_  __/   | / ___// //_/ ___/    |  :/\:  |"
           DISPLAY "      / / / /| | \__ \/ ,<  \__ \     |  (__)  |"
           DISPLAY "     / / / ___ |___/ / /| |___/ /     |  '--' I|"
           DISPLAY "    /_/ /_/  |_/____/_/ |_/____/      '--------'"
           DISPLAY "==================================================="
           DISPLAY "|[1] ADD TASK            | [2] UPDATE TASK        |"
           DISPLAY"===================================================="

           DISPLAY " "
           DISPLAY "(\_/)"
           DISPLAY "(o.o)  What would you like to do? :)"
           DISPLAY "(> <) "
           DISPLAY " "
           DISPLAY " "
           DISPLAY "                                      ___________"
           DISPLAY "                                     || BACK (0)||"
           DISPLAY "                                     ||_________||"
           DISPLAY "                                     |/_________\|"
           PERFORM GET-VALID-ACTION-TASKS.

       GET-VALID-ACTION-TASKS.
            DISPLAY "> Enter your choice: " WITH NO ADVANCING.
            ACCEPT USER-CHOICE
              
            EVALUATE USER-CHOICE
            WHEN '0'
                PERFORM DASHBOARD
            WHEN '1'
                PERFORM ADD-TASK
            WHEN '2'
                PERFORM UPDATE-TASK
            WHEN OTHER
                DISPLAY " "
                DISPLAY 
                "(\_/)  Oops! Please select only between 1 and 2."
                DISPLAY 
                "(o.o)  Let's try again. :)"
                DISPLAY "(> <) "
                DISPLAY " "
                PERFORM GET-VALID-ACTION-TASKS
            END-EVALUATE.
        
       ADD-TASK.
            DISPLAY " "
            DISPLAY "(\_/)  Add a new task!"
            DISPLAY "(o.o)  Stay organized and productive."
            DISPLAY "(> <)  "
            DISPLAY " "
            DISPLAY 
            "> Enter the task description: " WITH NO ADVANCING
            ACCEPT USER-TASK-DESCRIPTION
            DISPLAY 
            "> Enter the due date (MM/DD/YYYY): " WITH NO ADVANCING
            ACCEPT USER-DATE-INPUT
            MOVE USER-DATE-INPUT TO TASKS-DATE
            DISPLAY
            "> Enter the status [1] TO-DO, [2] ONGOING [3] DONE: " 
            WITH NO ADVANCING
            ACCEPT USER-TASK-STATUS
            
            OPEN INPUT ACCOUNT-FILE
            OPEN EXTEND TASKS-FILE
            
            MOVE CURRENT-SESSION TO TASKS-USER
            MOVE USER-TASK-DESCRIPTION TO TASKS-DESCRIPTION
            MOVE USER-DATE-INPUT TO TASKS-DATE
            MOVE USER-TASK-STATUS TO TASKS-STATUS

            WRITE TASKS-FILE-RECORD

            DISPLAY " "
            DISPLAY "(\_/)  Task added successfully!"
            DISPLAY "(o.o)  Keep up the good work."
            DISPLAY "(> <)  "
            DISPLAY " "

            CLOSE ACCOUNT-FILE
            CLOSE TASKS-FILE
            PERFORM BADGE-LEVEL-II
            PERFORM DASHBOARD.
       
       STAT.
           DISPLAY " "
           DISPLAY "                                    .----------."
           DISPLAY "   __  _____  _   _____       __    |II .--.   |"
           DISPLAY "  / _\/__   \/_\ /__   \/\ /\/ _\   |   :/\:   |"
           DISPLAY "  \ \   / /\//_\\  / /\/ / \ \ \    |   (__)   |"
           DISPLAY "   \ \ / / /  _  \/ /  \ \_/ /\ \   |   '--' II|"
           DISPLAY "  \__/ \/  \_/ \_/\/    \___/\__/   '----------'"
           DISPLAY "==================================================="
           DISPLAY "| [1] LOGS             |  [2] ACHIEVEMENTS        |"
           DISPLAY"===================================================="
           DISPLAY " "
           DISPLAY "(\_/)"
           DISPLAY "(o.o)  What would you like to do? :)"
           DISPLAY "(> <) "
           DISPLAY " "
           DISPLAY " "
           DISPLAY "                                      ___________"
           DISPLAY "                                     || BACK (0)||"
           DISPLAY "                                     ||_________||"
           DISPLAY "                                     |/_________\|"
           DISPLAY " "
           PERFORM GET-VALID-ACTION-STAT.
       
       GET-VALID-ACTION-STAT.
           DISPLAY " "
           DISPLAY "> Enter your choice: " WITH NO ADVANCING.
           ACCEPT USER-CHOICE
              
           EVALUATE USER-CHOICE
           WHEN '0'
                PERFORM DASHBOARD
           WHEN '1'
                PERFORM LOGS
           WHEN '2'
                PERFORM ACHIEVEMENTS
           WHEN OTHER
                DISPLAY " "
                DISPLAY 
                "(\_/)  Oops! Please select only between 1 and 2."
                DISPLAY 
                "(o.o)  Let's try again. :)"
                DISPLAY "(> <) "
                DISPLAY " "
                PERFORM GET-VALID-ACTION-STAT
           END-EVALUATE.
        
       LOGS.
           OPEN INPUT MOOD-FILE
           MOVE 'N' TO EOF
           DISPLAY " "
           DISPLAY 
           "(\_/)  Hi! " CURRENT-SESSION
           DISPLAY 
           "(o.o)  Here's the list of your account logs:"
           DISPLAY "(> <)  "
           DISPLAY " "
           DISPLAY 
           "Date                                     Mood"
           DISPLAY 
           "----------------------------------------------"
           PERFORM UNTIL EOF = 'Y'
               READ MOOD-FILE INTO MOOD-FILE-RECORD
                   AT END
                       MOVE 'Y' TO EOF
                   NOT AT END
                       IF MOOD-USER = CURRENT-SESSION
                           EVALUATE MOOD-STATUS
                              WHEN 1
                                  MOVE "Happy" TO MOOD-LOG
                              WHEN 2
                                  MOVE "Sad" TO MOOD-LOG
                              WHEN 3
                                  MOVE "Tired" TO MOOD-LOG
                              WHEN 4
                                  MOVE "Angry" TO MOOD-LOG
                           END-EVALUATE
                       DISPLAY MOOD-DATE "                     "MOOD-LOG
                       END-IF
               END-READ
           END-PERFORM

           CLOSE MOOD-FILE
           
           PERFORM GET-VALID-ACTION-STAT.    
       
       ACHIEVEMENTS.
           PERFORM BADGES.
       
       BADGES.
           DISPLAY " "
           DISPLAY "(\_/)  "
           DISPLAY "(o.o)  Here are the list of badges:"
           DISPLAY "(> <)  "
           DISPLAY " "

           DISPLAY ".----------."
           DISPLAY "|I .----.  |    > User able to successfully login"
           DISPLAY "|   :/\:   |"
           DISPLAY "|   (__)   |"
           DISPLAY "|  '----'I |"
           DISPLAY "'----------'"
           
           DISPLAY ".----------."
           DISPLAY "|II.----.  |    > User able to add a first task"
           DISPLAY "|   (\/)   |"
           DISPLAY "|    \/    |"
           DISPLAY "|  '----'II|"
           DISPLAY "'----------'"
           
           DISPLAY ".----------."
           DISPLAY "|III.--.   |    > User able to update a task"
           DISPLAY "|    /\    |"
           DISPLAY "|   /__\   |"
           DISPLAY "|   '--'III|"
           DISPLAY "'----------'"

           DISPLAY ".----------."
           DISPLAY "|IV.----.  |    > User completed a task"
           DISPLAY "|    /\    |"
           DISPLAY "|    \/    |"
           DISPLAY "|  '----'IV|"
           DISPLAY "'----------'"

           DISPLAY " "
           DISPLAY "(\_/)  "
           DISPLAY "(o.o)  Your current level:"
           DISPLAY "(> <)  "
           DISPLAY " "
           PERFORM DISPLAY-USER-CURRENT-BADGE
           DISPLAY "                                      ___________"
           DISPLAY "                                     || BACK (0)||"
           DISPLAY "                                     ||_________||"
           DISPLAY "                                     |/_________\|"
           PERFORM GET-VALID-ACTION-STAT.
       
       DISPLAY-USER-CURRENT-BADGE.
           OPEN INPUT BADGES-FILE
           MOVE 'N' TO EOF
           MOVE 'N' TO BADGE-FOUND

           PERFORM UNTIL EOF = 'Y'
               READ BADGES-FILE INTO BADGES-FILE-RECORD
                    AT END
                       MOVE 'Y' TO EOF
                    NOT AT END
                       IF BADGE-USER = CURRENT-SESSION
                          MOVE USER-CURRENT-BADGE TO BADGE-TYPE
                          EVALUATE BADGE-TYPE
                              WHEN 1
                                   DISPLAY ".----------."
                                   DISPLAY "|I .----.  |"
                DISPLAY "|   :/\:   |  Congrats on logging in!"
                DISPLAY "|   (__)   |  Keep up the good work. :)"
                                   DISPLAY "|  '----'I |"
                                   DISPLAY "'----------'"
                              WHEN 2
                                   DISPLAY ".----------."
                                   DISPLAY "|II.----.  |"
                DISPLAY "|  :(\/):  |  Congrats on adding a task!"
                DISPLAY "|    \/    |  Keep up the good work. :)"
                                   DISPLAY "|  '----'II|"
                                   DISPLAY "'----------'"
                              WHEN 3
                                  DISPLAY ".----------."
                                  DISPLAY "|III.--.   |"
                DISPLAY "|    /\    |  Congrats on updating a task!"
                DISPLAY "|   /__\   |  Keep up the good work. <3"
                                  DISPLAY "|   '--'III|"
                                  DISPLAY "'----------'"
                              WHEN 4
                                  DISPLAY ".----------."
                                  DISPLAY "|IV.----.  |"
                DISPLAY "|    /\    |  Congrats on completing a task"
                DISPLAY "|    \/    |  Keep up the good work. <3"
                                  DISPLAY "|  '----'IV|"
                                  DISPLAY "'----------'"
                          END-EVALUATE
                          MOVE 'Y' TO BADGE-FOUND
                       END-IF
               END-READ
           END-PERFORM
           
           IF BADGE-FOUND = 'N'
               DISPLAY "         No badge obtained yet."
           END-IF

           CLOSE BADGES-FILE.
           
       BADGE-LEVEL-I.
           MOVE 'N' TO EOF
           MOVE 1 TO BADGE
           OPEN INPUT ACCOUNT-FILE
           OPEN I-O BADGES-FILE

           PERFORM UNTIL EOF = 'Y'
               READ ACCOUNT-FILE INTO ACCOUNT-FILE-RECORD
                   AT END
                       MOVE 'Y' TO EOF
                   NOT AT END
                       IF USER-NAME = CURRENT-SESSION
                           MOVE 'N' TO RECORD-FOUND

                           PERFORM UNTIL EOF = 'Y'
                               READ BADGES-FILE INTO BADGES-FILE-RECORD
                                   AT END
                                       MOVE 'Y' TO EOF
                                   NOT AT END
                                    IF BADGE-USER = USER-NAME
                                    IF USER-CURRENT-BADGE < BADGE
                                    MOVE BADGE TO USER-CURRENT-BADGE
                                    REWRITE BADGES-FILE-RECORD
                                    DISPLAY " "
                                    DISPLAY
                                "(\_/)	Congratulations!"
                                    DISPLAY 
                                "(o.o)	You have obtained Badge I!"
                                    DISPLAY 
                                "(> <) "
                                    DISPLAY " "
                                    END-IF
                                    MOVE 'Y' TO RECORD-FOUND
                                    END-IF
                               END-READ
                           END-PERFORM

                           IF RECORD-FOUND = 'N'
                               CLOSE BADGES-FILE
                               OPEN EXTEND BADGES-FILE
                               IF USER-CURRENT-BADGE < BADGE
                               MOVE USER-NAME TO BADGE-USER
                               MOVE BADGE TO USER-CURRENT-BADGE
                               WRITE BADGES-FILE-RECORD
                               DISPLAY " "
                               DISPLAY "(\_/)  Congratulations!"
                               DISPLAY 
                               "(o.o)  You have obtained Badge I!"
                               DISPLAY "(> <)  "
                               DISPLAY " "
                               END-IF
                           END-IF
                       END-IF
               END-READ
           END-PERFORM
           
           CLOSE ACCOUNT-FILE
           CLOSE BADGES-FILE.   

       BADGE-LEVEL-II.
           MOVE 'N' TO EOF
           MOVE 2 TO BADGE
           OPEN INPUT ACCOUNT-FILE
           OPEN I-O BADGES-FILE

           PERFORM UNTIL EOF = 'Y'
               READ ACCOUNT-FILE INTO ACCOUNT-FILE-RECORD
                   AT END
                       MOVE 'Y' TO EOF
                   NOT AT END
                       IF USER-NAME = CURRENT-SESSION
                           MOVE 'N' TO RECORD-FOUND

                           PERFORM UNTIL EOF = 'Y'
                               READ BADGES-FILE INTO BADGES-FILE-RECORD
                                   AT END
                                       MOVE 'Y' TO EOF
                                   NOT AT END
                                    IF BADGE-USER = USER-NAME
                                    IF USER-CURRENT-BADGE < BADGE
                                    MOVE BADGE TO USER-CURRENT-BADGE
                                    REWRITE BADGES-FILE-RECORD
                                    DISPLAY " "
                                    DISPLAY
                                "(\_/)	Congratulations!"
                                    DISPLAY 
                                "(o.o)	You have obtained Badge II!"
                                    DISPLAY 
                                "(> <) "
                                    DISPLAY " "
                                    END-IF
                                    MOVE 'Y' TO RECORD-FOUND
                                    END-IF
                               END-READ
                           END-PERFORM

                           IF RECORD-FOUND = 'N'
                               CLOSE BADGES-FILE
                               OPEN EXTEND BADGES-FILE
                               IF USER-CURRENT-BADGE < BADGE
                               MOVE USER-NAME TO BADGE-USER
                               MOVE BADGE TO USER-CURRENT-BADGE
                               WRITE BADGES-FILE-RECORD
                               DISPLAY " "
                               DISPLAY "(\_/)  Congratulations!"
                               DISPLAY 
                               "(o.o)  You have obtained Badge II!"
                               DISPLAY "(> <)  "
                               DISPLAY " "
                               END-IF
                           END-IF
                       END-IF
               END-READ
           END-PERFORM
           
           CLOSE ACCOUNT-FILE
           CLOSE BADGES-FILE.   

       BADGE-LEVEL-III.
           MOVE 'N' TO EOF
           MOVE 3 TO BADGE
           OPEN INPUT ACCOUNT-FILE
           OPEN I-O BADGES-FILE

           PERFORM UNTIL EOF = 'Y'
               READ ACCOUNT-FILE INTO ACCOUNT-FILE-RECORD
                   AT END
                       MOVE 'Y' TO EOF
                   NOT AT END
                       IF USER-NAME = CURRENT-SESSION
                           MOVE 'N' TO RECORD-FOUND

                           PERFORM UNTIL EOF = 'Y'
                               READ BADGES-FILE INTO BADGES-FILE-RECORD
                                   AT END
                                       MOVE 'Y' TO EOF
                                   NOT AT END
                                    IF BADGE-USER = USER-NAME
                                    IF USER-CURRENT-BADGE < BADGE
                                    MOVE BADGE TO USER-CURRENT-BADGE
                                    REWRITE BADGES-FILE-RECORD
                                    DISPLAY " "
                                    DISPLAY
                                "(\_/)	Congratulations!"
                                    DISPLAY 
                                "(o.o)	You have obtained Badge III!"
                                    DISPLAY 
                                "(> <) "
                                    DISPLAY " "
                                    END-IF
                                    MOVE 'Y' TO RECORD-FOUND
                                    END-IF
                               END-READ
                           END-PERFORM

                           IF RECORD-FOUND = 'N'
                               CLOSE BADGES-FILE
                               OPEN EXTEND BADGES-FILE
                               IF USER-CURRENT-BADGE < BADGE
                               MOVE USER-NAME TO BADGE-USER
                               MOVE BADGE TO USER-CURRENT-BADGE
                               WRITE BADGES-FILE-RECORD
                               DISPLAY " "
                               DISPLAY "(\_/)  Congratulations!"
                               DISPLAY 
                               "(o.o)  You have obtained Badge III!"
                               DISPLAY "(> <)  "
                               DISPLAY " "
                               END-IF
                           END-IF
                       END-IF
               END-READ
           END-PERFORM
           
           CLOSE ACCOUNT-FILE
           CLOSE BADGES-FILE.   
           
       BADGE-LEVEL-IV.
           MOVE 'N' TO EOF
           MOVE 4 TO BADGE
           OPEN INPUT ACCOUNT-FILE
           OPEN I-O BADGES-FILE

           PERFORM UNTIL EOF = 'Y'
               READ ACCOUNT-FILE INTO ACCOUNT-FILE-RECORD
                   AT END
                       MOVE 'Y' TO EOF
                   NOT AT END
                       IF USER-NAME = CURRENT-SESSION
                           MOVE 'N' TO RECORD-FOUND

                           PERFORM UNTIL EOF = 'Y'
                               READ BADGES-FILE INTO BADGES-FILE-RECORD
                                   AT END
                                       MOVE 'Y' TO EOF
                                   NOT AT END
                                    IF BADGE-USER = USER-NAME
                                    IF USER-CURRENT-BADGE < BADGE
                                    MOVE BADGE TO USER-CURRENT-BADGE
                                    REWRITE BADGES-FILE-RECORD
                                    DISPLAY " "
                                    DISPLAY
                                "(\_/)	Congratulations!"
                                    DISPLAY 
                                "(o.o)	You have obtained Badge IV!"
                                    DISPLAY 
                                "(> <) "
                                    DISPLAY " "
                                    END-IF
                                    MOVE 'Y' TO RECORD-FOUND
                                    END-IF
                               END-READ
                           END-PERFORM

                           IF RECORD-FOUND = 'N'
                               CLOSE BADGES-FILE
                               OPEN EXTEND BADGES-FILE
                               IF USER-CURRENT-BADGE < BADGE
                               MOVE USER-NAME TO BADGE-USER
                               MOVE BADGE TO USER-CURRENT-BADGE
                               WRITE BADGES-FILE-RECORD
                               DISPLAY " "
                               DISPLAY "(\_/)  Congratulations!"
                               DISPLAY 
                               "(o.o)  You have obtained Badge IV!"
                               DISPLAY "(> <)  "
                               DISPLAY " "
                               END-IF
                           END-IF
                       END-IF
               END-READ
           END-PERFORM
           
           CLOSE ACCOUNT-FILE
           CLOSE BADGES-FILE.   
           