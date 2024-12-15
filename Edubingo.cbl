      ***********************PROJECT DESCRIPTION************************
      
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
       
      *************FILE DEFINITION FOR ALL THE FILES NEEDED*************
       DATA DIVISION.
       FILE SECTION.
       FD ACCOUNT-FILE.
       01 ACCOUNT-FILE-RECORD.
           05 USER-NAME         PIC X(20).
           05 USER-PASSWORD     PIC X(20).
       FD MOOD-FILE.
       01 MOOD-FILE-RECORD.
           05 MOOD-USER        PIC X(20).
           05 MOOD-DATE        PIC X(20).
           05 MOOD-STATUS      PIC 9.

      *******************VARIABLES USED ON OUR PROGRAM******************
       WORKING-STORAGE SECTION.
       01 USER-CHOICE           PIC X(1).
       01 USER-NAME-INPUT       PIC X(20).
       01 USER-PASSWORD-INPUT   PIC X(20).
       01 CURRENT-SESSION       PIC X(20).
       01 CURRENT-PASSWORD      PIC X(20).
       01 ACCOUNT-FOUND         PIC X(1) VALUE 'N'.
       01 EOF                   PIC X VALUE 'N'.
       01 RAW-DATE              PIC 9(8).
       01 TODAY-MONTH           PIC 99.    
       01 TODAY-DAY             PIC 99.   
       01 TODAY-YEAR            PIC 9(4).  
       01 TODAY-DATE            PIC X(10).
       01 MOOD-STATUS-INPUT     PIC 9.
       01 MOOD-FOUND            PIC X(1) VALUE 'N'.

      ****************VARIABLES FOR GENERATING THE OUTPUT***************

      ********************MAIN PROCESS OF THE PROGRAM*******************
       PROCEDURE DIVISION.
       MAIN.
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
           PERFORM MOOD-TRACKER.

           STOP RUN.


       ACCOUNT-MENU.
           DISPLAY " "
           DISPLAY "> What would you like to do?".
           DISPLAY "  [1] Register".
           DISPLAY "  [2] Login".
           DISPLAY "  [3] Exit".
           DISPLAY " ".
           DISPLAY "> Enter your choice: " WITH NO ADVANCING.
           ACCEPT USER-CHOICE.
           
           EVALUATE USER-CHOICE
               WHEN '1'
                   PERFORM SIGN-UP
               WHEN '2'
                   PERFORM LOGIN
               WHEN '3'
                   DISPLAY " "
                   DISPLAY "(\_/)"
                   DISPLAY "(o.o)  See you next time! Keep safe :)"
                   DISPLAY "(> <) "
                   DISPLAY " "
                   CLOSE MOOD-FILE
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
           DISPLAY"===================================================".
           DISPLAY " "
           DISPLAY "(\_/)"
           DISPLAY "(o.o)  What would you like to do? :)"
           DISPLAY "(> <) "
           DISPLAY " "

           PERFORM GET-VALID-ACTION-DASHBOARD.

       GET-VALID-ACTION-DASHBOARD.
           DISPLAY "> Enter your choice: " WITH NO ADVANCING.
           ACCEPT USER-CHOICE
           
           EVALUATE USER-CHOICE
               WHEN '1'
                   DISPLAY "You chose TASKS."
               WHEN '2'
                   DISPLAY "You chose STATUS."
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

           OPEN INPUT ACCOUNT-FILE.

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
                       
               OPEN EXTEND ACCOUNT-FILE
        
               MOVE USER-NAME-INPUT TO USER-NAME
               MOVE USER-PASSWORD-INPUT TO USER-PASSWORD
               WRITE ACCOUNT-FILE-RECORD
           
               DISPLAY " "
               DISPLAY 
               "(\_/)  Account created successfully!"
               DISPLAY 
               "(o.o)  You can now log in with your credentials."
               DISPLAY 
               "(> <) "
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
                   MOVE 'Y' TO ACCOUNT-FOUND
                   END-IF
               END-READ.
       
       MOOD-TRACKER.
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
           
           MOVE CURRENT-SESSION TO MOOD-USER
           OPEN INPUT MOOD-FILE

           MOVE 'N' TO MOOD-FOUND
           MOVE 'N' TO EOF

           PERFORM CHECK-MOOD-RECORDS UNTIL EOF ='Y'

           IF MOOD-FOUND = 'Y'
               PERFORM DASHBOARD
           ELSE 
               CLOSE MOOD-FILE
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
               WRITE MOOD-FILE-RECORD
               
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
           END-IF

           CLOSE MOOD-FILE.
       
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
           DISPLAY " "
           DISPLAY " ____    ______  _____                 "
           DISPLAY "/\  _`\ /\  _  \/\  __`\               "
           DISPLAY "\ \ \L\_\ \ \L\ \ \ \/\ \    ____      "
           DISPLAY " \ \  _\/\ \  __ \ \ \ \ \  /',__\     "
           DISPLAY "  \ \ \/  \ \ \/\ \ \ \\'\\/\__, `\    "
           DISPLAY "   \ \_\   \ \_\ \_\ \___\_\/\____/    "
           DISPLAY "    \/_/    \/_/\/_/\/__//_/\/___/     ".