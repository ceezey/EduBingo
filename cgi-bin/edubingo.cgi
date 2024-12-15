#!/usr/bin/cobc
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EDU-BINGO.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 QUERY-STRING           PIC X(255).
       01 CONTENT-LENGTH         PIC 9(5).
       01 INPUT-DATA             PIC X(255).
       01 USER-EMAIL             PIC X(50).
       01 USER-PASSWORD          PIC X(20).
       01 OUTPUT-MESSAGE         PIC X(100).
       01 LOGGED-IN              PIC 9 VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PROCESS.
           DISPLAY "Content-Type: text/html"
           DISPLAY " "

           ACCEPT QUERY-STRING FROM ENVIRONMENT "QUERY_STRING"
           ACCEPT CONTENT-LENGTH FROM ENVIRONMENT "CONTENT_LENGTH"
           ACCEPT INPUT-DATA

           PERFORM PARSE-INPUT
           PERFORM LOGIN-PROCESS

           IF LOGGED-IN = 1
               MOVE "Login successful! Welcome to EduBingo!" TO 
                 OUTPUT-MESSAGE
           ELSE
               MOVE "Invalid login. Please try again." TO 
                 OUTPUT-MESSAGE
           END-IF

           PERFORM GENERATE-HTML

           STOP RUN.

       PARSE-INPUT.
           IF QUERY-STRING NOT EQUAL SPACES
               UNSTRING QUERY-STRING DELIMITED BY "&"
                   INTO USER-EMAIL, USER-PASSWORD
           END-IF.

       LOGIN-PROCESS.
           IF USER-EMAIL = "student@example.com" AND
           USER-PASSWORD = "password123"
               MOVE 1 TO LOGGED-IN
           ELSE
               MOVE 0 TO LOGGED-IN
           END-IF.

       GENERATE-HTML.
           DISPLAY "<html>"
           DISPLAY "<head><title>EduBingo</title></head>"
           DISPLAY "<body>"
           DISPLAY "<h1>" OUTPUT-MESSAGE "</h1>"
           DISPLAY "</body>"
           DISPLAY "</html>".
