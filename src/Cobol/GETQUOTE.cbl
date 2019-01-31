       PROCESS NODYNAM,RENT,APOST,CICS,TRUNC(OPT)
      *----------------------------------------------------------------*
      *  Licensed Materials - Property of IBM                          *
      *  SAMPLE                                                        *
      *  (c) Copyright IBM Corp. 2019 All Rights Reserved              *
      *  US Government Users Restricted Rights - Use, duplication or   *
      *  disclosure restricted by GSA ADP Schedule Contract with       *
      *  IBM Corp                                                      *
      *----------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      * Module Name        GETQUOTE.CBL                                *
      *                                                                *
      * LOANS and SCORING sample - LOANS application                   *
      *                                                                *
      * This program expects to be invoked from the terminal with      *
      * an user input, the format is:                                  *
      * <transaction_code> <customer_number> <amount> <duration>       *
      * Then it goes to the SCORING application to get a score.        *
      * This score determines whether a loan is granted.               *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.              GETQUOTE.
      *
       ENVIRONMENT DIVISION.
      *
       DATA DIVISION.
      *
       WORKING-STORAGE SECTION.
      *
      *    SCOREREQ copybook
           COPY SCOREREQ REPLACING SCORE-REQ BY WS-SCORE-REQ.
      *
      *    SCOREREP copybook
           COPY SCOREREP REPLACING SCORE-REP BY WS-SCORE-REP.
      *
      *    JWTCLAIM copybook
           COPY JWTCLAIM REPLACING JWT-CLAIMS BY WS-JWT-CLAIMS.
      *
      *    JWTTOKEN copybook
           COPY JWTTOKEN REPLACING JWT-TOKEN BY WS-JWT-TOKEN.
      *
      *    Working storage definitions
       01  WS-STORAGE.
           03 WS-TERMINAL-INPUT     PIC X(80)         VALUE SPACES.
           03 WS-RECEIVE-LENGTH     PIC 9(4)  COMP    VALUE ZERO.
           03 WS-TRANID             PIC X(4)          VALUE SPACES.
           03 LINK-RESP             PIC 9(8)  COMP    VALUE ZERO.
           03 LINK-RESP2            PIC 9(8)  COMP    VALUE ZERO.
           03 WS-HTTP-REQUEST       PIC X(80)         VALUE SPACES.
           03 WS-HTTP-REQUEST-LEN   PIC S9(8) USAGE BINARY VALUE ZERO.
           03 WS-HTTP-RESPONSE      PIC X(80)         VALUE SPACES.
           03 WS-HTTP-RESPONSE-LEN   PIC S9(8) USAGE BINARY VALUE ZERO.
           03 WS-SESSTOKEN          PIC X(8).
           03 WS-CONTENTTYPE PIC X(12) VALUE 'Content-Type'.
           03 WS-MEDIATYPE   PIC X(16) VALUE 'application/json'.
           03 WS-AUTHBEARER  PIC X(13) VALUE 'Authorization'.

      *    Message to display for normal completion.
      *    Display Supplier ID and name.
       01 RESPONSE-MESSAGE.
          03 FILLER PIC X(28) VALUE 'QUOTE ACCEPTED WITH MONTHLY '.
          03 FILLER PIC X(16) VALUE 'REPAYMENTS OF: $'.
          03 WS-MONTHLY PIC 9(6).
       01 NONAPPROVED-MESSAGE PIC X(14) VALUE 'QUOTE REJECTED'.
       01 EMPTYINPUTS-MESSAGE PIC X(32)
                VALUE 'QUOT REQUIRES 3 INPUT PARAMETERS'.
      *   Error message to display if Link to Liberty fails.
      *   Include slots for target PROGRAM, RESP and RESP2.
       01 ERROR-MESSAGE.
          03 FILLER PIC X(17) VALUE 'ERROR LINKING TO '.
          03 ERROR-PROG PIC X(8) DISPLAY.
          03 FILLER PIC X(7) VALUE '. RESP:'.
          03 ERROR-RESP PIC 9(8) DISPLAY.
          03 FILLER PIC X(7) VALUE ' RESP2:'.
          03 ERROR-RESP2 PIC 9(8) DISPLAY.
      *   Names of various CICS constructs
       77 LIBERTY-CHANNEL PIC X(16) VALUE 'L2LCHANNEL'.
       77 LIBERTY-PROGRAM PIC X(8)  VALUE 'BUILDJWT'.
       77 CONT-JWT-REQ    PIC X(16) VALUE 'JWT-REQ'.
       77 CONT-JWT-REP    PIC X(16) VALUE 'JWT-REP'.
      *
      *
       PROCEDURE DIVISION USING DFHEIBLK.
      *
       MAIN-PROCESSING SECTION.
           INITIALIZE WS-SCORE-REQ.
           INITIALIZE WS-SCORE-REP.
      *    Receive data from terminal
           MOVE LENGTH OF WS-TERMINAL-INPUT TO WS-RECEIVE-LENGTH.
           EXEC CICS RECEIVE INTO(WS-TERMINAL-INPUT)
                     LENGTH(WS-RECEIVE-LENGTH) END-EXEC.

      *    Fold input to uppercase if not already done by CICS
           MOVE FUNCTION UPPER-CASE(WS-TERMINAL-INPUT)
               TO WS-TERMINAL-INPUT.

      *    Parse the input into custno, amount, duration
           UNSTRING WS-TERMINAL-INPUT DELIMITED BY ALL SPACES
               INTO WS-TRANID,
               CUSTNO OF WS-SCORE-REQ,
               AMOUNT OF WS-SCORE-REQ,
               DURATION OF WS-SCORE-REQ
           END-UNSTRING.

      *    Check that inputs are not empty
           IF CUSTNO OF WS-SCORE-REQ EQUAL SPACES
              OR AMOUNT OF WS-SCORE-REQ EQUAL ZEROES
              OR DURATION OF WS-SCORE-REQ EQUAL ZEROES THEN
                  EXEC CICS SEND TEXT FROM(EMPTYINPUTS-MESSAGE)
                       ERASE FREEKB END-EXEC
                  EXEC CICS RETURN END-EXEC
           END-IF.

      *    Retrieve information to generate JWT
           EXEC CICS ASSIGN USERID(SUBJECT) END-EXEC.
           MOVE 'SCORING' TO AUDIENCE.
      *    Normally the role would be retrieved from the user
      *    registry, but for simplicity it is hardcoded
           MOVE 'clerk' TO ROLE.
      *    Write the jwt req to the correct container.
           EXEC CICS PUT CONTAINER(CONT-JWT-REQ)
                     CHANNEL(LIBERTY-CHANNEL)
                     FROM(WS-JWT-CLAIMS) END-EXEC.

      *    Link to BUILDJWT program passing channel
           EXEC CICS LINK PROGRAM(LIBERTY-PROGRAM)
                     CHANNEL(LIBERTY-CHANNEL)
                     RESP(LINK-RESP) RESP2(LINK-RESP2) END-EXEC.
      *    Perform basic response checking from LINK, report error.
           IF LINK-RESP NOT EQUAL DFHRESP(NORMAL) THEN
              MOVE LIBERTY-PROGRAM TO ERROR-PROG
              MOVE LINK-RESP TO ERROR-RESP
              MOVE LINK-RESP2 TO ERROR-RESP2
      *       Send the response data to the terminal.
              EXEC CICS SEND TEXT FROM(ERROR-MESSAGE)
                     ERASE FREEKB END-EXEC

      *       Return control to CICS (end transaction).
              EXEC CICS RETURN END-EXEC
           END-IF.

      *    Get output container from the channel
           EXEC CICS GET CONTAINER(CONT-JWT-REP)
                     CHANNEL(LIBERTY-CHANNEL)
                     INTO(WS-JWT-TOKEN) END-EXEC.

           IF BUILD-RETURN-CODE NOT EQUAL 0 THEN
      *    Send error message to the terminal.
               EXEC CICS SEND TEXT
                         FROM(JWT-STRING)
                         ERASE FREEKB END-EXEC

      *       Return control to CICS (end transaction).
              EXEC CICS RETURN END-EXEC
           END-IF.

      *    Generate JSON message for HTTP request
           JSON GENERATE WS-HTTP-REQUEST FROM WS-SCORE-REQ
               COUNT WS-HTTP-REQUEST-LEN.
      *    Send HTTP request
           EXEC CICS WEB OPEN
             URIMAP('SCORECLT')
             SESSTOKEN(WS-SESSTOKEN)
           END-EXEC.

           EXEC CICS WEB WRITE HTTPHEADER(WS-CONTENTTYPE)
             NAMELENGTH(LENGTH OF WS-CONTENTTYPE)
             SESSTOKEN(WS-SESSTOKEN)
             VALUE(WS-MEDIATYPE) VALUELENGTH(LENGTH OF WS-MEDIATYPE)
           END-EXEC.

           EXEC CICS WEB WRITE HTTPHEADER(WS-AUTHBEARER)
             NAMELENGTH(LENGTH OF WS-AUTHBEARER)
             SESSTOKEN(WS-SESSTOKEN)
             VALUE(JWT-STRING) VALUELENGTH(JWT-STRING-LEN)
           END-EXEC.

           EXEC CICS WEB CONVERSE
             SESSTOKEN(WS-SESSTOKEN)
             URIMAP('SCORECLT')
             POST
             FROM(WS-HTTP-REQUEST)
             FROMLENGTH(WS-HTTP-REQUEST-LEN)
             NOCLICONVERT
             INTO(WS-HTTP-RESPONSE)
             TOLENGTH(WS-HTTP-RESPONSE-LEN)
           END-EXEC.

           JSON PARSE WS-HTTP-RESPONSE(1:WS-HTTP-RESPONSE-LEN)
             INTO WS-SCORE-REP.

      *    Compute monthly payment
           IF SCORE > 60 THEN
               COMPUTE WS-MONTHLY = AMOUNT OF WS-SCORE-REQ * 1.3;
               COMPUTE WS-MONTHLY = WS-MONTHLY / (
                   DURATION OF WS-SCORE-REQ * 12);

      *    Send response message to the terminal.
               EXEC CICS SEND TEXT FROM(RESPONSE-MESSAGE)
                         ERASE FREEKB END-EXEC
           ELSE
      *    Send response message to the terminal.
               EXEC CICS SEND TEXT FROM(NONAPPROVED-MESSAGE)
                         ERASE FREEKB END-EXEC
           END-IF.
      *
      *    Return control to CICS (end transaction).
           EXEC CICS RETURN END-EXEC.
      *
           GOBACK.
