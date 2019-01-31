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
      * Module Name        GETSCORE.CBL                                *
      *                                                                *
      * LOANS and SCORING sample - SCORING application                 *
      *                                                                *
      * This program expects to be invoked with an HTTP POST request   *
      * containing a JSON message and it needs to have a valid JWT in  *
      * Authorization header. It returns a JSON message with a score   *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.              GETSCORE.
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
       01 WS-STORAGE.
          03 LINK-RESP             PIC 9(8)  COMP    VALUE ZERO.
          03 LINK-RESP2            PIC 9(8)  COMP    VALUE ZERO.
          03 READ-RESP             PIC 9(8)  COMP    VALUE ZERO.
          03 READ-RESP2            PIC 9(8)  COMP    VALUE ZERO.
          03 WS-HTTP-REQUEST       PIC X(80)         VALUE SPACES.
          03 WS-HTTP-REQUEST-LEN   PIC S9(8) USAGE BINARY VALUE ZERO.
          03 WS-HTTP-RESPONSE      PIC X(80)         VALUE SPACES.
          03 WS-HTTP-RESPONSE-LEN  PIC S9(8) USAGE BINARY VALUE ZERO.
          03 WS-MEDIATYPE          PIC X(56) VALUE 'application/json'.
          03 WS-AUTHBEARER         PIC X(13) VALUE 'Authorization'.
          03 WS-HTTP-ERROR-LEN     PIC S9(8) USAGE BINARY VALUE 70.
          03 WS-HTTP-ERROR.
             05 FILLER PIC X(8) VALUE '{"msg":"'.
             05 HTTP-ERROR-MSG PIC X(60) VALUE ZEROES.
             05 FILLER PIC X(2) VALUE '"}'.
          03 WS-BAD-REQ-CODE PIC S9(4) USAGE BINARY VALUE 400.
          03 WS-BAD-REQ PIC X(11) VALUE 'BAD REQUEST'.
          03 WS-INT-ERR-CODE PIC S9(4) USAGE BINARY VALUE 500.
          03 WS-INT-ERR PIC X(14) VALUE 'INTERNAL ERROR'.
          03 WS-ERR-CODE PIC S9(4) USAGE BINARY VALUE ZEROES.
          03 WS-ERR-MSG PIC X(14) VALUE SPACES.
          03 WS-ERR-MSG-LEN PIC S9(8) USAGE BINARY VALUE ZEROES.
       01 ERROR-MESSAGE.
          03 ERROR-DFLT PIC X(17) VALUE 'ERROR LINKING TO '.
          03 ERROR-PROG PIC X(8) DISPLAY.
          03 FILLER PIC X(7) VALUE '. RESP:'.
          03 ERROR-RESP PIC 9(8) DISPLAY.
          03 FILLER PIC X(7) VALUE ' RESP2:'.
          03 ERROR-RESP2 PIC 9(8) DISPLAY.
      *   Names of various CICS constructs
       77 LIBERTY-CHANNEL PIC X(16) VALUE 'L2LCHANNEL'.
       77 LIBERTY-PROGRAM PIC X(8)  VALUE 'CNSMJWT'.
       77 CONT-JWT-REQ    PIC X(16) VALUE 'JWT-REQ'.
       77 CONT-JWT-REP    PIC X(16) VALUE 'JWT-REP'.
      *
       PROCEDURE DIVISION USING DFHEIBLK.
      *
       MAIN-PROCESSING SECTION.
           INITIALIZE WS-SCORE-REQ.
           INITIALIZE WS-SCORE-REP.
           INITIALIZE WS-JWT-TOKEN.
           INITIALIZE WS-JWT-CLAIMS.
           MOVE LENGTH OF JWT-STRING TO JWT-STRING-LEN.
      *    Receive JWT from HTTP header Authorization
           EXEC CICS WEB READ HTTPHEADER(WS-AUTHBEARER)
               NAMELENGTH(LENGTH OF WS-AUTHBEARER)
               VALUE(JWT-STRING) VALUELENGTH(JWT-STRING-LEN)
               RESP(READ-RESP) RESP2(READ-RESP2)
           END-EXEC.
           IF READ-RESP EQUAL DFHRESP(NOTFND) THEN
              MOVE READ-RESP TO ERROR-RESP
              MOVE READ-RESP2 TO ERROR-RESP2
              MOVE 'NO JWT IN HEADER' TO ERROR-DFLT
              MOVE SPACES TO ERROR-PROG
              MOVE ERROR-MESSAGE TO HTTP-ERROR-MSG

              MOVE WS-BAD-REQ TO WS-ERR-MSG
              MOVE LENGTH OF WS-BAD-REQ TO WS-ERR-MSG-LEN
              MOVE WS-BAD-REQ-CODE TO WS-ERR-CODE
              PERFORM ERR-RESPONSE
           END-IF.
      *    Put JWT in container
           EXEC CICS PUT CONTAINER(CONT-JWT-REQ)
                     CHANNEL(LIBERTY-CHANNEL)
                     FROM(WS-JWT-TOKEN) END-EXEC.

      *    Link to CNSMJWT program passing channel
           EXEC CICS LINK PROGRAM(LIBERTY-PROGRAM)
                     CHANNEL(LIBERTY-CHANNEL)
                     RESP(LINK-RESP) RESP2(LINK-RESP2) END-EXEC.
      *    Perform basic response checking from LINK, report error.
           IF LINK-RESP NOT EQUAL DFHRESP(NORMAL) THEN
              MOVE LIBERTY-PROGRAM TO ERROR-PROG
              MOVE LINK-RESP TO ERROR-RESP
              MOVE LINK-RESP2 TO ERROR-RESP2
              MOVE ERROR-MESSAGE TO HTTP-ERROR-MSG

              MOVE WS-INT-ERR TO WS-ERR-MSG
              MOVE LENGTH OF WS-INT-ERR TO WS-ERR-MSG-LEN
              MOVE WS-INT-ERR-CODE TO WS-ERR-CODE
              PERFORM ERR-RESPONSE
           END-IF.

      *    Get output container from the channel
           EXEC CICS GET CONTAINER(CONT-JWT-REP)
                     CHANNEL(LIBERTY-CHANNEL)
                     INTO(WS-JWT-CLAIMS) END-EXEC.

           IF VALIDATE-RETURN-CODE NOT EQUAL 0 THEN
                   MOVE 'INVALID JWT'  TO HTTP-ERROR-MSG
                   MOVE WS-BAD-REQ TO WS-ERR-MSG
                   MOVE LENGTH OF WS-BAD-REQ TO WS-ERR-MSG-LEN
                   MOVE WS-BAD-REQ-CODE TO WS-ERR-CODE
                   PERFORM ERR-RESPONSE
           END-IF.

      *    Receive JSON from HTTP request
           EXEC CICS WEB RECEIVE INTO(WS-HTTP-REQUEST)
                     LENGTH(WS-HTTP-REQUEST-LEN)
                     MAXLENGTH(80)
                     NOTRUNCATE
           END-EXEC.

           JSON PARSE WS-HTTP-REQUEST(1:WS-HTTP-REQUEST-LEN)
               INTO WS-SCORE-REQ
               ON EXCEPTION
                   MOVE 'INVALID JSON' TO HTTP-ERROR-MSG
                   MOVE WS-BAD-REQ TO WS-ERR-MSG
                   MOVE LENGTH OF WS-BAD-REQ TO WS-ERR-MSG-LEN
                   MOVE WS-BAD-REQ-CODE TO WS-ERR-CODE
                   PERFORM ERR-RESPONSE
           END-JSON.
      *
           DISPLAY '[SCORING] USER=' SUBJECT 'WITH ROLE=' ROLE
               ' HAS CALLED THE SCORING APP'.
      *
           MOVE CUSTNO OF WS-SCORE-REQ TO CUSTNO OF WS-SCORE-REP.
           MOVE AMOUNT OF WS-SCORE-REQ TO AMOUNT OF WS-SCORE-REP.
           MOVE DURATION OF WS-SCORE-REQ TO DURATION OF WS-SCORE-REP.
           MOVE 75 TO SCORE OF WS-SCORE-REP.

           JSON GENERATE WS-HTTP-RESPONSE FROM WS-SCORE-REP
               COUNT WS-HTTP-RESPONSE-LEN
               ON EXCEPTION DISPLAY JSON-CODE ' ' JSON-STATUS
           END-JSON.

           EXEC CICS WEB SEND FROM(WS-HTTP-RESPONSE)
               FROMLENGTH(WS-HTTP-RESPONSE-LEN)
               MEDIATYPE(WS-MEDIATYPE)
           END-EXEC.
      *    Return control to CICS (end transaction).
           EXEC CICS RETURN END-EXEC.
      *
           GOBACK.
      *================================================================*
      * Procedure that sends an error message                          *
      *================================================================*
        ERR-RESPONSE.
           EXEC CICS WEB SEND FROM(WS-HTTP-ERROR)
               FROMLENGTH(WS-HTTP-ERROR-LEN)
               MEDIATYPE(WS-MEDIATYPE)
               STATUSCODE(WS-ERR-CODE) STATUSTEXT(WS-ERR-MSG)
               STATUSLEN(WS-ERR-MSG-LEN)
               SRVCONVERT
           END-EXEC.
           EXEC CICS RETURN END-EXEC.

           EXIT.
