//CERTGEN  JOB MSGLEVEL=(1,1),REGION=0M,NOTIFY=&SYSUID,
//             MSGCLASS=A           
//*                                                              
//STEP1    EXEC PGM=IKJEFT01                                       
//*                                                                
//SYSPRINT DD  DUMMY                                               
//SYSTSPRT DD  SYSOUT=*                                            
//SYSTSIN  DD  *                                                   
  RACDCERT GENCERT +
  ID(<certOwner>) +
  SUBJECTSDN( +
    CN('cnValue') +
    OU('ouValue') +
    O('oValue') +
    C('cValue')) +
  SIZE(2048) +
  NOTAFTER(DATE(yyyy-mm-dd)) +
  WITHLABEL('JWT signer') +
  KEYUSAGE(DATAENCRYPT DOCSIGN HANDSHAKE)
                                                                   
  RACDCERT ADDRING(SignJWT) ID(<ringOwner>)                              
                                                                   
  RACDCERT CONNECT( +
    ID(<certOwner>) +
    LABEL('JWT signer') +
    RING(SignJWT) +
    USAGE(PERSONAL)) +
  ID(<ringOwner>)
                                                                   
  SETROPTS RACLIST(DIGTCERT DIGTRING) REFRESH                      
/*                                                                 
//                                                                 
