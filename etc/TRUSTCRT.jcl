//TRUSTCRT JOB MSGLEVEL=(1,1),REGION=0M,NOTIFY=&SYSUID,
//             MSGCLASS=A                                          
//*                                                                
//STEP1    EXEC PGM=IKJEFT01                                       
//*                                                                
//SYSPRINT DD  DUMMY                                               
//SYSTSPRT DD  SYSOUT=*                                            
//SYSTSIN  DD  *                                                   
  RACDCERT ADDRING(TrustJWT) ID(<ringOwner>)                             
                                                                   
  RACDCERT CONNECT( +
    ID(<certOwner>) +
    LABEL('JWT signer') +
    RING(TrustJWT) +
    USAGE(CERTAUTH)) +
  ID(<ringOwner>)
                                                                   
  SETROPTS RACLIST(DIGTRING) REFRESH                               
/*                                                                 
//                                                                 
