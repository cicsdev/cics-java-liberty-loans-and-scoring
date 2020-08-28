cics-java-liberty-link-jwt-sample
=================================

This repository provides sample materials for the article "[Using the Liberty JWT Feature with CICS](https://developer.ibm.com/cics/2019/02/06/json-web-tokens/)" or "[Temporary internal link](https://github.com/cicsdev/cics-java-liberty-loans-and-scoring/blob/master/CICS-Paper-Using-the-Liberty-JWT-Feature-with-CICS.pdf)" that illustrate how CICS and Liberty for z/OS capabilities can be used to handle JSON Web Token (JWT). The article explains the scenario and how to install the sample.
For more information about the different technologies and functions used in this sample, check the Reference section.

Once the CICS resources installed, and the Cobol and Java programs deployed, the way to test the scenario is to start the *QUOT* transaction with the input parameters:
```
QUOT <customerID> <amount> <duration>
```
where:
  * customerID - is an alphanumeric identifier with a length of 8 characters;
  * amount - is an integer number with a maximum of 6 digits;
  * duration - is the duration in years with a maximum of 2 digits;

## Getting started

A guide to deploying these samples into CICS can be found in the referred blog article "[Using the Liberty JWT Feature with CICS](https://developer.ibm.com/cics/2019/02/06/json-web-tokens/)".

## Repository contents

### Eclipse Dynamic Web Project

* [com.ibm.cicsdev.cicsjwt.builder](projects/com.ibm.cicsdev.cicsjwt.builder) - Java project that contains one main class [`CICSJwtBuilder`](projects/com.ibm.cicsdev.cicsjwt.builder/src/com/ibm/cicsdev/cicsjwt/builder/CICSJwtBuilder.java) which uses the JwtBuilder class to build a JWT and is callable from a CICS program.

* [com.ibm.cicsdev.cicsjwt.consumer](projects/com.ibm.cicsdev.cicsjwt.consumer) - Java project that contains one main class [`CICSJwtConsumer`](projects/com.ibm.cicsdev.cicsjwt.consumer/src/com/ibm/cicsdev/cicsjwt/consumer/CICSJwtConsumer.java) which uses the JwtConsumer class to validate a JWT and callable from a CICS program.

Both `CICSJwtBuilder`and `CICSJwtConsumer` have a method that can be linked from a CICS program. As such, they need to be able to interact with channel and containers. The `JWTCLAIM` and `JWTTOKEN` copybooks describing the container data structures have been used with the **IBM Record Generator for Java** to generate the Java classes used to handle the data. The `com.ibm.cicsdev.cicsjwt.datastructures.jar` JAR file contains these Java classes and is provided in each project *WebContent/WEB-INF/lib/* folder.

### CICS Bundle Project

* [com.ibm.cicsdev.cicsjwt.builder.bundle](projects/com.ibm.cicsdev.cicsjwt.builder.bundle) - bundle packaging the `com.ibm.cicsdev.cicsjwt.builder` application.

* [com.ibm.cicsdev.cicsjwt.consumer.bundle](projects/com.ibm.cicsdev.cicsjwt.consumer.bundle) - bundle packaging the `com.ibm.cicsdev.cicsjwt.consumer` application.


### COBOL copybooks

* [JWTCLAIM.cpy](src/Cobol/JWTCLAIM.cpy) - copybook used to generate the `JwtClaimData` class, it contains the JWT claims.
* [JWTTOKEN.cpy](src/Cobol/JWTTOKEN.cpy) - copybook used to generate the `JwtTokenData` class. it contain the JWT token.
* [SCOREREQ.cpy](src/Cobol/SCOREREQ.cpy) - copybook used to generate the JSON request message.
* [SCOREREP.cpy](src/Cobol/SCOREREP.cpy) - copybook used to generate the JSON response message.

### COBOL source

* [GETQUOTE.cbl](src/Cobol/GETQUOTE.cbl) - the program reads the user input, links to Liberty to generate a JWT, sends an HTTP request to the SCORING application and decides whether to grant the loan based on the score.
* [GETSCORE.cbl](src/Cobol/GETSCORE.cbl) - the program receives HTTP requests, links to Liberty to validate the incoming JWT, if the JWT is valid the program responds with a score of 75.

### Supporting files

* [DFHCSDQT.txt](etc/DFHCSDQT.txt) - output from a DFHCSDUP EXTRACT command for the resources used by the LOANS application.
* [DFHCSDSC.txt](etc/DFHCSDSC.txt) - output from a DFHCSDUP EXTRACT command for the resources used by the SCORING application.

## Server.xml snippets

In this sample, two Liberty JVM servers are used: **JWTB** for the generation of the JWT and **JWTC** for the validation of the JWT.
The next two sub-sections contain server.xml snippets showing the required Liberty configuration for the sample. More information can be found in the article.

### JWTB JVM server

* Enable the `cicsts:link-1.0` and `jwt-1.0` features:
```xml
<featureManager>                      
     <feature>cicsts:core-1.0</feature>
     <feature>jsp-2.3</feature>        
     <feature>cicsts:link-1.0</feature>
     <feature>jwt-1.0</feature>        
 </featureManager>                     
```

* Define the `keyStore` that contains the signing certificate (with private key) used for the generation of JWTs:
```xml
<keyStore id="JWTsigner" fileBased="false" location="safkeyring:///SignJWT" password="password" readOnly="true" type="JCERACFKS"/>
```

* Define the `jwtBuilder` configuration for the generation of JWTs:
```xml
<jwtBuilder id="myJWTBuilder" expiry="1h" issuer="CICSMOBT" keyAlias="JWT signer" keyStoreRef="JWTsigner"/>
```

### JWTC JVM server

* Enable the `cicsts:link-1.0` and `jwt-1.0` features:
```xml
<featureManager>                      
     <feature>cicsts:core-1.0</feature>
     <feature>jsp-2.3</feature>        
     <feature>cicsts:link-1.0</feature>
     <feature>jwt-1.0</feature>        
 </featureManager>                     
```

* Define the `keyStore` that contains the signing certificate used for the JWT validation:
```xml
<keyStore id="TrustJWT" fileBased="false" location="safkeyring:///TrustJWT" password="password" readOnly="true" type="JCERACFKS"/>
```

* Define the `jwtConsumer` configuration for the validation of JWTs:
```xml
<jwtConsumer id="myJWTConsumer" audiences="SCORING" issuer="CICSMOBT" trustStoreRef="TrustJWT" trustedAlias="JWT signer"/>
```


## Reference

See the following site for further details on the Liberty JWT feature:

* [Building and consuming JSON Web Token (JWT) tokens in Liberty](https://www.ibm.com/support/knowledgecenter/en/SS7K4U_liberty/com.ibm.websphere.wlp.zseries.doc/ae/twlp_sec_config_jwt.html)

See the following RedBook for further details on the Liberty in IBM CICS:

* [Liberty in IBM CICS](http://www.redbooks.ibm.com/abstracts/sg248418.html)

See the following sites for further details on the Link to Liberty functionality:

* [Linking to a Java EE application from a CICS program](https://www.ibm.com/support/knowledgecenter/SSGMCP_5.4.0/applications/developing/java/link_2_liberty.html)
in the IBM Knowledge Center
* [Link to Liberty now available in CICS TS V5.3](https://developer.ibm.com/cics/2016/11/14/link-to-liberty-now-available-in-cics-ts-v5-3/)
article in the CICS Developer Center

## License
This project is licensed under [Apache License Version 2.0](LICENSE).

