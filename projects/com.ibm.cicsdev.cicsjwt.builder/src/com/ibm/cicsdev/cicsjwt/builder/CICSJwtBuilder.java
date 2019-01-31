/* Licensed Materials - Property of IBM                               */
/*                                                                    */
/* SAMPLE                                                             */
/*                                                                    */
/* (c) Copyright IBM Corp. 2019 All Rights Reserved                   */
/*                                                                    */
/* US Government Users Restricted Rights - Use, duplication or        */
/* disclosure restricted by GSA ADP Schedule Contract with IBM Corp   */
/*                                                                    */

package com.ibm.cicsdev.cicsjwt.builder;

import java.util.ArrayList;

import com.ibm.cics.server.Channel;
import com.ibm.cics.server.CicsConditionException;
import com.ibm.cics.server.Container;
import com.ibm.cics.server.Task;
import com.ibm.cics.server.invocation.CICSProgram;
import com.ibm.cicsdev.cicsjwt.datastructures.JwtClaimsData;
import com.ibm.cicsdev.cicsjwt.datastructures.JwtTokenData;
import com.ibm.websphere.security.jwt.InvalidBuilderException;
import com.ibm.websphere.security.jwt.InvalidClaimException;
import com.ibm.websphere.security.jwt.JwtBuilder;
import com.ibm.websphere.security.jwt.JwtException;
import com.ibm.websphere.security.jwt.JwtToken;

public class CICSJwtBuilder {

    @CICSProgram("BUILDJWT")
    public void buildJwt() throws CicsConditionException
    {
        // Retrieve the current channel
        Channel ch = Task.getTask().getCurrentChannel();

        // Get the request container
        Container requestContainer = ch.getContainer("JWT-REQ");
        
        // Convert container data to an instance of JwtClaimsData
        JwtClaimsData jwtClaimsData = new JwtClaimsData(requestContainer.get());
        
        // Extract the subject and audience claims
        String subject = jwtClaimsData.getSubject().trim();
        String audience = jwtClaimsData.getAudience().trim();
        String role = jwtClaimsData.getRole().trim();
        ArrayList<String> audiences = new ArrayList<String>();
        audiences.add(audience);

        // Create an instance of JwtTokenData
        JwtTokenData jwtTokenData = new JwtTokenData();
        // If returnCode equals 0 then everything went okay
        jwtTokenData.setBuildReturnCode(0);
        try {
        	// Create an instance of JwtBuilder based on "myJWTBuilder" configuration & set subject,audiences
			JwtBuilder jwtBuilder = JwtBuilder.create("myJWTBuilder").subject(subject).audience(audiences).claim("role", role);
			// Build token
			JwtToken jwtToken = jwtBuilder.buildJwt();
			// Set JWT token and length in response container
			String jwtString = jwtToken.compact();
			jwtTokenData.setJwtString(jwtString);
			jwtTokenData.setJwtStringLen(jwtString.length());
			
			// The token is too long compared to the allocated space
			if (jwtTokenData.getJwtStringLen()>1000) {
				jwtTokenData.setBuildReturnCode(1);
				String errorMessage = "The JWT has a length of " + jwtTokenData.getJwtStringLen() + " which is more than the allocated space in the copybook."; 
				jwtTokenData.setJwtString(errorMessage);
				jwtTokenData.setJwtStringLen(errorMessage.length());
			}
        } catch (InvalidBuilderException | InvalidClaimException | JwtException e) {
			// Exception in the creation of the JWT, thus returnCode equals 1
        	jwtTokenData.setBuildReturnCode(1);
        	// The JWT string length in the copybook is 1000, thus the exception message may need to be truncated
			int maxIndex = 1000;
			if (e.getMessage().length()<maxIndex){
				maxIndex = e.getMessage().length();
			}
			jwtTokenData.setJwtString(e.getMessage().substring(0, maxIndex));
			jwtTokenData.setJwtStringLen(maxIndex);			
		}
        
        // Create response container and put data inside
        Container replyContainer = ch.createContainer("JWT-REP");
        replyContainer.put(jwtTokenData.getByteBuffer());
    }
}
