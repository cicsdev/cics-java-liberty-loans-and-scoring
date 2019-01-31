/* Licensed Materials - Property of IBM                               */
/*                                                                    */
/* SAMPLE                                                             */
/*                                                                    */
/* (c) Copyright IBM Corp. 2019 All Rights Reserved                   */
/*                                                                    */
/* US Government Users Restricted Rights - Use, duplication or        */
/* disclosure restricted by GSA ADP Schedule Contract with IBM Corp   */
/*                                                                    */

package com.ibm.cicsdev.cicsjwt.consumer;

import com.ibm.cics.server.Channel;
import com.ibm.cics.server.CicsConditionException;
import com.ibm.cics.server.Container;
import com.ibm.cics.server.Task;
import com.ibm.cics.server.invocation.CICSProgram;
import com.ibm.cicsdev.cicsjwt.datastructures.JwtClaimsData;
import com.ibm.cicsdev.cicsjwt.datastructures.JwtTokenData;
import com.ibm.websphere.security.jwt.Claims;
import com.ibm.websphere.security.jwt.InvalidConsumerException;
import com.ibm.websphere.security.jwt.InvalidTokenException;
import com.ibm.websphere.security.jwt.JwtConsumer;
import com.ibm.websphere.security.jwt.JwtToken;

public class CICSJwtConsumer {

    @CICSProgram("CNSMJWT")
    public void consumeJwt() throws CicsConditionException
    {
        // Retrieve the current channel
        Channel ch = Task.getTask().getCurrentChannel();

        // Get the request container
        Container requestContainer = ch.getContainer("JWT-REQ");
        
        // Convert container data to an instance of JwtTokenData
        JwtTokenData jwtTokenData = new JwtTokenData(requestContainer.get());
        
        // Extract the JWT string and trim trailing space
        String jwtString = jwtTokenData.getJwtString().trim();

        // Create an instance of JwtClaimsData
        JwtClaimsData jwtClaimsData = new JwtClaimsData();
        // If returnCode equals 0 then everything went okay
        jwtClaimsData.setValidateReturnCode(0);
        try {
        	// Create an instance of JwtConsumer based on "myJWTConsumer" configuration
			JwtConsumer jwtConsumer = JwtConsumer.create("myJWTConsumer");
			// Validate and parse JWT
			JwtToken jwtToken = jwtConsumer.createJwt(jwtString);
			// Retrieve the claims and set the values in the JwtClaimsData
			Claims claims = jwtToken.getClaims();
			jwtClaimsData.setAudience(claims.get("aud").toString());
			jwtClaimsData.setExpire(claims.getExpiration());
			jwtClaimsData.setIssuer(claims.getIssuer());
			jwtClaimsData.setSubject(claims.getSubject());
			jwtClaimsData.setRole(claims.get("role").toString());
			
		} catch (InvalidConsumerException | InvalidTokenException e) {
			// Exception in the validation of the JWT, thus returnCode equals 1
			jwtClaimsData.setValidateReturnCode(1);
			e.printStackTrace();			
		}
        
        // Create response container and put data inside
        Container replyContainer = ch.createContainer("JWT-REP");
        replyContainer.put(jwtClaimsData.getByteBuffer());
    }
}
