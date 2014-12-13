#---------------------------------------
# Description of the Back Off Algorithm
#---------------------------------------
# To predict the next term of the user specified sentence
# 1. first we use a FourGram; the first three words of which are the last three words of the user provided sentence
#    for which we are trying to predict the next word. The FourGram is already sorted from highest to lowest frequency
# 2. If no FourGram is found, we back off to ThreeGram (first two words of ThreeGram last two words of the sentence)
# 3. If no FourGram is found, we back off to TwoGram (first word of TwoGram last word of the sentence)
# 4. If no TwoGram is found, we back off to OneGram (the most common word with highest frequency)
#
PredNextTerm <- function(inStr)
{
    assign("mesg", "in PredNextTerm", envir = .GlobalEnv)
  
    # Clean up the input string and extract only the words with no leading and trailing white spaces
    inStr <- CleanInputString(inStr);

    # Split the input string across white spaces and then extract the length
    inStr <- unlist(strsplit(inStr, split=" "));
    inStrLen <- length(inStr);

    nxtTermFound <- FALSE;
    predNxtTerm <- as.character(NULL);
    #mesg <<- as.character(NULL);
    # 1. First test the Four Gram using the four gram data frame
    if (inStrLen >= 3 & !nxtTermFound)
    {
        # Assemble the terms of the input string separated by one white space each
        inStr1 <- paste(inStr[(inStrLen-2):inStrLen], collapse=" ");

        # Subset the Four Gram data frame 
        searchStr <- paste("^",inStr1, sep = "");
        fDF4Temp <- fDF4[grep (searchStr, fDF4$terms), ];
        
        # Check to see if any matching record returned
        if ( length(fDF4Temp[, 1]) > 1 )
        {
            predNxtTerm <- fDF4Temp[1,1];
            nxtTermFound <- TRUE;
            mesg <<- "Next word is predicted using 4-gram."
        }
        fDF4Temp <- NULL;
    }

    # 2. Next test the Three Gram using the three gram data frame
    if (inStrLen >= 2 & !nxtTermFound)
    {
        # Assemble the terms of the input string separated by one white space each
        inStr1 <- paste(inStr[(inStrLen-1):inStrLen], collapse=" ");

        # Subset the Three Gram data frame 
        searchStr <- paste("^",inStr1, sep = "");
        fDF3Temp <- fDF3[grep (searchStr, fDF3$terms), ];
        
        # Check to see if any matching record returned
        if ( length(fDF3Temp[, 1]) > 1 )
        {
            predNxtTerm <- fDF3Temp[1,1];
            nxtTermFound <- TRUE;
            mesg <<- "Next word is predicted using 3-gram."
        }
        fDF3Temp <- NULL;
    }

    # 3. Next test the Two Gram using the three gram data frame
    if (inStrLen >= 1 & !nxtTermFound)
    {
        # Assemble the terms of the input string separated by one white space each
        inStr1 <- inStr[inStrLen];

        # Subset the Two Gram data frame 
        searchStr <- paste("^",inStr1, sep = "");
        fDF2Temp <- fDF2[grep (searchStr, fDF2$terms), ];
        
        # Check to see if any matching record returned
        if ( length(fDF2Temp[, 1]) > 1 )
        {
            predNxtTerm <- fDF2Temp[1,1];
            nxtTermFound <- TRUE;
            mesg <<- "Next word is predicted using 2-gram.";
        }
        fDF2Temp <- NULL;
    }

    # 4. If no next term found in Four, Three and Two Grams return the most
    #    frequently used term from the One Gram using the one gram data frame
    if (!nxtTermFound & inStrLen > 0)
    {
        predNxtTerm <- fDF1$terms[1];
        mesg <- "No next word found, the most frequent word is selected as next word."
    }

    nextTerm <- word(predNxtTerm, -1);
       
    if (inStrLen > 0){
        dfTemp1 <- data.frame(nextTerm, mesg);
        return(dfTemp1);
    } else {
        nextTerm <- "";
        mesg <-"";
        dfTemp1 <- data.frame(nextTerm, mesg);
        return(dfTemp1);
    }
}