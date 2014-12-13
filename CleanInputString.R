#-------------------------------------------------
# This function "Clean up" the user input string 
# before it is used to predict the next term
#-------------------------------------------------
CleanInputString <- function(inStr)
{
   # Test sentence
   #inStr <- "This is. the; -  .   use's 12"

   # First remove the non-alphabatical characters
   inStr <- iconv(inStr, "latin1", "ASCII", sub=" ");
   inStr <- gsub("[^[:alpha:][:space:][:punct:]]", "", inStr);

   # Then convert to a Corpus
   inStrCrps <- VCorpus(VectorSource(inStr))

   # Convert the input sentence to lower case
   # Remove punctuations, numbers, white spaces
   # non alphabets characters
   inStrCrps <- tm_map(inStrCrps, content_transformer(tolower))
   inStrCrps <- tm_map(inStrCrps, removePunctuation)
   inStrCrps <- tm_map(inStrCrps, removeNumbers)
   inStrCrps <- tm_map(inStrCrps, stripWhitespace)
   inStr <- as.character(inStrCrps[[1]])
   inStr <- gsub("(^[[:space:]]+|[[:space:]]+$)", "", inStr)

   # Return the cleaned resulting senytense
   # If the resulting string is empty return empty and string.
   if (nchar(inStr) > 0) {
       return(inStr); 
   } else {
       return("");
   }
}