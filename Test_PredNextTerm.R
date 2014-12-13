suppressWarnings(library(tm))
suppressWarnings(library(stringr))

# Define relative folder path
setwd('C:\\Coursera_New_Home\\10 - Capstone Project\\Project\\InputData\\FreqDF');

load("fDF1.RData");
load("fDF2.RData");
load("fDF3.RData");
load("fDF4.RData");

# Redefine the relative folder path
setwd('C:\\Coursera_New_Home\\10 - Capstone Project\\Project');

source(CleanInputString.R);
source(PredNextTerm.R);

#String for testing the code
inStr4 <- "thanks for the follow"
inStr3 <- "thanks for the"
inStr2 <- "thanks for"
inStr1 <- "thanks"

nextTerm4 <- PredNextTerm(inStr4);
nextTerm3 <- PredNextTerm(inStr3);
nextTerm2 <- PredNextTerm(inStr2);
nextTerm1 <- PredNextTerm(inStr1);

nextTerm4;
nextTerm3;
nextTerm2;
nextTerm1;

# The predicted next word
nextWord41 <- word(nextTerm4[1,1], -1);
nextWord31 <- word(nextTerm3[1,1], -1);
nextWord21 <- word(nextTerm2[1,1], -1);
nextWord11 <- word(nextTerm1[1,1], -1);

nextWord41;
nextWord31;
nextWord21;
nextWord11;