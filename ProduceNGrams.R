options(java.parameters = "-Xmx3g")

suppressWarnings(library(tm))
suppressWarnings(library(tau))
suppressWarnings(library(stringr))
suppressWarnings(library(R.utils))
suppressWarnings(library(SnowballC))
suppressWarnings(library(RWeka))
suppressWarnings(library(pryr))
suppressWarnings(library(markdown))
suppressWarnings(library(ggplot2))
suppressWarnings(library(MASS))
suppressWarnings(library(data.table))

# Define file names with relative folder path
setwd('C:\\Coursera_New_Home\\10 - Capstone Project\\Project')

f_en_us_blogs_name <- "./InputData/en_US/en_US.blogs.txt"
f_en_us_twitter_name <- "./InputData/en_US/en_US.twitter.txt"
f_en_us_news_name <- "./InputData/en_US/en_US.news.txt"
fDF_nameA <- "./InputData/FreqDF1/DF1_"
fDF_nameB <- "./InputData/FreqDF2/DF2_"
fDF_nameC <- "./InputData/FreqDF3/DF3_"
fDF_nameD <- "./InputData/FreqDF4/DF4_"

mergeDFs <- function (df1, df2){
   df1 <- df1[order(df1$terms),]
   df2 <- df2[order(df2$terms),]

   df1$freq[df1$terms %in% df2$terms] <- df1$freq[df1$terms %in% df2$terms] + df2$freq[df2$terms %in% df1$terms]
   df3 <- rbind(df1, df2[!(df2$terms %in% df1$terms),])
   df3
}

getSmpl <- function(fData, pSmplSize)
{
  nData <- length(fData)
  smplSize <- as.integer(pSmplSize*nData)/100
  smplIndx <- sample(1:nData, size = smplSize, replace = F)
  smplData <- fData[smplIndx]
  return(smplData)
}

# Create term document matrix
tok1 <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
tok2 <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
tok3 <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
tok4 <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))

# Read blog text file
f_en_us_blogs <- readLines(f_en_us_blogs_name, n = -1L, ok = TRUE, warn = FALSE, encoding = "UTF-8", skipNul = FALSE)

# Randomly sample the data sources
smplBlgs <- getSmpl(f_en_us_blogs, 10)
f_en_us_blogs <- NULL

# Read twitter text file
f_en_us_twitter <- readLines(f_en_us_twitter_name, n = -1L, ok = TRUE, warn = FALSE, encoding = "unknown", skipNul = FALSE)

# Randomly sample the data sources
smplTwt <- getSmpl(f_en_us_twitter, 10)
f_en_us_twitter <- NULL

# Read news text file
f_en_us_news <- readLines(f_en_us_news_name, n = -1L, ok = TRUE, warn = FALSE, encoding = "UTF-8", skipNul = FALSE)

# Randomly sample the data sources
smplNews <- getSmpl(f_en_us_news, 10)
f_en_us_news <- NULL

# Combine the three data sets
combData <-  c(smplTwt, smplNews, smplBlgs)

# Remove non print data etc
combData <- iconv(combData, "latin1", "ASCII", sub=" ");
combData <- gsub("[^[:alpha:][:space:][:punct:]]", "", combData);

rm(smplTwt, smplNews, smplBlgs)

# Diveide data into small chunks
nComb <- length(combData)
sz = 1000
nDiv <- as.integer(nComb/sz)

# Empty data frames
wf1_old <- data.frame(terms=NA, freq=NA)[numeric(0), ]
wf2_old <- data.frame(terms=NA, freq=NA)[numeric(0), ]
wf3_old <- data.frame(terms=NA, freq=NA)[numeric(0), ]
wf4_old <- data.frame(terms=NA, freq=NA)[numeric(0), ]

for (i in 1:nDiv ) {
#for (i in 1:4 ) {
   j <- (i-1)*sz+1
   k <- i*sz
   txtSmpl <- combData[j:k]
   # Create a Corpus from the data txtSmpl
   fCorpus <- VCorpus(VectorSource(txtSmpl))

   # Clean and manage the data - Convert to lower case, 
   # Remove punctuations, numbers, stop words, white spaces
   # Stemp the document
   fCorpus <- tm_map(fCorpus, content_transformer(tolower))
   fCorpus <- tm_map(fCorpus, removePunctuation)
   fCorpus <- tm_map(fCorpus, removeNumbers)
   #fCorpus <- tm_map(fCorpus, removeWords, stopwords("english"))
   #fCorpus <- tm_map(fCorpus, stemDocument)
   fCorpus <- tm_map(fCorpus, stripWhitespace)

   tdm1 <- TermDocumentMatrix(fCorpus, control = list(tokenize = tok1))
   tdm2 <- TermDocumentMatrix(fCorpus, control = list(tokenize = tok2))
   tdm3 <- TermDocumentMatrix(fCorpus, control = list(tokenize = tok3))
   tdm4 <- TermDocumentMatrix(fCorpus, control = list(tokenize = tok4))

   freq1 <- sort(rowSums(as.matrix(tdm1)), decreasing=TRUE)
   freq2 <- sort(rowSums(as.matrix(tdm2)), decreasing=TRUE)
   freq3 <- sort(rowSums(as.matrix(tdm3)), decreasing=TRUE)
   freq4 <- sort(rowSums(as.matrix(tdm4)), decreasing=TRUE)

   wf1 <- data.table(terms=names(freq1), freq=freq1)
   wf2 <- data.table(terms=names(freq2), freq=freq2)
   wf3 <- data.table(terms=names(freq3), freq=freq3)
   wf4 <- data.table(terms=names(freq4), freq=freq4)
   
   fDF_name1 <- paste(fDF_nameA,i,sep="")
   fDF_name2 <- paste(fDF_nameB,i,sep="")
   fDF_name3 <- paste(fDF_nameC,i,sep="")
   fDF_name4 <- paste(fDF_nameD,i,sep="")
  
   write.table(wf1, fDF_name1, col.names = TRUE)
   write.table(wf2, fDF_name2, col.names = TRUE)
   write.table(wf3, fDF_name3, col.names = TRUE)
   write.table(wf4, fDF_name4, col.names = TRUE)

   print(i)
}

rm (fCorpus, freq1, tdm1, freq2, tdm2, freq3, tdm3, freq4, tdm4)

fDF_final <- "./InputData/FreqDF/fDF1_final"
fName = "./InputData/FreqDF/fDF1_final.RData"
fDF_name1 <- "./InputData/FreqDF1/DF1_"

fDF_final <- "./InputData/FreqDF/fDF2_final"
fName = "./InputData/FreqDF/fDF2_final.RData"
fDF_name1 <- "./InputData/FreqDF2/DF2_"

fDF_final <- "./InputData/FreqDF/fDF3_final"
fName = "./InputData/FreqDF/fDF3_final.RData"
fDF_name1 <- "./InputData/FreqDF3/DF3_"

fDF_final <- "./InputData/FreqDF/fDF4_final"
fName = "./InputData/FreqDF/fDF4_final.RData"
fDF_name1 <- "./InputData/FreqDF4/DF4_"

wf1_old <- data.frame(terms=NA, freq=NA)[numeric(0), ]
for (i in 1:nDiv ) {

   fDF_nameA <- paste(fDF_name1,i,sep="")
   wf1 <- read.table(fDF_nameA, header = TRUE)

   wf1_new <- mergeDFs(wf1_old, wf1)
   wf1_old <- wf1_new
   print(i)
}

wf1_new <- wf1_new[with(wf1_new, order(-freq)), ]
write.table(wf1_new, fDF_final, col.names = TRUE)
save(wf1_new, file=fName);

View(head(wf1_new))

#=====================================================

# Define file names with relative folder path
setwd('C:\\Coursera_New_Home\\10 - Capstone Project\\Project')

fDF1_name <- "./InputData/FreqDF/fDF1_final"
fDF2_name <- "./InputData/FreqDF/fDF2_final"
fDF3_name <- "./InputData/FreqDF/fDF3_final"
fDF4_name <- "./InputData/FreqDF/fDF4_final"

fDF1 <- read.table(fDF1_name, header = TRUE, stringsAsFactors = FALSE)
fDF2 <- read.table(fDF2_name, header = TRUE, stringsAsFactors = FALSE)
fDF3 <- read.table(fDF3_name, header = TRUE, stringsAsFactors = FALSE)
fDF4 <- read.table(fDF4_name, header = TRUE, stringsAsFactors = FALSE)

# Subset the data frames based on the frequency
fDF1 <- fDF1[fDF1$freq > 100,]
fDF2 <- fDF2[fDF2$freq > 2,]
fDF3 <- fDF3[fDF3$freq > 2,]
fDF4 <- fDF4[fDF4$freq > 2,]

# Define file names with relative folder path
setwd('C:\\Coursera_New_Home\\10 - Capstone Project\\Project\\InputData\\FreqDF')

save(fDF1, file="fDF1.RData");
save(fDF2, file="fDF2.RData");
save(fDF3, file="fDF3.RData");
save(fDF4, file="fDF4.RData");

load("fDF1.RData")
load("fDF2.RData")
load("fDF3.RData")
load("fDF4.RData")
