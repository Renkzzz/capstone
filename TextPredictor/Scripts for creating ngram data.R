#get the data to R:
blogs<-readLines("en_US.blogs.txt",warn=FALSE,encoding="UTF-8")
news<-readLines("en_US.news.txt",warn=FALSE,encoding="UTF-8")
twitter<-readLines("en_US.twitter.txt",warn=FALSE,encoding="UTF-8")

#take stratified random samples of the data and merge them together:
set.seed(369) #ensures reproducibility
prop<-0.005 #
sample_data  <-c(sample(blogs,   prop*length(blogs)),
                 sample(twitter, prop*length(twitter)),
                 sample(news,    prop*length(news)))

#remove the rest
rm(list= ls()[! (ls() %in% c('sample_data'))])

#convert the data into a corpus:
library(tm)
corpus <- VCorpus(VectorSource(sample_data))

#remove the rest
rm(list= ls()[! (ls() %in% c('corpus'))])

#change UTF-8 character vector to the ASCII format
corpus <- tm_map(corpus, function(x)  iconv(x, 'UTF-8', 'ASCII'))

#exclude numbers
corpus = tm_map(corpus, removeNumbers)

#put into lowercase
corpus <- tm_map(corpus, tolower)

#exclude punctuation
corpus = tm_map(corpus, removePunctuation)

#exclude white spaces
corpus <- tm_map(corpus, stripWhitespace)

#convert to plain text format
#removes any formatting or markup
corpus <- tm_map(corpus, PlainTextDocument)



######################################
#extract n-grams (combinations of 1 to 6 words) from the corpus
library(data.table)
system.time(
  for(i in 1:6) {
    print(paste0("Extracting in progress, obtaining", " ", i, "-grams from the corpus"))
    Tokens <- function(x) unlist(lapply(ngrams(words(x), i), paste, collapse = " "), use.names = F)
    termDM <- TermDocumentMatrix(corpus, control = list(tokenize = Tokens))

    termDMr <- sort(slam::row_sums(termDM, na.rm = T), decreasing=T) #calculates the term frequency
    termDMrT <- data.table(token = names(termDMr), count = unname(termDMr))
    termDMrT[,  paste0("w", seq(i)) := tstrsplit(token, " ", fixed=T)]
    termDMrT$token <- NULL

    #stores the results in separate data tables for each n-gram
    assign(paste0("ngram",i), termDMrT)
  }
)
rm(list= ls()[! (ls() %in%
                   c('ngram1', 'ngram2', 'ngram3', 'ngram4',
                     'ngram5', 'ngram6'))])

#function for computing relative frequencies
#of n-grams in the data
term_freq <- function(ngram) {
  ngram[, freq := count / sum(count)]
  return(ngram)
}

#compute these frequencies with the function
ngram1 <- term_freq(ngram1)
ngram2 <- term_freq(ngram2)
ngram3 <- term_freq(ngram3)
ngram4 <- term_freq(ngram4)
ngram5 <- term_freq(ngram5)
ngram6 <- term_freq(ngram6)

#excludes n-grams that have a count less than or equal to 1
rm_infreq <- function(ngram) {
  ngram <- subset(ngram, count > 1)
  return(ngram)
}

#removes infrequent entities with the function:
ngram1 <- rm_infreq(ngram1)
ngram2 <- rm_infreq(ngram2)
ngram3 <- rm_infreq(ngram3)
ngram4 <- rm_infreq(ngram4)
ngram5 <- rm_infreq(ngram5)
ngram6 <- rm_infreq(ngram6)

rm(list= ls()[! (ls() %in%
                   c('ngram1', 'ngram2', 'ngram3', 'ngram4',
                     'ngram5', 'ngram6'))])


#save image of ngrams and function for the app
save.image("ngrams.Rdata")

