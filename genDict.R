library(tm)
library(stringi)
library(ggplot2)
library(RWeka)
library(slam)
library(openNLP)

set.seed(852489)

enBlogs <- readLines("final/en_US/en_US.blogs.txt")
enNews <- readLines("final/en_US/en_US.news.txt")
enTwitter <- readLines("final/en_US/en_US.twitter.txt")

sampleBlogs <- sample(enBlogs,50000)
sampleNews <- sample(enNews,50000)
sampleTwitter <- sample(enTwitter,50000)

sample <- c(sampleBlogs,sampleNews,sampleTwitter)
corpus <- VCorpus(VectorSource(sample))
remove (enBlogs, enNews, enTwitter)
remove(sampleBlogs, sampleNews, sampleTwitter)

#rough standard cleaning
corpus <- tm_map(corpus, removeNumbers) # remove numbers
corpus <- tm_map(corpus, removePunctuation) # remove punctuation
corpus <- tm_map(corpus, content_transformer(tolower)) # convert to lower case

#remove cuss words
cussWords <- readLines('full-list-of-bad-words-banned-by-google.txt')
corpus <- tm_map(corpus, removeWords, cussWords)

#trim excessive whitespaces
corpus <- tm_map(corpus, stripWhitespace) 

# Frequencies of Words
dtm <- DocumentTermMatrix(corpus)
dtms <- removeSparseTerms(dtm , 0.999)
wordFreq <- sort(colSums(as.matrix(dtms)), decreasing=TRUE)
freqWf <- data.frame(word=names(wordFreq), freq=wordFreq)
freqWf$word <- factor(freqWf$word, levels=freqWf$word, ordered=TRUE)
save(wordFreq, file="wordFreq.RData")

### N-grams:  Frequencies of Pharses
NGrams <- function(data, n) {
    tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = n, max = n))
    ngrams <- DocumentTermMatrix(data, control=list(tokenize=tokenizer))
    ngrams <- sort(col_sums(ngrams), decreasing = TRUE)
    return(ngrams)
}

#### Bigrams
bigrams <- NGrams(corpus, 2)
save(bigrams, file="bigrams.RData")

#### Trigrams
trigrams <- NGrams(corpus, 3)
save(trigrams, file="trigrams.RData")

#### Quadgrams
quadgrams <- NGrams(corpus, 4)
save(quadgrams, file="quadgrams.RData")

#### Pentagrams
pentagrams <- NGrams(corpus, 5)
save(pentagrams, file="pentagrams.RData")

#### Hexagrams
hexagrams <- NGrams(corpus, 6)
save(hexagrams, file="hexagrams.RData")

#### Heptagrams
heptagrams <- NGrams(corpus, 7)
save(heptagrams, file="heptagrams.RData")

#### Octagrams
octagrams <- NGrams(corpus, 8)
save(octagrams, file="octagrams.RData")

#### nonagrams
nonagrams <- NGrams(corpus, 9)
save(nonagrams, file="nonagrams.RData")

#### decagrams
decagrams <- NGrams(corpus, 10)
save(decagrams, file="decagrams.RData")

##put the Ngrams into data.tables
library(stringr)
library(data.table)

##data.table gen function
nGrams2DataTab <- function (nGramsList, numRec){
    firstNGrams <- nGramsList[1:numRec]
    keyVal <- str_match(names(firstNGrams), "^(.+?)[\\s\b]+([\\S]+)$")
    ngramsDt <- data.table(lukey=keyVal[,2], val=keyVal[,3], freq=firstNGrams)
    setkey(ngramsDt, lukey)
    return(ngramsDt)
}

sampleSize <- 1000000

###Bigram dataTable
bigramsDT <- nGrams2DataTab(bigrams, sampleSize)
save(bigramsDT, file="bigramsDT.RData")

###Trigram dataTable
trigramsDT <- nGrams2DataTab(trigrams, sampleSize)
save(trigramsDT, file="trigramsDT.RData")

###Quadgram dataTable
quadgramsDT <- nGrams2DataTab(quadgrams, sampleSize)
save(quadgramsDT, file="quadgramsDT.RData")

###pentagram dataTable
pentagramsDT <- nGrams2DataTab(pentagrams, sampleSize)
save(pentagramsDT, file="pentagramsDT.RData")

###hexagram dataTable
hexagramsDT <- nGrams2DataTab(hexagrams, sampleSize)
save(hexagramsDT, file="hexagramsDT.RData")

### put all grams in to a single object list
allNGrams<-list()
allNGrams[[1]]<-bigramsDT
allNGrams[[2]]<-trigramsDT
allNGrams[[3]]<-quadgramsDT
allNGrams[[4]]<-pentagramsDT
allNGrams[[5]]<-hexagramsDT
save(allNGrams, file="allNGrams.RData")

### save a small wordFreq
smallwordFreq <- wordFreq[1:50]
save(smallWordFreq, file="smallwordFreq.RData")

##look up a data.table
# setkey(trigramsDT, lukey)
# found <- trigramsDT["in a",]
# tops <- found[order(-freq)]
# head(tops)