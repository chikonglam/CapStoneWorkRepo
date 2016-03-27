library(tm)
library(stringr)

MAXINPUTLEN <- 5
MAXRESULTLEN <- 10

cleanAndPutInArray <- function(inputText){
    inputCorpus <- VCorpus(VectorSource(inputText))
    inputCorpus <- tm_map(inputCorpus, removeNumbers)
    inputCorpus <- tm_map(inputCorpus, removePunctuation)
    inputCorpus <- tm_map(inputCorpus, content_transformer(tolower))
    inputCorpus <- tm_map(inputCorpus, stripWhitespace) 
    cleanedText <- inputCorpus[[1]]$content
    inputArr <- str_split(cleanedText, "[\\s\b]+")
    inputLen <- length(inputArr[[1]])
    trimmedInputLen <- min(MAXINPUTLEN, inputLen)
    trimmedInput <- tail(inputArr[[1]], trimmedInputLen)
    return(trimmedInput)
}

getNGram <- function(cleanInputArr){
    inputLen <- length(cleanInputArr)
    setkey(allNGrams[[inputLen]], lukey)
    keyString <- paste(cleanInputArr, collapse=" ")
    found <- allNGrams[[inputLen]][keyString]
    if ( nrow(found) < 1 || (nrow(found) == 1 && is.na(found[1]$val))){  #nothing is found
        return(NULL)
    } else {    #something is found
        tops <- found[order(-freq)]
        tops <- head(tops, n=MAXRESULTLEN)
        tops[, "freq" := lapply(.SD, function(x) x*((inputLen+1)^2)), .SDcols="freq"]   #scale with the string length
        return(tops)
    }
}

predictNextWords <- function(userInput){
    cleanInputArr <- cleanAndPutInArray(userInput)
    resultTable <- data.table(lukey=character(0),val=character(0),freq=integer(0))
    lenOfInput <- length(cleanInputArr)
    for (iter in 1:lenOfInput){
        curSuggs <- suppressWarnings(getNGram(tail(cleanInputArr, n=iter)))
        if (! is.null(curSuggs)){
            resultTable <- rbind(resultTable, curSuggs)
        }
    }

    if (nrow(resultTable) >= MAXRESULTLEN ){
        resultTable <- resultTable[,sum(freq), by=val]
        return( head(resultTable$val, n=MAXRESULTLEN) )
    } else {
        return( c(resultTable$val, names(head(smallWordFreq, n=(MAXRESULTLEN- nrow(resultTable)) ))) )
    }
    
}

