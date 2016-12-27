##Loads data from the website and puts into *.raw files

library(NLP) 
library(openNLP)
library(tm)
library(quanteda)
library(data.table)
library(dplyr)

source("POS.R")

loadNgramData <- function(seed = 1000) {
        print("loading text data...")
        start.time <- Sys.time()

        if (file.exists("textsamples.raw.robj")) {
                print("loading text samples from disk")
                load("textsamples.raw.robj")
                
        ##if the saved objects aren't there then create them
        }else {
                print("loading text samples from online source")
                #prep work...
                url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
                zipfile <- "download.zip"
                filestounzip <- c(
                        "final/en_US/en_US.twitter.txt",
                        "final/en_US/en_US.news.txt",
                        "final/en_US/en_US.blogs.txt"
                )
                
                #download, if the download file isn't already there
                if (!file.exists(zipfile)) {
                        download.file(url, zipfile, method = "auto",quiet = FALSE, mode =
                                              "w", cacheOK = TRUE)
                }
                
                #unzip if the twitter file isn't already there (assuming if it's there, all 3 are there)
                if (!file.exists("en_US.twitter.txt")) {
                        unzip(zipfile, files = filestounzip, overwrite = TRUE, 
                              junkpath = TRUE, exdir = ".", unzip = "internal", 
                              setTimes = FALSE)
                }
                #create a list object for each, if that's not arleady done. 
                if(!exists("blogs.raw")) {
                        blogs.raw <- scan("en_US.blogs.txt", character(0), sep ="\n")
                        news.raw <- scan("en_US.news.txt", character(0), sep = "\n")
                        twitter.raw <- scan("en_US.twitter.txt", character(0), sep = "\n")
                } 
                save("blogs.raw", "news.raw", "twitter.raw", file = "textsamples.raw.robj")
        }
        
        end.time <- Sys.time()
        time.taken <- end.time - start.time
        print("loading text data complete")
        print(time.taken)
        
        return(list("blogs" = blogs.raw, "twitter" = twitter.raw, "news" = news.raw))
        
}

getSample <- function (samplesize, blogs.raw, news.raw, twitter.raw) {
        #sample the data (to speed things up)
        blogs <- blogs.raw[sample(length(blogs.raw), samplesize * length(blogs.raw))]
        news <- news.raw[sample(length(news.raw), samplesize * length(news.raw))]
        twitter <- twitter.raw[sample(length(twitter.raw), samplesize * length(twitter.raw))]
        
        #combine into one list
        combined <- c(blogs, news, twitter)
        
        return(combined)
}

buildLittleSample <- function (lines) {
        
        training_percent <- .95
        
        ## divide lines into training and test 
        inTrain <- sample(length(lines), training_percent * length(lines))
        training = lines[inTrain]
        testing = lines[-inTrain] 
        
        ## build the training dataset
        uni <- createNGram(1L,training, addCols = FALSE)
        bi <- createNGram(2L,training, addCols = FALSE)
        tri <- createNGram(3L,training, addCols = FALSE)
        quat <- createNGram(4L, training, addCols = FALSE)
        
        ls <- list(uni = uni, bi = bi, tri = tri, quat = quat, testing = testing)
        
        ## return the three (or four) ngram dataframes and the test vector
        return (ls)
}

getChunkRange <- function(chunk_size, chunk_num) {
        start <- chunk_size*(chunk_num-1)+1
        end <- chunk_num*chunk_size
        return (list("start" = start, "end" = end))
}

saveChunk <- function (ls, chunk, seed = 1000) {
        uni <- ls$uni
        bi <- ls$bi
        tri <- ls$tri
        quat <- ls$quat
        testing <- ls$testing
        
        #save the little sample)
        save("uni", file=paste0("samplebuild/uni/uni", chunk, ".robj"))
        save("bi", file=paste0("samplebuild/bi/bi", chunk, ".robj"))
        save("tri", file=paste0("samplebuild/tri/tri", chunk, ".robj"))
        save("quat", file=paste0("samplebuild/quat/quat", chunk, ".robj"))
        save("testing", file=paste0("samplebuild/testing/testing", chunk, ".robj"))
}

aggregateChunks <- function (type, num_chunks, remove_under = 0) {
        print(paste0("aggregating ",type))
        start.time <- Sys.time()
        #load the data and combine into one dataframe
        if (type != "testing") {
                agg <- data.frame()
                for (chunk in 1:num_chunks) {
                        load(paste0("samplebuild/",type,"/",type,chunk,".robj"))
                        agg <- rbind(agg, get(type))
                }
                agg <- aggregate(freq~ngram,agg,sum)                            #aggregate
                if (remove_under > 1) {
                        agg <- agg[agg$freq >= remove_under,]                    #remove under remove_under
                }
                agg <- agg[order(agg$freq, decreasing = TRUE),]                 #order
                agg$leader <- unlist(lapply(agg$ngram,getLeader))               #add leader column
                agg$lastWord <- unlist(lapply(agg$ngram,getLastWord))           #add lastWord column
                colnames(agg) <- c("ngram", "freq", "leader", "lastWord")       #update column names
        }
        else { #type == testing - which is not a vector, not a dataframe
                agg <- character()
                for (chunk in 1:num_chunks) {
                        load(paste0("samplebuild/",type,"/",type,chunk,".robj"))
                        agg <- c(agg, get(type))
                }
        }
        
        #wrap-up
        end.time <- Sys.time()
        time.taken <- end.time - start.time
        print(paste0("aggregating ",type, " complete."))
        print(time.taken)

        return(agg)
}

nGramType <- function(x) {
        #returns the number of ngrams in x
        places <- unlist(gregexpr(pattern ='_',x))
        if (places[[1]] == -1) {
                ngrams <- 1
        }
        else {
                ngrams <- length(places) + 1
        }
        
        return(ngrams)
}

buildBigSample <- function (samplesize, seed = 1000, num_chunks = 3, removeUnder = 2) {
        library(tm)
        library(quanteda)
        
        print("building sample iteratively...")
        start.time <- Sys.time()
        
        set.seed(seed = seed)
        
        raw <- loadNgramData()
        #combined <- c(raw$blogs, raw$news, raw$twitter)
        combined <- c(raw$blogs, raw$news)
        sample <- combined[sample(length(combined), samplesize * length(combined))]
        chunk_size <- as.integer(length(sample) / num_chunks)
        bs <- list()
        
        #build and save the chunks
        for (chunk in 1:num_chunks) { 
                print(paste0("starting chunk ", chunk))
                start.time <- Sys.time()
                r <- getChunkRange(chunk_size, chunk)
        
                #build & save a chunk
                saveChunk(buildLittleSample(sample[r$start:r$end]), chunk)

                print(paste0("finished chunk ", chunk))
                end.time <- Sys.time()
                time.taken <- end.time - start.time
                print(time.taken)
        }
        
        #aggregate chunks
        uniGram <- aggregateChunks("uni",num_chunks)
        biGram <- aggregateChunks("bi",num_chunks)
        triGram <- aggregateChunks("tri",num_chunks, remove_under = removeUnder)
        quatGram <- aggregateChunks("quat",num_chunks, remove_under = removeUnder)
        testing <- aggregateChunks("testing", num_chunks)

        #add sample
        sample <- sample
        
        #save 
        save(list = c("uniGram", "biGram", "triGram", "quatGram", "testing", "sample"), file = "BigNgramtables.robj")
         
        #wrap-up
        end.time <- Sys.time()
        time.taken <- end.time - start.time
        print("iterative sample build complete")
        print(time.taken)
        
        return(bs)
}

testBigSample <- function (samplesize, seed = 1000) {
        print("testing process started...")
        start.time <- Sys.time()
        
        #build the same sample and build the dataset directly rather than iteratively
        #num_chunks <- 2
        removeUnder <- 2
        set.seed(seed = seed)
        
        
        raw <- loadNgramData()
        combined <- c(raw$blogs, raw$news, raw$twitter)
        sample <- combined[sample(length(combined), samplesize * length(combined))]
        
        #divides data into training and test sets
        inTrain <- sample(length(sample), .95 * length(sample))
        training = sample[inTrain]
        testing = sample[-inTrain] #reserved observations for later testing
        
        #creates ngram tables for training
        uni <- createNGram(1L,training)
        bi <- createNGram(2L,training)
        tri <- createNGram(3L,training)
        quat <- createNGram(4L, training)
        
        #optionally, removes ngrams with counts less than removeUnder
        quat <- quat[quat$freq >= removeUnder,]
        tri <- tri[tri$freq >= removeUnder,]
        
        testNg <- (list("tri" = tri, "quat" = quat, "bi" = bi, "uni" = uni, "testing" = testing, "sample" = sample))
        
        #clean-up
        remove(combined, inTrain, training, raw)
        
        #now get the iteratively created version
        Ng <- buildBigSample(samplesize = samplesize)
        
        #then compare some things
        testResults <- list()
        testResults$sampleLength <- length(Ng) == length(testNg)
        testResults$triRows <- nrow(Ng$tri) == nrow(testNg$tri)
        testResults$biRows <- nrow(Ng$bi) == nrow(testNg$bi)
        testResults$uniRows <- nrow(Ng$uni) == nrow(testNg$uni)
        testResults$quatRows <- nrow(Ng$quat) == nrow(testNg$quat)
        
        end.time <- Sys.time()
        time.taken <- end.time - start.time
        print("testing process complete")
        print(time.taken)
        
        return (list("Ng" = Ng, "testNg" = testNg, "results" = testResults))
}

createNGram <- function (n, lines, addCols = TRUE) {
        nGram <- as.data.frame(rev(sort(table(
                tokenize(tolower(lines), what=c("word"), 
                         removeNumbers = TRUE,removePunct = TRUE,
                         removeSymbols = TRUE, removeSeparators = TRUE, 
                         removeTwitter = TRUE, removeHyphens = TRUE, removeURL = TRUE, 
                         ngrams = n, skip = 0L,concatenator = "_", simplify = TRUE, 
                         verbose = FALSE)
        ))), stringsAsFactors = FALSE)
        colnames(nGram) <- c("ngram", "freq")
        
        if(addCols == TRUE) {
                nGram$leader <- unlist(lapply(nGram$ngram,getLeader))
                nGram$lastWord <- unlist(lapply(nGram$ngram,getLastWord))
                colnames(nGram) <- c("ngram", "freq", "leader", "lastWord")
        }
         

        return(nGram)
}

tester <- function(numTests, guessesAllowed, seed = 10000) {
        
        start.time <- Sys.time()
        
        set.seed(seed)
        trials <- quatGramTest[sample(nrow(quatGramTest), size = numTests),]
        
        success = c()
        
        for (i in 1:numTests) {
                distro <- getDistro(trials$leader[i], triGram, biGram, uniGram)
                success[i] <- trials$lastWord[i] %in% head(distro$lastWord, guessesAllowed)
                #coverage[i] <- nrow(distro) > guessesAllowed
        }
       
         print(paste("Finished in", timetaken(start.time), "...", "to run", numTests, "tests"))
        
        return(success)
}

#gets the last word from an ngram
getLastWord <- function (ngramIn) {
        places <- gregexpr(pattern ='_',ngramIn)[[1]]
        lastWord <- substring(ngramIn, places[length(places)]+1, nchar(ngramIn))
        return(lastWord)
}

#getst the last word from an ngram
getLeader <- function (ngramIn) {
        if(!is.null(ngramIn)) {
                places <- gregexpr(pattern ='_',ngramIn)[[1]]
                leader <- substring(ngramIn, 1, places[length(places)]-1)
        }
        else {leader <- NULL}
        return(leader)
}


getLastNTokens <- function (ngramIn, n) {
        if (!is.null(ngramIn)) {
                places <- gregexpr(pattern ='_',ngramIn)[[1]]
                start <- places[length(places)-(n-1)] + 1
                lastN <- substring(ngramIn, start, nchar(ngramIn))
        }
        else {lastN = NULL}
        return(lastN)
}


countFreq <- function(k, v = NULL) {
        return(sum(v==k))
}




getDistrobu <- function(line, nGramIn, triGramsIn, biGramsIn, uniGramsIn, dict, POStriplets) {
        #this is a linear interpolation model
        
        #flag for using part of speech or not
        POSflag <- FALSE
        
        #these are the alpha values for weighting the counts for trigram hits, bigram hits, and unigrams
        aTri <- .9
        aBi <- .08
        aUni <- .02
        
        #get leaders
        biGramIn <- paste0(getLastWord(getLeader(nGramIn)), "_", getLastWord(nGramIn))
        uniGramIn <- getLastWord(nGramIn)
        
        #get matches
        triMatches <- triGramsIn[biGramIn == triGramsIn$leader,]
        biMatches <- biGramsIn[uniGramIn == biGramsIn$leader,]
        if (POSflag){
                POSUnigram <- POSFilter(line, uniGramsIn$lastWord, POSDictionary, POStriplets) 
                uniMatches <- uniGramsIn[POSUnigram,]   
        }
        else {uniMatches <- uniGramsIn}
        #uniMatches <- uniGramsIn #remember that all unigrams "match"
        
        #remove duplicates
        biMatches <- biMatches[!(biMatches$lastWord %in% triMatches$lastWord),]
        uniMatches <- uniMatches[!(uniMatches$lastWord %in% triMatches$lastWord),]
        uniMatches <- uniMatches[!(uniMatches$lastWord %in% biMatches$lastWord),]
        
        #weight the frequencies
        triMatches$freq <- triMatches$freq * aTri
        biMatches$freq <- biMatches$freq * aBi
        uniMatches$freq <- uniMatches$freq * aUni
        
        matches <- rbind(triMatches, biMatches, uniMatches)
        
        #add a frequency percent column
        c = sum(as.numeric(matches$freq))
        matches$freq_per <- as.numeric(matches$freq)/c
        
        return(matches[order(matches$freq_per, decreasing = TRUE),])
        
}

#the workhorse - gives the "answer" - a distribution of possible last words
getDistro <- function(line, nGramIn, triGramsIn, biGramsIn, uniGramsIn, dict, POStriplets) {
        
        #this line lets the user pass in any ngram size larger than unigram
        biGramIn <- paste0(getLastWord(getLeader(nGramIn)), "_", getLastWord(nGramIn))
        
        #get tri-gram matches
        matches <- triGramsIn[biGramIn == triGramsIn$leader,]
        
        #apply standard discount .5
        preFreqSum <- sum(as.integer(matches$freq))
        matches$freq <- matches$freq - .5
        postFreqSum <- sum(as.numeric(matches$freq))
        D = preFreqSum - postFreqSum # the probability mass available for lower level ngrams
        
        #Get bigram matches     
        bimatches <- biGramsIn[getLastWord(biGramIn) == biGramsIn$leader,]
        bimatches <- bimatches[!(bimatches$lastWord %in% matches$lastWord), ]
        bic = sum(bimatches$freq)
        if (D > 0) {
                bimatches$freq <- (bimatches$freq/bic) * D
        }
        matches <- rbind(matches, bimatches)
        
        if (nrow(matches) < 50) {
                addUnis <- uniGramsIn
                POSUnigram <- POSFilter(line, uniGramsIn$lastWord, POSDictionary, POStriplets) 
                addUnis <- uniGramsIn[POSUnigram,]
                addUnis <- addUnis[!(addUnis$lastWord %in% matches$lastWord), ]
                addUnis$freq <- addUnis$freq/100000
                
                matches <- rbind(matches, addUnis)
        }
        
        #add a frequency percent column
        c = sum(as.numeric(matches$freq))
        matches$freq_per <- as.numeric(matches$freq)/c
        
        return(matches[order(matches$freq_per, decreasing = TRUE),])
}

#this function will return the subset of a distribution's answers that start with a character string
getStartsWithLastWords <- function (distroIn, prefix, count) {
        logicStartsWith <- startsWith(as.character(distroIn$lastWord), prefix)
        return(head(distroIn[logicStartsWith,]$lastWord,count))
}

#this function will return the last n words of a line of text as a ngram
#with separator separating the words
getLastNGram <- function (line, n, separator = "_") {
        if (is.null(line)) {return(NULL)}
        tokens <- tokenize(tolower(line), what=c("word"), 
                           removeNumbers = TRUE,removePunct = TRUE,
                           removeSymbols = TRUE, removeSeparators = TRUE, 
                           removeTwitter = TRUE, removeHyphens = TRUE, removeURL = TRUE, 
                           ngrams = n, skip = 0L,concatenator = separator, simplify = TRUE, 
                           verbose = FALSE)
        return(tokens[length(tokens)])
}

isEndOfWord <- function (ch) {
        #takes a single letter in and tests for end of word markers
        return(ch %in% c(" ", "?", ".", "!", ",", ";", ":"))
}

getBigramDistro <- function(triGrams, totalfreq) {
        ##REMEMBER TO RERUN WITH SINGLETON TRIGRAMS!
        getBi <- function (tri) {
                return(paste0(getLastWord(getLeader(tri)), "_", getLastWord(tri)))
        }
        
        #get the ending bigram for each triGram
        biInstances <- sapply(triGrams, getBi)
        
        #get distribution of each ending bigram
        tBis <- table(biInstances)
        freqSum <- sum(tBis)
        freqPerc <- tBis/freqSum
        bi <- data.frame(ngram = names(freqPerc), freq = as.numeric(freqPerc) * totalfreq)
        bi$leader <- sapply(as.character(bi$ngram), getLeader)
        bi$lastWord <- sapply(as.character(bi$ngram), getLastWord)
        bi <- bi[order(bi$freq, decreasing = TRUE),]
        return(bi)
}

getUnigramDistro <- function(lastwords, totalfreq) {
        #builds a unigram distro based on the number of unique bigrams the unigram completes
        
        tablelw <- table(lastwords)
        totallw <- sum(tablelw)
        perclw <- tablelw/totallw
        uni <- data.frame(ngram = names(perclw), freq = as.numeric(perclw) * totalfreq, leader = "", lastWord = names(perclw) )
        uni <- uni[order(uni$freq, decreasing = TRUE),]
}