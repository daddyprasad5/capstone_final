##Build a function for testing

library ( plyr )
library ( dplyr )
library ( NLP )
library ( openNLP )
#library ( openNLPmodels.en )
library ( tm )
library ( stringr )
library ( gsubfn )
library(data.table)
library(quanteda)

#source("ngramBootstrap.R")

getTestSet <- function(testing) {
        
        test_tokens_raw <- tokenize(tolower(testing), what=c("word"), 
                                    removeNumbers = TRUE,removePunct = TRUE,
                                    removeSymbols = TRUE, removeSeparators = TRUE, 
                                    removeTwitter = TRUE, removeHyphens = TRUE, removeURL = TRUE, 
                                    ngrams = 3L, skip = 0L,concatenator = "_", simplify = TRUE, 
                                    verbose = TRUE)
        
        nObs <- length(test_tokens_raw)
        lastWord <- character(nObs)
        leader <- character(nObs)
        ngram <- character(nObs)
        
        for(i in 1:nObs) {
                ngram[i] <- test_tokens_raw[i]
                places <- gregexpr(pattern ='_',fourword)[[1]]
                leader[i] <- getLeader(fourword)
                lastWord[i] <- getLastWord(fourword)
                ngram[i] <- fourword
        }
        
        quatGramTest <- data.frame(leader, ngram, lastWord, stringsAsFactors = FALSE)
        
        end.time <- Sys.time()
        time.taken <- end.time - start.time
        time.taken
        
        return(quatGramTest)        
}

getPerplexity <- function(testing, guesses, triGram, biGram, uniGram, POSDictionary, POStriplets) {
        
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
        
        eval <- function(w, guesses, triGram, biGram, uniGram, POSDictionary, POStriplets) {
                leader <- getLeader(w)
                lw <- getLastWord(w)
                d <- getDistro(w, leader, triGram, biGram, uniGram, POSDictionary, triplets)
                
                #get the type of ngram for each of the top guesses
                t <- sapply(head(d$ngram,guesses), nGramType)
                
                #determine if the actual last word is in the top guesses
                if (lw %in% head(d$lastWord, guesses)) { m <- TRUE }
                else { m <- FALSE}
                
                #get 1 / probability of this last word following the leader, given training data
                n <- sum(d$freq) #total of all hits in training set
                f <- d[d$lastWord == lw,]$freq #hits for the actual next word in this test
                if (length(f) == 0) {f<-.2} ###NEED TO FIGURE MORE CORRECT WAY TO HANDLE THIS
                p <- 1 / (f/n) #1 divieded by probability in training set of the next word, given the leader
                
                return(list("p" = p, "m" = m, "t" = t))
        }
       
        evalHit <- function(w, guesses, triGram, biGram, uniGram, POSDictionary, POStriplets) {
                leader <- getLeader(w)
                lw <- getLastWord(w)
                d <- getDistro(w, leader, triGram, biGram, uniGram,POSDictionary, POStriplets)
                
                #get the type of ngram for each of the top guesses
                t <- sapply(head(d$ngram,guesses), nGramType)
                
                if (lw %in% head(d$lastWord, guesses)) { m <- TRUE }
                else { m <- FALSE}
                return(list("m" = m, "t" = t))
        }

        
        testSet<- tokenize(tolower(testing), what=c("word"), 
                           removeNumbers = TRUE,removePunct = TRUE,
                           removeSymbols = TRUE, removeSeparators = TRUE, 
                           removeTwitter = TRUE, removeHyphens = TRUE, removeURL = TRUE, 
                           ngrams = 3L, skip = 0L,concatenator = "_", simplify = TRUE, 
                           verbose = TRUE)
        N = length(testSet)
        
        #parallel version
#         no_cores = detectCores() - 1
#         c1 <- makeCluster(no_cores, type="FORK")
#         e <- parSapply(c1, X = testSet, FUN = eval, guesses, triGram, biGram, uniGram)
        #m <- parSapply(c1, X = testSet, FUN = evalHit, guesses, triGram, biGram, uniGram)
        #p <- parSapply(c1, X = testSet, FUN = getperp, triGram, biGram, uniGram)
        
        # stopCluster(c1)        
        
        #single-threaded version
        e <- sapply(X = testSet, FUN = eval, guesses, triGram, biGram, uniGram, POSDictionary, POStriplets)
        #e <- sapply(X = testSet, FUN = evalHit, guesses, triGram, biGram, uniGram, POSDictionary, POStriplets)
        
        p <- unlist(e[1,])
        m <- unlist(e[2,])
        t <- unlist(e[3,])
        
        mm <- table(m)[2]/sum(table(m))
        pp <- 10^(sum(log(p, 10))*1/N)
        
        return(list("mm" = mm, "m" = m, "p" = p, "pp" = pp, "t" = t))
        #return(list("mm" = mm, "m" = m, "t" = t))
}

getMatchRate <- function(testing, guesses, triGram, biGram, uniGram, POSDictionary, POStriplets) {
        
        evalHit <- function(w, guesses, triGram, biGram, uniGram, POSDictionary, POStriplets ) {
                leader <- getLeader(w)
                lw <- getLastWord(w)
                d <- getDistro(w, leader, triGram, biGram, uniGram,POSDictionary, POStriplets )
                if (lw %in% head(d$lastWord, guesses)) { ret <- TRUE }
                else { ret <- FALSE}
                return(ret)
        }
        
        testSet<- tokenize(tolower(testing), what=c("word"), 
                           removeNumbers = TRUE,removePunct = TRUE,
                           removeSymbols = TRUE, removeSeparators = TRUE, 
                           removeTwitter = TRUE, removeHyphens = TRUE, removeURL = TRUE, 
                           ngrams = 3L, skip = 0L,concatenator = "_", simplify = TRUE, 
                           verbose = TRUE)
        
        N = length(testSet)
        m <- vapply(X = testSet, FUN = evalHit, FUN.VALUE = c("m" = TRUE), guesses, triGram, biGram, uniGram)
        return(m)
}

test_intraword <- function(testing, num_char, triGram, biGram, uniGram, POSDictionary, POStriplets) {
        
        eval <- function(w, num_char, triGram, biGram, uniGram, POSDictionary, POStriplets) {
                leader <- getLeader(w)
                answer <- getLastWord(w)
                d <- getDistro(w, leader, triGram, biGram, uniGram,POSDictionary, POStriplets )
                prefix <- substr(answer, 1, num_char)
                best_guess <- getStartsWithLastWords(d, prefix,1)
                if (length(best_guess) == 0) {ret <- FALSE}
                else {ret <- (answer == best_guess)}
                return (ret)
        }
        
        testSet<- tokenize(tolower(testing), what=c("word"), 
                           removeNumbers = TRUE,removePunct = TRUE,
                           removeSymbols = TRUE, removeSeparators = TRUE, 
                           removeTwitter = TRUE, removeHyphens = TRUE, removeURL = TRUE, 
                           ngrams = 3L, skip = 0L,concatenator = "_", simplify = TRUE, 
                           verbose = TRUE)
        vgetLastWord <- Vectorize(getLastWord)
        testSet <- testSet[nchar(vgetLastWord(testSet))>num_char]
        pf <- vapply(X = testSet, FUN = eval, FUN.VALUE = c("pf" = TRUE), num_char, triGram, biGram, uniGram, POSDictionary, triplets)
        pfrate <- table(pf)[2]/sum(table(pf))
        return(list("pfrate" = pfrate, "pf" = pf))
}

start.time <- Sys.time()
print("starting at")
print(start.time)
print("setting up...")
samplesize <- .1
trials = 30
seed = 10000
set.seed(seed)
guesses <- 1
prefix_length <- 1

source("ngramBootstrap.R")

print("sampling...")
set.seed(seed)
testData <- testing[sample(length(testing), size = trials)]

print("testing...")
#res <- getPerplexity(testData, guesses, triGram, biGram, uniGram, POSDictionary, triplets)
res <- test_intraword(testData, prefix_length, triGram, biGram, uniGram, POSDictionary, triplets)

print(paste("Finished in", timetaken(start.time), "..."))