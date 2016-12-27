get#bootstrap for ngram model build 

#load libraries

library(qdapDictionaries)
library(reshape2)
library(ggplot2)
library(gmodels)
library(caret)
library(data.table)
library(ISLR)
library(e1071)
library(gbm)
library(gtools)
library(plyr)
library(dplyr)
library(NLP) 
library(openNLP)
library(tm)
library(quanteda)

gc()
 
#parameters
seed = 10000
set.seed(seed)
samplesize <- .10
removeUnder <- 2


#load necessary functions
source('ngramRPP.R')

##load ngram tables if the objects are available on disk
ngramFile <- paste0("ngramtables", as.character(100*samplesize),".robj")
if (file.exists(ngramFile)) {
        print("loading ngram tables from disk")
        load(ngramFile)
##if the saved objects aren't there then create them
}else {
        print("building ngram tables...")
        raw <- loadNgramData()
        
        #get sample_size percent of the data into a combined vector of lines
        combined <- getSample(samplesize, raw$blogs, raw$news, raw$twitter)
        
        #divides data into training and test sets
        inTrain <- sample(length(combined), .95 * length(combined))
        training = combined[inTrain]
        testing = combined[-inTrain] #reserved observations for later testing
        
        #creates ngram tables for training
        triGram <- createNGram(3L,training)
        biGram <- createNGram(3L,training)
        biGram <- createNGram(1L,training)
        #biGram <- getBigramDistro(triGram$ngram, sum(triGram$freq))
        #uniGram <- getUnigramDistro(biGram$lastWord, sum(triGram$freq))
        
        #optionally, removes ngrams with counts less than removeUnder
        triGram <- triGram[triGram$freq >= removeUnder,]
        
        #save objects to file
        save("uniGram", "triGram", "biGram", "testing", file = ngramFile)
        
        #clean-up
        remove(combined, inTrain, samplesize, raw, ngramFile)
        
}

if(!exists("testing")) {
        print("building ngram tables...")
        raw <- loadNgramData()
        
        #get sample_size percent of the data into a combined vector of lines
        set.seed(seed)
        combined <- getSample(samplesize, raw$blogs, raw$news, raw$twitter)
        
        #divides data into training and test sets - here we are just recreating the test set
        inTrain <- sample(length(combined), .95 * length(combined))
        testing = combined[-inTrain] #reserved observations for later testing
}

dictFile <- "POSDictionary.robj"
load(dictFile)
tripFile <- "triplets.robj"
load(tripFile)
