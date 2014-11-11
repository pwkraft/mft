setwd("/data/Uni/projects/2014/mft/calc")
rm(list=ls())
library(plyr)
library(stringr)
library(tmt)
#library(tm)
#library(dplyr)


# set option for data.frame and read.csv!
options(stringsAsFactors = FALSE)

# load dataset
raw <- read.csv("/data/Dropbox/1-src/data/anes/anes2012TS_openends.csv")
raw <- raw[,c(1,grep("candlik",names(raw)),grep("ptylik",names(raw)))]

# convert to lower cases
raw <- data.frame(mapply(function(x){tolower(x)},raw))
# all these commands would also work with data.frame(apply(raw,2,tolower))


# replace w/ b/w w/o
raw <- data.frame(mapply(function(x){gsub("w/o","without ",x,fixed=T)},raw))
raw <- data.frame(mapply(function(x){gsub("w/","with ",x,fixed=T)},raw))
raw <- data.frame(mapply(function(x){gsub("b/w","between ",x,fixed=T)},raw))

# convert NA's
raw <- data.frame(mapply(function(x){sub("1 inapplicable",NA,x,fixed=T)},raw))
raw <- data.frame(mapply(function(x){sub("7 refused",NA,x,fixed=T)},raw))

# get rid of punctuation
raw <- data.frame(mapply(function(x){gsub("[[:punct:]+]"," ",x)},raw))

# remove white spaces
raw <- data.frame(mapply(function(x){gsub("[[:space:]]+"," ",x)},raw))
raw <- data.frame(mapply(str_trim,raw))

# replace empty strings with NA's
for(i in 2:ncol(raw)){
  raw[,i][raw[,i]==""] <- NA
}

# id as numeric
raw[,1] <- as.numeric(raw[,1])

# spell checks
spell <- raw
for(i in 2:ncol(raw)){
  spell[,i] <- aspellCheck(raw[,i], "fix", sep=T, split_missing=F, mode="normal"
                            ,word_flag=c("barack","obama","obamacare","romney"))
}
colnames(spell) <- c("id","ca.li.dem","ca.di.dem","ca.li.rep","ca.di.rep","pa.li.dem","pa.di.dem","pa.li.rep","pa.di.rep")

# replace NA strings (created by aspellCheck) with actual NAs
for(i in 2:ncol(spell)){
  spell[,i][spell[,i]=="NA"] <- NA
}

# load dictionaries
dict <- list(auth = read.csv("./in/authority.csv",allowEscapes=T)[,1]
             , fair = read.csv("./in/fairness.csv",allowEscapes=T)[,1]
             , harm = read.csv("./in/harm.csv",allowEscapes=T)[,1]
             , ingr = read.csv("./in/ingroup.csv",allowEscapes=T)[,1]
             , puri = read.csv("./in/purity.csv",allowEscapes=T)[,1])

# check responses for dictionary entries
resp <- data.frame(spell[,1])
for(v in 2:ncol(spell)){
  for(d in 1:length(dict)){
    resp <- cbind(resp,apply(laply(dict[[d]], function(x) {str_detect(spell[,v], x)}),2,sum))
  }
}
colnames(resp) <- c("id",as.vector(laply(names(dict),function(x) paste(x,colnames(spell)[-1],sep="."))))

# function to count number of words
nwords <- function(string, pseudo=F){
  ifelse( pseudo, 
          pattern <- "\\S+", 
          pattern <- "[[:alpha:]]+" 
        )
  str_count(string, pattern)
}
# count number of words in each item
num <- data.frame(apply(spell[,-1],2,nwords))
colnames(num) <- paste("num", colnames(num), sep = ".")
# adjust for NAs
for(i in 1:ncol(num)){
    num[is.na(spell[,i+1]),i] <- 0
}
# calculate total numer of words
num$num.total <- apply(num, 1, sum)
# add to resp matrix
resp <- cbind(resp,num)

# save objects for analyses
save(resp, spell, file="/data/Dropbox/1-src/data/anes/anes2012mft.RData")
