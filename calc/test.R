###########################################################################################
## Project:  Moral Foundations of Political Reasoning
## File:     prep.R
## Overview: prepares open-ended survey responses in the 2012 ANES as well as the original
##           time series datasets for subsequent analyses in analyses.R
## Author:   Patrick Kraft
###########################################################################################

## packages
pkg <- c("readstata13","car","dplyr","quanteda")
invisible(lapply(pkg, library, character.only = TRUE))
rm(list=ls())


data <- data.frame(rbind(c(1,1,1,1,1)
                         , c(2,2,2,0,0)
                         , c(1,1,1,0,0)
                         , c(0,0,0,1,0)
                         , c(0,0,0,2,0)))

N <- nrow(data)
nt <- apply(data,2,function(x) sum(x>0))
idf <- log10(N/nt)
idfe <- log(N/nt)
wdoc <- apply(data,1,sum)

data %>% as.dfm() %>% tfidf()
t(t(data)*idf)

data %>% as.dfm() %>% tfidf(normalize = T)
t(t(data/wdoc)*idf)

data %>% as.dfm() %>% tfidf(normalize = T, base=exp(1))
t(t(data/wdoc)*idfe)


## check how addition of mft dict changes formula
dat <- data[-1,]
N <- nrow(dat) + 1
nt <- apply(dat,2,function(x) sum(x>0)) + 1
idf <- log10(N/nt)
wdoc <- apply(dat,1,sum)
t(t(dat)*idf)
t(t(dat/wdoc)*idf)


