library(plyr)
library(stringr)
#library(dplyr)

opend_mft <- function(data, use_dict = "new") {
    ##########################################
    # this function uses the MFT dictionnaires
    # to count instances for each dimension in open responses
    # arguments:
    # - data: preprocessed open ended survey responses
    # - dict: either "new" or "old" for both dictionary versions
    # output:
    ##########################################
    
    # set option for data.frame and read.csv!
    options(stringsAsFactors = FALSE)

    # load dictionary (new or old)
    if(use_dict == "new"){
        dict <- list(auth_vice = read.csv("./in/dict/authority_virtue.csv",allowEscapes=T)[,1]
                , fair_vice = read.csv("./in/dict/fairness_virtue.csv",allowEscapes=T)[,1]
                , harm_vice = read.csv("./in/dict/harm_virtue.csv",allowEscapes=T)[,1]
                , ingr_vice = read.csv("./in/dict/ingroup_virtue.csv",allowEscapes=T)[,1]
                , puri_vice = read.csv("./in/dict/purity_virtue.csv",allowEscapes=T)[,1]
                , auth_virtue = read.csv("./in/dict/authority_virtue.csv",allowEscapes=T)[,1]
                , fair_virtue = read.csv("./in/dict/fairness_virtue.csv",allowEscapes=T)[,1]
                , harm_virtue = read.csv("./in/dict/harm_virtue.csv",allowEscapes=T)[,1]
                , ingr_virtue = read.csv("./in/dict/ingroup_virtue.csv",allowEscapes=T)[,1]
                , puri_virtue = read.csv("./in/dict/purity_virtue.csv",allowEscapes=T)[,1])
    } else if(use_dict == "old"){
        dict <- list(auth = read.csv("in/graham/authority.csv",allowEscapes=T)[,1]
               , fair = read.csv("./in/graham/fairness.csv",allowEscapes=T)[,1]
               , harm = read.csv("./in/graham/harm.csv",allowEscapes=T)[,1]
               , ingr = read.csv("./in/graham/ingroup.csv",allowEscapes=T)[,1]
               , puri = read.csv("./in/graham/purity.csv",allowEscapes=T)[,1])
    } else {stop("Argument 'dict' must be either 'new' or 'old'")}

    # check responses for dictionary entries
    resp <- data.frame(data[,1])
    for(v in 2:ncol(data)){
        for(d in 1:length(dict)){
            resp <- cbind(resp,apply(laply(dict[[d]], function(x) {str_detect(data[,v], x)}),2,sum))
        }
    }
    colnames(resp) <- c("id",as.vector(laply(names(dict),function(x) paste(x,colnames(data)[-1],sep="_"))))

    # function to count number of words
    nwords <- function(string, pseudo=F){
        ifelse(pseudo, 
               pattern <- "\\S+", 
               pattern <- "[[:alpha:]]+" 
               )
        str_count(string, pattern)
    }

    # count number of words in each item
    num <- data.frame(apply(data[,-1],2,nwords))
    colnames(num) <- paste("num", colnames(num), sep = "_")

    # adjust for NAs
    for(i in 1:ncol(num)){
        num[is.na(data[,i+1]),i] <- 0
    }
    
    # calculate total number of words
    num$num.total <- apply(num, 1, sum)

    # add to resp matrix
    resp <- cbind(resp,num)
    return(resp)
}

