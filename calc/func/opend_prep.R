library(plyr)
library(stringr)
library(tmt)
#library(tm)
#library(dplyr)

opend_prep <- function(src, varlist, raw_out = FALSE
                   , word_flag = c("barack","obama","obamacare","romney", "mccain")
                       ) {
    #########################################################
    # this function reads in a .csv file of openended survey
    # responses and preprocesses it for further analyses
    # it also adjust varnames to prepare for the usage
    # in mft_check
    # arguments:
    # - src: csv file location (string)
    # - varlist: original variable names to be replaced
    # - raw: logical argument, should output include raw data?
    # - word_flag: vector of strings that should not be checked for spelling
    # output:
    # - data frame consisting of preprocessed items
    #########################################################

    # set option for data.frame and read.csv!
    options(stringsAsFactors = FALSE)

    # load dataset
    raw <- read.csv(src)

    # select variables included in varlist
    raw <- raw[, c(which(names(raw) %in% names(varlist)))]

    # warning message if variables were not matched
    if(length(varlist)!=ncol(raw)) warning("Variables in varlist were not properly matched!")

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
        raw[,i][raw[,i] == ""] <- NA
    }

    # id as numeric
    raw[,1] <- as.numeric(raw[,1])

    # spell checks
    spell <- raw
    spell <- rbind(c(0,rep("xcaascjaclabcasaf", (nrow(spell)-1))), spell)
    # weird bug-fix for aspellCheck
    # it seems like the function does not work if there is no word that is not recognized
    # Without it, the corrections are not properly implemented
    # I should rewrite this or look for alternative ways, but it will work for now... 
    for(i in 2:ncol(raw)){
        spell[,i] <- aspellCheck(spell[,i], "fix", sep=T, split_missing=F, mode="normal"
                                , word_flag = tolower(word_flag))
    }
    spell <- spell[-1,]
    
    # adjust colum names
    spell <- spell[names(varlist)]
    colnames(spell) <- as.vector(varlist)

    # replace NA strings (created by aspellCheck) with actual NAs
    for(i in 2:ncol(spell)){
        spell[,i][spell[,i]=="NA"] <- NA
    }

    ifelse(raw_out
          , out <- list(spell = spell, raw = raw, vars = varlist)
          , out <- list(spell = spell, vars = varlist))
    return(out)           
}

