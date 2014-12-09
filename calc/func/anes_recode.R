##########################################################################################
# Project:  Moral foundations of Political Reasoning
# File:     anes_recode.R
# Overview: this file contains several functions used in mft_prep.R to preprocess
#           the open-ended responses, recode each anes TS dataset,
#           and merge both datasets.
# Author:   Patrick Kraft
# Date:     12/08/2014
##########################################################################################


###############################
### Load packages and functions

# install / load required packages
pkg <- c("plyr","stringr","tmt","foreign", "car") # tm, dplyr
inst <- pkg %in% installed.packages()  
if(length(pkg[!inst]) > 0) install.packages(pkg[!inst])  
lapply(pkg,function(x){suppressPackageStartupMessages(library(x,character.only=TRUE))})
rm(pkg, inst)


###############################################################
### Preprocess open-ended survey responses, spell-checking etc.

opend_prep <- function(csv_src, varlist, raw_out = FALSE
                   , word_flag = c("barack","obama","obamacare","romney", "mccain")
                       ) {
    #########################################################
    # This function reads in a .csv file of open-ended survey
    # responses and preprocesses it for further analyses
    # it also adjust varnames to prepare for the usage
    # in mft_check
    # arguments:
    # - src: csv file location (string)
    # - varlist: original variable names to be replaced
    # - raw_out: logical argument, should output include raw data?
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


######################################
### word count based on mft dictionary

opend_mft <- function(data, use_dict = "new") {
    #####################################################################
    # This function uses the MFT dictionaries to count instances
    # for each dimension in open responses
    # arguments:
    # - data: preprocessed open-ended survey responses (via openend_prep)
    # - dict: either "new" or "old" for both dictionary versions
    # output:
    # - data frame containing counts for each moral foundation
    #   according to the dictinary specified in the function
    #   as well as a total word count for each item
    #####################################################################
    
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


############################################################
### Basic variable recoding for each ANES time series survey

ts_recode <- function(dta_src, raw_out = FALSE
                      , id          = NULL
                      , ideol       = NULL
                      , issues      = NULL
                      , issue_aid   = NULL
                      , issue_abort = NULL
                      , issue_gay   = NULL
                      , issue_women = NULL
                      , pid         = NULL
                      , polint      = NULL
                      , polmedia    = NULL
                      , polknow     = NULL
                      , poldisc     = NULL
                      , regdisc     = list(year = NULL, byear = NULL, bmonth = NULL)
                      , pastvote    = NULL
                      , age         = NULL
                      , female      = NULL
                      , black       = NULL
                      , educ        = NULL
                      , relig       = list(oft = NULL, ever = NULL, more = NULL)
                      ){
    ###############################################################
    # This function implements basic recoding for each of the 
    # anes time-series datasets
    # arguments:
    # - dta_src: dta file location
    # - raw_out: logical argument indicating whether the original
    #            dataset should be included in the output
    # - id to relig: respective variable names to be recoded
    # output:
    # - data: dataset consisting of recoded variables
    # - raw: raw .dta dataset
    ###############################################################

    ### set option for data.frame and read.csv!
    options(stringsAsFactors = FALSE)

    ### recode independent variables
    raw <- read.dta(dta_src, convert.factors = FALSE)
    if(is.null(id)) stop("ID variable must be specified!")
    dat <- data.frame(id=raw[,id])

    if(!is.null(ideol)){
        ## ideology
        # d/k -> moderate
        dat$ideol <- factor(recode(raw[,ideol]
                            , "1:3=1; c(-2,-8,4)=3; 5:7=2; else=NA")
                     , labels = c("Liberal","Conservative","Moderate"))
        dat$ideol_lib <- as.numeric(dat$ideol=="Liberal")
        dat$ideol_con <- as.numeric(dat$ideol=="Conservative")

        ## strength of ideology
        dat$ideol_str <- abs(recode(raw[,ideol], "c(-2,-8)=0; -9=NA") - 4)
        dat$ideol_str_c <- dat$ideol_str - mean(dat$ideol_str, na.rm = T)
    }

    if(!is.null(issues)){
        ## issue positions (7-point scales)
        if(class(issues)!="list") stop("'issues' argument must be a list")
        for(i in 1:length(issues)){
            dat$issue <- recode(raw[,issues[[i]][1]], "-2=4; c(-9,-8,-1)=NA")
            if(length(issues[[i]])>1) warning("Insert second var in function")
            colnames(dat[,ncol(dat)]) <- paste0("issue_",names(issues)[i])
        }
    }

    if(!is.null(issue_aid)){
        ## assistance to poor
        dat$issue_aid <- recode(raw[,issue_aid], "2=-1; 3=0; c(-9,-8)=NA")
    }

    if(!is.null(issue_abort)){
        ## abortion
        dat$issue_abort <- recode(raw[,issue_abort], "c(-9,-8,5)=NA")
    }

    if(!is.null(issue_gay)){
        ## gay adoption
        dat$issue_gay <- recode(raw[,issue_gay], "c(-9,-8)=NA; 2=0")
    }

    if(!is.null(issue_women)){
        ## women's role
        # item does not seem to be part of anes 2012
    }

    if(!is.null(pid)){
        ## party identification
        dat$pid <- factor(recode(raw[,pid]
                          , "1:3=1; 4=3; 5:7=2; else=NA")
                   , labels = c("Democrat","Republican","Independent"))
        dat$pid_dem <- as.numeric(dat$pid=="Democrat")
        dat$pid_rep <- as.numeric(dat$pid=="Republican")

        ## strength of partisanship
        dat$pid_str <- abs(recode(raw[,pid], "-2 = NA") - 4)
        dat$pid_str_c <- dat$pid_str - mean(dat$pid_str, na.rm = T)
    }

    if(!is.null(polint)){
        ## political interest
        dat$polint <- (-1) * recode(raw[,polint], "lo:0 = NA") + 5
        dat$polint_c <- dat$polint - mean(dat$polint, na.rm = T)
    }

    if(!is.null(polmedia)){
        ## political media consumption
        
    }

    if(!is.null(polknow)){
        ## political knowledge
        
    }

    if(!is.null(poldisc)){
        ## political discussion
    }

    if(!is.null(regdisc)){
        ## regression discontinuity based on eligibility in last election
    }

    if(!is.null(age)){
        ## age
        dat$age <- recode(raw[,age], "-2 = NA")
    }

    if(!is.null(female)){
        ## sex
        dat$female <- raw[,female] - 1
    }

    if(!is.null(black)){
        ## race
        dat$black <- as.numeric(recode(raw[,black], "lo:0 = NA") == 2)
    }

    if(!is.null(educ)){
        ## education: college degree (bachelor)
        dat$educ <- recode(raw[,educ], "1:3=0; 4:5=1; lo:0 = NA")
    }
    
    if(!is.null(relig$oft)){
        ## religiosity (church attendance)
        dat$relig <- (-1) * recode(raw[,relig$oft], "lo:0 = NA") + 5
        if(!is.null(relig$ever)) dat$relig[raw[,relig$ever]==2] <- 0
        if(!is.null(relig$more)) dat$relig[raw[,relig$more]==2] <- 5
    }
}


######################################
### recode response data

load("/data/Dropbox/1-src/data/anes/anes2012mft.RData")

## delete spanish open-ended responses: web / pre capi / post capi
lookfor(raw,"lang")
spell[raw$profile_spanishsurv==1,2:ncol(spell)] <- NA
spell[raw$admin_pre_lang_start==2,2:ncol(spell)] <- NA
spell[raw$admin_post_lang_start==2,2:ncol(spell)] <- NA
resp[raw$profile_spanishsurv==1,2:ncol(resp)] <- NA
resp[raw$admin_pre_lang_start==2,2:ncol(resp)] <- NA
resp[raw$admin_post_lang_start==2,2:ncol(resp)] <- NA
mft <- data.frame(id = resp[,1])

## aggregating over all items
respAgg <- function(groupname){
  x <- as.numeric(apply(resp[,grep(groupname,colnames(resp))],1,sum,na.rm=T) > 0)
  x[apply(!is.na(resp[,grep(groupname,colnames(resp))]),1,sum)==0] <- NA
  x
}
mft$harm_all <- respAgg("harm")
mft$fair_all <- respAgg("fair")
mft$ingr_all <- respAgg("ingr")
mft$auth_all <- respAgg("auth")
mft$puri_all <- respAgg("puri")
mft$mft_all <- as.numeric(apply(mft[,grep("_all",colnames(mft))],1,sum) > 0)

## aggregating over party evaluations
mft$harm_pa <- respAgg("harm_[:lower:]*_pa")
mft$fair_pa <- respAgg("fair_[:lower:]*_pa")
mft$ingr_pa <- respAgg("ingr_[:lower:]*_pa")
mft$auth_pa <- respAgg("auth_[:lower:]*_pa")
mft$puri_pa <- respAgg("puri_[:lower:]*_pa")
mft$mft_pa <- as.numeric(apply(mft[,grep("_pa",colnames(mft))],1,sum) > 0)

## aggregating over candidate evaluations
mft$harm_ca <- respAgg("harm_[:lower:]*_ca")
mft$fair_ca <- respAgg("fair_[:lower:]*_ca")
mft$ingr_ca <- respAgg("ingr_[:lower:]*_ca")
mft$auth_ca <- respAgg("auth_[:lower:]*_ca")
mft$puri_ca <- respAgg("puri_[:lower:]*_ca")
mft$mft_ca <- as.numeric(apply(mft[,grep("_ca",colnames(mft))],1,sum) > 0)

### merge datasets
anes <- merge(anes,mft)
