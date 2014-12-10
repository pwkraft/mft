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
                       , word_flag = c("barack","obama","obamas","obamacare"
                                       ,"romney","romneys","mccain","mccains")
                       ){
    #########################################################
    # This function reads in a .csv file of open-ended survey
    # responses and preprocesses it for further analyses
    # it also adjust varnames to prepare for the usage
    # in mft_check
    # arguments:
    # - src: csv file location (string)
    # - varlist: variables to be generated based on original varnames
    # - raw_out: logical argument, should output include raw data?
    # - word_flag: vector of strings that should not be checked for spelling
    # output:
    # - data frame consisting of preprocessed items
    #########################################################

    # set option for data.frame and read.csv!
    options(stringsAsFactors = FALSE)

    # load dataset
    raw <- read.csv(csv_src)

    # select variables included in varlist
    raw <- raw[, c(which(names(raw) %in% unlist(varlist)))]

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
    spell <- spell[unlist(varlist)]
    colnames(spell) <- names(varlist)

    # replace NA strings (created by aspellCheck) with actual NAs
    for(i in 2:ncol(spell)){
        spell[,i][spell[,i]=="NA"] <- NA
    }

    if(raw_out == TRUE){
        list(spell = spell, raw = raw, vars = as.matrix(varlist), call = match.call())
    } else if(raw_out == FALSE){
        list(spell = spell, vars = as.matrix(varlist), call = match.call())
    } else stop("'raw_out' must be a logical argument")
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
    num$num_total <- apply(num, 1, sum)

    # add to resp matrix
    resp <- cbind(resp,num)
    return(resp)
}


############################################################
### Basic variable recoding for each ANES time series survey

ts_recode <- function(dta_src, raw_out = FALSE
                      , id          = NULL
                      , weight      = NULL
                      , ideol       = NULL
                      , issues      = NULL
                      , issue_aid   = NULL
                      , issue_abort = NULL
                      , issue_gay   = NULL
                      , issue_women = NULL
                      , pid         = NULL
                      , polmedia    = NULL
                      , polknow     = NULL
                      , poldisc     = list(oft = NULL, ever = NULL, alternative = NULL)
                      , regrdisc    = list(year = NULL, byear = NULL, bmonth = NULL)
                      , pastvote    = NULL
                      , age         = NULL
                      , female      = NULL
                      , black       = NULL
                      , educ        = NULL
                      , relig       = list(oft = NULL, ever = NULL, more = NULL)
                      , spanish     = NULL
                      ){
    ###############################################################
    # This function implements basic recoding for each of the
    # anes time-series datasets
    # arguments:
    # - dta_src: dta file location
    # - raw_out: logical argument indicating whether the original
    #            dataset should be included in the output
    # - id to spanish: respective variable names to be recoded
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

    if(!is.null(weight)){
        ## survey weights
        dat$weight = raw[,weight]
    }

    if(!is.null(ideol)){
        ## ideology
        dat$ideol <- factor(recode(raw[,ideol[1]]
                            , "1:3=1; 4=3; 5:7=2; else=NA")
                     , labels = c("Liberal","Conservative","Moderate"))
        if(length(ideol)>1){
            tmp <- factor(recode(raw[,ideol[2]], "lo:0=NA")
                        , labels = c("Liberal","Conservative","Moderate"))
            dat$ideol[is.na(dat$ideol)] <- tmp[is.na(dat$ideol)]
            rm(tmp)
        }
        dat$ideol_lib <- as.numeric(dat$ideol=="Liberal")
        dat$ideol_con <- as.numeric(dat$ideol=="Conservative")

        ## strength of ideology
        dat$ideol_str <- abs(recode(raw[,ideol[1]], "lo:0=NA") - 4)
        if(length(ideol)>1) dat$ideol_str[ideol[2]>0] <- 0
        dat$ideol_str_c <- dat$ideol_str - mean(dat$ideol_str, na.rm = T)
    }

    if(!is.null(issues)){
        ## issue positions (7-point scales)
        if(class(issues)!="list") stop("'issues' argument must be a list")
        for(i in 1:length(issues)){
            dat$issue <- recode(raw[,issues[[i]][1]], "c(-2,-7)=4; c(-9,-8,-1)=NA")
            if(length(issues[[i]])==2){
                tmp <- recode(raw[,issues[[i]][2]], "c(-1,-8,-9)=NA")
                dat$issue[is.na(dat$issue)] <- tmp[is.na(dat$issue)]
                rm(tmp)
            }
            if(length(issues[[i]])==3 & issues[[i]][3]=="reversed"){
                tmp <- (-1) * recode(raw[,issues[[i]][2]], "c(-1,-8,-9)=NA") + 8
                dat$issue[is.na(dat$issue)] <- tmp[is.na(dat$issue)]
                rm(tmp)
            }
            
            colnames(dat)[ncol(dat)] <- paste0("issue_",names(issues)[i])
        }
    }

    if(!is.null(issue_aid)){
        ## assistance to poor
        dat$issue_aid <- recode(raw[,issue_aid], "c(-8,-9) = NA")
        dat$issue_aid <- as.numeric(factor(dat$issue_aid
                                         , labels = (1:length(table(dat$issue_aid)))))
        dat$issue_aid <- recode(dat$issue_aid, "c(2,4)=-1; 3=0", as.numeric.result = T)
    }

    if(!is.null(issue_abort)){
        ## abortion
        dat$issue_abort <- recode(raw[,issue_abort], "c(-9,-8,5)=NA")
    }

    if(!is.null(issue_gay)){
        ## gay adoption
        tmp <- recode(raw[,issue_gay], "c(-9,-8)=NA")
        dat$issue_gay <- as.numeric(tmp == 1)
        rm(tmp)
    }

    if(!is.null(issue_women)){
        ## women's role
        dat$issue_women <- recode(raw[,issue_women[1]], "c(-9,-7,-1)=NA")
    }

    if(!is.null(pid)){
        ## party identification
        tmp <- raw[,pid] + ifelse(max(raw[,pid])==6, 1, 0)
        dat$pid <- factor(recode(raw[,pid]
                          , "1:3=1; 4=3; 5:7=2; else=NA")
                   , labels = c("Democrat","Republican","Independent"))
        dat$pid_dem <- as.numeric(dat$pid=="Democrat")
        dat$pid_rep <- as.numeric(dat$pid=="Republican")

        ## strength of partisanship
        dat$pid_str <- abs(tmp - 4)
        dat$pid_str_c <- dat$pid_str - mean(dat$pid_str, na.rm = T)
        rm(tmp)
    }

    if(!is.null(polmedia)){
        ## political media consumption
        if(class(polmedia)!="list") stop("'polmedia' argument must be a list")
        dat$polmedia <- 0
        for(i in 1:length(polmedia)){
            tmp <- recode(raw[,polmedia[[i]][1]], "c(-8,-9)=NA; -1=0")
            if(length(polmedia[[i]])>1){
                tmp[raw[,polmedia[[i]][1]]==-1] <- recode(raw[,polmedia[[i]][2]]
                                , "c(-8,-9,-1)=NA")[raw[,polmedia[[i]][1]]==-1]
            }
            dat$polmedia <- dat$polmedia + tmp
            rm(tmp)
        }
        dat$polmedia_c <- dat$polmedia - mean(dat$polmedia, na.rm = T)
    }

    if(!is.null(polknow)){
        ## political knowledge
        if(class(polknow)!="list") stop("'polknow' argument must be a list")
        dat$polknow <- 0
        for(i in 1:length(polknow)){
            tmp <- recode(raw[,names(polknow)[i]], "c(-2)=NA") # DK/mis treated as 0
            dat$polknow <- dat$polknow + as.numeric(tmp == polknow[[i]])
            rm(tmp)
        }
        dat$polknow_c <- dat$polknow - mean(dat$polknow, na.rm = T)
    }

    if(!is.null(poldisc$oft)){
        ## political discussion
        if(class(poldisc)!="list") stop("'poldisc' argument must be a list")
        dat$poldisc <- recode(raw[,poldisc$oft], "lo:-1 = NA")
        if(!is.null(poldisc$ever)) dat$poldisc[raw[,poldisc$ever]>1] <- 0
        if(!is.null(poldisc$alternative)){
            dat$poldisc[raw[,poldisc$oft]==-1] <- recode(raw[,poldisc$alternative]
               , "lo:-1 = NA")[raw[,poldisc$oft]==-1]
        }
        dat$poldisc_c <- dat$poldisc - mean(dat$poldisc, na.rm = T)
    }

    if(!is.null(regrdisc$year) * !is.null(regrdisc$byear)){
        ## regression discontinuity based on eligibility in last election
        if(class(regrdisc)!="list") stop("'regrdisc' argument must be a list")
        tmp <- recode(raw[,regrdisc$byear], "lo:0=NA")
        dat$regrdisc <- regrdisc$year - 4 - tmp
        dat$regrdisc <- recode(dat$regrdisc, "18=1; 17=0; else=NA")
        rm(tmp)
        if(!is.null(regrdisc$bmonth)){
            dat$regrdisc[!is.na(dat$regrdisc) & dat$regrdisc==0] <- (-1) * recode(
               raw[,regrdisc$bmonth], "lo:0=NA")[!is.na(dat$regrdisc) & dat$regrdisc==0] - 2
            dat$regrdisc[!is.na(dat$regrdisc) & dat$regrdisc==1] <- (-1) * (recode(
               raw[,regrdisc$bmonth], "lo:0=NA")[!is.na(dat$regrdisc) & dat$regrdisc==1] - 11)
        }
    }

    if(!is.null(pastvote)){
        ## voted in previous election
        dat$pastvote <- recode(raw[,pastvote], "lo:0=NA; c(2,5)=0")
    }

    if(!is.null(age)){
        ## age
        dat$age <- recode(raw[,age], "c(-2,-9,-8) = NA")
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
        if(class(educ)!="list") stop("'educ' argument must be a list")
        dat$educ <- as.numeric(raw[,names(educ)[1]]>=educ)
        dat$educ[raw[,names(educ)[1]]<0] <- NA
    }

    if(!is.null(relig$oft)){
        ## religiosity (church attendance)
        if(class(relig)!="list") stop("'relig' argument must be a list")
        dat$relig <- (-1) * recode(raw[,relig$oft], "lo:0 = NA") + 5
        if(!is.null(relig$ever)) dat$relig[raw[,relig$ever]!=1] <- 0
        if(!is.null(relig$more)) dat$relig[raw[,relig$more]==2] <- 5
    }

    if(!is.null(spanish)){
        ## spanish speaking respondent
        if(class(spanish)!="list") stop("'spanish' argument must be a list")
        dat$spanish <- 0
        for(i in 1:length(spanish)){
            dat$spanish[raw[,names(spanish)[i]]==spanish[[i]]] <- 1
        }
    }

    if(raw_out==TRUE){
        out <- list(data = dat, raw = raw, call = match.call())
    } else {
        out <- list(data = dat, call = match.call())
    }
}


######################################
### recode response data

## internal function to aggregate over all items
respAgg <- function(data, mftdim){
    ###############################################################
    # This is an internal function for anes_merge. It creates a
    # dummy indicating whether the respective moral foundation
    # dimension was mentioned in any of the items
    # arguments:
    # - data: matrix of response 
    # - mftdim: character string indicating the respective
    #           mft dimension
    # output:
    # - x: dummy variable indicating whether mftdim was mentioned
    ###############################################################
    x <- as.numeric(apply(data[, grep(mftdim, colnames(data), perl = TRUE)]
                         , 1, sum,na.rm = TRUE) > 0)
    x[apply(!is.na(data[, grep(mftdim, colnames(data), perl = TRUE)])
           , 1, sum) == 0] <- NA
    return(x)
}

anes_merge <- function(ts, opend, valence = FALSE, check = TRUE){
    ###############################################################
    # This function aggregates the mft instances over defined
    # item categories, aggregates word counts, and then merges
    # the aggregated data with the recoded time-series datasets
    # arguments:
    # - ts: time-series/cross-sectional anes data
    # - open: preprocessed open-ended anes data (from opend_mft)
    # - valence: logical argument indicating whether valence in
    #            the dictionaries should be differentiated for
    #            aggregation
    # - check: logical argument indicating whether the output
    #          should contain an additional full data frame to
    #          make it easier to check all recodings
    # output:
    # - data: dataset ready for subsequent analyses; contains
    #         ts as well as evaluated opend data
    # - check: complete dataset to check all recodings
    # - call: original function call
    ###############################################################
    
    ## extract relevant objects from ts and opend
    dat <- ts$data
    resp <- opend$resp
    spell <- opend$spell

    ## recode spanish open-ended responses as NA
    spell[spell$id %in% dat$id[dat$spanish==1], 2:ncol(spell)] <- NA
    resp[resp$id %in% dat$id[dat$spanish==1], 2:ncol(resp)] <- NA
    mft <- data.frame(id = resp$id)

    ## aggregating over all items
    mft$harm_all <- respAgg(resp,"harm")
    mft$fair_all <- respAgg(resp,"fair")
    mft$ingr_all <- respAgg(resp,"ingr")
    mft$auth_all <- respAgg(resp,"auth")
    mft$puri_all <- respAgg(resp,"puri")
    mft$mft_all <- as.numeric(apply(mft[,grep("_all",colnames(mft))],1,sum) > 0)

    ## aggregating over party evaluations
    mft$harm_pa <- respAgg(resp,"harm.*_pa")
    mft$fair_pa <- respAgg(resp,"fair.*_pa")
    mft$ingr_pa <- respAgg(resp,"ingr.*_pa")
    mft$auth_pa <- respAgg(resp,"auth.*_pa")
    mft$puri_pa <- respAgg(resp,"puri.*_pa")
    mft$mft_pa <- as.numeric(apply(mft[,grep("_pa",colnames(mft))],1,sum) > 0)

    ## aggregating over candidate evaluations
    mft$harm_ca <- respAgg(resp,"harm.*_ca")
    mft$fair_ca <- respAgg(resp,"fair.*_ca")
    mft$ingr_ca <- respAgg(resp,"ingr.*_ca")
    mft$auth_ca <- respAgg(resp,"auth.*_ca")
    mft$puri_ca <- respAgg(resp,"puri.*_ca")
    mft$mft_ca <- as.numeric(apply(mft[,grep("_ca",colnames(mft))],1,sum) > 0)
        
    if(valence == TRUE){
        ## aggregating over all items
        mft$harm_virtue_all <- respAgg(resp,"harm_virtue")
        mft$harm_vice_all   <- respAgg(resp,"harm_vice")
        mft$fair_virtue_all <- respAgg(resp,"fair_virtue")
        mft$fair_vice_all   <- respAgg(resp,"fair_vice")
        mft$ingr_virtue_all <- respAgg(resp,"ingr_virtue")
        mft$ingr_vice_all   <- respAgg(resp,"ingr_vice")
        mft$auth_virtue_all <- respAgg(resp,"auth_virtue")
        mft$auth_vice_all   <- respAgg(resp,"auth_vice")
        mft$puri_virtue_all <- respAgg(resp,"puri_virtue")
        mft$puri_vice_all   <- respAgg(resp,"puri_vice")
        mft$mft_virtue_all  <- as.numeric(apply(mft[,grep("virtue_all"
                                                        ,colnames(mft))],1,sum) > 0)
        mft$mft_vice_all    <- as.numeric(apply(mft[,grep("vice_all"
                                                        ,colnames(mft))],1,sum) > 0)

        ## aggregating over party evaluations
        mft$harm_virtue_pa <- respAgg(resp,"harm_virtue_pa")
        mft$harm_vice_pa   <- respAgg(resp,"harm_vice_pa")
        mft$fair_virtue_pa <- respAgg(resp,"fair_virtue_pa")
        mft$fair_vice_pa   <- respAgg(resp,"fair_vice_pa")
        mft$ingr_virtue_pa <- respAgg(resp,"ingr_virtue_pa")
        mft$ingr_vice_pa   <- respAgg(resp,"ingr_vice_pa")
        mft$auth_virtue_pa <- respAgg(resp,"auth_virtue_pa")
        mft$auth_vice_pa   <- respAgg(resp,"auth_vice_pa")
        mft$puri_virtue_pa <- respAgg(resp,"puri_virtue_pa")
        mft$puri_vice_pa   <- respAgg(resp,"puri_vice_pa")
        mft$mft_virtue_pa  <- as.numeric(apply(mft[,grep("virtue_pa"
                                                        ,colnames(mft))],1,sum) > 0)
        mft$mft_vice_pa    <- as.numeric(apply(mft[,grep("vice_pa"
                                                        ,colnames(mft))],1,sum) > 0)

        ## aggregating over candidate evaluations
        mft$harm_virtue_ca <- respAgg(resp,"harm_virtue_ca")
        mft$harm_vice_ca   <- respAgg(resp,"harm_vice_ca")
        mft$fair_virtue_ca <- respAgg(resp,"fair_virtue_ca")
        mft$fair_vice_ca   <- respAgg(resp,"fair_vice_ca")
        mft$ingr_virtue_ca <- respAgg(resp,"ingr_virtue_ca")
        mft$ingr_vice_ca   <- respAgg(resp,"ingr_vice_ca")
        mft$auth_virtue_ca <- respAgg(resp,"auth_virtue_ca")
        mft$auth_vice_ca   <- respAgg(resp,"auth_vice_ca")
        mft$puri_virtue_ca <- respAgg(resp,"puri_virtue_ca")
        mft$puri_vice_ca   <- respAgg(resp,"puri_vice_ca")
        mft$mft_virtue_ca  <- as.numeric(apply(mft[,grep("virtue_ca"
                                                        ,colnames(mft))],1,sum) > 0)
        mft$mft_vice_ca    <- as.numeric(apply(mft[,grep("vice_ca"
                                                        ,colnames(mft))],1,sum) > 0)
    }

    ## include number of words in items
    mft$num_total <- resp$num_total
    mft$num_ca <- apply(resp[, grep("num_ca", colnames(resp))], 1, sum,na.rm = TRUE)
    mft$num_pa <- apply(resp[, grep("num_pa", colnames(resp))], 1, sum,na.rm = TRUE)

    ## output
    if(check == TRUE){
        anes <- merge(dat,mft)
        anes_check <- merge(dat,spell)
        anes_check <- merge(anes_check,resp)
        anes_check <- merge(anes_check, mft)
        out <- list(data = anes, check = anes_check, call = match.call())
    } else {
        anes <- merge(dat,mft)
        out <- list(data = anes, call = match.call())
    }
}
