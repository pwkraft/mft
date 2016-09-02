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

## working directory
setwd("/data/Dropbox/Uni/Projects/2014/mft/calc")

## data directory
datsrc <- "/data/Dropbox/Uni/Data/anes2012/"



###########################
### recode time-series data


## load raw data
raw2012 <- read.dta13(paste0(datsrc,"anes_timeseries_2012.dta"), convert.factors = F)

## respondent id, wave, weight, interview mode (1=FTF, 2=online)
anes2012 <- data.frame(id=raw2012$caseid, year=2012
                       , weight = raw2012$weight_full
                       , mode=raw2012$mode)

## ideology (factor/dummies)
anes2012$ideol <- factor(recode(raw2012$libcpre_self, "1:3=1; 4=2; 5:7=3; else=NA")
                         , labels = c("Liberal","Moderate","Conservative"))
anes2012$ideol_lib <- as.numeric(anes2012$ideol=="Liberal")
anes2012$ideol_con <- as.numeric(anes2012$ideol=="Conservative")

## ideology (continuous, -1 to 1)
anes2012$ideol_ct <- (recode(raw2012$libcpre_self, "lo:0=NA") - 4)/3

## strength of ideology
anes2012$ideol_str <- abs(anes2012$ideol_ct)

## party identification (factor/dummies)
anes2012$pid <- factor(recode(raw2012$pid_x
                              , "1:2=1; c(3,4,5)=2; 6:7=3; else=NA")
                       , labels = c("Democrat","Independent","Republican"))
anes2012$pid_dem <- as.numeric(anes2012$pid=="Democrat")
anes2012$pid_rep <- as.numeric(anes2012$pid=="Republican")

## pid continuous
anes2012$pid_cont <- (recode(raw2012$pid_x, "lo:0=NA") - 4)/3

## strength of partisanship
anes2012$pid_str <- abs(anes2012$pid_cont)

## political media exposure
anes2012$polmedia <- with(raw2012, recode(prmedia_wkinews, "lo:-4=NA; -1=0")
                          + recode(prmedia_wktvnws, "lo:-4=NA; -1=0")
                          + recode(prmedia_wkpaprnws, "lo:-4=NA; -1=0")
                          + recode(prmedia_wkrdnws, "lo:-4=NA; -1=0")) / 28

## political media exposure (mean centered)
anes2012$polmedia_c <- scale(anes2012$polmedia, scale=F)

## political knowledge (factual knowledge questions, pre-election)
anes2012$polknow <- with(raw2012, ((preknow_prestimes==2) + (preknow_sizedef==1)
                                           + (preknow_senterm==6) + (preknow_medicare==1)
                                           + (preknow_leastsp==1))/5)

## political knowledge (mean centered)
anes2012$polknow_c <- scale(anes2012$polknow, scale=F)

## political discussion
anes2012$poldisc <- recode(raw2012$discuss_discpstwk, "lo:-1 = NA")/7
anes2012$poldisc[raw2012$discuss_disc>1] <- 0

## political discussion (mean centered)
anes2012$poldisc_c <- scale(anes2012$poldisc, scale=F)

## candidate evaluations (feeling thermometer)
anes2012$eval_cand <- (recode(raw2012$ft_dpc, "lo:-1=NA; 101:hi=NA") - 
                         recode(raw2012$ft_rpc, "lo:-1=NA; 101:hi=NA"))

## party evaluations (feeling thermometer)
anes2012$eval_party <- (recode(raw2012$ft_dem, "lo:-1=NA; 101:hi=NA") -
                          recode(raw2012$ft_rep, "lo:-1=NA; 101:hi=NA"))

## voted in previous election
anes2012$pastvote <- recode(raw2012$interest_voted2008, "c(2,5)=0; lo:-1=NA")

## voted in current election
anes2012$vote <- recode(raw2012$rvote2012_x, "2=0; lo:-1=NA")

## voted for democratic presidential candidate
anes2012$vote_dem <- recode(raw2012$presvote2012_x, "2=0; c(-2,5)=NA")

## participated in protest march / rally
anes2012$protest <- recode(raw2012$dhsinvolv_march, "c(2,5)=0; lo:-1=NA")

## signed a petition
anes2012$petition <- as.numeric((recode(raw2012$dhsinvolv_netpetition, "c(2,5)=0; lo:-1=NA") +
                                   recode(raw2012$dhsinvolv_petition, "c(2,5)=0; lo:-1=NA")) > 0)

## wear a campaign button
anes2012$button <- recode(raw2012$mobilpo_sign, "c(2,5)=0; lo:-1=NA")

## additive index protest behavior
anes2012$part <- with(anes2012, as.numeric((protest + petition + button)>0))

## age
anes2012$age <- recode(raw2012$dem_age_r_x, "c(-2,-9,-8) = NA")

## sex
anes2012$female <- raw2012$gender_respondent_x - 1

## race
anes2012$black <- as.numeric(recode(raw2012$dem_raceeth_x, "lo:0 = NA") == 2)

## religiosity (church attendance)
anes2012$relig <- (5 - recode(raw2012$relig_churchoft, "lo:0 = NA"))/5
anes2012$relig[raw2012$relig_church != 1] <- 0
anes2012$relig[raw2012$relig_churchwk == 2] <- 1

## education (bachelor degree)
anes2012$educ <- as.numeric(raw2012$dem_edugroup_x >= 4)
anes2012$educ[raw2012$raw2012$dem_edugroup_x < 0] <- NA

## education (continuous)
anes2012$educ_cont <- (recode(raw2012$dem_edugroup_x, "lo:0=NA") - 1)/4

## spanish speaking respondent
anes2012$spanish <- as.numeric(raw2012$profile_spanishsurv == 1 |
                                 raw2012$admin_pre_lang_start == 2 |
                                 raw2012$admin_post_lang_start == 2)



###############################
### open-ended survey responses


## read original open-ended responses (downloaded from anes website)
anes2012opend <- read.csv(paste0(datsrc,"anes2012TS_openends.csv"), as.is = T) %>%
  select(caseid, candlik_likewhatdpc, candlik_dislwhatdpc, candlik_likewhatrpc, candlik_dislwhatrpc
         , ptylik_lwhatdp, ptylik_dwhatdp, ptylik_lwhatrp, ptylik_dwhatrp)

## minor pre-processing
anes2012spell <- apply(anes2012opend[,-1], 2, function(x){
  x <- gsub("(^\\s+|\\s+$)","", x)
  x[x %in% c("-1 Inapplicable","-7 Refused","N/A","no","none","#(43042)","i am","Nome")] <- ""
  x <- gsub("//"," ", x , fixed = T)
  x <- gsub("[[:punct:]]"," ", x)
  x <- gsub("\\s+"," ", x)
  x <- gsub("(^\\s+|\\s+$)","", x)
  return(x)
})

## num-lock issue
# maybe look into this later

## fix words without whitespace
# maybe look into this later

## spell-checking
write.table(anes2012spell, file = "out/anes2012TS_combined.csv"
            , sep = ",", col.names = F, row.names = F)
spell <- aspell("out/anes2012TS_combined.csv") %>%
  filter(Suggestions!="NULL")

## replace incorrect words
for(i in 1:nrow(spell)){
  anes2012spell[spell$Line[i],] <- gsub(spell$Original[i], unlist(spell$Suggestions[i])[1]
                                        , anes2012spell[spell$Line[i],])
}
anes2012spell <- data.frame(id = anes2012opend$caseid, anes2012spell,stringsAsFactors = F)


### add meta information about responses

## overall response length
anes2012$wc <- apply(anes2012spell[,-1], 1, function(x){
  length(unlist(strsplit(x,"\\s+")))
})
anes2012$lwc <- log(anes2012$wc)

## number of items answered
anes2012$nitem <- apply(anes2012spell[,-1] != "", 1, sum, na.rm = T)


### calculate cosine similarity between dictionaries and documents

## load dictionary
dict <- sapply(c("authority","fairness","harm","ingroup","purity"), function(x){
  read.csv(paste0("in/graham/",x,"_noregex.csv")) %>%
    sapply(paste, collapse = " ") %>% as.character()
})
dict_df <- sapply(c("authority","fairness","harm","ingroup","purity"), function(x){
  cbind(read.csv(paste0("in/graham/",x,".csv"), allowEscapes = T, stringsAsFactors = F)[[1]]
        , read.csv(paste0("in/graham/",x,"_noregex.csv"), stringsAsFactors = F)[[1]])
}) %>% do.call("rbind", .)

## load open-ended responses
anes2012resp <- apply(anes2012spell[,-1], 1, paste, collapse = " ")
anes2012resp <- gsub("NA\\s*","",anes2012resp)
names(anes2012resp) <- anes2012spell$id
anes2012resp <- anes2012resp[anes2012resp != ""]
for(i in 1:nrow(dict_df)){
  anes2012resp <- gsub(dict_df[i,1],dict_df[i,2],anes2012resp)
}

## combine dictionary and responses in common dfm/tfidf
anes2012tfidf <- corpus(c(dict,anes2012resp)
                        , docnames = c("authority","fairness","harm","ingroup","purity"
                                       , names(anes2012resp))) %>% dfm() %>% tfidf()

## calculate similarity (check pr_DB$get_entries() for options)
# normalization is not necessary, cosine similarity is length invariant so results are unchanged
anes2012sim <- similarity(anes2012tfidf
                          , selection =  c("harm","fairness","ingroup","authority","purity")
                          , margin = "documents", method = "cosine") %>% as.matrix() %>% data.frame()
anes2012sim$general <- apply(anes2012sim,1,sum)
anes2012sim <- cbind(anes2012sim, apply(anes2012sim, 2, function(x) as.numeric(x>0)))
colnames(anes2012sim)[7:12] <- paste0(colnames(anes2012sim)[7:12],"_d")
anes2012sim$id <- as.numeric(rownames(anes2012sim))
anes2012sim <- anes2012sim %>% arrange(id) %>% filter(!is.na(id))

## create scaled variables for moral foundations (throws an error if run twice, not sure why)
anes2012sim <- mutate(anes2012sim, harm_s = scale(harm, center = FALSE), fairness_s = scale(fairness, center = FALSE)
                   , ingroup_s = scale(ingroup, center = FALSE), authority_s = scale(authority, center = FALSE)
                   , purity_s = scale(purity, center = FALSE), general_s = scale(general, center = FALSE))


### merge ts data and open-ended data and save objects for analyses

anes2012 <- merge(anes2012, anes2012sim)
save(anes2012, anes2012opend, anes2012spell, file="out/anes.RData")
