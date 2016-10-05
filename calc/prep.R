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

## load additional functions
source("func.R")

## data directory
datsrc <- "/data/Dropbox/Uni/Data/"



###########################
### recode time-series data


## load raw data
raw2012 <- read.dta13(paste0(datsrc,"anes2012/anes_timeseries_2012.dta"), convert.factors = F)
raw2008 <- read.dta13(paste0(datsrc,"anes2008/anes_timeseries_2008.dta"), convert.factors = F)

## respondent id, wave, weight, interview mode (1=FTF, 2=online)
anes2012 <- data.frame(id=raw2012$caseid, year=2012
                       , weight = raw2012$weight_full
                       , mode = raw2012$mode-1)

anes2008 <- data.frame(id=raw2008$V080001, year=2008
                       , weight = raw2008$V080101
                       , mode = 1)

## ideology (factor/dummies)
anes2012$ideol <- factor(recode(raw2012$libcpre_self, "1:3=1; 4=2; 5:7=3; else=NA")
                         , labels = c("Liberal","Moderate","Conservative"))
anes2012$ideol_lib <- as.numeric(anes2012$ideol=="Liberal")
anes2012$ideol_con <- as.numeric(anes2012$ideol=="Conservative")

anes2008$ideol <- factor(recode(raw2008$V083069, "1:3=1; 4=2; 5:7=3; else=NA")
                         , labels = c("Liberal","Moderate","Conservative"))
anes2008$ideol_lib <- as.numeric(anes2008$ideol=="Liberal")
anes2008$ideol_con <- as.numeric(anes2008$ideol=="Conservative")

## ideology (continuous, -1 to 1)
anes2012$ideol_ct <- (recode(raw2012$libcpre_self, "lo:0=NA") - 4)/3
anes2008$ideol_ct <- (recode(raw2008$V083069, "lo:0=NA") - 4)/3

## strength of ideology
anes2012$ideol_str <- abs(anes2012$ideol_ct)
anes2008$ideol_str <- abs(anes2008$ideol_ct)

## party identification (factor/dummies)
anes2012$pid <- factor(recode(raw2012$pid_x
                              , "1:2=1; c(3,4,5)=2; 6:7=3; else=NA")
                       , labels = c("Democrat","Independent","Republican"))
anes2012$pid_dem <- as.numeric(anes2012$pid=="Democrat")
anes2012$pid_rep <- as.numeric(anes2012$pid=="Republican")

anes2008$pid <- factor(recode(raw2008$V083098x
                              , "0:1=1; c(2,3,4)=2; 5:6=3; else=NA")
                       , labels = c("Democrat","Independent","Republican"))
anes2008$pid_dem <- as.numeric(anes2008$pid=="Democrat")
anes2008$pid_rep <- as.numeric(anes2008$pid=="Republican")

## pid continuous
anes2012$pid_cont <- (recode(raw2012$pid_x, "lo:0=NA") - 4)/3
anes2008$pid_cont <- (recode(raw2008$V083098x, "lo:-1=NA") - 3)/3

## strength of partisanship
anes2012$pid_str <- abs(anes2012$pid_cont)
anes2008$pid_str <- abs(anes2008$pid_cont)

## political media exposure
anes2012$polmedia <- with(raw2012, recode(prmedia_wkinews, "lo:-4=NA; -1=0")
                          + recode(prmedia_wktvnws, "lo:-4=NA; -1=0")
                          + recode(prmedia_wkpaprnws, "lo:-4=NA; -1=0")
                          + recode(prmedia_wkrdnws, "lo:-4=NA; -1=0")) / 28

polmedia <- list(c("V083019","V083024"), c("V083021a", "V083025")
                 , c("V083021b", "V083023"), c("V083022", "V083026"))
anes2008$polmedia <- 0
for(i in 1:length(polmedia)){
  tmp <- recode(raw2008[,polmedia[[i]][1]], "c(-4,-8,-9)=NA; -1=0")
  if(length(polmedia[[i]])>1){
    tmp[raw2008[,polmedia[[i]][1]]==-1] <- recode(raw2008[,polmedia[[i]][2]]
                                                  , "c(-8,-9,-1)=NA")[raw2008[,polmedia[[i]][1]]==-1]
  }
  anes2008$polmedia <- anes2008$polmedia + tmp
  rm(tmp)
}
rm(polmedia)

## political media exposure (mean centered)
anes2012$polmedia_c <- scale(anes2012$polmedia, scale=F)
anes2008$polmedia_c <- scale(anes2008$polmedia, scale=F)

## political knowledge (factual knowledge questions, pre-election)
anes2012$polknow <- with(raw2012, ((preknow_prestimes==2) + (preknow_sizedef==1)
                                           + (preknow_senterm==6) + (preknow_medicare==1)
                                           + (preknow_leastsp==1))/5)
anes2008$polknow <- recode(raw2008$V085119a, "-2=NA; 5=1; else=0")

## political knowledge (mean centered)
anes2012$polknow_c <- scale(anes2012$polknow, scale=F)
anes2008$polknow_c <- scale(anes2008$polknow, scale=F)

## political discussion
anes2012$poldisc <- recode(raw2012$discuss_discpstwk, "lo:-1 = NA")/7
anes2012$poldisc[raw2012$discuss_disc>1] <- 0

anes2008$poldisc <- recode(raw2008$V085108a,"lo:-1 = NA")
anes2008$poldisc[raw2008$V085108>1] <- 0
anes2008$poldisc[raw2008$V085108a==-1] <- recode(raw2008$V085109,"lo:-1=NA")[raw2008$V085108a==-1]

## political discussion (mean centered)
anes2012$poldisc_c <- scale(anes2012$poldisc, scale=F)
anes2008$poldisc_c <- scale(anes2008$poldisc, scale=F)

## candidate evaluations (feeling thermometer)
anes2012$eval_cand <- (recode(raw2012$ft_dpc, "lo:-1=NA; 101:hi=NA") - 
                         recode(raw2012$ft_rpc, "lo:-1=NA; 101:hi=NA"))

anes2008$eval_cand <- (recode(raw2008$V083037a, "lo:-1=NA; 101:hi=NA") - 
                         recode(raw2008$V083037b, "lo:-1=NA; 101:hi=NA"))

## party evaluations (feeling thermometer)
anes2012$eval_party <- (recode(raw2012$ft_dem, "lo:-1=NA; 101:hi=NA") -
                          recode(raw2012$ft_rep, "lo:-1=NA; 101:hi=NA"))

anes2008$eval_party <- (recode(raw2008$V083044a, "lo:-1=NA; 101:hi=NA") -
                          recode(raw2008$V083044b, "lo:-1=NA; 101:hi=NA"))

## voted in previous election
anes2012$pastvote <- recode(raw2012$interest_voted2008, "c(2,5)=0; lo:-1=NA")
anes2008$pastvote <- recode(raw2008$V083007, "c(2,5)=0; lo:-1=NA")

## voted in current election
anes2012$vote <- recode(raw2012$rvote2012_x, "2=0; lo:-1=NA")
anes2008$vote <- recode(raw2008$V085036x, "2=0; lo:-1=NA")

## voted for democratic presidential candidate
anes2012$vote_dem <- recode(raw2012$presvote2012_x, "2=0; c(-2,5)=NA")
anes2008$vote_dem <- recode(raw2008$V083169a, "2=0; c(-2,5)=NA")

## participated in protest march / rally
anes2012$protest <- recode(raw2012$dhsinvolv_march, "c(2,5)=0; lo:-1=NA")
anes2008$protest <- recode(raw2008$V085201a, "c(2,5)=0; lo:-1=NA")

## letter to congressman/senator
anes2012$letter <- recode(raw2012$dhsinvolv_contact1, "2=0; lo:-1=NA")

## signed a petition
anes2012$petition <- as.numeric((recode(raw2012$dhsinvolv_netpetition, "c(2,5)=0; lo:-1=NA") +
                                   recode(raw2012$dhsinvolv_petition, "c(2,5)=0; lo:-1=NA")) > 0)

anes2008$petition <- as.numeric((recode(raw2008$V085201c, "c(2,5)=0; lo:-1=NA") +
                                   recode(raw2008$V085201d, "c(2,5)=0; lo:-1=NA")) > 0)

## wear a campaign button
anes2012$button <- recode(raw2012$mobilpo_sign, "c(2,5)=0; lo:-1=NA")
anes2008$button <- recode(raw2008$V085031, "c(2,5)=0; lo:-1=NA")

## additive index protest behavior
anes2012$part <- with(anes2012, as.numeric((protest + petition + button)>0))
anes2008$part <- with(anes2008, as.numeric((protest + petition + button)>0))

## age
anes2012$age <- recode(raw2012$dem_age_r_x, "c(-2,-9,-8) = NA")
anes2008$age <- recode(raw2008$V081104, "c(-2,-9,-8) = NA")

## sex
anes2012$female <- raw2012$gender_respondent_x - 1
anes2008$female <- raw2008$V081101 - 1

## race
anes2012$black <- as.numeric(recode(raw2012$dem_raceeth_x, "lo:0 = NA") == 2)
anes2008$black <- as.numeric(recode(raw2008$V081102, "lo:0 = NA") == 2)

## religiosity (church attendance)
anes2012$relig <- (5 - recode(raw2012$relig_churchoft, "lo:0 = NA"))/5
anes2012$relig[raw2012$relig_church != 1] <- 0
anes2012$relig[raw2012$relig_churchwk == 2] <- 1

anes2008$relig <- (5 - recode(raw2008$V083186a, "lo:0 = NA"))/5
anes2008$relig[raw2008$V083186 != 1] <- 0
anes2008$relig[raw2008$V083186b == 2] <- 1

## education (bachelor degree)
anes2012$educ <- recode(raw2012$dem_edugroup_x, "lo:-1=NA; 0:3=0; 3:hi=1")
anes2008$educ <- recode(raw2008$V083218x, "lo:-1=NA; 1:5=0; 6:hi=1")

## education (continuous)
anes2012$educ_cont <- (recode(raw2012$dem_edugroup_x, "lo:-1=NA") - 1)/4
anes2008$educ_cont <- recode(raw2008$V083218x, "lo:-1=NA")/7

## spanish speaking respondent
anes2012$spanish <- as.numeric(raw2012$profile_spanishsurv == 1 |
                                 raw2012$admin_pre_lang_start == 2 |
                                 raw2012$admin_post_lang_start == 2)

anes2008$spanish <- as.numeric(raw2008$V082011 == 2 |
                                 raw2008$V082011 == 3)

## wordsum literacy test
anes2012$wordsum <- with(raw2012, (wordsum_setb == 5) + (wordsum_setd == 3)
                         + (wordsum_sete == 1) + (wordsum_setf == 3)
                         + (wordsum_setg == 5) + (wordsum_seth == 4)
                         + (wordsum_setj == 1) + (wordsum_setk == 1)
                         + (wordsum_setl == 4) + (wordsum_seto == 2))/10



###############################
### open-ended survey responses


## read original open-ended responses (downloaded from anes website)
anes2012opend <- read.csv(paste0(datsrc,"anes2012/anes2012TS_openends.csv"), as.is = T) %>%
  select(caseid, candlik_likewhatdpc, candlik_dislwhatdpc, candlik_likewhatrpc, candlik_dislwhatrpc
         , ptylik_lwhatdp, ptylik_dwhatdp, ptylik_lwhatrp, ptylik_dwhatrp)

anes2008opend <- read.csv(paste0(datsrc,"anes2008/anes2008TSopenends_redacted_Dec2012Revision.csv"), as.is = T) %>%
  select(caseid, DemPC_like, DemPC_dislike, RepPC_like, RepPC_dislike
         , DemParty_like, DemParty_dislike, RepParty_like, RepParty_dislike)

## load dictionary
dict <- sapply(c("authority","fairness","harm","ingroup","purity"), function(x){
  read.csv(paste0("in/graham/",x,"_noregex.csv")) %>%
    sapply(paste, collapse = " ") %>% as.character()
})

## regex replacements for dictionary
dict_df <- sapply(c("authority","fairness","harm","ingroup","purity"), function(x){
  cbind(read.csv(paste0("in/graham/",x,".csv"), allowEscapes = T, stringsAsFactors = F)[[1]]
        , read.csv(paste0("in/graham/",x,"_noregex.csv"), stringsAsFactors = F)[[1]])
}) %>% do.call("rbind", .)

## pre-process open-ended data and calculate similarity
anes2012sim <- mftSimilarity(opend = anes2012opend[-1], id = anes2012opend$caseid
                             , dict = dict, regex = dict_df)

anes2008sim <- mftSimilarity(opend = anes2008opend[-1], id = anes2008opend$caseid
                             , dict = dict, regex = dict_df)

## merge ts data and open-ended data and save objects for analyses
anes2012 <- merge(anes2012, anes2012sim)
anes2008 <- merge(anes2008, anes2008sim)



##########################
### media content analysis


## read textfiles
docs2012 <- textfile(list.files(path = paste0(datsrc, "anes2012/media")
                                , pattern = "\\.txt$", full.names = TRUE, recursive = FALSE))

txtproc <- function(x) {
  gsub("\\n\\nLOAD-DATE:\\s+\\w*\\s+\\d{1,2},\\s+\\d{4}\\n\\n.*$","", x) %>%
    tail(-1) %>% paste(collapse=" ")
}

docs2012@texts <- docs2012@texts %>%
  strsplit("\\n\\nLENGTH:\\s+\\d+\\s+words\\n\\n") %>%
  sapply(txtproc)

## replace regular expressions with word stems
pb <- txtProgressBar(min = 0, max = nrow(dict_df), style = 3)
for(i in 1:nrow(dict_df)){
  docs2012@texts <- gsub(dict_df[i,1], dict_df[i,2], docs2012@texts)
  setTxtProgressBar(pb, i)
}
close(pb)

## combine dictionary and responses in common dfm/tfidf
media2012_tfidf <- corpus(c(dict, docs2012@texts), docnames = c(names(dict), names(docs2012@texts))) %>% 
  dfm() %>% tfidf()

## calculate cosine similarity b/w dictionaries and documents
media2012_sim <- similarity(media2012_tfidf, selection = names(dict)
                            , margin = "documents", method = "cosine") %>% 
  as.matrix() %>% data.frame() %>%
  mutate(general = apply(.,1,sum), id = gsub("\\.txt","",rownames(.))) %>%
  filter(general != 1) %>% arrange(id) %>% select(id, everything())

## create scaled variable for moral foundations
media2012_sim_s <- apply(select(media2012_sim, -id), 2, function(x) scale(x))
media2012_sim_d <- apply(select(media2012_sim, -id), 2, function(x) ifelse(x>=median(x),1,-1))
colnames(media2012_sim_s) <- paste0(colnames(media2012_sim_s),"_s")
colnames(media2012_sim_d) <- paste0(colnames(media2012_sim_d),"_d")

## combine similarity results
media2012 <- cbind(media2012_sim, media2012_sim_s, media2012_sim_d)

## recode anes media usage data
anes2012media <- data.frame(INET_CNN_com = raw2012$medsrc_websites_02==1
                            , INET_MSNBC_com = raw2012$medsrc_websites_10==1
                            , INET_TheNewYorkTimes = raw2012$medsrc_websites_11==1 | raw2012$medsrc_printnews_01==1 | raw2012$medsrc_inetnews_01==1
                            , INET_USAToday = raw2012$medsrc_websites_13==1 | raw2012$medsrc_printnews_02==1 | raw2012$medsrc_inetnews_02==1
                            , INET_Washingtonpost_com = raw2012$medsrc_websites_14==1 | raw2012$medsrc_inetnews_04==1
                            , NPR_AllThingsConsidered = raw2012$medsrc_radio_01==1
                            , NPR_FreshAir = raw2012$medsrc_radio_04==1
                            , NPR_MorningEdition = raw2012$medsrc_radio_08==1
                            , PRINT_TheWashingtonPost = raw2012$medsrc_printnews_04==1 
                            , PRINT_WallStreetJournal_Abstracts = raw2012$medsrc_printnews_03==1 | raw2012$medsrc_inetnews_03==1
                            , TV_ABC_60minutes = raw2012$medsrc_tvprog_02==1
                            , TV_ABC_GoodMorningAmerica = raw2012$medsrc_tvprog_24==1
                            , TV_ABC_ThisWeek = raw2012$medsrc_tvprog_45==1
                            , TV_ABC_WorldNews = raw2012$medsrc_tvprog_04==1
                            , TV_CBS_EveningNews = raw2012$medsrc_tvprog_11==1
                            , TV_CBS_FaceTheNation = raw2012$medsrc_tvprog_20==1
                            , TV_CBS_SundayMorning = raw2012$medsrc_tvprog_43==1
                            , TV_CBS_ThisMorning = raw2012$medsrc_tvprog_12==1
                            , TV_CNN_AndersonCooper = raw2012$medsrc_tvprog_09==1
                            , TV_Fox_Hannity = raw2012$medsrc_tvprog_25==1
                            , TV_Fox_OReillyFactor = raw2012$medsrc_tvprog_36==1
                            , TV_Fox_SpecialReport = raw2012$medsrc_tvprog_41==1
                            , TV_Fox_TheFive = raw2012$medsrc_tvprog_21==1
                            , TV_NBC_Dateline = raw2012$medsrc_tvprog_17==1
                            , TV_NBC_MeetThePress = raw2012$medsrc_tvprog_32==1
                            , TV_NBC_NightlyNews = raw2012$medsrc_tvprog_34==1
                            , TV_NBC_RockCenter = raw2012$medsrc_tvprog_39==1
                            , TV_NBC_TodayShow = raw2012$medsrc_tvprog_46==1
                            ) %>% apply(2,as.numeric)

## combine media usage with mft similarity scores and add to anes
tmp1 <- as.matrix(anes2012media) %*% as.matrix(select(media2012,-id))
colnames(tmp1) <- paste0("media_",colnames(tmp1))
tmp2 <- apply(tmp1[,grep("_d",colnames(tmp1))], 2, function(x) as.numeric(x>0))
colnames(tmp2) <- paste0(colnames(tmp2),"01")
anes2012 <- cbind(anes2012,tmp1,tmp2)
anes2012$media <- apply(anes2012media,1,sum)>0



##############################
### save output for analyses.R


save(anes2012, anes2012opend, anes2008, anes2008opend, media2012, file="out/anes.RData")
