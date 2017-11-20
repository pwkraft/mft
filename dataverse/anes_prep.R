###############################################################################################
## Project:  Measuring Morality in Political Attitude Expression
## File:     prep_anes.R
## Overview: Prepares 2012 ANES data for analyses_anes.R
## Requires: - ANES 2012 Time Series data (anes_timeseries_2012.dta)
##           - ANES 2012 Redacted Open-Ended Responses (anes2012TS_openends.csv)
##             (available at http://www.electionstudies.org/)
##           - MFT dictionary (mft_dictionary.rda)
##           - Custom auxiliary functions (func.R)
## Author:   Patrick Kraft
###############################################################################################

## packages
pkg <- c("readstata13","car","dplyr","quanteda")
invisible(lapply(pkg, library, character.only = TRUE))
rm(list=ls())

## load auxiliary functions
source("func.R")



###########################
### recode time-series data


## load raw data
raw2012 <- read.dta13("anes_timeseries_2012.dta", convert.factors = F)

## respondent id, wave, weight, interview mode (1=FTF, 2=online)
anes2012 <- data.frame(id=raw2012$caseid, year=2012
                       , weight = raw2012$weight_full
                       , mode = raw2012$mode-1)

## ideology (factor/dummies)
anes2012$ideol <- factor(Recode(raw2012$libcpre_self, "1:3=1; 4=2; 5:7=3; else=NA")
                         , labels = c("Liberal","Moderate","Conservative"))
anes2012$ideol_lib <- as.numeric(anes2012$ideol=="Liberal")
anes2012$ideol_con <- as.numeric(anes2012$ideol=="Conservative")

## ideology (continuous, -1 to 1)
anes2012$ideol_ct <- (Recode(raw2012$libcpre_self, "lo:0=NA") - 4)/3

## strength of ideology
anes2012$ideol_str <- abs(anes2012$ideol_ct)

## party identification (factor/dummies)
anes2012$pid <- factor(Recode(raw2012$pid_x
                              , "1:2=1; c(3,4,5)=2; 6:7=3; else=NA")
                       , labels = c("Democrat","Independent","Republican"))
anes2012$pid_dem <- as.numeric(anes2012$pid=="Democrat")
anes2012$pid_rep <- as.numeric(anes2012$pid=="Republican")

## pid continuous
anes2012$pid_cont <- (Recode(raw2012$pid_x, "lo:0=NA") - 4)/3

## strength of partisanship
anes2012$pid_str <- abs(anes2012$pid_cont)

## political media exposure
anes2012$wkinews <- Recode(raw2012$prmedia_wkinews, "lo:-4=NA; -1=0")
anes2012$wktvnws <- Recode(raw2012$prmedia_wktvnws, "lo:-4=NA; -1=0")
anes2012$wkpaprnws <- Recode(raw2012$prmedia_wkpaprnws, "lo:-4=NA; -1=0")
anes2012$wkrdnws <- Recode(raw2012$prmedia_wkrdnws, "lo:-4=NA; -1=0")
anes2012$polmedia <- with(anes2012, (wkinews + wktvnws + wkpaprnws + wkrdnws) / 28)

## political media exposure (mean centered)
anes2012$polmedia_c <- scale(anes2012$polmedia, scale=F)

## political knowledge (factual knowledge questions, pre-election)
anes2012$polknow <- with(raw2012, ((preknow_prestimes==2) + (preknow_sizedef==1)
                                   + (preknow_senterm==6) + (preknow_medicare==1)
                                   + (preknow_leastsp==1))/5)

## political knowledge (mean centered)
anes2012$polknow_c <- scale(anes2012$polknow, scale=F)

## political discussion
anes2012$poldisc <- Recode(raw2012$discuss_discpstwk, "lo:-1 = NA")/7
anes2012$poldisc[raw2012$discuss_disc>1] <- 0

## political discussion (mean centered)
anes2012$poldisc_c <- scale(anes2012$poldisc, scale=F)

## candidate evaluations (feeling thermometer)
anes2012$eval_cand <- (Recode(raw2012$ft_dpc, "lo:-1=NA; 101:hi=NA") - 
                         Recode(raw2012$ft_rpc, "lo:-1=NA; 101:hi=NA"))

## party evaluations (feeling thermometer)
anes2012$eval_party <- (Recode(raw2012$ft_dem, "lo:-1=NA; 101:hi=NA") -
                          Recode(raw2012$ft_rep, "lo:-1=NA; 101:hi=NA"))

## voted in previous election
anes2012$pastvote <- Recode(raw2012$interest_voted2008, "c(2,5)=0; lo:-1=NA")

## voted in current election
anes2012$vote <- Recode(raw2012$rvote2012_x, "2=0; lo:-1=NA")

## voted for democratic presidential candidate
anes2012$vote_dem <- Recode(raw2012$presvote2012_x, "2=0; c(-2,5)=NA")

## participated in protest march / rally
anes2012$protest <- Recode(raw2012$dhsinvolv_march, "c(2,5)=0; lo:-1=NA")

## letter to congressman/senator
anes2012$letter <- Recode(raw2012$dhsinvolv_contact1, "2=0; lo:-1=NA")

## signed a petition
anes2012$petition <- as.numeric((Recode(raw2012$dhsinvolv_netpetition, "c(2,5)=0; lo:-1=NA") +
                                   Recode(raw2012$dhsinvolv_petition, "c(2,5)=0; lo:-1=NA")) > 0)

## wear a campaign button
anes2012$button <- Recode(raw2012$mobilpo_sign, "c(2,5)=0; lo:-1=NA")

## additive index protest behavior
anes2012$part <- with(anes2012, as.numeric((protest + petition + button)>0))

## age
anes2012$age <- Recode(raw2012$dem_age_r_x, "c(-2,-9,-8) = NA")

## sex
anes2012$female <- raw2012$gender_respondent_x - 1

## race
anes2012$black <- as.numeric(Recode(raw2012$dem_raceeth_x, "lo:0 = NA") == 2)

## religiosity (church attendance)
anes2012$relig <- (5 - Recode(raw2012$relig_churchoft, "lo:0 = NA"))/5
anes2012$relig[raw2012$relig_church != 1] <- 0
anes2012$relig[raw2012$relig_churchwk == 2] <- 1

## education (bachelor degree)
anes2012$educ <- Recode(raw2012$dem_edugroup_x, "lo:-1=NA; 0:3=0; 3:hi=1")

## education (continuous)
anes2012$educ_cont <- (Recode(raw2012$dem_edugroup_x, "lo:-1=NA") - 1)/4

## spanish speaking respondent
anes2012$spanish <- as.numeric(raw2012$profile_spanishsurv == 1 |
                                 raw2012$admin_pre_lang_start == 2 |
                                 raw2012$admin_post_lang_start == 2)

## wordsum literacy test
anes2012$wordsum <- with(raw2012, (wordsum_setb == 5) + (wordsum_setd == 3)
                         + (wordsum_sete == 1) + (wordsum_setf == 3)
                         + (wordsum_setg == 5) + (wordsum_seth == 4)
                         + (wordsum_setj == 1) + (wordsum_setk == 1)
                         + (wordsum_setl == 4) + (wordsum_seto == 2))/10



###############################
### open-ended survey responses


## read original open-ended responses (downloaded from anes website)
anes2012opend <- read.csv("anes2012TS_openends.csv", as.is = T) %>%
  select(caseid, candlik_likewhatdpc, candlik_dislwhatdpc, candlik_likewhatrpc, candlik_dislwhatrpc
         , ptylik_lwhatdp, ptylik_dwhatdp, ptylik_lwhatrp, ptylik_dwhatrp)

## load moral foundations dictionary
load("mft_dictionary.rda")


### match responses and dictionary

## pre-process open-ended data and calculate similarity
anes2012sim <- mftScore(opend = anes2012opend[,-1], id = anes2012opend$caseid
                        , dict = dict, regex = dict_df, dict_list = dict_list)
anes2012sim <- mftRescale(anes2012sim, select = anes2012sim$id %in% 
                            intersect(anes2012$id[anes2012$spanish != 1]
                                      , anes2012sim$id[anes2012sim$wc > 5]))

# check rescaling
sd(anes2012sim$harm_s)
sd(anes2012sim$harm_s[anes2012$spanish != 1 & anes2012sim$wc > 5])
sd(anes2012sim$harm_s[anes2012sim$id %in% intersect(anes2012$id[anes2012$spanish != 1]
                                                    , anes2012sim$id[anes2012sim$wc > 5])])


## get weights for dictionary terms
anes2012weights <- mftScore(opend = anes2012opend[,-1], id = anes2012opend$caseid
                        , dict = dict, regex = dict_df, dict_list = dict_list, report_weights=T)

## new dictionary differentiating vice and virtues
anes2012newsim <- mftScore(opend = anes2012opend[,-1], id = anes2012opend$caseid
                           , dict = newdict, regex = newdict_df, dict_list = newdict_list)
anes2012newsim <- mftRescale(anes2012newsim
                             , vars = c("authority_virtue","fairness_virtue","harm_virtue","ingroup_virtue","purity_virtue"
                                        ,"authority_vice","fairness_vice","harm_vice","ingroup_vice","purity_vice")
                             , select = anes2012newsim$id %in% 
                            intersect(anes2012$id[anes2012$spanish != 1]
                                      , anes2012newsim$id[anes2012newsim$wc > 5])) %>%
  select(-spell,-wc,-lwc,-nitem,-general,-general_d,-general_s)

## only likes in OE responses (only scales mftScore!)
anes2012li <- mftScore(opend = anes2012opend[,grep("_l",colnames(anes2012opend))]
                       , id = anes2012opend$caseid
                       , dict = dict, regex = dict_df, dict_list = dict_list)
anes2012li <- mftRescale(anes2012li, select = anes2012li$id %in% 
                            intersect(anes2012$id[anes2012$spanish != 1]
                                      , anes2012li$id[anes2012li$wc > 5]))
anes2012li <- anes2012li[,c(1,grep("_s",colnames(anes2012li)))]
colnames(anes2012li) <- gsub("_s","_li",colnames(anes2012li))

## only dislikes in OE responses (only scales mftScore!)
anes2012di <- mftScore(opend = anes2012opend[,grep("_d",colnames(anes2012opend))]
                       , id = anes2012opend$caseid
                       , dict = dict, regex = dict_df, dict_list = dict_list)
anes2012di <- mftRescale(anes2012di, select = anes2012di$id %in% 
                            intersect(anes2012$id[anes2012$spanish != 1]
                                      , anes2012di$id[anes2012di$wc > 5]))
anes2012di <- anes2012di[,c(1,grep("_s",colnames(anes2012di)))]
colnames(anes2012di) <- gsub("_s","_di",colnames(anes2012di))

## only democratic party/candidate in OE responses (only scales mftScore!)
anes2012dem <- mftScore(opend = anes2012opend[,grep("whatdp",colnames(anes2012opend))]
                        , id = anes2012opend$caseid
                        , dict = dict, regex = dict_df, dict_list = dict_list)
anes2012dem <- mftRescale(anes2012dem, select = anes2012dem$id %in% 
                            intersect(anes2012$id[anes2012$spanish != 1]
                                      , anes2012dem$id[anes2012dem$wc > 5]))
anes2012dem <- anes2012dem[,c(1,grep("_s",colnames(anes2012dem)))]
colnames(anes2012dem) <- gsub("_s","_dem",colnames(anes2012dem))

## only republican party/candidate in OE responses (only scales mftScore!)
anes2012rep <- mftScore(opend = anes2012opend[,grep("whatrp",colnames(anes2012opend))]
                        , id = anes2012opend$caseid
                        , dict = dict, regex = dict_df, dict_list = dict_list)
anes2012rep <- mftRescale(anes2012rep, select = anes2012rep$id %in% 
                            intersect(anes2012$id[anes2012$spanish != 1]
                                      , anes2012rep$id[anes2012rep$wc > 5]))
anes2012rep <- anes2012rep[,c(1,grep("_s",colnames(anes2012rep)))]
colnames(anes2012rep) <- gsub("_s","_rep",colnames(anes2012rep))

## merge ts data and open-ended data and save objects for analyses
anes2012 <- merge(anes2012, anes2012sim) %>% merge(anes2012newsim) %>% 
  merge(anes2012li) %>% merge(anes2012di) %>% merge(anes2012dem) %>% merge(anes2012rep)



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
media2012_tfidf <- corpus(c(dict, docs2012@texts)
                          , docnames = c(names(dict), names(docs2012@texts))) %>% dfm()
media2012_tfidf <- media2012_tfidf[names(docs2012@texts),] %>% tfidf(scheme_tf = "prop",k=0)
media2012_tfidf <- media2012_tfidf[,dict_df[,2]]

## count relative tfidf weights for each media source
media2012_sim <- data.frame(
  authority = apply(media2012_tfidf[,dict_list$authority],1,sum,na.rm=T)
  , fairness = apply(media2012_tfidf[,dict_list$fairness],1,sum,na.rm=T)
  , harm = apply(media2012_tfidf[,dict_list$harm],1,sum,na.rm=T)
  , ingroup = apply(media2012_tfidf[,dict_list$ingroup],1,sum,na.rm=T)
  , purity = apply(media2012_tfidf[,dict_list$purity],1,sum,na.rm=T)
)
media2012_sim$general <- apply(media2012_sim[,1:4],1,sum)
media2012_sim$id <- gsub("\\.txt","",rownames(media2012_sim))

## create scaled variable for moral foundations
media2012_sim_s <- apply(select(media2012_sim, -id), 2, function(x) scale(x, center=median(x[x!=0])))
media2012_sim_d <- apply(select(media2012_sim, -id), 2, function(x) ifelse(x>=median(x),1,-1))
colnames(media2012_sim_s) <- paste0(colnames(media2012_sim_s),"_s")
colnames(media2012_sim_d) <- paste0(colnames(media2012_sim_d),"_d")

## combine similarity results
media2012 <- cbind(media2012_sim, media2012_sim_s, media2012_sim_d)

## recode overall anes media usage data
anes2012media <- data.frame(INET_CNN_com = raw2012$medsrc_websites_02==1
                            , INET_MSNBC_com = raw2012$medsrc_websites_10==1
                            , INET_TheNewYorkTimes = (raw2012$medsrc_websites_11==1
                                                      | raw2012$medsrc_printnews_01==1
                                                      | raw2012$medsrc_inetnews_01==1)
                            , INET_USAToday = (raw2012$medsrc_websites_13==1 
                                               | raw2012$medsrc_printnews_02==1 
                                               | raw2012$medsrc_inetnews_02==1)
                            , INET_Washingtonpost_com = (raw2012$medsrc_websites_14==1 
                                                         | raw2012$medsrc_inetnews_04==1)
                            , NPR_AllThingsConsidered = raw2012$medsrc_radio_01==1
                            , NPR_FreshAir = raw2012$medsrc_radio_04==1
                            , NPR_MorningEdition = raw2012$medsrc_radio_08==1
                            , PRINT_TheWashingtonPost = raw2012$medsrc_printnews_04==1 
                            , PRINT_WallStreetJournal_Abstracts = (raw2012$medsrc_printnews_03==1 
                                                                   | raw2012$medsrc_inetnews_03==1)
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


## internet news

# dichotomous indicator for each news outlet
anes2012inews <- data.frame(INET_CNN_com = raw2012$medsrc_websites_02==1
                            , INET_MSNBC_com = raw2012$medsrc_websites_10==1
                            , INET_TheNewYorkTimes = (raw2012$medsrc_websites_11==1
                                                      | raw2012$medsrc_inetnews_01==1)
                            , INET_USAToday = (raw2012$medsrc_websites_13==1 
                                               | raw2012$medsrc_inetnews_02==1)
                            , INET_Washingtonpost_com = (raw2012$medsrc_websites_14==1 
                                                         | raw2012$medsrc_inetnews_04==1)
                            , PRINT_WallStreetJournal_Abstracts = raw2012$medsrc_inetnews_03==1
                            ) %>% apply(2,as.numeric)

# proportion of each news outlet
anes2012inews <- anes2012inews / ifelse(apply(anes2012inews,1,sum)>0, apply(anes2012inews,1,sum), 1)


## tv news

# dichotomous indicator for each news outlet
anes2012tvnws <- data.frame(TV_ABC_60minutes = raw2012$medsrc_tvprog_02==1
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

# proportion of each news outlet
anes2012tvnws <- anes2012tvnws / ifelse(apply(anes2012tvnws,1,sum)>0, apply(anes2012tvnws,1,sum), 1)


## print news

# dichotomous indicator for each news outlet
anes2012paprnws <- data.frame(INET_TheNewYorkTimes = raw2012$medsrc_printnews_01==1
                              , INET_USAToday = raw2012$medsrc_printnews_02==1 
                              , PRINT_TheWashingtonPost = raw2012$medsrc_printnews_04==1 
                              , PRINT_WallStreetJournal_Abstracts = raw2012$medsrc_printnews_03==1
                              ) %>% apply(2,as.numeric)

# proportion of each news outlet
anes2012paprnws <- anes2012paprnws / ifelse(apply(anes2012paprnws,1,sum)>0, apply(anes2012paprnws,1,sum), 1)


## radio news

# dichotomous indicator for each news outlet
anes2012rdnws <- data.frame(NPR_AllThingsConsidered = raw2012$medsrc_radio_01==1
                            , NPR_FreshAir = raw2012$medsrc_radio_04==1
                            , NPR_MorningEdition = raw2012$medsrc_radio_08==1
                            ) %>% apply(2,as.numeric)

# proportion of each news outlet
anes2012rdnws <- anes2012rdnws / ifelse(apply(anes2012rdnws,1,sum)>0, apply(anes2012rdnws,1,sum), 1)


## combine media usage with mft similarity scores and add to anes (old version with aggregate usage)
tmp1 <- as.matrix(anes2012media) %*% as.matrix(select(media2012,-id))
colnames(tmp1) <- paste0("media_",colnames(tmp1))
tmp2 <- apply(tmp1[,grep("_d",colnames(tmp1))], 2, function(x) as.numeric(x>0))
colnames(tmp2) <- paste0(colnames(tmp2),"01")

## combine media usage with mft similarity scores as mean dimensions (new version)
tmp1 <- as.matrix(anes2012media) %*% as.matrix(select(media2012,-id))  / apply(anes2012media,1,sum)
tmp1[apply(anes2012media,1,sum)==0, ] <- 0
colnames(tmp1) <- paste0("media_",colnames(tmp1))

## combine media usage with mft similarity scores for each type with relative frequency (new version)
rownames(media2012) <- gsub(".txt","",rownames(media2012))
tmp2 <- as.matrix(anes2012inews) %*% as.matrix(select(media2012,-id))[colnames(anes2012inews),] +
  as.matrix(anes2012tvnws) %*% as.matrix(select(media2012,-id))[colnames(anes2012tvnws),] +
  as.matrix(anes2012paprnws) %*% as.matrix(select(media2012,-id))[colnames(anes2012paprnws),] +
  as.matrix(anes2012rdnws) %*% as.matrix(select(media2012,-id))[colnames(anes2012rdnws),]
colnames(tmp2) <- paste0("mediatype_",colnames(tmp2))

## add new variables to anes
anes2012$media <- apply(anes2012media,1,sum)>0

## combine media usage with mft similarity scores for each type separately (new version)
rownames(media2012) <- gsub(".txt","",rownames(media2012))
tmp1a <- as.matrix(anes2012inews) %*% as.matrix(select(media2012,-id))[colnames(anes2012inews),]
colnames(tmp1a) <- paste0("inews_",colnames(tmp1a))
tmp1b <- as.matrix(anes2012tvnws) %*% as.matrix(select(media2012,-id))[colnames(anes2012tvnws),]
colnames(tmp1b) <- paste0("tvnws_",colnames(tmp1b))
tmp1c <- as.matrix(anes2012paprnws) %*% as.matrix(select(media2012,-id))[colnames(anes2012paprnws),]
colnames(tmp1c) <- paste0("paprnws_",colnames(tmp1c))
tmp1d <- as.matrix(anes2012rdnws) %*% as.matrix(select(media2012,-id))[colnames(anes2012rdnws),]
colnames(tmp1d) <- paste0("rdnws_",colnames(tmp1d))

## add new variables to anes
anes2012 <- cbind(anes2012,tmp1,tmp2,tmp1a,tmp1b,tmp1c,tmp1d)

## rescale media variable, remove missings
anes2012$media_general[anes2012$media_general==0] <- NA
anes2012$media_general_s[is.na(anes2012$media_general)] <- NA


### compute bootstrapped standard errors for media content (word-based bootstrap)

## combine dictionary and responses in common dfm
media2012_dfm <- corpus(c(dict, docs2012@texts)
                        , docnames = c(names(dict), names(docs2012@texts))) %>% dfm()
media2012_dfm <- media2012_dfm[names(docs2012@texts),] %>% as.matrix()

## merge all non-mft words (to make computation easier)
media2012_dfm <- cbind(media2012_dfm[,dict_df[,2]]
                       , apply(media2012_dfm[,!colnames(media2012_dfm)%in%dict_df[,2]],1,sum))
colnames(media2012_dfm)[ncol(media2012_dfm)] <- "nomft"

## initialize object to store individual dfm bootstraps
nboot <- 1000
media2012_boot <- array(dim=c(dim(media2012_dfm),nboot)
                        , dimnames = list(docs = rownames(media2012_dfm)
                                          , features = colnames(media2012_dfm)
                                          , iter = 1:nboot))

## parametric bootstrap, create dfms
for(d in 1:nrow(media2012_dfm)){
  media2012_boot[d,,] <- rmultinom(nboot, size = sum(media2012_dfm[d,])
                                   , prob = media2012_dfm[d,]/sum(media2012_dfm[d,])
  )
}

## initialize object to store similarity results
tmp <- tmp_s <- array(dim=c(nrow(media2012_dfm),6,nboot)
                      , dimnames = list(docs = rownames(media2012_dfm)
                                        , mft = c(names(dict),"general")
                                        , iter = 1:nboot))

## compute similarity for bootstrapped dfms
pb <- txtProgressBar(min = 0, max = nboot, style = 3)
for(i in 1:nboot){
  tmp_tfidf <- media2012_boot[,,i] %>% as.dfm() %>% tfidf(scheme_tf = "prop",k=0)
  tmp_tfidf <- tmp_tfidf[,dict_df[,2]]
  
  ## count relative tfidf weights for each media source
  tmp[,,i] <- data.frame(
    authority = apply(tmp_tfidf[,dict_list$authority],1,sum,na.rm=T)
    , fairness = apply(tmp_tfidf[,dict_list$fairness],1,sum,na.rm=T)
    , harm = apply(tmp_tfidf[,dict_list$harm],1,sum,na.rm=T)
    , ingroup = apply(tmp_tfidf[,dict_list$ingroup],1,sum,na.rm=T)
    , purity = apply(tmp_tfidf[,dict_list$purity],1,sum,na.rm=T)
    , general = 0
  ) %>% as.matrix()
  
  ## compute general moralization
  tmp[,"general",i] <- apply(tmp[,1:4,i],1,sum)
  
  ## create scaled variable for moral foundations
  tmp_s[,,i] <- apply(tmp[,,i], 2, function(x) scale(x, center=median(x)))
  
  ## progress bar
  setTxtProgressBar(pb, i)
}
close(pb)

for(m in 1:dim(tmp)[2]){
  tmp_res <- t(apply(tmp_s[,m,],1,quantile,c(.025,.975)))
  colnames(tmp_res) <- paste(names(tmp_s[1,,1])[m],c("lo","hi"),sep="_")
  media2012 <- cbind(media2012, tmp_res); rm(tmp_res)
}

m=4
quantile(tmp_s["TV_NBC_Dateline.txt",m,],.025)
hist(tmp_s["TV_NBC_Dateline.txt",m,])
i=496

##############################
### save output for analyses.R


save(anes2012, anes2012opend, media2012, anes2012weights
     , mftLabs, polLabs, file="out/anes_prep.rda")
