##########################################################################################
# Project:  Moral foundations of Political Reasoning
# File:     mft_prep.R
# Overview: this file prepares the open ended survey responses in the anes as well as the
#           original anes time series datasets for the subsequent analyses in mft_analyses
# Author:   Patrick Kraft
# Date:     12/08/2014
##########################################################################################


rm(list=ls())
if(sessionInfo()$platform == "x86_64-apple-darwin10.8.0 (64-bit)"){
  setwd("~/research/mft/calc")
} else {
  setwd("/data/Uni/projects/2014/mft/calc") 
}
source("func/anes_recode.R")
# load("out/anes_full.RData")

### spell checking and preprocessing of open survey responses

anes2008opend <- opend_prep(csv_src = "/data/Copy/1-src/data/anes2008/anes2008TSopenends_redacted_Dec2012Revision.csv"
                      , varlist = list(id = "caseid"
                                  , ca_li_dem = "DemPC_like"
                                  , ca_di_dem = "DemPC_dislike"
                                  , ca_li_rep = "RepPC_like"
                                  , ca_di_rep = "RepPC_dislike"
                                  , pa_li_dem = "DemParty_like"
                                  , pa_di_dem = "DemParty_dislike"
                                  , pa_li_rep = "RepParty_like"
                                  , pa_di_rep = "RepParty_dislike"
                                  ), raw_out = TRUE)

anes2012opend <- opend_prep(csv_src = "/data/Copy/1-src/data/anes2012/anes2012TS_openends.csv"
                      , varlist = list(id = "caseid"
                                  , ca_li_dem = "candlik_likewhatdpc"
                                  , ca_di_dem = "candlik_dislwhatdpc"
                                  , ca_li_rep = "candlik_likewhatrpc"
                                  , ca_di_rep = "candlik_dislwhatrpc"
                                  , pa_li_dem = "ptylik_lwhatdp"
                                  , pa_di_dem = "ptylik_dwhatdp"
                                  , pa_li_rep = "ptylik_lwhatrp"
                                  , pa_di_rep = "ptylik_dwhatrp"
                                  ), raw_out = TRUE)


### mft dictionary and word count

anes2008opend$resp <- opend_mft(data = anes2008opend$spell, use_dict = "new")

anes2012opend$resp <- opend_mft(data = anes2012opend$spell, use_dict = "new")


### basic data recoding for each ANES survey

anes2008ts <- ts_recode(dta_src = "/data/Copy/1-src/data/anes2008/anes_timeseries_2008.dta"
                      , raw_out = TRUE
                      , id          = "V080001"
                      , year        = 2008
                      , weight      = "V080101"
                      , ideol       = "V083069"
                      , issues      = list(govspend = c("V083105","V083108x","reversed")
                                           , medins = c("V083119","V083124x"))
                        # jobs issue was only included for half of sample -> left out
                      , issue_aid   = "V083148"
                      , issue_abort = NULL
                        # abortion issue was only included for half of sample -> left out
                      , issue_gay   = "V083213"
                      , issue_women = NULL
                        # women issue was only included for half of the sample -> left out
                      , pid         = "V083098x"
                      , polmedia    = list(c("V083019","V083024")
                                           , c("V083021a", "V083025")
                                           , c("V083021b", "V083023")
                                           , c("V083022", "V083026"))
                      , polknow     = list(V085119a = 5)
                      , poldisc     = list(oft = "V085108a"
                                           , ever = "V085108"  
                                           , alternative = "V085109")  # postelection!!!
                      , pastvote    = "V083007"
                      , vote_dem    = "V083169a"
                      , age         = "V081104"
                      , regdi_month = list(byear="V083215a"
                                           , bmonth="V083215b")
                      , female      = "V081101"
                      , black       = "V081102"
                      , educ        = list(V083218x = 4)
                      , relig       = list(oft = "V083186a"
                                           , ever = "V083186"
                                           , more = "V083186b")
                      , spanish     = list(V082011 = 2
                                           , V082011 = 3)
                      )

anes2012ts <- ts_recode(dta_src = "/data/Copy/1-src/data/anes2012/anes_timeseries_2012.dta"
                      , raw_out = TRUE
                      , id          = "caseid"
                      , year        = 2012
                      , weight      = "weight_full"
                      , ideol       = "libcpre_self"
                      , issues      = list(govspend = "spsrvpr_ssself"
                                           , medins = "inspre_self"
                                           , jobs = "guarpr_self")
                      , issue_aid   = "fedspend_poor"
                      , issue_abort = "abortpre_4point"
                      , issue_gay   = "gayrt_adopt"
                      , issue_women = NULL
                      , pid         = "pid_x"
                      , polmedia    = list("prmedia_wkinews"
                                           , "prmedia_wktvnws"
                                           , "prmedia_wkpaprnws"
                                           , "prmedia_wkrdnws")
                      , polknow     = list(preknow_prestimes = 2
                                           , preknow_sizedef = 1
                                           , preknow_senterm = 6
                                           , preknow_medicare = 1
                                           , preknow_leastsp = 1)
                      , poldisc     = list(oft = "discuss_discpstwk"
                                           , ever = "discuss_disc")   # postelection!!!
                      , pastvote    = "interest_voted2008"
                      , vote_dem    = "prevote_intpreswho"
                      , age         = "dem_age_r_x"
                      , female      = "gender_respondent_x"
                      , black       = "dem_raceeth_x"
                      , educ        = list(dem_edugroup_x = 4)
                      , relig       = list(oft = "relig_churchoft"
                                           , ever = "relig_church"
                                           , more = "relig_churchwk")
                      , spanish     = list(profile_spanishsurv = 1
                                           , admin_pre_lang_start = 2
                                           , admin_post_lang_start = 2)
                      )


### merge anes time series and open-ended responses

anes2008merge <- anes_merge(ts = anes2008ts, opend = anes2008opend
                            , valence = FALSE, check = TRUE)

anes2012merge <- anes_merge(ts = anes2012ts, opend = anes2012opend
                            , valence = FALSE, check = TRUE)


### save objects for analyses

save(anes2008ts, anes2008opend, anes2008merge
     , anes2012ts, anes2012opend, anes2012merge
     , file="out/anes_full.RData")

anes2008 <- anes2008merge$data
anes2012 <- anes2012merge$data

save(anes2008, anes2012, file="out/anes.RData")
