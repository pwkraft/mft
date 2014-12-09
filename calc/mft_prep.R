##########################################################################################
# Project:  Moral foundations of Political Reasoning
# File:     mft_prep.R
# Overview: this file prepares the open ended survey responses in the anes as well as the
#           original anes time series datasets for the subsequent analyses in mft_analyses
# Author:   Patrick Kraft
# Date:     12/08/2014
##########################################################################################


rm(list=ls())
setwd("/data/Uni/projects/2014/mft/calc")
source("func/anes_recode.R")


### spell checking and preprocessing of open survey responses

anes2008opend <- opend_prep(csv_src = "/data/Dropbox/1-src/data/anes2008/anes2008TSopenends_redacted_Dec2012Revision.csv"
                      , varlist = list(caseid = "id"
                                  , DemPC_like       = "ca_li_dem"
                                  , DemPC_dislike    = "ca_di_dem"
                                  , RepPC_like       = "ca_li_rep"
                                  , RepPC_dislike    = "ca_di_rep"
                                  , DemParty_like    = "pa_li_dem"
                                  , DemParty_dislike = "pa_di_dem"
                                  , RepParty_like    = "pa_li_rep"
                                  , RepParty_dislike = "pa_di_rep"
                                  ), raw_out = TRUE)

anes2012opend <- opend_prep(csv_src = "/data/Dropbox/1-src/data/anes2012/anes2012TS_openends.csv"
                      , varlist = list(caseid = "id"
                                  , candlik_likewhatdpc = "ca_li_dem"
                                  , candlik_dislwhatdpc = "ca_di_dem"
                                  , candlik_likewhatrpc = "ca_li_rep"
                                  , candlik_dislwhatrpc = "ca_di_rep"
                                  , ptylik_lwhatdp      = "pa_li_dem"
                                  , ptylik_dwhatdp      = "pa_di_dem"
                                  , ptylik_lwhatrp      = "pa_li_rep"
                                  , ptylik_dwhatrp      = "pa_di_rep"
                                  ), raw_out = TRUE)


### mft dictionary and word count

anes2008opend$resp <- opend_mft(data = anes2008opend$spell, use_dict = "new")

anes2008opend$resp <- opend_mft(data = anes2012opend$spell, use_dict = "new")


### basic data recoding for each ANES survey

anes2008 <- ts_recode(dta_src = "/data/Dropbox/1-src/data/anes2008/anes_timeseries_2008.dta", raw_out = TRUE)

anes2012 <- ts_recode(dta_src = "/data/Dropbox/1-src/data/anes2012/anes_timeseries_2012.dta", raw_out = TRUE
                      , id          = "caseid"
                      , ideol       = "libcpre_self"
                      , issues      = list(govspend = "spsrvpr_ssself"
                                           , medins = "inspre_self"
                                           , jobs = "guarpr_self")
                      , issue_aid   = "fedspend_poor"
                      , issue_abort = "abortpre_4point"
                      , issue_gay   = "gayrt_adopt"
                      , issue_women = NULL
                      , pid         = "pid_x"
                      , polint      = "interest_attention"
                      , polmedia    = NULL
                      , polknow     = NULL
                      , poldisc     = NULL
                      , regdisc     = list(byear=NULL, bmonth=NULL)
                      , pastvote    = NULL
                      , age         = "dem_age_r_x"
                      , female      = "gender_respondent_x"
                      , black       = "dem_raceeth_x"                      
                      , educ        = "dem_edugroup_x"
                      , relig       = list(oft = "relig_churchoft"
                                           , ever = "relig_church"
                                           , more = "relig_churchwk")
                      )

library(foreign)
source("/data/Dropbox/1-src/func/lookfor.R")
tmp2008 <- read.dta("/data/Dropbox/1-src/data/anes2008/anes_timeseries_2008.dta", convert.factors = FALSE)
raw <- read.dta("/data/Dropbox/1-src/data/anes2012/anes_timeseries_2012.dta", convert.factors = FALSE)

lookfor(raw,"gay")
table(tmp2012$gayrt_adopt)

### merge anes time series and open-ended responses


### save objects for analyses

save(anes2008opend, anes2012opend, file="out/anes_opend.RData")
