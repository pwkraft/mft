setwd("/data/Uni/projects/2014/mft/calc")
rm(list=ls())
library(plyr)
library(stringr)
library(tmt)
#library(tm)
#library(dplyr)

### load functions
source("func/opend_prep.R")
source("func/opend_mft.R")

### spell checking and preprocessing
anes2008opend <- opend_prep(src = "/data/Dropbox/1-src/data/anes2008/anes2008TSopenends_redacted_Dec2012Revision.csv"
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
anes2012opend <- opend_prep(src = "/data/Dropbox/1-src/data/anes2012/anes2012TS_openends.csv"
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

### save objects for analyses
save(anes2008opend, anes2012opend, file="out/anes_opend.RData")
