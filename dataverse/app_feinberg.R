###########################################################################################
## Project:  Measuring Morality in Political Attitude Expression
## File:     app_feinberg.R
## Overview: Compare MFT scores with manual coding by Feinberg/Willer (2013)
## Requires: - Replication data for Feinberg/Willer study
##              (data_feinberg.rda, original data available on request)
##           - Custom auxiliary functions (func.R)
## Author:   Patrick Kraft
###########################################################################################

## packages
pkg <- c("tidyverse","quanteda","gridExtra")
invisible(lapply(pkg, library, character.only = TRUE))
rm(list=ls())

## load additional functions
source("func.R")

## load data
load("data_feinberg.rda")



##########################
### clean replication data

## change id
fbrg_mft$id <- paste(gsub("(^\\s+|\\s+$)","", fbrg_mft$NEWSPAPER), fbrg_mft$ARTICLENUMBER)

## change variable names
colnames(fbrg_mft) <- c("paper","article","harm","fairness","authority","ingroup","purity","id")

## add general mft
fbrg_mft$general <- with(fbrg_mft, harm + fairness + authority +ingroup + purity)

## note that there might be an ID error in the original dataset (USA Today 22 & 23 switched)
fbrg_mft$id[fbrg_mft$id != fbrg_text$id]



######################
### compute MFT scores

## load moral foundations dictionary
load("mft_dictionary.rda")

## minor pre-processing
fbrg_text$processed <- paste(fbrg_text$title, fbrg_text$text) %>%
  char_tolower() %>% gsub("(^\\s+|\\s+$)","", .) %>% gsub("//"," ", ., fixed = T) %>%
  gsub("[[:punct:]]"," ", .) %>% gsub("\\n"," ", .) %>% gsub("\\s+"," ", .) %>%
  gsub("(^\\s+|\\s+$)","", .)

## replace regular expressions with word stems
pb <- txtProgressBar(min = 0, max = nrow(dict_df), style = 3)
for(i in 1:nrow(dict_df)){
  fbrg_text$processed <- gsub(dict_df[i,1], dict_df[i,2], fbrg_text$processed)
  setTxtProgressBar(pb, i)
}
close(pb)

## combine dictionary and responses in common dfm
fbrg_dfm <- corpus(c(dict, fbrg_text$processed)
                   , docnames = c(names(dict), as.character(fbrg_text$id))) %>% dfm()
fbrg_dfm <- fbrg_dfm[as.character(fbrg_text$id),]

## convert dfm to tfidf
fbrg_tfidf <- fbrg_dfm %>% tfidf(scheme_tf = "prop", k=0)
fbrg_tfidf <- fbrg_tfidf[,dict_df[,2]]

## count relative term frequencies & tfidf weights for each media source
sim <- data.frame(
  authority_tfidf = apply(fbrg_tfidf[,dict_list$authority],1,sum,na.rm=T)
  , fairness_tfidf = apply(fbrg_tfidf[,dict_list$fairness],1,sum,na.rm=T)
  , harm_tfidf = apply(fbrg_tfidf[,dict_list$harm],1,sum,na.rm=T)
  , ingroup_tfidf = apply(fbrg_tfidf[,dict_list$ingroup],1,sum,na.rm=T)
  , purity_tfidf = apply(fbrg_tfidf[,dict_list$purity],1,sum,na.rm=T)
  , general_tfidf = apply(fbrg_tfidf[,c(dict_list$authority,dict_list$fairness
                                      ,dict_list$harm,dict_list$ingroup)],1,sum,na.rm=T)
)

## rescale all vars to unit variance
sim <- data.frame(apply(sim, 2, function(x) x/sd(x, na.rm = T)))

## add id
sim$id <- rownames(sim)

## combine similarity results
fbrg_mft <- merge(fbrg_mft, sim)



#######################
### figures in appendix

### Fig C.5: Comparing general MFT scores with manual coding

tmp <- as.character(paste0("italic(r) == ",round(cor(fbrg_mft$general,fbrg_mft$general_tfidf),2)))
ggplot(fbrg_mft, aes(x=general,y=general_tfidf)) + 
  geom_smooth(method="lm", col = "black", size=.5) + geom_point(alpha=.2, size=.5) + 
  theme_classic(base_size = 8) + theme(panel.border = element_rect(fill=NA)) +
  xlab("Manual Coding") + ylab("General MFT Score") + 
  annotate("text",x=0,y=6,label=tmp,hjust=0,size=2,parse=T)
ggsave("fig/appC5_feinberg_general.pdf", height=2, width=2)


### Fig C.6: Comparing separate MFT scores with manual coding

plot_df <- fbrg_mft %>% select(-paper,-article) %>% 
  gather("variable","Score",authority_tfidf:purity_tfidf)
plot_df$Foundation <- factor(gsub("_.*","",plot_df$variable)
                             , levels = c("purity", "authority", "ingroup", "fairness", "harm")
                             , labels = c("Sanctity","Authority","Loyalty","Fairness","Care"))

p <- NULL
vname <- c("harm","fairness","ingroup","authority")
vlab <- c("Care","Fairness","Loyalty","Authority")
for(i in 1:length(vname)){
  tmp <- as.character(paste0("italic(r) == ",round(cor(fbrg_mft[,vname[i]]
                                                       ,fbrg_mft[,paste0(vname[i],"_tfidf")]),2)))
  p[[paste0(vname[i],"_tfidf")]] <- ggplot(fbrg_mft, aes_string(x=vname[i],y=paste0(vname[i],"_tfidf"))) + 
    geom_smooth(method="lm", col = "black", size=.5) + geom_point(alpha=.2, size=.5) + 
    theme_classic(base_size = 8) + theme(panel.border = element_rect(fill=NA)) +
    ggtitle(vlab[i]) + ylab("MFT Score") + xlab("Manual Coding") + ylim(0,7) + xlim(0,6) +
    annotate("text",x=0,y=6.5,label=tmp,hjust=0,size=3,parse=T)
}

pdf("fig/appC6_feinberg_sep.pdf",height=4,width=4)
grid.arrange(grobs=p, ncol=2)
dev.off()
