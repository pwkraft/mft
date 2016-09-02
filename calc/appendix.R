###############################################################################################
## Project:  Moral foundations of Political Reasoning
## File:     appendix.R
## Overview: analyses for appendix, produces all additional plots and tables
##           based on data prepared in prep.R
## Author:   Patrick Kraft
###############################################################################################

## packages
pkg <- c("dplyr","ggplot2","stargazer","xtable","VGAM","gridExtra","pmisc")
invisible(lapply(pkg, library, character.only = TRUE))
rm(list=ls())

## working directory
setwd("/data/Dropbox/Uni/Projects/2014/mft/calc")

## load additional functions
source("func.R")

## load recoded dataset
load("out/anes.RData")


#################
### Data Overview


### table for missing cases

## prepare table
tab_mis <- rbind(c(table(anes2012$spanish)[2]
                   , table(anes2012$spanish)[2]*100/sum(table(anes2012$spanish)))
                 , c(table(anes2012$wc==0)[2]
                     , table(anes2012$wc==0)[2]*100/sum(table(anes2012$wc==0))))
colnames(tab_mis) <- c("N","Percent")
rownames(tab_mis) <- c("Spanish Interview", "No Responses")

## export table
print(xtable(tab_mis, align="lcc",digits=c(0,0,2)
             , caption = "Missing open-ended responses"
             , label="tab:app_mis"),file="tab/app_mis.tex")

## drop spanish respondents and empty responses
anes2012 <- anes2012[anes2012$spanish != 1 & anes2012$wc != 0,]


### plot number of words (for wc>0!)

## histogram/density of wc
wc_mean = mean(anes2012$wc)
p1 <- ggplot(anes2012, aes(wc)) + 
  geom_histogram(fill = "grey", binwidth = 25) + 
  theme_classic(base_size = 8) + 
  theme(panel.border = element_rect(fill=NA)) + 
  geom_vline(xintercept = wc_mean, linetype = 3) +
  ylab("Number of Respondents") + xlab("Word Count")

## histogram/density of lwc
lwc_mean = mean(anes2012$lwc)
p2 <- ggplot(anes2012, aes(lwc, ..density..)) + 
  geom_histogram(binwidth = 0.2, fill='grey') + geom_density() + 
  theme_classic(base_size = 8) + theme(panel.border = element_rect(fill=NA)) + 
  geom_vline(xintercept = lwc_mean, linetype = 3) + 
  ylab("Density") + xlab("log(Word Count)")

## combine plots
pdf("fig/app_wc.pdf",width=7, height=3)
grid.arrange(p1, p2, ncol=2)
dev.off()



