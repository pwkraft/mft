###########################################################################################
## Project:  Moral Foundations of Political Reasoning
## File:     analyses_feinberg.R
## Overview: validation of dictionary method
## Author:   Patrick Kraft
###########################################################################################

## packages
pkg <- c("tidyverse","foreign","car","quanteda",
         "gridExtra","stargazer","xtable","VGAM")
invisible(lapply(pkg, library, character.only = TRUE))
rm(list=ls())

## working directory
setwd("~/Dropbox/Uni/Projects/2014/mft/calc")

## load additional functions
source("func.R")

## load recoded dataset
load("out/prep_fbrg.RData")



## General MFT references

p <- NULL
m <- NULL

m$general <- lm(general~general_rtf, data=fbrg_mft)
#tmp <- as.character(paste0("R^2 == ",round(summary(m$general)$r.squared*100,2)))
tmp <- as.character(paste0("italic(r) == ",round(cor(fbrg_mft$general,fbrg_mft$general_rtf),2)))

p$general <- ggplot(fbrg_mft, aes(x=general,y=general_rtf)) + 
  geom_smooth(method="lm", col = "black", size=.5) + geom_point(alpha=.2, size=.5) + 
  theme_classic(base_size = 8) + theme(panel.border = element_rect(fill=NA)) +
  ggtitle("General MFT") + xlab("Manual Coding") + ylab("Traditional Dictionary") + 
  annotate("text",x=0,y=6,label=tmp,hjust=0,size=2,parse=T)

m$general_tfidf <- lm(general~general_tfidf, data=fbrg_mft)
#tmp <- as.character(paste0("R^2 == ",round(summary(m$general_tfidf)$r.squared*100,2)))
tmp <- as.character(paste0("italic(r) == ",round(cor(fbrg_mft$general,fbrg_mft$general_tfidf),2)))

p$general_tfidf <- ggplot(fbrg_mft, aes(x=general,y=general_tfidf)) + 
  geom_smooth(method="lm", col = "black", size=.5) + geom_point(alpha=.2, size=.5) + 
  theme_classic(base_size = 8) + theme(panel.border = element_rect(fill=NA)) +
  ggtitle("General MFT") + xlab("Manual Coding") + ylab("General MFT Score") + 
  annotate("text",x=0,y=6,label=tmp,hjust=0,size=2,parse=T)

png("fig/feinberg_2versions.png",height=3,width=5,units="in",res=300)
grid.arrange(grobs=p, ncol=2)
dev.off()

p$general_tfidf + ggtitle("")
ggsave("fig/feinberg_general.pdf", height=2, width=2)

