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

## plot bivariate relationships
plot_df <- fbrg_mft %>% select(-paper,-article) %>% 
  gather("variable","Score",authority_rtf:purity_tfidf)
plot_df$Foundation <- factor(gsub("_.*","",plot_df$variable)
                             , levels = c("purity", "authority", "ingroup"
                                          , "fairness", "harm")
                             , labels = c())
plot_df$Method
grep("harm", plot_df$variable)


p <- NULL
vname <- c("harm","fairness","ingroup","authority")
vlab <- c("Harm/Care","Fairness/Reciprocity","Ingroup/Loyalty","Authority/Respect")
for(i in 1:length(vname)){
  
  m <- lm(fbrg_mft[,vname[i]]~fbrg_mft[,paste0(vname[i],"_rtf")])
  t <- as.character(paste0("R^2 == ",round(summary(m)$r.squared*100,2)))

  p[[vname[i]]] <- ggplot(fbrg_mft, aes_string(y=vname[i],x=paste0(vname[i],"_rtf"))) + 
    geom_smooth(method="lm", col = "black") + geom_point(alpha=.2, size=1) + 
    theme_classic(base_size = 8) + theme(panel.border = element_rect(fill=NA)) +
    ggtitle(vlab[i]) + ylab("Manual Coding") + xlab("") + ylim(0,6) +
    annotate("text",x=0,y=6,label=t,hjust=0,size=3,parse=T)
  
  m <- lm(fbrg_mft[,vname[i]]~fbrg_mft[,paste0(vname[i],"_tfidf")])
  t <- as.character(paste0("R^2 == ",round(summary(m)$r.squared*100,2)))
  
  p[[paste0(vname[i],"_tfidf")]] <- ggplot(fbrg_mft, aes_string(y=vname[i],x=paste0(vname[i],"_tfidf"))) + 
    geom_smooth(method="lm", col = "black") + geom_point(alpha=.2, size=1) + 
    theme_classic(base_size = 8) + theme(panel.border = element_rect(fill=NA)) +
    ggtitle("") + ylab("") + xlab("") + ylim(0,6) +
    annotate("text",x=0,y=6,label=t,hjust=0,size=3,parse=T)
  
  if(i == length(vname)){
    p[[vname[i]]] <- p[[vname[i]]] + xlab("Traditional Dictionary")
    p[[paste0(vname[i],"_tfidf")]] <- p[[paste0(vname[i],"_tfidf")]] + xlab("Weighted Dictionary")
  }
}

png("fig/feinberg.png",height=10,width=5,units="in",res=300)
grid.arrange(grobs=p, ncol=2)
dev.off()

