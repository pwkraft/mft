###########################################################################################
## Project:  Measuring Morality in Political Attitude Expression
## File:     appendix_feinberg.R
## Overview: validation of dictionary method (additional plots for appendix)
## Author:   Patrick Kraft
###########################################################################################

## packages
pkg <- c("tidyverse","foreign","car","quanteda",
         "gridExtra","xtable","VGAM")
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
                                          , "fairness", "harm", "general")
                             , labels = c("Sanctity","Authority","Loyalty"
                                          ,"Fairness","Care","General"))
plot_df$Method
grep("harm", plot_df$variable)


p <- NULL
m <- NULL
vname <- c("harm","fairness","ingroup","authority")#,"purity")
vlab <- c("Care","Fairness","Loyalty","Authority")#,"Sanctity")
for(i in 1:length(vname)){
  
  # m[[vname[i]]] <- lm(fbrg_mft[,vname[i]]~fbrg_mft[,paste0(vname[i],"_rtf")])
  # #tmp <- as.character(paste0("R^2 == ",round(summary(m[[vname[i]]])$r.squared*100,2)))
  # tmp <- as.character(paste0("italic(r) == ",round(cor(fbrg_mft[,vname[i]]
  #                                                      ,fbrg_mft[,paste0(vname[i],"_rtf")]),2)))
  # p[[vname[i]]] <- ggplot(fbrg_mft, aes_string(x=vname[i],y=paste0(vname[i],"_rtf"))) + 
  #   geom_smooth(method="lm", col = "black", size=.5) + geom_point(alpha=.2, size=.5) + 
  #   theme_classic(base_size = 8) + theme(panel.border = element_rect(fill=NA)) +
  #   ggtitle(vlab[i]) + ylab("Weighted Dictionary") + xlab("Manual Coding") + ylim(0,7) + xlim(0,6) +
  #   annotate("text",x=0,y=6.5,label=tmp,hjust=0,size=3,parse=T)
  
  m[[paste0(vname[i],"_tfidf")]] <- lm(fbrg_mft[,vname[i]]~fbrg_mft[,paste0(vname[i],"_tfidf")])
  #tmp <- as.character(paste0("R^2 == ",round(summary(m[[paste0(vname[i],"_tfidf")]])$r.squared*100,2)))
  tmp <- as.character(paste0("italic(r) == ",round(cor(fbrg_mft[,vname[i]]
                                                       ,fbrg_mft[,paste0(vname[i],"_tfidf")]),2)))
  
  p[[paste0(vname[i],"_tfidf")]] <- ggplot(fbrg_mft, aes_string(x=vname[i],y=paste0(vname[i],"_tfidf"))) + 
    geom_smooth(method="lm", col = "black", size=.5) + geom_point(alpha=.2, size=.5) + 
    theme_classic(base_size = 8) + theme(panel.border = element_rect(fill=NA)) +
    ggtitle(vlab[i]) + ylab("MFT Score") + xlab("Manual Coding") + ylim(0,7) + xlim(0,6) +
    annotate("text",x=0,y=6.5,label=tmp,hjust=0,size=3,parse=T)
  
  # if(i == length(vname)){
  #   p[[vname[i]]] <- p[[vname[i]]] + xlab("Traditional Dictionary")
  #   p[[paste0(vname[i],"_tfidf")]] <- p[[paste0(vname[i],"_tfidf")]] + xlab("Weighted Dictionary")
  # }
}

pdf("fig/feinberg_sep.pdf",height=4,width=4)
grid.arrange(grobs=p, ncol=2)
dev.off()


library(corrplot)
M <- cor(select(fbrg_mft, -article, -id, -paper))

corrplot(M)
