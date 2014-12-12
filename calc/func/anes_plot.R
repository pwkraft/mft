##########################################################################################
# Project:  Moral foundations of Political Reasoning
# File:     anes_plot.R
# Overview: this file contains several functions used in mft_analyses.R to plot the 
#           dependent variables as well as the model results
# Author:   Patrick Kraft
# Date:     12/11/2014
##########################################################################################

### Load packages and data
setwd("/data/Uni/projects/2014/mft/calc")
pkg <- c("reshape2","ggplot2","Hmisc")
inst <- pkg %in% installed.packages()  
if(length(pkg[!inst]) > 0) install.packages(pkg[!inst])  
lapply(pkg,function(x){suppressPackageStartupMessages(library(x,character.only=TRUE))})
rm(pkg,inst)


### function to plot proportions

prop_plot <- function(data, title, mftvarnames, groupvarname, legendname, file = NULL){
  ## prepare dataset
  ci <- function(x){1.96 * sqrt((mean(x, na.rm=T)*(1-mean(x, na.rm=T)))/sum(!is.na(x)))}
  
  prop_df <- NULL
  for(i in 1:length(data)){
    tmp <-  cbind(melt(aggregate(data[[i]][,mftvarnames]*data[[i]]$weight
                                     ,by=list(groupvar = data[[i]][,groupvarname]),FUN=mean,na.rm=T))
                      , melt(aggregate(data[[i]][,mftvarnames]*data[[i]]$weight,by=list(groupvar = data[[i]][,groupvarname])
                                       ,FUN=function(x){mean(x, na.rm=T) - ci(x)}))[,3]
                      , melt(aggregate(data[[i]][,mftvarnames]*data[[i]]$weight,by=list(groupvar = data[[i]][,groupvarname])
                                       ,FUN=function(x){mean(x, na.rm=T) + ci(x)}))[,3]
                  )
    tmp$year <- unique(data[[i]]$year)[1]
    prop_df <- rbind(prop_df, tmp)
    rm(tmp)
  }
  colnames(prop_df) <- c("groupvar", "mft", "Proportion", "cilo", "cihi","year")
  
  
  ## create plot
  out <- ggplot(prop_df, aes(x = Proportion, y = as.numeric(mft)+.30-.15*as.numeric(groupvar), shape=groupvar, color=groupvar)) +
            geom_point(size=3) + geom_errorbarh(aes(xmax=cihi,xmin=cilo),height=.1) +
            facet_grid(year ~ .) + labs(y = "Moral Foundation", x = "Proportion of Respondents") +
            guides(color=guide_legend(title=legendname), shape=guide_legend(title=legendname)) +
            scale_x_continuous(limits = c(0, 0.521)) + theme_bw() + theme(legend.position="bottom") +
            scale_y_continuous(breaks=1:5, labels=c("Purity / Sanctity", "Authority / Respect", "Ingroup / Loyalty"
                                                    , "Fairness / Reciprocity", "Harm / Care")) 
  
  ## save plot
  if(!is.null(file)){
    ggsave(filename = file, plot = out)
  }
  
  out
}



# library(plyr)
# # your data
# df <- data.frame(gp = rep(LETTERS[1:5], each =8), y = sample(1:4,40,replace=TRUE))
# # calculate offsets
# df <- ddply(df, .(y, gp), transform, offset = (1:length(gp)-1)/20)
# qplot(gp, y, data=df) + stat_identity(aes(as.numeric(gp)+offset)) + theme_bw() 
