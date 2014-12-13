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
  out <- ggplot(prop_df, aes(x = Proportion, y = as.numeric(mft)+.30-.15*as.numeric(groupvar), shape=groupvar, color = groupvar)) +
            geom_point(size=3) + geom_errorbarh(aes(xmax=cihi,xmin=cilo),height=.1) +
            scale_color_manual(values=c("royalblue", "forestgreen", "firebrick")) +
            facet_grid(year ~ .) + labs(y = "Moral Foundation", x = "Proportion of Respondents") +
            ggtitle(title) + theme_bw() +
            guides(color=guide_legend(title=legendname), shape=guide_legend(title=legendname)) +
            scale_x_continuous(limits = c(0, 0.521)) + theme(legend.position="bottom") +
            scale_y_continuous(breaks=1:5, labels=c("Purity / Sanctity", "Authority / Respect", "Ingroup / Loyalty"
                                                    , "Fairness / Reciprocity", "Harm / Care"))
  
  ## save plot
  if(!is.null(file)){
    ggsave(filename = file, plot = out)
  }
  
  out
}

# Multiple plot function
# source: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_%28ggplot2%29/
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

