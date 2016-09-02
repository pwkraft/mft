###########################################################################################
## Project:  Moral Foundations of Political Reasoning
## File:     func.R
## Overview: functions used in analyses.R
## Author:   Patrick Kraft
###########################################################################################

### Load packages
pkg <- c("reshape2","ggplot2")
invisible(lapply(pkg, library, character.only = TRUE))
rm(pkg,inst)


### function to plot proportions

prop_plot <- function(data, title, mftvarnames, groupvarname, legendname, file = NULL
                    , width = par("din")[1], height = par("din")[2]){
    ## prepare dataset
    ci <- function(x){1.96 * sqrt((mean(x, na.rm=T)*(1-mean(x, na.rm=T)))/sum(!is.na(x)))}

    prop_df <- NULL
    for(i in 1:length(data)){
        tmp <-  cbind(melt(aggregate(data[[i]][,mftvarnames]*data[[i]]$weight
                                    ,by=list(groupvar = data[[i]][,groupvarname]),FUN=mean,na.rm=T))
                    , melt(aggregate(data[[i]][,mftvarnames]*data[[i]]$weight
                                    ,by=list(groupvar = data[[i]][,groupvarname])
                                    ,FUN=function(x){mean(x, na.rm=T) - ci(x)}))[,3]
                    , melt(aggregate(data[[i]][,mftvarnames]*data[[i]]$weight
                                    ,by=list(groupvar = data[[i]][,groupvarname])
                                    ,FUN=function(x){mean(x, na.rm=T) + ci(x)}))[,3]
                      )
        tmp$year <- unique(data[[i]]$year)[1]
        prop_df <- rbind(prop_df, tmp)
        rm(tmp)
    }
    colnames(prop_df) <- c("groupvar", "mft", "Proportion", "cilo", "cihi","year")


    ## create plot
    out <- ggplot(prop_df, aes(x = Proportion, y = as.numeric(mft)+.4-.2*as.numeric(groupvar)
                             , shape=groupvar, color = groupvar)) +
        geom_point(size=3) + geom_errorbarh(aes(xmax=cihi,xmin=cilo),height=0) +
        scale_color_manual(values=c("royalblue", "forestgreen", "firebrick")) +
        labs(y = "Moral Foundation", x = "Proportion of Respondents") +
        ggtitle(title) + theme_classic(base_size = 8) + theme(panel.border = element_rect(fill=NA)) + geom_hline(yintercept = seq(1.5,4.5,1), col = "grey") +
        guides(color=guide_legend(title=legendname), shape=guide_legend(title=legendname)) +
        scale_x_continuous(limits = c(0, 0.5)) + theme(legend.position="bottom") +
        scale_y_continuous(breaks=1:5, labels=c("Purity / \nSanctity", "Authority / \nRespect"
                                              , "Ingroup / \nLoyalty", "Fairness / \nReciprocity"
                                              , "Harm / \nCare"))
    if(length(data)>1) out <- out + facet_grid(year ~ .)

    ## save plot
    if(!is.null(file)){
        ggsave(filename = file, plot = out, width = width, height = height)
    }

    out
}