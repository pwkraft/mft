###########################################################################################
## Project:  Moral foundations of Political Reasoning
## File:     anes_plot.R
## Overview: functions used in mft_analyses.R and mft_analyses_prelim.R to plot the 
##           dependent variables as well as the model results
## Author:   Patrick Kraft
###########################################################################################

### Load packages and data
setwd("/data/Uni/projects/2014/mft/calc")
pkg <- c("reshape2","ggplot2","Hmisc","MASS","sandwich")
inst <- pkg %in% installed.packages()  
if(length(pkg[!inst]) > 0) install.packages(pkg[!inst])  
lapply(pkg,function(x){suppressPackageStartupMessages(library(x,character.only=TRUE))})
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
        geom_point(size=3) + geom_errorbarh(aes(xmax=cihi,xmin=cilo),height=.2) +
        scale_color_manual(values=c("royalblue", "forestgreen", "firebrick")) +
        labs(y = "Moral Foundation", x = "Proportion of Respondents") +
        ggtitle(title) + theme_bw() + geom_hline(yintercept = seq(1.5,4.5,1), col = "grey") +
        guides(color=guide_legend(title=legendname), shape=guide_legend(title=legendname)) +
        scale_x_continuous(limits = c(0, 0.6)) + theme(legend.position="bottom") +
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


### Function to calculate expected values/first differences (replaces Zelig call)

sim <- function(models, iv, robust=F, ci=c(0.025,0.975)){
    ## function to calculate first differences in predicted probabilities for probit model
    ## models: list of glm probit models
    ## iv: data frame containing the values for comparison (only 2 rows, selected variables)

    ## prepare output object, convert input to model list
    out <- NULL
    if(class(models)[1] != "list") models <- list(models)

    for(i in 1:length(models)){
        ## simulate betas from sampling distribution
        if(robust == T){
            betas <- mvrnorm(1000, coef(models[[i]]), vcovHC(models[[i]]))
        } else {
            betas <- mvrnorm(1000, coef(models[[i]]), vcov(models[[i]]))
        }

        ## extract variable names
        vars <- names(coef(models[[i]]))
        int <- grep("[^)]:", vars)
        varsInt <- strsplit(vars[int], ":")

        ## generate matrix of covariates
        X <- matrix(1, nrow=length(vars), ncol=nrow(iv))
        X[vars %in% names(iv),] <- t(iv[vars[vars %in% names(iv)]])
        if(class(models[[i]])[1]=="lm"){
            means <- apply(models[[i]]$model[vars[-c(1,which(vars %in% names(iv)),int)]]
                         , 2, mean, na.rm=T)
        } else if(class(models[[i]])[1] == "glm"){
            means <- apply(models[[i]]$data[vars[-c(1,which(vars %in% names(iv)),int)]]
                         , 2, mean, na.rm=T)
        } else if(class(models[[i]])[1] == "vglm" & models[[i]]@family@vfamily == "tobit"){
            means <- apply(models[[i]]@x[,vars[-c(1,2,which(vars %in% names(iv)),int)]]
                         , 2, mean, na.rm=T)
        } else stop("Model type not supported")
        X[vars %in% names(means),] <- means

        ## calculate interaction effects
        if(length(varsInt)>0){
            for(j in 1:length(varsInt)){
                X[int[j],] <- apply(X[vars %in% varsInt[[j]],],2,prod)
            }
        }
        
        ## calculate expected values
        if(class(models[[i]])[1]=="lm"){
            evs <- betas %*% X
        } else if(class(models[[i]])[1] == "glm"){
            if(models[[i]]$family$link == "logit"){
                evs <- 1/(1+exp(-betas %*% X))
            } else if(models[[i]]$family$link == "probit"){
                evs <- pnorm(betas %*% X)
            } else stop("Model type not supported")
        } else if(class(models[[i]])[1] == "vglm" & models[[i]]@family@vfamily == "tobit"){
            ## CONTINUE HERE, check whether this is correct
            ## IDEA: each original evs column as a new matrix where each column is an individual
            ## simulation with the given mean of the row of the original matrix.
            ## then decompose effect of tobit in dP(Y>0) and dY|Y>0
            evs <- matrix(rnorm(nrow(betas)*100, betas[,-2] %*% X[-2,]
                              , sd = exp(betas[,2])), ncol = 2)
            evs2 <- apply(evs, 2, function(x) )
            
            unique(models[[i]]@misc$Lower)
        } else stop("Model type not supported")
        
        if(nrow(iv)==2){
            ## calculate first differences
            evs <- evs[,2] - evs[,1]
        } else if(nrow(iv)==4) {
            ## calculate difference-in-difference
            evs <- (evs[,2] - evs[,1]) - (evs[,4] - evs[,3])
        } else {
            warning("Check number of scenarios")
        }

        ## generate output table
        res <- data.frame(mean = mean(evs)
                        , cilo = quantile(evs, ci[1])
                        , cihi = quantile(evs, ci[2])
                        , dv = as.factor(colnames(models[[i]]$model)[1])
                        , iv = as.factor(paste(colnames(iv), collapse = "_")))
        out <- rbind(out, res)
    }

    ## return output table
    rownames(out) <- NULL
    return(out)
}


