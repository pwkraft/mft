###########################################################################################
## Project:  Moral Foundations of Political Reasoning
## File:     func.R
## Overview: functions used in analyses.R
## Author:   Patrick Kraft
###########################################################################################

### Load packages
pkg <- c("reshape2","ggplot2","dplyr","quanteda")
invisible(lapply(pkg, library, character.only = TRUE))
rm(pkg)


### function to pre-process open-ended responses and calculate cosine similarity

mftSimilarity <- function(opend, id, dict, regex){
  if(nrow(opend) != length(id))          stop("ID vector must be equal to number of observations/documents")
  if(length(unique(id)) != length(id))  stop("IDs are not unique")
  
  ## minor pre-processing
  spell <- apply(opend, 2, function(x){
    x <- gsub("(^\\s+|\\s+$)","", x)
    x[x %in% c("-1 Inapplicable","-7 Refused","N/A","no","none","#(43042)","i am","Nome")] <- ""
    x <- gsub("//"," ", x , fixed = T)
    x <- gsub("[[:punct:]]"," ", x)
    x <- gsub("\\s+"," ", x)
    x <- gsub("(^\\s+|\\s+$)","", x)
    return(x)
  })
  
  ## num-lock issue
  # maybe look into this later
  
  ## fix words without whitespace
  # maybe look into this later
  
  ## spell-checking
  write.table(spell, file = "opend_combined.csv"
              , sep = ",", col.names = F, row.names = F)
  spellCheck <- aspell("opend_combined.csv") %>%
    filter(Suggestions!="NULL")
  file.remove("opend_combined.csv")
  
  ## replace incorrect words
  for(i in 1:nrow(spellCheck)){
    spell[spellCheck$Line[i],] <- gsub(spellCheck$Original[i], unlist(spellCheck$Suggestions[i])[1]
                                          , spell[spellCheck$Line[i],])
  }
  
  ## meta-info: overall response length
  wc <- apply(spell, 1, function(x){length(unlist(strsplit(x,"\\s+")))})
  lwc <- log(wc)
  
  ## meta-info: number of items answered
  nitem <- apply(spell != "", 1, sum, na.rm = T)
  
  ## aggregate open-ended responses
  spell <- toLower(apply(spell, 1, paste, collapse = " "))
  spell <- gsub("NA\\s*","",spell)
  names(spell) <- id
  #spell <- spell[spell != ""]
  
  ## replace regular expressions with word stems
  for(i in 1:nrow(regex)){
    spell <- gsub(regex[i,1], regex[i,2], spell)
  }
  
  ## combine dictionary and responses in common dfm/tfidf
  spell_tfidf <- corpus(c(dict, spell), docnames = c(names(dict), names(spell))) %>% 
    dfm() %>% tfidf()
  
  ## calculate cosine similarity b/w dictionaries and documents (check pr_DB$get_entries() for options)
  # normalization is not necessary, cosine similarity is length invariant so results are unchanged
  sim <- similarity(spell_tfidf, selection = names(dict)
                    , margin = "documents", method = "cosine") %>% 
    as.matrix() %>% data.frame() %>% 
    mutate(general = apply(.,1,sum), id = as.numeric(rownames(.))) %>%
    arrange(id) %>% filter(!is.na(id))
  
  ## create scaled variable for moral foundations
  sim_s <- apply(select(sim,-id), 2, function(x) x/sd(x[wc>0]))
  colnames(sim_s) <- paste0(colnames(sim_s),"_s")
  
  ## dichotmous indicator for moral foundations
  sim_d <- apply(select(sim,-id), 2, function(x) as.numeric(x>0))
  colnames(sim_d) <- paste0(colnames(sim_d),"_d")
  
  ## combine similarity results
  res <- cbind(sim, sim_d, sim_s)
  
  ## add spell-checked open-ends
  out <- data.frame(id, spell=spell, wc=wc, lwc=lwc, nitem=nitem
                    , stringsAsFactors = F) %>% merge(res)
  return(out)
}


### function to plot proportions

prop_plot <- function(data, title, mftvarnames, groupvarname, legendname, file = NULL
                    , width = par("din")[1], height = par("din")[2], lim = c(0, 0.6)){
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
        geom_point() + geom_errorbarh(aes(xmax=cihi,xmin=cilo),height=0) +
        scale_color_manual(values=c("royalblue", "forestgreen", "firebrick")) +
        labs(y = "Moral Foundation", x = "Proportion of Respondents") +
        ggtitle(title) + theme_classic(base_size = 8) + theme(panel.border = element_rect(fill=NA)) + 
        geom_hline(yintercept = seq(1.5,4.5,1), col = "lightgrey") +
        guides(color=guide_legend(title=legendname), shape=guide_legend(title=legendname)) +
        scale_x_continuous(limits = lim) + theme(legend.position="bottom") +
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