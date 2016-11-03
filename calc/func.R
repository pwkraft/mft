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


### global labels for plots

mftLabs <- c("Authority / \nRespect", "Ingroup / \nLoyalty"
             , "Fairness / \nReciprocity", "Harm / \nCare")
polLabs <- c("Political\nKnowledge","Political Media\nExposure","Political\nDiscussions")
covLabs <- c("Church Attendance","Education (College Degree)","Age","Sex (Female)"
             ,"Race (African American)","Number of Words")


### function to pre-process open-ended responses and calculate cosine similarity

mftScore <- function(opend, id, dict, regex, dict_list){
  if(nrow(opend) != length(id)) stop("ID vector must be equal to number of observations/documents")
  if(length(unique(id)) != length(id))  stop("IDs are not unique")
  
  ## minor pre-processing
  spell <- apply(opend, 2, function(x){
    x <- toLower(x)
    x <- gsub("(^\\s+|\\s+$)","", x)
    x <- gsub("//"," ", x , fixed = T)
    x <- gsub("[[:punct:]]"," ", x)
    x <- gsub("\\s+"," ", x)
    x <- gsub("(^\\s+|\\s+$)","", x)
    x[x %in% c("1 inapplicable","7 refused","n a","no","none","43042","i am","nome"
               ,"i refuse", "i rwfuse to disclose", "refuse to disclose"
               ,"dk","skip","no5","don t know","same","not really"
               ,"no idea", "can t say","no comment","no views","nope","not at all"
               ,"no i can t","no i cant", "i don t know","iguess not","i dont know"
               , "dont know", "dint care","no no comment","no not really", "again no"
               , "1", "1 dk","dk5","no answer","hi","i","not","nothing","no commont"
               , "can t answer","no can not","dosen t know","he is not sure"
               , "its confidential","no answwer","not reaslly","lkjlkj","skjzhdkjhsd"
               , "you can", "even", "can","dont know dont talk about politics"
               , "dont knoiw","nono","not sure","do not know it","quit"
               , "doesnt know","she doesnt know","no not thinking","cant say"
               , "i don t know much", "would rather not explain","past"
               , "skipped question", "skip the question", "hjkdhfkjhdskjh"
               , "theuyidhfjdhkjdhfiaesjrhdjhflike shit", "dfdsjfksdjfkdsjf","dfsadfsf"
               , "god knows no i can t","no comments","dont want to comment"
               , "doesn t know","wants to skip","no not sure","no i caint", "not really no"
               , "i really cant say let me think","nope i don t know what liberal is"
               , "dont know what a conservative is dont care","she cannot"
               , "doesn t klnow", "no i cain t", "decline", "really can t"
               , "i choose not to","no i don t want to","no skip")] <- ""
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
  spell_tfidf <- corpus(c(dict, spell), docnames = c(names(dict), names(spell))) %>% dfm()
  spell_tfidf <- spell_tfidf[names(spell),] %>% tfidf(normalize=T,k=1)
  spell_tfidf <- spell_tfidf[,regex[,2]]
  
  ## count relative tfidf weights for each media source
  sim <- data.frame(
    authority = apply(spell_tfidf[,dict_list$authority],1,sum)
    , fairness = apply(spell_tfidf[,dict_list$fairness],1,sum)
    , harm = apply(spell_tfidf[,dict_list$harm],1,sum)
    , ingroup = apply(spell_tfidf[,dict_list$ingroup],1,sum)
    , purity = apply(spell_tfidf[,dict_list$purity],1,sum)
  )
  sim$general <- apply(sim,1,sum)
  sim$id <- gsub("\\.txt","",rownames(sim))
  
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



