###########################################################################################
## Project:  Moral Foundations of Political Reasoning
## File:     func.R
## Overview: functions used in analyses_anes.R, analyses_lisurvey.R,
##           appendix_anes.R, appendix_lisurvey.R
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


### function to simulate expected values/first differences (replaces Zelig)
#' built: 2016-08-27, Patrick Kraft
#' @importFrom MASS mvrnorm
#' @importFrom sandwich vcovHC
#' @param models: list of model results (lm, glm, or vglm/tobit)
#' @param iv: data frame containing the values for comparison (only 2 rows, selected variables)
#' @param robust: logical, should robust standard errors be used
#' @param nsim: number of simulations
#' @return data.frame: contains expected values, confidence intervals, variable names
#' @export

sim <- function(models, iv, robust=F, ci=c(0.025,0.975), nsim = 1000){
  
  ## prepare output object, convert input to model list
  out <- NULL
  if(class(models)[1] != "list") models <- list(models)
  
  for(i in 1:length(models)){
    ## simulate betas from sampling distribution
    if(robust == T){
      betas <- MASS::mvrnorm(nsim, coef(models[[i]]), sandwich::vcovHC(models[[i]]))
    } else {
      betas <- MASS::mvrnorm(nsim, coef(models[[i]]), vcov(models[[i]]))
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
      ## IDEA: decompose effect of tobit in dP(Y>0) and dY|Y>0
      ## based on predicted values (rather than EVs)
      ## note that betas[,2] is log(Sigma) estimate
      ## CHECK CALCULATIONS!
      if(unique(models[[i]]@misc$Upper)!=Inf) stop("Upper limit not supported")
      if(unique(models[[i]]@misc$Lower)!=0) warning("Limit != 0 not testes yet!")
      loLim <- unique(models[[i]]@misc$Lower)[1,1]
      
      ## expected values for z>0
      evsTemp <- betas[,-2] %*% X[-2,]
      evs <- evsTemp + exp(betas[,2]) * dnorm(evsTemp/exp(betas[,2])) / pnorm(evsTemp/exp(betas[,2]))
      
      ## probability of z>0
      pvs <- array(dim = c(nsim,ncol(X),nsim))
      for(j in 1:nrow(pvs)){
        pvs[j,,] <- matrix(rnorm(nsim*ncol(X), mean = evsTemp[j,], sd = exp(betas[j,2]))
                           , ncol = nsim)
      }
      prob <- apply(pvs, 2, function(x) apply(x, 1, function(x) mean(x>loLim)))
    } else stop("Model type not supported")
    
    skip <- F
    
    if(nrow(iv)==2){
      ## calculate first differences
      evs <- evs[,2] - evs[,1]
      if(class(models[[i]])[1] == "vglm"){
        if(models[[i]]@family@vfamily == "tobit")
          prob <- prob[,2] - prob[,1]
      }
    } else if(nrow(iv)==4) {
      ## calculate difference-in-difference
      evs <- (evs[,2] - evs[,1]) - (evs[,4] - evs[,3])
      if(class(models[[i]])[1] == "vglm"){
        if(models[[i]]@family@vfamily == "tobit")
          prob <- (prob[,2] - prob[,1]) - (prob[,4] - prob[,3])
      }
    } else {
      ## compute predicted values for each step
      warning("Check number of scenarios - STILL TESTING")
      res <- data.frame(mean = apply(evs, 2, mean)
                        , cilo = apply(evs, 2, quantile, ci[1])
                        , cihi = apply(evs, 2, quantile, ci[2])
                        , dv = as.factor(colnames(models[[i]]$model)[1])
                        , iv = as.factor(paste(colnames(iv), collapse = "_")))
      out <- rbind(out, res)
      skip <- T
    }
    
    ## warning for Inf/-Inf in single iterations
    if(Inf %in% evs|-Inf %in% evs){
      warning(paste0("Inf/-Inf in ",length(evs[evs==Inf])+length(evs[evs==-Inf])," evs iteration(s)"))
      evs[evs==Inf|evs==-Inf] <- NA
    }
    
    if(!skip){
      ## generate output table
      if(class(models[[i]])[1] != "vglm"){
        res <- data.frame(mean = mean(evs)
                          , cilo = quantile(evs, ci[1])
                          , cihi = quantile(evs, ci[2])
                          , dv = as.factor(colnames(models[[i]]$model)[1])
                          , iv = as.factor(paste(colnames(iv), collapse = "_")))
      } else {
        res <- data.frame(mean = c(mean(prob, na.rm = T), mean(evs, na.rm = T))
                          , cilo = c(quantile(prob, ci[1], na.rm = T),quantile(evs, ci[1], na.rm = T))
                          , cihi = c(quantile(prob, ci[2], na.rm = T), quantile(evs, ci[2], na.rm = T))
                          , dv = as.factor(sub("(.*) \\~.*", "\\1", models[[i]]@call[2]))
                          , iv = as.factor(paste(colnames(iv), collapse = "_"))
                          , value = factor(c("Probability P(y>0)","Expected Value E(y|y>0)")
                                           , levels = c("Probability P(y>0)","Expected Value E(y|y>0)")))
      }
      out <- rbind(out, res)
    }
  }
  
  ## return output table
  rownames(out) <- NULL
  return(out)
}


### Function to print tables of model estimates

latexTable <- function(x, caption=NULL, label=NULL, align=NULL, digits=3
                       , varlabs=NULL, mlabs=NULL, ...){
  ############################################################################################
  ### Function to print model results in latex table
  ### replaces stargazer for unsupported models
  ## x: model list
  ## caption: see xtable for details
  ## label: see xtable for details
  ## align: see xtable for details
  ## digits: nteger indicating the number of decimal places to be used
  ## varlabs: list of variable names and respective labels (also determines order in table)
  ## mlabs: labels for each model
  ## ...: further arguments passed to print.xtable (e.g. file etc.)
  ############################################################################################
  
  ## save model in list
  if(class(x)!="list") x <- list(x)
  tbl <- data.frame(vars = names(coef(x[[1]])))
  
  for(m in 1:length(x)){
    ## extract varnames, coefs and se
    vars <- names(coef(x[[m]]))
    coefs <- round(coef(x[[m]]), digits)
    se <- paste0("(",round(sqrt(diag(vcov(x[[m]]))), digits),")")
    tmp <- data.frame(vars,coefs,se)
    colnames(tmp) <- c("vars",paste0(c("coefs","se"),m))
    
    ## merge model results
    tbl <- merge(tbl,tmp,by="vars",all=T)
  }
  
  ## arrange variables
  if(!is.null(varlabs)){
    if(nrow(tbl)!=length(varlabs)) stop("Variable lables do not have correct length")
    rownames(tbl) <- as.character(tbl$vars)
    tbl <- tbl[names(varlabs),]
    tbl$vars <- unlist(varlabs)
  }
  
  ## prepare full variable names for output
  out <- data.frame(vars = as.vector(t(cbind(as.character(tbl$vars),""))))
  
  ## coefs and se in single column for each model
  for(m in 1:length(x)){
    out <- cbind(out, as.vector(t(tbl[,grep(m,colnames(tbl))])))
  }
  
  ## model names
  if(!is.null(mlabs)){
    if(length(mlabs)!=(ncol(out)-1)) stop("Model labels do not have correct length")
    colnames(out) <- c("Variable", mlabs)
  } else {
    colnames(out) <- c("Variable", paste0("(",1:length(x),")"))
  }
  
  ## convert table to character
  out <- apply(out, 2, as.character)
  
  ## add number of observations
  if(class(x[[1]])=="vglm"){
    out <- rbind(out, c("N",sapply(x, function(x) length(residuals(x))/2)))
  } else {
    out <- rbind(out, c("N",sapply(x, function(x) length(residuals(x)))))
  }
  
  ## add LogLik or R-Squared
  if(class(x[[1]])=="lm"){
    out <- rbind(out, c("R-squared (adj.)",sapply(x, function(x) 
      round(summary(x)$adj.r.squared, digits))))
  } else {
    out <- rbind(out, c("Log-Likelihood",sapply(x, function(x) 
      round(logLik(x), 0))))
  }
  
  ## adjust align for excluded rownames
  if(!is.null(align)) align <- paste0("l",align)
  
  ## export table
  print(xtable(out, caption=caption, label=label, align=align), include.rownames=F
        , hline.after=c(-1,0,nrow(out)-2,nrow(out)), ...)
}

