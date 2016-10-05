###############################################################################################
## Project:  Moral foundations of Political Reasoning
## File:     media_prelim.R
## Overview: preliminary analyses of media content effects
##           based on data prepared in prep.R
## Author:   Patrick Kraft
###############################################################################################

## packages
pkg <- c("tidyverse","gridExtra","stargazer","xtable","VGAM","pmisc")
invisible(lapply(pkg, library, character.only = TRUE))

## working directory
setwd("~/Dropbox/Uni/Projects/2014/mft/calc")

## load additional functions
source("func.R")

## load recoded dataset
load("out/anes.RData")

## drop spanish respondents and empty responses
anes2012 <- anes2012[anes2012$spanish != 1 & anes2012$wc != 0, ]

plot_df <- media2012 %>% select(id, authority, fairness, harm, ingroup) %>%
  gather(mft, similarity, -id)

ggplot(plot_df, aes(y=reorder(id, similarity), x=similarity)) + geom_point() + facet_wrap(~mft)

ggplot(media2012, aes(y=reorder(id, harm_s), x=harm_s)) + geom_point() + geom_vline(xintercept = 0)
ggplot(media2012, aes(y=reorder(id, fairness_s), x=fairness_s)) + geom_point() + geom_vline(xintercept = 0)
ggplot(media2012, aes(y=reorder(id, ingroup_s), x=ingroup_s)) + geom_point() + geom_vline(xintercept = 0)
ggplot(media2012, aes(y=reorder(id, authority_s), x=authority_s)) + geom_point() + geom_vline(xintercept = 0)



##########################
### Media content analysis

m4_cont <- list(NULL)
m4_cont[[1]] <- vglm(harm_s ~ I(media_harm_d>0) + relig + educ + age + female + black + 
                       lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
m4_cont[[2]] <- vglm(fairness_s ~ I(media_fairness_d>0) + relig + educ + age + female + black + 
                       lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
m4_cont[[3]] <- vglm(ingroup_s ~ I(media_ingroup_d>0) + relig + educ + age + female + black + 
                       lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
m4_cont[[4]] <- vglm(authority_s ~ I(media_authority_d>0) + relig + educ + age + female + black + 
                       lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
lapply(m4_cont, summary)


m4_cont <- list(NULL)
m4_cont[[1]] <- vglm(harm_s ~ I(media_harm_d>0)*polmedia_c + relig + educ + age + female + black + 
                       lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
m4_cont[[2]] <- vglm(fairness_s ~ I(media_fairness_d>0)*polmedia_c + relig + educ + age + female + black + 
                       lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
m4_cont[[3]] <- vglm(ingroup_s ~ I(media_ingroup_d>0)*polmedia_c + relig + educ + age + female + black + 
                       lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
m4_cont[[4]] <- vglm(authority_s ~ I(media_authority_d>0)*polmedia_c + relig + educ + age + female + black + 
                       lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
lapply(m4_cont, summary)

m4_cont <- list(NULL)
m4_cont[[1]] <- vglm(harm_s ~ media_harm_d + relig + educ + age + female + black + 
                       lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
m4_cont[[2]] <- vglm(fairness_s ~ media_fairness_d + relig + educ + age + female + black + 
                       lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
m4_cont[[3]] <- vglm(ingroup_s ~ media_ingroup_d + relig + educ + age + female + black + 
                       lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
m4_cont[[4]] <- vglm(authority_s ~ media_authority_d + relig + educ + age + female + black + 
                       lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
lapply(m4_cont, summary)

m4_cont <- list(NULL)
m4_cont[[1]] <- vglm(harm_s ~ media_harm + relig + educ + age + female + black + 
                       lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
m4_cont[[2]] <- vglm(fairness_s ~ media_fairness + relig + educ + age + female + black + 
                       lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
m4_cont[[3]] <- vglm(ingroup_s ~ media_ingroup + relig + educ + age + female + black + 
                       lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
m4_cont[[4]] <- vglm(authority_s ~ media_authority + relig + educ + age + female + black + 
                       lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
lapply(m4_cont, summary)

m4_cont <- list(NULL)
m4_cont[[1]] <- vglm(harm_s ~ media_harm_s + relig + educ + age + female + black + 
                       lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
m4_cont[[2]] <- vglm(fairness_s ~ media_fairness_s + relig + educ + age + female + black + 
                       lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
m4_cont[[3]] <- vglm(ingroup_s ~ media_ingroup_s + relig + educ + age + female + black + 
                       lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
m4_cont[[4]] <- vglm(authority_s ~ media_authority_s + relig + educ + age + female + black + 
                       lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
lapply(m4_cont, summary)

m4_cont <- list(NULL)
m4_cont[[1]] <- vglm(harm_s ~ polmedia_c + relig + educ + age + female + black + 
                       lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
m4_cont[[2]] <- vglm(fairness_s ~ polmedia_c + relig + educ + age + female + black + 
                       lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
m4_cont[[3]] <- vglm(ingroup_s ~ polmedia_c + relig + educ + age + female + black + 
                       lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
m4_cont[[4]] <- vglm(authority_s ~ polmedia_c + relig + educ + age + female + black + 
                       lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
lapply(m4_cont, summary)

m4_cont <- list(NULL)
m4_cont[[1]] <- vglm(harm_s ~ media_harm*polmedia_c + relig + educ + age + female + black + 
                       lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
m4_cont[[2]] <- vglm(fairness_s ~ media_fairness_s*polmedia_c + relig + educ + age + female + black + 
                       lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
m4_cont[[3]] <- vglm(ingroup_s ~ media_ingroup*polmedia_c + relig + educ + age + female + black + 
                       lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
m4_cont[[4]] <- vglm(authority_s ~ media_authority*polmedia_c + relig + educ + age + female + black + 
                       lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
lapply(m4_cont, summary)

## check this one
m4_cont <- list(NULL)
m4_cont[[1]] <- vglm(harm_s ~ media_harm_s*polmedia_c + relig + educ + age + female + black + 
                       lwc + wordsum + mode, tobit(Lower = 0), data=anes2012[anes2012$media,])
m4_cont[[2]] <- vglm(fairness_s ~ media_fairness_s*polmedia_c + relig + educ + age + female + black + 
                       lwc + wordsum + mode, tobit(Lower = 0), data=anes2012[anes2012$media,])
m4_cont[[3]] <- vglm(ingroup_s ~ media_ingroup_s*polmedia_c + relig + educ + age + female + black + 
                       lwc + wordsum + mode, tobit(Lower = 0), data=anes2012[anes2012$media,])
m4_cont[[4]] <- vglm(authority_s ~ media_authority_s*polmedia_c + relig + educ + age + female + black + 
                       lwc + wordsum + mode, tobit(Lower = 0), data=anes2012[anes2012$media,])
lapply(m4_cont, summary)

m4_cont <- list(NULL)
m4_cont[[1]] <- vglm(harm_s ~ media_harm_s + polmedia_c + relig + educ + age + female + black + 
                       lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
m4_cont[[2]] <- vglm(fairness_s ~ media_fairness_s + polmedia_c + relig + educ + age + female + black + 
                       lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
m4_cont[[3]] <- vglm(ingroup_s ~ media_ingroup_s + polmedia_c + relig + educ + age + female + black + 
                       lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
m4_cont[[4]] <- vglm(authority_s ~ media_authority_s + polmedia_c + relig + educ + age + female + black + 
                       lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
lapply(m4_cont, summary)

m4_cont <- list(NULL)
m4_cont[[1]] <- vglm(harm_s ~ media_harm + relig + educ + age + female + black + 
                       lwc + wordsum + mode, tobit(Lower = 0), data=anes2012[anes2012$media,])
m4_cont[[2]] <- vglm(fairness_s ~ media_fairness + relig + educ + age + female + black + 
                       lwc + wordsum + mode, tobit(Lower = 0), data=anes2012[anes2012$media,])
m4_cont[[3]] <- vglm(ingroup_s ~ media_ingroup + relig + educ + age + female + black + 
                       lwc + wordsum + mode, tobit(Lower = 0), data=anes2012[anes2012$media,])
m4_cont[[4]] <- vglm(authority_s ~ media_authority + relig + educ + age + female + black + 
                       lwc + wordsum + mode, tobit(Lower = 0), data=anes2012[anes2012$media,])
lapply(m4_cont, summary)

m4_cont <- list(NULL)
m4_cont[[1]] <- vglm(harm_s ~ media_harm*polmedia_c + relig + educ + age + female + black + 
                       lwc + wordsum + mode, tobit(Lower = 0), data=anes2012[anes2012$media==T,])
m4_cont[[2]] <- vglm(fairness_s ~ media_fairness*polmedia_c + relig + educ + age + female + black + 
                       lwc + wordsum + mode, tobit(Lower = 0), data=anes2012[anes2012$media==T,])
m4_cont[[3]] <- vglm(ingroup_s ~ media_ingroup*polmedia_c + relig + educ + age + female + black + 
                       lwc + wordsum + mode, tobit(Lower = 0), data=anes2012[anes2012$media==T,])
m4_cont[[4]] <- vglm(authority_s ~ media_authority*polmedia_c + relig + educ + age + female + black + 
                       lwc + wordsum + mode, tobit(Lower = 0), data=anes2012[anes2012$media==T,])
lapply(m4_cont, summary)

m4_cont <- list(NULL)
m4_cont[[1]] <- vglm(harm_s ~ polmedia_c + relig + educ + age + female + black + 
                       lwc + wordsum + mode, tobit(Lower = 0), data=anes2012[anes2012$media==T,])
m4_cont[[2]] <- vglm(fairness_s ~ polmedia_c + relig + educ + age + female + black + 
                       lwc + wordsum + mode, tobit(Lower = 0), data=anes2012[anes2012$media==T,])
m4_cont[[3]] <- vglm(ingroup_s ~ polmedia_c + relig + educ + age + female + black + 
                       lwc + wordsum + mode, tobit(Lower = 0), data=anes2012[anes2012$media==T,])
m4_cont[[4]] <- vglm(authority_s ~ polmedia_c + relig + educ + age + female + black + 
                       lwc + wordsum + mode, tobit(Lower = 0), data=anes2012[anes2012$media==T,])
lapply(m4_cont, summary)

m4_cont <- list(NULL)
m4_cont[[1]] <- vglm(harm_s ~ media_harm + relig + educ + age + female + black + 
                       lwc + wordsum + mode, tobit(Lower = 0), data=anes2012[anes2012$media,])
m4_cont[[2]] <- vglm(harm_s ~ media_harm + media_fairness + media_ingroup + media_authority +
                       relig + educ + age + female + black + 
                       lwc + wordsum + mode, tobit(Lower = 0), data=anes2012[anes2012$media,])
m4_cont[[3]] <- vglm(fairness_s ~ media_fairness + relig + educ + age + female + black + 
                       lwc + wordsum + mode, tobit(Lower = 0), data=anes2012[anes2012$media,])
m4_cont[[4]] <- vglm(fairness_s ~ media_harm + media_fairness + media_ingroup + media_authority +
                       relig + educ + age + female + black + 
                       lwc + wordsum + mode, tobit(Lower = 0), data=anes2012[anes2012$media,])
m4_cont[[5]] <- vglm(ingroup_s ~ media_ingroup + relig + educ + age + female + black + 
                       lwc + wordsum + mode, tobit(Lower = 0), data=anes2012[anes2012$media,])
m4_cont[[6]] <- vglm(ingroup_s ~ media_harm + media_fairness + media_ingroup + media_authority +
                       relig + educ + age + female + black + 
                       lwc + wordsum + mode, tobit(Lower = 0), data=anes2012[anes2012$media,])
m4_cont[[7]] <- vglm(authority_s ~ media_authority + relig + educ + age + female + black + 
                       lwc + wordsum + mode, tobit(Lower = 0), data=anes2012[anes2012$media,])
m4_cont[[8]] <- vglm(authority_s ~ media_harm + media_fairness + media_ingroup + media_authority +
                       relig + educ + age + female + black + 
                       lwc + wordsum + mode, tobit(Lower = 0), data=anes2012[anes2012$media,])
lapply(m4_cont, summary)

m4_cont <- list(NULL)
m4_cont[[1]] <- vglm(harm_s ~ media_harm*polmedia + relig + educ + age + female + black + 
                       lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
m4_cont[[2]] <- vglm(harm_s ~ media_harm*polmedia + media_fairness*polmedia + media_ingroup*polmedia + media_authority*polmedia +
                       relig + educ + age + female + black + 
                       lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
m4_cont[[3]] <- vglm(fairness_s ~ media_fairness*polmedia + relig + educ + age + female + black + 
                       lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
m4_cont[[4]] <- vglm(fairness_s ~ media_harm*polmedia + media_fairness*polmedia + media_ingroup*polmedia + media_authority*polmedia +
                       relig + educ + age + female + black + 
                       lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
m4_cont[[5]] <- vglm(ingroup_s ~ media_ingroup*polmedia + relig + educ + age + female + black + 
                       lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
m4_cont[[6]] <- vglm(ingroup_s ~ media_harm*polmedia + media_fairness*polmedia + media_ingroup*polmedia + media_authority*polmedia +
                       relig + educ + age + female + black + 
                       lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
m4_cont[[7]] <- vglm(authority_s ~ media_authority*polmedia + relig + educ + age + female + black + 
                       lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
m4_cont[[8]] <- vglm(authority_s ~ media_harm*polmedia + media_fairness*polmedia + media_ingroup*polmedia + media_authority*polmedia +
                       relig + educ + age + female + black + 
                       lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
lapply(m4_cont, summary)

m4_cont <- list(NULL)
m4_cont[[1]] <- glm(harm_d ~ media_harm + relig + educ + age + female + black + 
                      lwc + wordsum + mode, family = binomial(link = "logit"), data=anes2012)
m4_cont[[2]] <- glm(harm_d ~ media_harm + media_fairness + media_ingroup + media_authority +
                      relig + educ + age + female + black + 
                      lwc + wordsum + mode, family = binomial(link = "logit"), data=anes2012)
m4_cont[[3]] <- glm(fairness_d ~ media_fairness + relig + educ + age + female + black + 
                      lwc + wordsum + mode, family = binomial(link = "logit"), data=anes2012)
m4_cont[[4]] <- glm(fairness_d ~ media_harm + media_fairness + media_ingroup + media_authority +
                      relig + educ + age + female + black + 
                      lwc + wordsum + mode, family = binomial(link = "logit"), data=anes2012)
m4_cont[[5]] <- glm(ingroup_d ~ media_ingroup + relig + educ + age + female + black + 
                      lwc + wordsum + mode, family = binomial(link = "logit"), data=anes2012)
m4_cont[[6]] <- glm(ingroup_d ~ media_harm + media_fairness + media_ingroup + media_authority +
                      relig + educ + age + female + black + 
                      lwc + wordsum + mode, family = binomial(link = "logit"), data=anes2012)
m4_cont[[7]] <- glm(authority_d ~ media_authority + relig + educ + age + female + black + 
                      lwc + wordsum + mode, family = binomial(link = "logit"), data=anes2012)
m4_cont[[8]] <- glm(authority_d ~ media_harm + media_fairness + media_ingroup + media_authority +
                      relig + educ + age + female + black + 
                      lwc + wordsum + mode, family = binomial(link = "logit"), data=anes2012)
lapply(m4_cont, summary)

m4_cont <- list(NULL)
m4_cont[[1]] <- vglm(harm_s ~ media_harm_s + relig + educ + age + female + black + 
                       lwc + wordsum + mode, tobit(Lower = 0), data=anes2012[anes2012$media==T,])
m4_cont[[2]] <- vglm(harm_s ~ media_harm_s + media_fairness_s + media_ingroup_s + media_authority_s +
                       relig + educ + age + female + black + 
                       lwc + wordsum + mode, tobit(Lower = 0), data=anes2012[anes2012$media==T,])
m4_cont[[3]] <- vglm(fairness_s ~ media_fairness_s + relig + educ + age + female + black + 
                       lwc + wordsum + mode, tobit(Lower = 0), data=anes2012[anes2012$media==T,])
m4_cont[[4]] <- vglm(fairness_s ~ media_harm_s + media_fairness_s + media_ingroup_s + media_authority_s +
                       relig + educ + age + female + black + 
                       lwc + wordsum + mode, tobit(Lower = 0), data=anes2012[anes2012$media==T,])
m4_cont[[5]] <- vglm(ingroup_s ~ media_ingroup_s + relig + educ + age + female + black + 
                       lwc + wordsum + mode, tobit(Lower = 0), data=anes2012[anes2012$media==T,])
m4_cont[[6]] <- vglm(ingroup_s ~ media_harm_s + media_fairness_s + media_ingroup_s + media_authority_s +
                       relig + educ + age + female + black + 
                       lwc + wordsum + mode, tobit(Lower = 0), data=anes2012[anes2012$media==T,])
m4_cont[[7]] <- vglm(authority_s ~ media_authority_s + relig + educ + age + female + black + 
                       lwc + wordsum + mode, tobit(Lower = 0), data=anes2012[anes2012$media==T,])
m4_cont[[8]] <- vglm(authority_s ~ media_harm_s + media_fairness_s + media_ingroup_s + media_authority_s +
                       relig + educ + age + female + black + 
                       lwc + wordsum + mode, tobit(Lower = 0), data=anes2012[anes2012$media==T,])
lapply(m4_cont, summary)

m4_cont <- list(NULL)
m4_cont[[1]] <- vglm(harm_s ~ media_harm*polmedia + relig + educ + age + female + black + 
                       lwc + wordsum + mode, tobit(Lower = 0), data=anes2012[anes2012$media==T,])
m4_cont[[2]] <- vglm(harm_s ~ media_harm*polmedia + media_fairness*polmedia + media_ingroup*polmedia + media_authority*polmedia +
                       relig + educ + age + female + black + 
                       lwc + wordsum + mode, tobit(Lower = 0), data=anes2012[anes2012$media==T,])
m4_cont[[3]] <- vglm(fairness_s ~ media_fairness*polmedia + relig + educ + age + female + black + 
                       lwc + wordsum + mode, tobit(Lower = 0), data=anes2012[anes2012$media==T,])
m4_cont[[4]] <- vglm(fairness_s ~ media_harm*polmedia + media_fairness*polmedia + media_ingroup*polmedia + media_authority*polmedia +
                       relig + educ + age + female + black + 
                       lwc + wordsum + mode, tobit(Lower = 0), data=anes2012[anes2012$media==T,])
m4_cont[[5]] <- vglm(ingroup_s ~ media_ingroup*polmedia + relig + educ + age + female + black + 
                       lwc + wordsum + mode, tobit(Lower = 0), data=anes2012[anes2012$media==T,])
m4_cont[[6]] <- vglm(ingroup_s ~ media_harm*polmedia + media_fairness*polmedia + media_ingroup*polmedia + media_authority*polmedia +
                       relig + educ + age + female + black + 
                       lwc + wordsum + mode, tobit(Lower = 0), data=anes2012[anes2012$media==T,])
m4_cont[[7]] <- vglm(authority_s ~ media_authority*polmedia + relig + educ + age + female + black + 
                       lwc + wordsum + mode, tobit(Lower = 0), data=anes2012[anes2012$media==T,])
m4_cont[[8]] <- vglm(authority_s ~ media_harm*polmedia + media_fairness*polmedia + media_ingroup*polmedia + media_authority*polmedia +
                       relig + educ + age + female + black + 
                       lwc + wordsum + mode, tobit(Lower = 0), data=anes2012[anes2012$media==T,])
lapply(m4_cont, summary)

summary(vglm(fairness_s ~ media_fairness + relig + educ + age + female + black + 
               lwc + wordsum + mode, tobit(Lower = 0), data=anes2012))
summary(vglm(fairness_s ~ media_harm + media_fairness + media_ingroup + media_authority
             , tobit(Lower = 0), data=anes2012))
summary(vglm(fairness ~ media_harm + media_fairness + media_ingroup + media_authority
             , tobit(Lower = 0), data=anes2012[anes2012$media==T,]))
summary(vglm(harm ~ media_harm*polmedia + media_fairness*polmedia + media_ingroup*polmedia + media_authority*polmedia
             , tobit(Lower = 0), data=anes2012[anes2012$media==T,]))
