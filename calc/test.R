###############################################################################################
## Project:  Measuring Morality in Political Attitude Expression
## File:     test.R
## Overview: test some models for the next iteration
##           based on data prepared in prep_anes.R
## Author:   Patrick Kraft
###############################################################################################

## packages
pkg <- c("tidyverse","gridExtra","stargazer","xtable","VGAM")
invisible(lapply(pkg, library, character.only = TRUE))
rm(list=ls())

## working directory
setwd("~/Dropbox/Uni/Projects/2014/mft/calc")

## load additional functions
source("func.R")

## load recoded dataset
load("out/prep_anes.RData")

## drop spanish respondents and empty responses
anes2012 <- anes2012[anes2012$spanish != 1 & anes2012$wc != 0, ]



### Fig 9: Media content and moral foundations (tobit)

## model estimation
tobit_cont <- list(NULL)
tobit_cont[[1]] <- vglm(harm_s ~ media_harm_s + relig + educ + age + female + black 
                        + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
tobit_cont[[2]] <- vglm(fairness_s ~ media_fairness_s + relig + educ + age + female + black
                        + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
tobit_cont[[3]] <- vglm(ingroup_s ~ media_ingroup_s + relig + educ + age + female + black 
                        + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
tobit_cont[[4]] <- vglm(authority_s ~ media_authority_s + relig + educ + age + female + black 
                        + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)

## new analyses
tobit_cont[[1]] <- vglm(harm_s ~ media_harm_s + media_fairness_s + media_ingroup_s + media_authority_s + relig + educ + age + female + black 
                        + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
tobit_cont[[2]] <- vglm(fairness_s ~ media_harm_s + media_fairness_s + media_ingroup_s + media_authority_s + relig + educ + age + female + black
                        + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
tobit_cont[[3]] <- vglm(ingroup_s ~ media_harm_s + media_fairness_s + media_ingroup_s + media_authority_s + relig + educ + age + female + black 
                        + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
tobit_cont[[4]] <- vglm(authority_s ~ media_harm_s + media_fairness_s + media_ingroup_s + media_authority_s + relig + educ + age + female + black 
                        + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)

lapply(tobit_cont, summary)

## simulate expected values / marginal effects
tobit_cont_res <- rbind(sim(models = tobit_cont[[1]]
                            , iv=data.frame(media_harm_s=range(anes2012$media_harm_s)))
                        , sim(models = tobit_cont[[2]]
                              , iv=data.frame(media_fairness_s=range(anes2012$media_fairness_s)))
                        , sim(models = tobit_cont[[3]]
                              , iv=data.frame(media_ingroup_s=range(anes2012$media_ingroup_s)))
                        , sim(models = tobit_cont[[4]]
                              , iv=data.frame(media_authority_s=range(anes2012$media_authority_s))))
tobit_cont_res$var <- factor(tobit_cont_res$dv)
levels(tobit_cont_res$var) <- rev(mftLabs)

## generate plot
ggplot(tobit_cont_res, aes(x = mean, y = var)) +
  geom_vline(xintercept=0, col="lightgrey") + geom_point() +
  geom_errorbarh(aes(xmax=cilo,xmin=cihi),height=0) +
  ggtitle("Change in Predicted Emphasis on Moral Foundation") +
  labs(y = "Dependent Variable: Moral Foundation"
       , x = "Marginal Effect (MFT Media Content)") +
  theme_classic(base_size = 8) + theme(panel.border = element_rect(fill=NA)) + 
  facet_grid(.~value) + 
  scale_y_discrete(limits = rev(levels(tobit_cont_res$var)))
ggsave("fig/tobit_cont.pdf", width = 5, height = 2.5)


### test binding vs. individualizing foundations

anes2012$bind <- with(anes2012, ingroup_s + authority_s)
anes2012$indi <- with(anes2012, harm_s + fairness_s)
anes2012$bind_media <- with(anes2012, media_ingroup_s + media_authority_s)
anes2012$indi_media <- with(anes2012, media_harm_s + media_fairness_s)

m1 <- vglm(bind ~ bind_media + indi_media + relig + educ + age + female + black 
                        + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
m2 <- vglm(indi ~ bind_media + indi_media + relig + educ + age + female + black
                        + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
summary(m1)
summary(m2)


####################################################
### test backwards compatibility issues w/ quanteda


rbind(anes2012[5,colnames(anes2012)[colnames(anes2012)%in%colnames(anes2012old)]],anes2012old[5,colnames(anes2012old)[colnames(anes2012old)%in%colnames(anes2012)]])

plot(anes2012$harm_s, anes2012old$harm_s)
cor(anes2012$harm_s, anes2012old$harm_s)

cor(anes2012$fairness, anes2012old$fairness)
plot(anes2012$fairness, anes2012old$fairness)

plot(density(residuals(lm(anes2012$fairness ~ anes2012old$fairness))))
plot(density(residuals(lm(anes2012$fairness ~ anes2012old$fairness))^2))

tmp1 <- anes2012[residuals(lm(anes2012$fairness ~ anes2012old$fairness))^2>0.00001,]
tmp2 <- anes2012old[residuals(lm(anes2012$fairness ~ anes2012old$fairness))^2>0.00001,]

View(tmp1)
View(tmp2)

# the issue might be due to numbers in OE responses. Maybe dfm/corpus/quanteda changed how it handeled numbers?
# -> use stable version of quanteda to make results reproducible