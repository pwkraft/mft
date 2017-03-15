###############################################################################################
## Project:  Moral foundations of Political Reasoning
## File:     appendix_anes.R
## Overview: analyses for appendix (anes data), produces all additional plots and tables
##           based on data prepared in prep_anes.R and models estimated in analyses_anes.R
## Author:   Patrick Kraft
###############################################################################################

## packages
pkg <- c("tidyverse","gridExtra","stargazer","xtable","VGAM")
invisible(lapply(pkg, library, character.only = TRUE))
rm(list=ls())

## working directory
setwd("/data/Dropbox/Uni/Projects/2014/mft/calc")

## load additional functions
source("func.R")

## load recoded dataset
load("out/prep_anes.RData")


######################################
### Additional Descriptive information


### Fig 1: Moral foundations in open-ended responses

## prepare data for plotting
plot_df <- anes2012 %>% select(purity_d, authority_d, ingroup_d, fairness_d, harm_d) %>%
  apply(2,function(x) c(mean(x, na.rm=T),sd(x, na.rm=T)/sqrt(sum(!is.na(x))))) %>%
  t() %>% data.frame() %>% mutate(var = rownames(.), varnum = as.factor(1:5))

## generate plot
ggplot(plot_df, aes(x=X1, xmin=X1-1.96*X2, xmax=X1+1.96*X2, y=varnum)) +
  geom_point() + geom_errorbarh(height=0) + xlim(0,.5) +
  labs(y = "Moral Foundation", x = "Proportion of Respondents") +
  ggtitle("Moral Reasoning in Open-Ended Responses") + 
  theme_classic(base_size = 8) + theme(panel.border = element_rect(fill=NA)) + 
  scale_y_discrete(labels=c("Sanctity", mftLabs))
ggsave(file = "fig/prop_mft.pdf", width = 5, height = 3)


### Tab B.1: Missing open-ended responses

## prepare table
tab_mis <- rbind(c(table(anes2012$spanish)[2]
                   , table(anes2012$spanish)[2]*100/sum(table(anes2012$spanish)))
                 , c(table(anes2012$wc==0)[2]
                     , table(anes2012$wc==0)[2]*100/sum(table(anes2012$wc==0))))
colnames(tab_mis) <- c("N","Percent")
rownames(tab_mis) <- c("Spanish Interview", "No Responses")

## export table
print(xtable(tab_mis, align="lcc",digits=c(0,0,2)
             , caption = "Missing open-ended responses"
             , label="tab:app_mis")
      , table.placement="ht", caption.placement="top"
      , file="tab/app_mis.tex")


### Fig B.1: Individual open-ended response lengths (for wc>0!)

## drop spanish respondents and empty responses
anes2012 <- anes2012[anes2012$spanish != 1 & anes2012$wc != 0,]

## histogram/density of wc
wc_mean = mean(anes2012$wc)
p1 <- ggplot(anes2012, aes(wc)) + 
  geom_histogram(fill = "grey", binwidth = 25) + 
  theme_classic(base_size = 8) + 
  theme(panel.border = element_rect(fill=NA)) + 
  geom_vline(xintercept = wc_mean, linetype = 3) +
  ylab("Number of Respondents") + xlab("Word Count")

## histogram/density of lwc
lwc_mean = mean(anes2012$lwc)
p2 <- ggplot(anes2012, aes(lwc, ..density..)) + 
  geom_histogram(binwidth = 0.2, fill='grey') + geom_density() + 
  theme_classic(base_size = 8) + theme(panel.border = element_rect(fill=NA)) + 
  geom_vline(xintercept = lwc_mean, linetype = 3) + 
  ylab("Density") + xlab("log(Word Count)")

## combine plots
pdf("fig/app_wc.pdf",width=7, height=3)
grid.arrange(p1, p2, ncol=2)
dev.off()


### Fig B.2: Histograms of variables included in the analyses

desc <- list(NULL)
plot_default <- theme_classic(base_size = 8) + theme(panel.border = element_rect(fill=NA))
desc[[1]] <- ggplot(anes2012, aes(x=ideol)) + geom_bar(stat="count") + 
  labs(y="Count", x="Ideology") + plot_default
desc[[2]] <- ggplot(anes2012, aes(x=polknow)) + geom_bar(stat="count") + 
  labs(y="Count", x="Political Knowledge") + plot_default
desc[[3]] <- ggplot(anes2012, aes(x=polmedia)) + geom_bar(stat="count") + 
  labs(y="Count", x="Political Media Exposure") + plot_default
desc[[4]] <- ggplot(anes2012, aes(x=poldisc)) + geom_bar(stat="count") + 
  labs(y="Count", x="Political Discussions") + plot_default
desc[[5]] <- ggplot(anes2012, aes(x=eval_cand)) + geom_histogram(binwidth = 20) + 
  labs(y="Count", x="Feeling Thermometer (Candidates)") + plot_default
desc[[6]] <- ggplot(anes2012, aes(x=eval_party)) + geom_histogram(binwidth = 20) + 
  labs(y="Count", x="Feeling Thermometer (Parties)") + plot_default
desc[[7]] <- ggplot(anes2012, aes(x=factor(vote, labels=c("No","Yes")))) + 
  geom_bar(stat="count") + labs(y="Count", x="Voted in 2012") + plot_default
desc[[8]] <- ggplot(anes2012, aes(x=factor(vote_dem, labels=c("No","Yes")))) + 
  geom_bar(stat="count") + labs(y="Count", x="Voted for Democratic Candidate") + plot_default
desc[[9]] <- ggplot(anes2012, aes(x=factor(pastvote, labels=c("No","Yes")))) + 
  geom_bar(stat="count") + labs(y="Count", x="Voted in 2008") + plot_default
desc[[10]] <- ggplot(anes2012, aes(x=factor(protest, labels=c("No","Yes")))) + 
  geom_bar(stat="count") + labs(y="Count", x="Participated in Protest") + plot_default
desc[[11]] <- ggplot(anes2012, aes(x=factor(letter, labels=c("No","Yes")))) + 
  geom_bar(stat="count") + labs(y="Count", x="Letter to Congressmen/Senator") + plot_default
desc[[12]] <- ggplot(anes2012, aes(x=factor(petition, labels=c("No","Yes")))) + 
  geom_bar(stat="count") + labs(y="Count", x="Signed Petition") + plot_default
desc[[13]] <- ggplot(anes2012, aes(x=factor(button, labels=c("No","Yes")))) + 
  geom_bar(stat="count") + labs(y="Count", x="Wearing Campaign Button") + plot_default
desc[[14]] <- ggplot(anes2012, aes(x=age)) + geom_bar(stat="count") + 
  labs(y="Count", x="Age") + plot_default
desc[[15]] <- ggplot(anes2012, aes(x=factor(female,labels=c("Male","Female")))) + 
  geom_bar(stat="count") + labs(y="Count", x="Sex") + plot_default
desc[[16]] <- ggplot(anes2012, aes(x=factor(black,labels=c("Other","Black non-Hispanic")))) + 
  geom_bar(stat="count") + labs(y="Count", x="Race/Ethnicity") + plot_default
desc[[17]] <- ggplot(anes2012, aes(x=relig)) + geom_bar(stat="count") + 
  labs(y="Count", x="Church Attendance") + plot_default
desc[[18]] <- ggplot(anes2012, aes(x=factor(educ, labels=c("No College","College")))) + 
  geom_bar(stat="count") + labs(y="Count", x="Education") + plot_default
desc[[19]] <- ggplot(anes2012, aes(x=pid)) + geom_bar(stat="count") + 
  labs(y="Count", x="Party Identification") + plot_default
desc[[20]] <- ggplot(anes2012, aes(x=factor(mode, labels=c("Face-to-Face","Online")))) + 
  geom_bar(stat="count") + labs(y="Count", x="Survey Mode") + plot_default
desc[[21]] <- ggplot(anes2012, aes(x=wordsum)) + geom_bar(stat="count") + 
  labs(y="Count", x="Wordsum Literacy Test") + plot_default
pdf("fig/app_desc.pdf", width=7, height=9)
grid.arrange(grobs=desc,ncol=3)
dev.off()


### Fig B.3: MFT scores for media content

## prepare data for plot
plot_df <- cbind(gather(select(media2012, id, authority_s:ingroup_s), mft, score, -id)
                 , gather(select(media2012, id, authority_lo, fairness_lo, harm_lo, ingroup_lo)
                          , mft_lo, score_lo, -id)[,-1]
                 , gather(select(media2012, id, authority_hi, fairness_hi, harm_hi, ingroup_hi)
                          , mft_hi, score_hi, -id)[,-1]) %>%
  mutate(mft = factor(mft, levels = rev(c("authority_s","ingroup_s","fairness_s","harm_s"))
                      , labels = rev(mftLabs)))

## generate plot
ggplot(plot_df, aes(y=reorder(id, score), x=score,xmin=score_lo,xmax=score_hi)) + 
  geom_point() + geom_errorbarh(height=0) + theme_classic(base_size = 8) + 
  theme(panel.border = element_rect(fill=NA)) +
  facet_grid(.~mft) + ggtitle("Moral Content of Media Sources (October 2012)") +
  xlab("MFT Score (rescaled)") + ylab("News Source") +
  geom_vline(xintercept=0, col="lightgrey")
ggsave("fig/media_desc.pdf",width = 7, height = 4)

## generate plot
ggplot(media2012, aes(y=reorder(id, general_s), x=general_s,xmin=general_lo,xmax=general_hi)) + 
  geom_point() + geom_errorbarh(height=0) + theme_classic(base_size = 8) + 
  theme(panel.border = element_rect(fill=NA)) +
  ggtitle("Moralization in Media Sources\n(October 2012)") +
  xlab("General MFT Score (rescaled)") + ylab("News Source")
ggsave("fig/media_desc.pdf",width = 4, height = 4)


##################################################
### Additional Model Results and Robustness Checks


## load model results
load("out/analyses_anes.RData")


### Fig 3: Moral foundations and feeling thermometer differentials (ols)

## model estimation
ols_feel <- NULL
ols_feel[[1]] <- lm(eval_party ~ harm_s + fairness_s + ingroup_s + authority_s
                    + relig + educ + age + female + black + lwc + wordsum + mode
                    , data=anes2012)
ols_feel[[2]] <- lm(eval_party ~ harm_s + fairness_s + ingroup_s + authority_s
                    + pid_dem + pid_rep + relig + educ + age + female + black
                    + lwc + wordsum + mode, data=anes2012)
ols_feel[[3]] <- lm(eval_cand ~ harm_s + fairness_s + ingroup_s + authority_s
                    + relig + educ + age + female + black + lwc + wordsum + mode
                    , data=anes2012)
ols_feel[[4]] <- lm(eval_cand ~ harm_s + fairness_s + ingroup_s + authority_s
                    + pid_dem + pid_rep + relig + educ + age + female + black
                    + lwc + wordsum + mode, data=anes2012)

## simulate expected values / marginal effects
ols_feel_res <- rbind(sim(ols_feel, iv=data.frame(harm_s = min(anes2012$harm_s)+c(0,1))
                          , robust=T)
                      , sim(ols_feel, iv=data.frame(fairness_s = min(anes2012$fairness_s)+c(0,1))
                            , robust=T)
                      , sim(ols_feel, iv=data.frame(ingroup_s = min(anes2012$ingroup_s)+c(0,1))
                            , robust=T)
                      , sim(ols_feel, iv=data.frame(authority_s = min(anes2012$authority_s)+c(0,1))
                            , robust=T))
ols_feel_res$cond <- rep(c("No","Yes"),8)
ols_feel_res$var <- rep(4:1,each=4)
ols_feel_res$year <- "2012"
levels(ols_feel_res$dv) <- c("Party Evaluation", "Candidate Evaluation")

## generate plot
ggplot(ols_feel_res, aes(x = mean, y = var+.1-.2*(cond=="Yes"), col=cond, shape=cond)) +
  geom_vline(xintercept=0, col="lightgrey") + geom_point() +
  geom_errorbarh(aes(xmax=cihi,xmin=cilo),height=0) +
  labs(y = "Independent Variable: Moral Foundation"
       , x= "Change in Feeling Thermometer (Democrat - Republican)") +
  theme_classic(base_size = 8) + theme(panel.border = element_rect(fill=NA)) + 
  ggtitle("Change in Feeling Thermometer Differentials") +
  guides(col=guide_legend(title="Control for Party Identification")
         , shape=guide_legend(title="Control for Party Identification")) +
  theme(legend.position="bottom", legend.box="horizontal") +
  scale_y_continuous(breaks=1:4, labels=mftLabs) + facet_wrap(~dv) +
  scale_color_grey(start=0,end=.5)
ggsave(filename = "fig/ols_feel.pdf", width = 5, height = 3)



### Fig C.1: Participation and general moral reasoning (tobit)

## model estimation
tobit_part <- list(NULL)
tobit_part[[1]] <- vglm(general_s ~ pastvote + relig + educ + age + female + black
                        + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
tobit_part[[2]] <- vglm(general_s ~ protest + relig + educ + age + female + black 
                        + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
tobit_part[[3]] <- vglm(general_s ~ petition + relig + educ + age + female + black 
                        + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
tobit_part[[4]] <- vglm(general_s ~ button + relig + educ + age + female + black 
                        + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
tobit_part[[5]] <- vglm(general_s ~ letter + relig + educ + age + female + black 
                        + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
tobit_part[[6]] <- vglm(general_s ~ pastvote + protest + petition + button + letter
                        + relig + educ + age + female + black + lwc + wordsum + mode
                        , tobit(Lower = 0), data=anes2012)

## simulate expected values / marginal effects
tobit_part_res <- rbind(sim(tobit_part[[1]], iv=data.frame(pastvote=range(anes2012$pastvote,na.rm=T)))
                        , sim(tobit_part[[2]], iv=data.frame(protest=range(anes2012$protest,na.rm=T)))
                        , sim(tobit_part[[3]], iv=data.frame(petition=range(anes2012$petition,na.rm=T)))
                        , sim(tobit_part[[4]], iv=data.frame(button=range(anes2012$button,na.rm=T)))
                        , sim(tobit_part[[5]], iv=data.frame(letter=range(anes2012$letter,na.rm=T)))
                        , sim(tobit_part[[6]], iv=data.frame(pastvote=range(anes2012$pastvote,na.rm=T)))
                        , sim(tobit_part[[6]], iv=data.frame(protest=range(anes2012$protest,na.rm=T)))
                        , sim(tobit_part[[6]], iv=data.frame(petition=range(anes2012$petition,na.rm=T)))
                        , sim(tobit_part[[6]], iv=data.frame(button=range(anes2012$button,na.rm=T)))
                        , sim(tobit_part[[6]], iv=data.frame(letter=range(anes2012$letter,na.rm=T))))
tobit_part_res$cond <- rep(c("No", "Yes"), each=10)
tobit_part_res$var <- rep(c(5:1,5:1),each=2)
tobit_part_res$year <- "2012"

## generate plot
ggplot(tobit_part_res, aes(x = mean, y = var+.1-.2*(cond=="Yes"), col=cond, shape=cond)) +
  geom_vline(xintercept=0, col="lightgrey") + geom_point() +
  geom_errorbarh(aes(xmax=cihi,xmin=cilo),height=0) +
  labs(y = "Independent Variable", x= "Marginal Effect") +
  theme_classic(base_size = 8) + theme(panel.border = element_rect(fill=NA)) + 
  scale_y_continuous(breaks=5:1, labels=c("Voted in 2008", "Protest", "Petition", "Button", "Letter")) +
  ggtitle("Change in Predicted Emphasis on any Moral Foundation") +
  guides(col=guide_legend(title="Control for remaining variables")
         , shape=guide_legend(title="Control for remaining variables")) +
  theme(legend.position="bottom", legend.box="horizontal") +
  scale_color_grey(start=0,end=.5) + facet_grid(~value)
ggsave(filename = "fig/tobit_part.pdf", width = 5, height = 3)



### Fig 5: Knoledge/media/discussion and general moral reasoning (tobit)

## model estimation
tobit_learn <- list(NULL)
tobit_learn[[1]] <- vglm(general_s ~ polknow + relig + educ + age + female + black
                         + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
tobit_learn[[2]] <- vglm(general_s ~ polmedia + relig + educ + age + female + black
                         + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
tobit_learn[[3]] <- vglm(general_s ~ poldisc + relig + educ + age + female + black 
                         + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
tobit_learn[[4]] <- vglm(general_s ~ polknow + polmedia + poldisc
                         + relig + educ + age + female + black + lwc + wordsum + mode
                         , tobit(Lower = 0), data=anes2012)

## simulate expected values / marginal effects
tobit_learn_res <- rbind(sim(tobit_learn[[1]]
                             , iv=data.frame(polknow=range(anes2012$polknow, na.rm = T)))
                         , sim(tobit_learn[[2]]
                               , iv=data.frame(polmedia=range(anes2012$polmedia, na.rm = T)))
                         , sim(tobit_learn[[3]]
                               , iv=data.frame(poldisc=range(anes2012$poldisc, na.rm = T)))
                         , sim(tobit_learn[[4]]
                               , iv=data.frame(polknow=range(anes2012$polknow, na.rm = T)))
                         , sim(tobit_learn[[4]]
                               , iv=data.frame(polmedia=range(anes2012$polmedia, na.rm = T)))
                         , sim(tobit_learn[[4]]
                               , iv=data.frame(poldisc=range(anes2012$poldisc, na.rm = T))))
tobit_learn_res$cond <- rep(c("No", "Yes"), each=6)
tobit_learn_res$var <- rep(c(3:1,3:1),each=2)
tobit_learn_res$year <- "2012"

## generate plot
ggplot(tobit_learn_res, aes(x = mean, y = var+.1-.2*(cond=="Yes"), col=cond, shape=cond)) +
  geom_vline(xintercept=0, col="lightgrey") + geom_point() +
  geom_errorbarh(aes(xmax=cihi,xmin=cilo),height=0) +
  labs(y = "Independent Variable", x= "Marginal Effect") +
  theme_classic(base_size = 8) + theme(panel.border = element_rect(fill=NA)) + 
  scale_y_continuous(breaks=3:1, labels=polLabs) +
  ggtitle("Change in Predicted Emphasis on any Moral Foundation") +
  guides(col=guide_legend(title="Control for remaining variables")
         , shape=guide_legend(title="Control for remaining variables")) +
  theme(legend.position="bottom", legend.box="horizontal") +
  scale_color_grey(start=0,end=.5) + facet_grid(~value)
ggsave(filename = "fig/tobit_learn.pdf", width = 5, height = 3)



### Fig 6: Knowledge and ideological differences in moral foundations (tobit)

## model estimation
tobit_ideol_know <- list(NULL)
tobit_ideol_know[[1]] <- vglm(harm_s ~ ideol*polknow_c + relig + educ + age + female + black 
                              + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
tobit_ideol_know[[2]] <- vglm(fairness_s ~ ideol*polknow_c + relig + educ + age + female + black 
                              + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
tobit_ideol_know[[3]] <- vglm(ingroup_s ~ ideol*polknow_c + relig + educ + age + female + black 
                              + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
tobit_ideol_know[[4]] <- vglm(authority_s ~ ideol*polknow_c + relig + educ + age + female + black 
                              + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)

## simulate expected values / marginal effects
tobit_ideol_res <- rbind(sim(models = tobit_ideol_know
                             , iv=data.frame(polknow_c=min(anes2012$polknow_c, na.rm = T)
                                             , ideolModerate = c(0,0)
                                             , ideolConservative = c(1,0)))
                         , sim(models = tobit_ideol_know
                               , iv=data.frame(polknow_c=max(anes2012$polknow_c, na.rm = T)
                                               , ideolModerate = c(0,0)
                                               , ideolConservative = c(1,0))))
tobit_ideol_res$var <- factor(tobit_ideol_res$dv)
levels(tobit_ideol_res$dv) <- rev(mftLabs)
tobit_ideol_res$cond <- rep(c("Low Knowledge","High Knowledge"),each=8)

## generate plot
ggplot(tobit_ideol_res, aes(x = mean, y = dv)) +
  geom_vline(xintercept=0, col="lightgrey") + geom_point() +
  geom_errorbarh(aes(xmax=cilo,xmin=cihi),height=0) +
  ggtitle("Change in Predicted Emphasis on Moral Foundation") +
  labs(y = "Dependent Variable: Moral Foundation"
       , x = "Marginal Effect (Liberal - Conservative)") +
  theme_classic(base_size = 8) + theme(panel.border = element_rect(fill=NA)) + 
  facet_grid(cond~value) + 
  scale_y_discrete(limits = rev(levels(tobit_ideol_res$dv)))
ggsave(filename = "fig/tobit_ideol_know.pdf", width = 4, height = 3)


### Fig 7: Media exposure and ideological differences in moral foundations (tobit)

## model estimation
tobit_ideol_media <- list(NULL)
tobit_ideol_media[[1]] <- vglm(harm_s ~ ideol*polmedia_c + relig + educ + age + female + black 
                               + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
tobit_ideol_media[[2]] <- vglm(fairness_s ~ ideol*polmedia_c + relig + educ + age + female + black 
                               + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
tobit_ideol_media[[3]] <- vglm(ingroup_s ~ ideol*polmedia_c + relig + educ + age + female + black 
                               + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
tobit_ideol_media[[4]] <- vglm(authority_s ~ ideol*polmedia_c + relig + educ + age + female + black 
                               + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)

## simulate expected values / marginal effects
tobit_ideol_res <- rbind(sim(models = tobit_ideol_media
                             , iv=data.frame(polmedia_c=min(anes2012$polmedia_c, na.rm = T)
                                             , ideolModerate = c(0,0)
                                             , ideolConservative = c(1,0)))
                         , sim(models = tobit_ideol_media
                               , iv=data.frame(polmedia_c=max(anes2012$polmedia_c, na.rm = T)
                                               , ideolModerate = c(0,0)
                                               , ideolConservative = c(1,0))))
tobit_ideol_res$var <- factor(tobit_ideol_res$dv)
levels(tobit_ideol_res$dv) <- rev(mftLabs)
tobit_ideol_res$cond <- rep(c("Low Media Exposure","High Media Exposure"),each=8)

## generate plot
ggplot(tobit_ideol_res, aes(x = mean, y = dv)) +
  geom_vline(xintercept=0, col="lightgrey") + geom_point() +
  geom_errorbarh(aes(xmax=cilo,xmin=cihi),height=0) +
  ggtitle("Change in Predicted Emphasis on Moral Foundation") +
  labs(y = "Dependent Variable: Moral Foundation"
       , x = "Marginal Effect (Liberal - Conservative)") +
  theme_classic(base_size = 8) + theme(panel.border = element_rect(fill=NA)) + 
  facet_grid(cond~value) + 
  scale_y_discrete(limits = rev(levels(tobit_ideol_res$dv)))
ggsave(filename = "fig/tobit_ideol_media.pdf", width = 4, height = 3)


### Fig 8: Political discussion and ideological differences in moral foundations (tobit)

## model estimation
tobit_ideol_disc <- list(NULL)
tobit_ideol_disc[[1]] <- vglm(harm_s ~ ideol*poldisc_c + relig + educ + age + female + black
                              + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
tobit_ideol_disc[[2]] <- vglm(fairness_s ~ ideol*poldisc_c + relig + educ + age + female + black 
                              + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
tobit_ideol_disc[[3]] <- vglm(ingroup_s ~ ideol*poldisc_c + relig + educ + age + female + black 
                              + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
tobit_ideol_disc[[4]] <- vglm(authority_s ~ ideol*poldisc_c + relig + educ + age + female + black 
                              + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)

## simulate expected values / marginal effects
tobit_ideol_res <- rbind(sim(models = tobit_ideol_disc
                             , iv=data.frame(poldisc_c=min(anes2012$poldisc_c, na.rm = T)
                                             , ideolModerate = c(0,0)
                                             , ideolConservative = c(1,0)))
                         , sim(models = tobit_ideol_disc
                               , iv=data.frame(poldisc_c=max(anes2012$poldisc_c, na.rm = T)
                                               , ideolModerate = c(0,0)
                                               , ideolConservative = c(1,0))))
tobit_ideol_res$var <- factor(tobit_ideol_res$dv)
levels(tobit_ideol_res$dv) <- rev(mftLabs)
tobit_ideol_res$cond <- rep(c("Low Discussion Frequency","High Discussion Frequency"),each=8)

## generate plot
ggplot(tobit_ideol_res, aes(x = mean, y = dv)) +
  geom_vline(xintercept=0, col="lightgrey") + geom_point() +
  geom_errorbarh(aes(xmax=cilo,xmin=cihi),height=0) +
  ggtitle("Change in Predicted Emphasis on Moral Foundation") +
  labs(y = "Dependent Variable: Moral Foundation"
       , x = "Marginal Effect (Liberal - Conservative)") +
  theme_classic(base_size = 8) + theme(panel.border = element_rect(fill=NA)) + 
  facet_grid(cond~value) + 
  scale_y_discrete(limits = rev(levels(tobit_ideol_res$dv)))
ggsave(filename = "fig/tobit_ideol_disc.pdf", width = 4, height = 3)


### Fig 9: Media content and moral foundations (tobit)

## model estimation
tobit_cont <- list(NULL)
tobit_cont[[1]] <- vglm(harm_s ~ media_harm*polmedia_c + relig + educ + age + female + black 
                        + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
tobit_cont[[2]] <- vglm(fairness_s ~ media_fairness*polmedia_c + relig + educ + age + female + black
                        + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
tobit_cont[[3]] <- vglm(ingroup_s ~ media_ingroup*polmedia_c + relig + educ + age + female + black 
                        + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
tobit_cont[[4]] <- vglm(authority_s ~ media_authority*polmedia_c + relig + educ + age + female + black 
                        + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
lapply(tobit_cont, summary)

## simulate expected values / marginal effects
tobit_cont_res <- rbind(sim(models = tobit_cont[[1]]
                            , iv=data.frame(media_harm=range(anes2012$media_harm), polmedia = mean(anes2012$polmedia, na.rm = T)))
                        , sim(models = tobit_cont[[2]]
                              , iv=data.frame(media_fairness=range(anes2012$media_fairness), polmedia = mean(anes2012$polmedia, na.rm = T)))
                        , sim(models = tobit_cont[[3]]
                              , iv=data.frame(media_ingroup=range(anes2012$media_ingroup), polmedia = mean(anes2012$polmedia, na.rm = T)))
                        , sim(models = tobit_cont[[4]]
                              , iv=data.frame(media_authority=range(anes2012$media_authority), polmedia = mean(anes2012$polmedia, na.rm = T))))
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



### Fig C.2: Knowledge/media/discussion and ideological differences in moral foundations (tobit, did)

## model estimation
tobit_ideol_all <- list(NULL)
tobit_ideol_all[[1]] <- vglm(harm_s ~ ideol*polknow_c + ideol*polmedia_c + ideol*poldisc_c + 
                               relig + educ + age + female + black + lwc + wordsum + mode
                             , tobit(Lower = 0), data=anes2012)
tobit_ideol_all[[2]] <- vglm(fairness_s ~ ideol*polknow_c + ideol*polmedia_c + ideol*poldisc_c +
                               relig + educ + age + female + black + lwc + wordsum + mode
                             , tobit(Lower = 0), data=anes2012)
tobit_ideol_all[[3]] <- vglm(ingroup_s ~ ideol*polknow_c + ideol*polmedia_c + ideol*poldisc_c +
                               relig + educ + age + female + black + lwc + wordsum + mode
                             , tobit(Lower = 0), data=anes2012)
tobit_ideol_all[[4]] <- vglm(authority_s ~ ideol*polknow_c + ideol*polmedia_c + ideol*poldisc_c +
                               relig + educ + age + female + black + lwc + wordsum + mode
                             , tobit(Lower = 0), data=anes2012)

## simulate expected values / difference-in-difference
tobit_ideol_res <- rbind(sim(models = tobit_ideol_know
                             , iv=data.frame(polknow_c=rep(range(anes2012$polknow_c, na.rm = T)
                                                           ,each=2)
                                             , ideolModerate = rep(0,4)
                                             , ideolConservative = c(0,1,0,1)))
                         , sim(models = tobit_ideol_media
                               , iv=data.frame(polmedia_c=rep(range(anes2012$polmedia_c, na.rm = T)
                                                              ,each=2)
                                               , ideolModerate = rep(0,4)
                                               , ideolConservative = c(0,1,0,1)))
                         , sim(models = tobit_ideol_disc
                               , iv=data.frame(poldisc_c=rep(range(anes2012$poldisc_c, na.rm = T)
                                                             ,each=2)
                                               , ideolModerate = rep(0,4)
                                               , ideolConservative = c(0,1,0,1)))
                         , sim(models = tobit_ideol_all
                               , iv=data.frame(polknow_c=rep(range(anes2012$polknow_c, na.rm = T)
                                                             ,each=2)
                                               , ideolModerate = rep(0,4)
                                               , ideolConservative = c(0,1,0,1)))
                         , sim(models = tobit_ideol_all
                               , iv=data.frame(polmedia_c=rep(range(anes2012$polmedia_c, na.rm = T)
                                                              ,each=2)
                                               , ideolModerate = rep(0,4)
                                               , ideolConservative = c(0,1,0,1)))
                         , sim(models = tobit_ideol_all
                               , iv=data.frame(poldisc_c=rep(range(anes2012$poldisc_c, na.rm = T)
                                                             ,each=2)
                                               , ideolModerate = rep(0,4)
                                               , ideolConservative = c(0,1,0,1))))
tobit_ideol_res$mod <- factor(rep(rep(1:3,each=8),2), labels = gsub("\n"," ",polLabs))
tobit_ideol_res$var <- rep(c(4:1,4:1),each=2)
tobit_ideol_res$cond <- rep(c("No","Yes"), each = 24)
tobit_ideol_res$year <- "2012"

## generate plot
ggplot(tobit_ideol_res, aes(x = mean, y = var+.1-.2*(cond=="Yes"), col=cond, shape=cond)) +
  geom_vline(xintercept=0, col="lightgrey") + geom_point() +
  geom_errorbarh(aes(xmax=cihi,xmin=cilo),height=0) +
  labs(y = "Moderating Variable", x= "Change in Effect of Ideology (Liberal - Conservative)") +
  theme_classic(base_size = 8) + theme(panel.border = element_rect(fill=NA)) + 
  scale_y_continuous(breaks=1:4, labels=mftLabs) +
  ggtitle("Change in Effect of Ideology on the\nEmphasis of each Moral Foundation") +
  guides(col=guide_legend(title="Control for remaining variables")
         , shape=guide_legend(title="Control for remaining variables")) +
  theme(legend.position="bottom", legend.box="horizontal") + facet_grid(mod~value) +
  scale_color_grey(start=0,end=.5)
ggsave(filename = "fig/tobit_ideol_difdif.pdf", width = 4, height = 5)



###################################
### Media content robustness checks

## differentiating by foundation
tobit_cont <- list(NULL)
tobit_cont[[1]] <- vglm(harm_s ~ media_harm + polmedia_c + relig + educ + age + female + black 
                        + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
tobit_cont[[2]] <- vglm(fairness_s ~ media_fairness + polmedia_c + relig + educ + age + female + black
                        + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
tobit_cont[[3]] <- vglm(ingroup_s ~ media_ingroup + polmedia_c + relig + educ + age + female + black 
                        + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
tobit_cont[[4]] <- vglm(authority_s ~ media_authority + polmedia_c + relig + educ + age + female + black 
                        + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
lapply(tobit_cont, summary)

## checking alternative recodings
tobit_cont <- list(NULL)
tobit_cont[[1]] <- vglm(harm_s ~ media_harm_s + polmedia_c + relig + educ + age + female + black 
                        + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
tobit_cont[[2]] <- vglm(fairness_s ~ media_fairness_s + polmedia_c + relig + educ + age + female + black
                        + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
tobit_cont[[3]] <- vglm(ingroup_s ~ media_ingroup_s + polmedia_c + relig + educ + age + female + black 
                        + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
tobit_cont[[4]] <- vglm(authority_s ~ media_authority_s + polmedia_c + relig + educ + age + female + black 
                        + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
lapply(tobit_cont, summary)

## differentiate by media source
m <- vglm(general_s ~ wkinews + inews_general + wktvnws + tvnws_general
          + wkpaprnws + paprnws_general + wkrdnws + rdnws_general
          + relig + educ + age + female + black 
          + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
summary(m)

## check alternative recoding
m <- vglm(general_s ~ wkinews + inews_general_s + wktvnws + tvnws_general_s
          + wkpaprnws + paprnws_general_s + wkrdnws + rdnws_general_s
          + relig + educ + age + female + black 
          + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
summary(m)



#############################
### Tables of Model Estimates


### Fig 2: Ideological differences in moral foundations (tobit)

## print summary
lapply(tobit_ideol, summary)

## create labels
varlabs = list(ideolConservative="Ideology (Conservative)", ideolModerate="Ideology (Moderate)"
               , relig="Church Attendance", educ="Education (College Degree)"
               , age="Age", female="Sex (Female)", black="Race (African American)"
               , lwc="Word Count (log)", wordsum="Wordsum Score",mode="Survey Mode (Online)"
               , "(Intercept):1"="Intercept", "(Intercept):2"="log(Sigma)")
mlabs <- c("Harm", "Fairness", "Ingroup", "Authority")

## create table
latexTable(tobit_ideol, caption="Tobit models predicting MFT score for each foundation based 
           on ideology. Positive coefficients indicate stronger emphasis on the respective 
           foundation. Standard errors in parentheses. Estimates are used for Figure 
           \\ref{fig:tobit_ideol} in the main text."
           , label="tab:tobit_ideol", align="lcccc"
           , varlabs=varlabs, mlabs=mlabs
           , file="tab/tobit_ideol.tex"
           , table.placement="ht", caption.placement="top"
           , size="footnotesize")


### Fig 3: Moral foundations and feeling thermometer differentials (ols)

## print summary
lapply(ols_feel, summary)

## create labels
varlabs = list(harm_s="Harm", fairness_s="Fairness", ingroup_s="Ingroup", authority_s="Authority"
               , pid_dem="PID (Democrat)", pid_rep="PID (Republican)"
               , relig="Church Attendance", educ="Education (College Degree)"
               , age="Age", female="Sex (Female)", black="Race (African American)"
               , lwc="Word Count (log)", wordsum="Wordsum Score",mode="Survey Mode (Online)"
               , "(Intercept)"="Intercept")
mlabs <- c("Party (1)", "Party (2)", "Cand. (1)", "Cand. (2)")

## create table
latexTable(ols_feel, caption="OLS models predicting feeling thermometer differentials based on
           MFT score for each foundation. Positive coefficients indicate more favorable evaluation 
           of Democratic candidate/party than the Republican candidate/party, and vice versa. 
           Standard errors in parentheses. Estimates are used for Figure \\ref{fig:ols_feel} 
           in the main text."
           , label="tab:ols_feel", align="lcccc"
           , varlabs=varlabs, mlabs=mlabs
           , file="tab/ols_feel.tex"
           , table.placement="h", caption.placement="top"
           , size="footnotesize")


### Fig 4: Moral foundations and democratic vote (logit)

## print summary
lapply(logit_vote, summary)

## create labels
varlabs = list(harm_s="Harm", fairness_s="Fairness", ingroup_s="Ingroup", authority_s="Authority"
               , pid_dem="PID (Democrat)", pid_rep="PID (Republican)"
               , relig="Church Attendance", educ="Education (College Degree)"
               , age="Age", female="Sex (Female)", black="Race (African American)"
               , lwc="Word Count (log)", wordsum="Wordsum Score",mode="Survey Mode (Online)"
               , "(Intercept)"="Intercept")
mlabs <- NULL

## create table
latexTable(logit_vote, caption="Logit models predicting democratic vote choice based on
           MFT score for each foundation. Positive coefficients indicate higher likelihood
           to vote for the Democratic candidate than the Republican candidate. Standard errors 
           in parentheses. Estimates are used for Figure \\ref{fig:logit_vote} in the main text."
           , label="tab:logit_vote", align="lcc"
           , varlabs=varlabs, mlabs=mlabs
           , file="tab/logit_vote.tex"
           , table.placement="ht", caption.placement="top"
           , size="footnotesize")


### Fig 5: Knoledge/media/discussion and general moral reasoning (tobit)

## print summary
lapply(tobit_learn, summary)

## create labels
varlabs = list(polknow="Political Knowledge", polmedia="Political Media Exposure"
               , poldisc="Political\nDiscussions"
               , relig="Church Attendance", educ="Education (College Degree)"
               , age="Age", female="Sex (Female)", black="Race (African American)"
               , lwc="Word Count (log)", wordsum="Wordsum Score",mode="Survey Mode (Online)"
               , "(Intercept):1"="Intercept", "(Intercept):2"="log(Sigma)")
mlabs <- NULL

## create table
latexTable(tobit_learn, caption="Tobit models predicting overall reliance on moral foundations
           (sum of MFT scores) based on political knowledge, media exposure, and frequency of 
           political discussions. Positive coefficients indicate stronger emphasis on any foundation.
           Standard errors in parentheses. Estimates are used for Figure \\ref{fig:tobit_learn} in 
           the main text."
           , label="tab:tobit_learn", align="lcccc"
           , varlabs=varlabs, mlabs=mlabs
           , file="tab/tobit_learn.tex"
           , table.placement="ht", caption.placement="top"
           , size="footnotesize")


### Fig 6: Knowledge and ideological differences in moral foundations (tobit)

## print summary
lapply(tobit_ideol_know, summary)

## create labels
varlabs = list(polknow_c="Political Knowledge", ideolConservative="Ideology (Conservative)"
               , "ideolConservative:polknow_c"="Knowledge * Conservative"
               , ideolModerate="Ideology (Moderate)"
               , "ideolModerate:polknow_c"="Knowledge * Moderate"
               , relig="Church Attendance", educ="Education (College Degree)"
               , age="Age", female="Sex (Female)", black="Race (African American)"
               , lwc="Word Count (log)", wordsum="Wordsum Score",mode="Survey Mode (Online)"
               , "(Intercept):1"="Intercept", "(Intercept):2"="log(Sigma)")
mlabs <- c("Harm", "Fairness", "Ingroup", "Authority")

## create table
latexTable(tobit_ideol_know, caption="Tobit models predicting MFT score for each foundation based 
           on political knowledge (mean-centered) and ideology. Positive coefficients indicate stronger 
           emphasis on the respective foundation. Standard errors in parentheses. Estimates are used 
           for Figure \\ref{fig:tobit_ideol_know} in the main text."
           , label="tab:tobit_ideol_know", align="lcccc"
           , varlabs=varlabs, mlabs=mlabs
           , file="tab/tobit_ideol_know.tex"
           , table.placement="ht", caption.placement="top"
           , size="footnotesize")


### Fig 7: Media exposure and ideological differences in moral foundations (tobit)

## print summary
lapply(tobit_ideol_media, summary)

## create labels
varlabs = list(polmedia_c="Political Media Exposure", ideolConservative="Ideology (Conservative)"
               , "ideolConservative:polmedia_c"="Media * Conservative"
               , ideolModerate="Ideology (Moderate)"
               , "ideolModerate:polmedia_c"="Media * Moderate"
               , relig="Church Attendance", educ="Education (College Degree)"
               , age="Age", female="Sex (Female)", black="Race (African American)"
               , lwc="Word Count (log)", wordsum="Wordsum Score",mode="Survey Mode (Online)"
               , "(Intercept):1"="Intercept", "(Intercept):2"="log(Sigma)")
mlabs <- c("Harm", "Fairness", "Ingroup", "Authority")

## create table
latexTable(tobit_ideol_media, caption="Tobit models predicting MFT score for each foundation based 
           on political media exposure (mean-centered) and ideology. Positive coefficients indicate 
           stronger emphasis on the respective foundation. Standard errors in parentheses. Estimates 
           are used for Figure \\ref{fig:tobit_ideol_media} in the main text."
           , label="tab:tobit_ideol_media", align="lcccc"
           , varlabs=varlabs, mlabs=mlabs
           , file="tab/tobit_ideol_media.tex"
           , table.placement="ht", caption.placement="top"
           , size="footnotesize")


### Fig 8: Political discussion and ideological differences in moral foundations (tobit)

## print summary
lapply(tobit_ideol_disc, summary)

## create labels
varlabs = list(poldisc_c="Political Discussion", ideolConservative="Ideology (Conservative)"
               , "ideolConservative:poldisc_c"="Discussion * Conservative"
               , ideolModerate="Ideology (Moderate)"
               , "ideolModerate:poldisc_c"="Discussion * Moderate"
               , relig="Church Attendance", educ="Education (College Degree)"
               , age="Age", female="Sex (Female)", black="Race (African American)"
               , lwc="Word Count (log)", wordsum="Wordsum Score",mode="Survey Mode (Online)"
               , "(Intercept):1"="Intercept", "(Intercept):2"="log(Sigma)")
mlabs <- c("Harm", "Fairness", "Ingroup", "Authority")

## create table
latexTable(tobit_ideol_disc, caption="Tobit models predicting MFT score for each foundation based 
           on political discussion frequency (mean-centered) and ideology. Positive coefficients 
           indicate stronger emphasis on the respective foundation. Standard errors in parentheses. 
           Estimates are used for Figure \\ref{fig:tobit_ideol_disc} in the main text."
           , label="tab:tobit_ideol_disc", align="lcccc"
           , varlabs=varlabs, mlabs=mlabs
           , file="tab/tobit_ideol_disc.tex"
           , table.placement="ht", caption.placement="top"
           , size="footnotesize")


### Fig 9: Media content and moral foundations (tobit)

## print summary
lapply(tobit_cont, summary)

## create labels
varlabs = list(media_harm_s="Media MFT score (harm)"
               , media_fairness_s="Media MFT score (fairness)"
               , media_ingroup_s="Media MFT score (ingroup)"
               , media_authority_s="Media MFT score (authority)"
               , relig="Church Attendance", educ="Education (College Degree)"
               , age="Age", female="Sex (Female)", black="Race (African American)"
               , lwc="Word Count (log)", wordsum="Wordsum Score",mode="Survey Mode (Online)"
               , "(Intercept):1"="Intercept", "(Intercept):2"="log(Sigma)")
mlabs <- c("Harm", "Fairness", "Ingroup", "Authority")

## create table
latexTable(tobit_cont, caption="Tobit models predicting MFT score for each foundation based 
           on moral content of individual media environments. Positive coefficients 
           indicate stronger emphasis on the respective foundation. Standard errors in parentheses. 
           Estimates are used for Figure \\ref{fig:tobit_cont} in the main text."
           , label="tab:tobit_cont", align="lcccc"
           , varlabs=varlabs, mlabs=mlabs
           , file="tab/tobit_cont.tex"
           , table.placement="ht", caption.placement="top"
           , size="footnotesize")


### Fig C.1: Participation and general moral reasoning (tobit)

## print summary
lapply(tobit_part, summary)

## create labels
varlabs = list(pastvote="Voted in 2008", protest="Protest", petition="Petition"
               , button="Button", letter="Letter"
               , relig="Church Attendance", educ="Education (College Degree)"
               , age="Age", female="Sex (Female)", black="Race (African American)"
               , lwc="Word Count (log)", wordsum="Wordsum Score",mode="Survey Mode (Online)"
               , "(Intercept):1"="Intercept", "(Intercept):2"="log(Sigma)")
mlabs <- NULL

## create table
latexTable(tobit_part, caption="Tobit models predicting overall reliance on moral foundations
           (sum of MFT scores) based on political participation. Positive coefficients indicate 
           stronger emphasis on any foundation. Standard errors in parentheses. Estimates are 
           used for Figure \\ref{fig:tobit_part} in the appendix."
           , label="tab:tobit_part", align="lcccccc"
           , varlabs=varlabs, mlabs=mlabs
           , file="tab/tobit_part.tex"
           , table.placement="ht", caption.placement="top"
           , size="footnotesize")


### Fig C.2: Knowledge/media/discussion and ideological differences in moral foundations (tobit, did)

## print summary
lapply(tobit_ideol_all, summary)

## create labels
varlabs = list(polknow_c="Political Knowledge", polmedia_c="Political Media Exposure"
               , poldisc_c="Political Discussion"
               , ideolConservative="Ideology (Conservative)"
               , "ideolConservative:polknow_c"="Knowledge * Conservative"
               , "ideolConservative:polmedia_c"="Media * Conservative"
               , "ideolConservative:poldisc_c"="Discussion * Conservative"
               , ideolModerate="Ideology (Moderate)"
               , "ideolModerate:polknow_c"="Knowledge * Moderate"
               , "ideolModerate:polmedia_c"="Media * Moderate"
               , "ideolModerate:poldisc_c"="Discussion * Moderate"
               , relig="Church Attendance", educ="Education (College Degree)"
               , age="Age", female="Sex (Female)", black="Race (African American)"
               , lwc="Word Count (log)", wordsum="Wordsum Score",mode="Survey Mode (Online)"
               , "(Intercept):1"="Intercept", "(Intercept):2"="log(Sigma)")
mlabs <- c("Harm", "Fairness", "Ingroup", "Authority")

## create table
latexTable(tobit_ideol_all, caption="Tobit models predicting MFT score for each foundation based 
           on political knowledge, media exposure, and discussion frequency (all mean-centered)
           as well as ideology. Positive coefficients indicate stronger emphasis on the respective
           foundation. Standard errors in parentheses. Estimates are used for Figure
           \\ref{fig:tobit_ideol_difdif} in the appendix."
           , label="tab:tobit_ideol_difdif", align="lcccc"
           , varlabs=varlabs, mlabs=mlabs
           , file="tab/tobit_ideol_difdif.tex"
           , table.placement="ht", caption.placement="top"
           , size="footnotesize")

