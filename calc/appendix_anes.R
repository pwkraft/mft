###############################################################################################
## Project:  Moral foundations of Political Reasoning
## File:     appendix_anes.R
## Overview: analyses for appendix (anes data), produces all additional plots and tables
##           based on data prepared in prep_anes.R and models estimated in analyses_anes.R
## Author:   Patrick Kraft
###############################################################################################

## packages
pkg <- c("tidyverse","gridExtra","stargazer","xtable","VGAM","pmisc")
invisible(lapply(pkg, library, character.only = TRUE))
rm(list=ls())

## working directory
setwd("/data/Dropbox/Uni/Projects/2014/mft/calc")

## load additional functions
source("func.R")

## load recoded dataset
load("out/analyses_anes.RData")



######################################
### Additional Descriptive information


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
             , label="tab:app_mis"),file="tab/app_mis.tex")


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
                      , labels = gsub("\\n","", rev(mftLabs))))

## generate plot
ggplot(plot_df, aes(y=reorder(id, score), x=score,xmin=score_lo,xmax=score_hi)) + 
  geom_point() + geom_errorbarh(height=0) + theme_classic(base_size = 8) + 
  theme(panel.border = element_rect(fill=NA)) +
  facet_grid(.~mft) + ggtitle("Moral Content of Media Sources (October 2012)") +
  xlab("MFT Score (rescaled)") + ylab("News Source") +
  geom_vline(xintercept=0, col="lightgrey")
ggsave("fig/media_desc.pdf",width = 7, height = 4)



##################################################
### Additional Model Results and Robustness Checks


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


