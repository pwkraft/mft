setwd("/data/Dropbox/Uni/618-Ideology/paper")
rm(list=ls())
library(foreign)
library(car)
library(reshape2)
library(ggplot2)
library(stargazer)
# I had to install an old Zelig version because the new one still had bugs...
# install.packages("/data/Dropbox/Uni/618-Ideology/paper/analyses/Zelig_3.5.5.tar.gz",repos=NULL)
library(Zelig)
source("/data/Dropbox/1-src/func/lookfor.R")


####################
# Data Preparation #
####################


### recode independent variables

raw <- read.dta("/data/Dropbox/1-src/data/anes/anes_timeseries_2012.dta",convert.factors=F)
anes <- data.frame(id=raw$caseid)

## ideology
# d/k -> moderate
anes$ideol <- factor(recode(raw$libcpre_self
                            , "1:3=1; c(-2,-8,4)=3; 5:7=2; else=NA")
                     , labels = c("Liberal","Conservative","Moderate"))
anes$ideol.lib <- as.numeric(anes$ideol=="Liberal")
anes$ideol.con <- as.numeric(anes$ideol=="Conservative")

## strength of ideology
anes$ideol.str <- abs(recode(raw$libcpre_self, "c(-2,-8)=0; -9=NA") - 4)
anes$ideol.str.c <- anes$ideol.str - mean(anes$ideol.str, na.rm = T)

## party identification
anes$pid <- factor(recode(raw$pid_x
                          , "1:3=1; 4=3; 5:7=2; else=NA")
                   , labels = c("Democrat","Republican","Independent"))
anes$pid.dem <- as.numeric(anes$pid=="Democrat")
anes$pid.rep <- as.numeric(anes$pid=="Republican")

## strength of partisanship
anes$pid.str <- abs(recode(raw$pid_x, "-2 = NA") - 4)
anes$pid.str.c <- anes$pid.str - mean(anes$pid.str, na.rm = T)

## political interest
anes$polint <- (-1) * recode(raw$interest_attention, "lo:0 = NA") + 5
anes$polint.c <- anes$polint - mean(anes$polint, na.rm = T)

## religiosity (church attendance)
anes$relig <- (-1) * recode(raw$relig_churchoft, "lo:0 = NA") + 5
anes$relig[raw$relig_church==2] <- 0
anes$relig[raw$relig_churchwk==2] <- 5

## education: college degree (bachelor)
anes$educ <- recode(raw$dem_edugroup_x, "1:3=0; 4:5=1; lo:0 = NA")

## age
anes$age <- recode(raw$dem_age_r_x, "-2 = NA")

## sex
anes$female <- raw$gender_respondent_x - 1

## race
anes$black <- as.numeric(recode(raw$dem_raceeth_x, "lo:0 = NA") == 2)


### recode response data

load("/data/Dropbox/1-src/data/anes/anes2012mft.RData")

## delete spanish open-ended responses: web / pre capi / post capi
lookfor(raw,"lang")
spell[raw$profile_spanishsurv==1,2:ncol(spell)] <- NA
spell[raw$admin_pre_lang_start==2,2:ncol(spell)] <- NA
spell[raw$admin_post_lang_start==2,2:ncol(spell)] <- NA
resp[raw$profile_spanishsurv==1,2:ncol(resp)] <- NA
resp[raw$admin_pre_lang_start==2,2:ncol(resp)] <- NA
resp[raw$admin_post_lang_start==2,2:ncol(resp)] <- NA
mft <- data.frame(id = resp[,1])

## aggregating over all items
respAgg <- function(groupname){
  x <- as.numeric(apply(resp[,grep(groupname,colnames(resp))],1,sum,na.rm=T) > 0)
  x[apply(!is.na(resp[,grep(groupname,colnames(resp))]),1,sum)==0] <- NA
  x
}
mft$harm.all <- respAgg("harm")
mft$fair.all <- respAgg("fair")
mft$ingr.all <- respAgg("ingr")
mft$auth.all <- respAgg("auth")
mft$puri.all <- respAgg("puri")
mft$mft.all <- as.numeric(apply(mft[,grep(".all",colnames(mft))],1,sum) > 0)

## aggregating over party evaluations
mft$harm.pa <- respAgg("harm.pa")
mft$fair.pa <- respAgg("fair.pa")
mft$ingr.pa <- respAgg("ingr.pa")
mft$auth.pa <- respAgg("auth.pa")
mft$puri.pa <- respAgg("puri.pa")
mft$mft.pa <- as.numeric(apply(mft[,grep(".pa",colnames(mft))],1,sum) > 0)

## aggregating over candidate evaluations
mft$harm.ca <- respAgg("harm.ca")
mft$fair.ca <- respAgg("fair.ca")
mft$ingr.ca <- respAgg("ingr.ca")
mft$auth.ca <- respAgg("auth.ca")
mft$puri.ca <- respAgg("puri.ca")
mft$mft.ca <- as.numeric(apply(mft[,grep(".ca",colnames(mft))],1,sum) > 0)

### merge datasets
anes <- merge(anes,mft)



#####################################
# Data Overview: Dependent Variable #
#####################################

### function to plot proportions

prop.plot <- function(data, title, fmtvarnames, groupvarname){
  ci <- function(x){1.96 * sqrt((mean(x, na.rm=T)*(1-mean(x, na.rm=T)))/sum(!is.na(x)))}
  prop.df <-  cbind(melt(aggregate(data[,fmtvarnames]
                                   ,by=list(groupvar = data[,groupvarname]),FUN="mean",na.rm=T))
                    , melt(aggregate(data[,fmtvarnames],by=list(groupvar = data[,groupvarname])
                                     ,FUN=function(x){mean(x, na.rm=T) - ci(x)}))[,3]
                    , melt(aggregate(data[,fmtvarnames],by=list(groupvar = data[,groupvarname])
                                     ,FUN=function(x){mean(x, na.rm=T) + ci(x)}))[,3]
                    )
  colnames(prop.df) <- c("groupvar", "mft", "Proportion", "cilo", "cihi")
  levels(prop.df$mft)[grep("puri",levels(prop.df$mft))] <- "Purity / Sanctity"
  levels(prop.df$mft)[grep("auth",levels(prop.df$mft))] <- "Authority / Respect"
  levels(prop.df$mft)[grep("ingr",levels(prop.df$mft))] <- "Ingroup / Loyalty"
  levels(prop.df$mft)[grep("fair",levels(prop.df$mft))] <- "Fairness / Reciprocity"
  levels(prop.df$mft)[grep("harm",levels(prop.df$mft))] <- "Harm / Care"
  ggplot(prop.df, aes(x = Proportion, y = mft)) +
    geom_point(size=3) + geom_errorbarh(aes(xmax=cihi,xmin=cilo),height=.2) +
    facet_grid(groupvar ~ .) + labs(y = "Moral Foundation", x = "Proportion of Respondents") +
    scale_x_continuous(limits = c(0, 0.521)) + theme_bw()
}

# plot overview
pdf("p1_mft_ideol.pdf")
prop.plot(data=anes, fmtvarnames=c("puri.all", "auth.all", "ingr.all", "fair.all", "harm.all"), groupvarname="ideol")
dev.off()

pdf("p2_mft_ideol_pa.pdf")
prop.plot(data=anes, fmtvarnames=c("puri.pa", "auth.pa", "ingr.pa", "fair.pa", "harm.pa"), groupvarname="ideol")
dev.off()

pdf("p3_mft_ideol_ca.pdf")
prop.plot(data=anes, fmtvarnames=c("puri.ca", "auth.ca", "ingr.ca", "fair.ca", "harm.ca"), groupvarname="ideol")
dev.off()

pdf("a1_mft_pid.pdf")
prop.plot(data=anes, fmtvarnames=c("puri.all", "auth.all", "ingr.all", "fair.all", "harm.all"), groupvarname="pid")
dev.off()

pdf("a2_mft_pid_pa.pdf")
prop.plot(data=anes, fmtvarnames=c("puri.pa", "auth.pa", "ingr.pa", "fair.pa", "harm.pa"), groupvarname="pid")
dev.off()

pdf("a3_mft_pid_ca.pdf")
prop.plot(data=anes, fmtvarnames=c("puri.ca", "auth.ca", "ingr.ca", "fair.ca", "harm.ca"), groupvarname="pid")
dev.off()



############
# Analyses #
############

## models predicting references to moral foundations in general
m1a <- zelig(mft.all ~ ideol + polint.c + relig + educ + age + female + black, data=anes, model="logit")
m1b <- zelig(mft.all ~ ideol*polint.c + relig + educ + age + female + black, data=anes, model="logit")
stargazer(m1a,m1b
          , type="text", out="m1_all.tex"
          , title="Logit Models Predicting overall References to Moral Foundations"
          , covariate.labels=c("Conservative","Moderate","Political Interest"
                               ,"Conservative X Political Interest","Moderate X Political Interest"
                               ,"Church Attendance","Education (College Degree)","Age","Sex (Female)","Race (African American)")
          , order=c(1:3,9,10,4:8), dep.var.labels="Any Moral Foundation"
          , align=T, column.sep.width="1pt", digits=3, digits.extra=1, font.size="scriptsize"
          , label="tab:m1_all", no.space=T#, table.placement="ht"
)

## models predicting references to specific moral foundations
m2a <- zelig(harm.all ~ ideol + polint.c + relig + educ + age + female + black, data=anes, model="logit")
m2b <- zelig(harm.all ~ ideol*polint.c + relig + educ + age + female + black, data=anes, model="logit")
m2c <- zelig(fair.all ~ ideol + polint.c + relig + educ + age + female + black, data=anes, model="logit")
m2d <- zelig(fair.all ~ ideol*polint.c + relig + educ + age + female + black, data=anes, model="logit")
m2e <- zelig(ingr.all ~ ideol + polint.c + relig + educ + age + female + black, data=anes, model="logit")
m2f <- zelig(ingr.all ~ ideol*polint.c + relig + educ + age + female + black, data=anes, model="logit")
m2g <- zelig(auth.all ~ ideol + polint.c + relig + educ + age + female + black, data=anes, model="logit")
m2h <- zelig(auth.all ~ ideol*polint.c + relig + educ + age + female + black, data=anes, model="logit")
stargazer(m2a,m2b,m2c,m2d,m2e,m2f,m2g,m2h
          , type="text", out="m2_specific.tex"
          , title="Logit Models Predicting Specific Moral Foundations"
          , covariate.labels=c("Conservative","Moderate","Political Interest"
                               ,"Conservative X Pol. Interest","Moderate X Pol. Interest"
                               ,"Church Attendance","Education (College Degree)","Age","Sex (Female)","Race (African American)")
          , order=c(1:3,9,10,4:8)
          , dep.var.labels=c("Harm/Care", "Fairness/Reciprocity", "Ingroup/Loyalty", "Authority/Respect")
          , align=T, column.sep.width="0pt", digits=3, digits.extra=1, font.size="scriptsize"
          , label="tab:m2_specific", no.space=T#, table.placement="c"
)

## Plot predicted probabilities / expected values
m2.x <- setx(m2a, ideol=c("Liberal","Conservative"))
m2a.sim <- sim(m2a,x=m2.x)
m2c.sim <- sim(m2c,x=m2.x)
m2e.sim <- sim(m2e,x=m2.x)
m2g.sim <- sim(m2g,x=m2.x)
m2.sim <- data.frame(rbind(c(mean(m2a.sim$qi$ev[,1] - m2a.sim$qi$ev[,2])
                             , quantile(m2a.sim$qi$ev[,1] - m2a.sim$qi$ev[,2], probs=c(0.025,0.975)))
                           , c(mean(m2c.sim$qi$ev[,1] - m2c.sim$qi$ev[,2])
                               , quantile(m2c.sim$qi$ev[,1] - m2c.sim$qi$ev[,2], probs=c(0.025,0.975)))
                           , c(mean(m2e.sim$qi$ev[,1] - m2e.sim$qi$ev[,2])
                               , quantile(m2e.sim$qi$ev[,1] - m2e.sim$qi$ev[,2], probs=c(0.025,0.975)))
                           , c(mean(m2g.sim$qi$ev[,1] - m2g.sim$qi$ev[,2])
                               , quantile(m2g.sim$qi$ev[,1] - m2g.sim$qi$ev[,2], probs=c(0.025,0.975)))
                           )
                     )
colnames(m2.sim) <- c("mean","cilo","cihi")
m2.sim$mft <- factor(x=1:4, labels=c("Harm / Care", "Fairness / Reciprocity", "Ingroup / Loyalty", "Authority / Respect"), ordered=T)
pdf("p4_models.pdf")
ggplot(m2.sim, aes(x = mean, y = factor(mft, levels = rev(levels(mft))))) +
  geom_point(size=4) + geom_errorbarh(aes(xmax=cihi,xmin=cilo),height=.1) +
  labs(y = "Moral Foundation", x= "Liberals - Conservatives") + geom_vline(xintercept=0) +
  theme_bw()
dev.off()

## models predicting including party identification for appendix
m3a <- zelig(harm.all ~ ideol + polint.c + pid + relig + educ + age + female + black, data=anes, model="logit")
m3b <- zelig(fair.all ~ ideol + polint.c + pid + relig + educ + age + female + black, data=anes, model="logit")
m3c <- zelig(ingr.all ~ ideol + polint.c + pid + relig + educ + age + female + black, data=anes, model="logit")
m3d <- zelig(auth.all ~ ideol + polint.c + pid + relig + educ + age + female + black, data=anes, model="logit")
stargazer(m3a,m3b,m3c,m3d
          , type="text", out="m3_app.tex"
          , title="Logit Models Predicting Specific Moral Foundations"
          , covariate.labels=c("Conservative","Moderate","Political Interest"
                               , "Republican", "Independent"
                               ,"Church Attendance","Education (College Degree)","Age","Sex (Female)","Race (African American)")
          , dep.var.labels=c("Harm/Care", "Fairness/Reciprocity", "Ingroup/Loyalty", "Authority/Respect")
          , align=T, column.sep.width="0pt", digits=3, digits.extra=1, font.size="scriptsize"
          , label="tab:m3_app", no.space=T#, table.placement="ht"
)