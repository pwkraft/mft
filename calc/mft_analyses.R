setwd("/data/Uni/projects/2014/mft")
pkg <- c("foreign","car","reshape2","ggplot2","stargazer","Zelig")
inst <- pkg %in% installed.packages()  
if(length(pkg[!inst]) > 0) install.packages(pkg[!inst])  
lapply(pkg,function(x){suppressPackageStartupMessages(library(x,character.only=TRUE))})
rm(list=ls())
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
anes$ideol_lib <- as.numeric(anes$ideol=="Liberal")
anes$ideol_con <- as.numeric(anes$ideol=="Conservative")

## strength of ideology
anes$ideol_str <- abs(recode(raw$libcpre_self, "c(-2,-8)=0; -9=NA") - 4)
anes$ideol_str_c <- anes$ideol_str - mean(anes$ideol_str, na.rm = T)

## party identification
anes$pid <- factor(recode(raw$pid_x
                          , "1:3=1; 4=3; 5:7=2; else=NA")
                   , labels = c("Democrat","Republican","Independent"))
anes$pid_dem <- as.numeric(anes$pid=="Democrat")
anes$pid_rep <- as.numeric(anes$pid=="Republican")

## strength of partisanship
anes$pid_str <- abs(recode(raw$pid_x, "-2 = NA") - 4)
anes$pid_str_c <- anes$pid_str - mean(anes$pid_str, na.rm = T)

## political interest
anes$polint <- (-1) * recode(raw$interest_attention, "lo:0 = NA") + 5
anes$polint_c <- anes$polint - mean(anes$polint, na.rm = T)

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
mft$harm_all <- respAgg("harm")
mft$fair_all <- respAgg("fair")
mft$ingr_all <- respAgg("ingr")
mft$auth_all <- respAgg("auth")
mft$puri_all <- respAgg("puri")
mft$mft_all <- as.numeric(apply(mft[,grep("_all",colnames(mft))],1,sum) > 0)

## aggregating over party evaluations
mft$harm_pa <- respAgg("harm_pa")
mft$fair_pa <- respAgg("fair_pa")
mft$ingr_pa <- respAgg("ingr_pa")
mft$auth_pa <- respAgg("auth_pa")
mft$puri_pa <- respAgg("puri_pa")
mft$mft_pa <- as.numeric(apply(mft[,grep("_pa",colnames(mft))],1,sum) > 0)

## aggregating over candidate evaluations
mft$harm_ca <- respAgg("harm_ca")
mft$fair_ca <- respAgg("fair_ca")
mft$ingr_ca <- respAgg("ingr_ca")
mft$auth_ca <- respAgg("auth_ca")
mft$puri_ca <- respAgg("puri_ca")
mft$mft_ca <- as.numeric(apply(mft[,grep("_ca",colnames(mft))],1,sum) > 0)

### merge datasets
anes <- merge(anes,mft)



#####################################
# Data Overview: Dependent Variable #
#####################################

### function to plot proportions

prop_plot <- function(data, title, fmtvarnames, groupvarname){
  ci <- function(x){1.96 * sqrt((mean(x, na.rm=T)*(1-mean(x, na.rm=T)))/sum(!is.na(x)))}
  prop_df <-  cbind(melt(aggregate(data[,fmtvarnames]
                                   ,by=list(groupvar = data[,groupvarname]),FUN="mean",na.rm=T))
                    , melt(aggregate(data[,fmtvarnames],by=list(groupvar = data[,groupvarname])
                                     ,FUN=function(x){mean(x, na.rm=T) - ci(x)}))[,3]
                    , melt(aggregate(data[,fmtvarnames],by=list(groupvar = data[,groupvarname])
                                     ,FUN=function(x){mean(x, na.rm=T) + ci(x)}))[,3]
                    )
  colnames(prop_df) <- c("groupvar", "mft", "Proportion", "cilo", "cihi")
  levels(prop_df$mft)[grep("puri",levels(prop_df$mft))] <- "Purity / Sanctity"
  levels(prop_df$mft)[grep("auth",levels(prop_df$mft))] <- "Authority / Respect"
  levels(prop_df$mft)[grep("ingr",levels(prop_df$mft))] <- "Ingroup / Loyalty"
  levels(prop_df$mft)[grep("fair",levels(prop_df$mft))] <- "Fairness / Reciprocity"
  levels(prop_df$mft)[grep("harm",levels(prop_df$mft))] <- "Harm / Care"
  ggplot(prop_df, aes(x = Proportion, y = mft)) +
    geom_point(size=3) + geom_errorbarh(aes(xmax=cihi,xmin=cilo),height=.2) +
    facet_grid(groupvar ~ .) + labs(y = "Moral Foundation", x = "Proportion of Respondents") +
    scale_x_continuous(limits = c(0, 0.521)) + theme_bw()
}

# plot overview
pdf("p1_mft_ideol.pdf")
prop_plot(data=anes, fmtvarnames=c("puri_all", "auth_all", "ingr_all", "fair_all", "harm_all"), groupvarname="ideol")
dev.off()

pdf("p2_mft_ideol_pa.pdf")
prop_plot(data=anes, fmtvarnames=c("puri_pa", "auth_pa", "ingr_pa", "fair_pa", "harm_pa"), groupvarname="ideol")
dev.off()

pdf("p3_mft_ideol_ca.pdf")
prop_plot(data=anes, fmtvarnames=c("puri_ca", "auth_ca", "ingr_ca", "fair_ca", "harm_ca"), groupvarname="ideol")
dev.off()

pdf("a1_mft_pid.pdf")
prop_plot(data=anes, fmtvarnames=c("puri_all", "auth_all", "ingr_all", "fair_all", "harm_all"), groupvarname="pid")
dev.off()

pdf("a2_mft_pid_pa.pdf")
prop_plot(data=anes, fmtvarnames=c("puri_pa", "auth_pa", "ingr_pa", "fair_pa", "harm_pa"), groupvarname="pid")
dev.off()

pdf("a3_mft_pid_ca.pdf")
prop_plot(data=anes, fmtvarnames=c("puri_ca", "auth_ca", "ingr_ca", "fair_ca", "harm_ca"), groupvarname="pid")
dev.off()



############
# Analyses #
############

## models predicting references to moral foundations in general
m1a <- zelig(mft_all ~ ideol + polint_c + relig + educ + age + female + black, data=anes, model="logit",cite=F)
m1b <- zelig(mft_all ~ ideol*polint_c + relig + educ + age + female + black, data=anes, model="logit",cite=F)
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
m2a <- zelig(harm_all ~ ideol + polint_c + relig + educ + age + female + black, data=anes, model="logit",cite=F)
m2b <- zelig(harm_all ~ ideol*polint_c + relig + educ + age + female + black, data=anes, model="logit",cite=F)
m2c <- zelig(fair_all ~ ideol + polint_c + relig + educ + age + female + black, data=anes, model="logit",cite=F)
m2d <- zelig(fair_all ~ ideol*polint_c + relig + educ + age + female + black, data=anes, model="logit",cite=F)
m2e <- zelig(ingr_all ~ ideol + polint_c + relig + educ + age + female + black, data=anes, model="logit",cite=F)
m2f <- zelig(ingr_all ~ ideol*polint_c + relig + educ + age + female + black, data=anes, model="logit",cite=F)
m2g <- zelig(auth_all ~ ideol + polint_c + relig + educ + age + female + black, data=anes, model="logit",cite=F)
m2h <- zelig(auth_all ~ ideol*polint_c + relig + educ + age + female + black, data=anes, model="logit",cite=F)
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
m2_x <- setx(m2a, ideol="Liberal")
m2_x1 <- setx(m2a, ideol="Conservative")
m2a_sim <- sim(m2a,x=m2_x, x1=m2_x1)
m2c_sim <- sim(m2c,x=m2_x)
m2e_sim <- sim(m2e,x=m2_x)
m2g_sim <- sim(m2g,x=m2_x)
m2_sim <- data.frame(rbind(c(mean(m2a_sim$qi$ev[,1] - m2a_sim$qi$ev[,2])
                             , quantile(m2a_sim$qi$ev[,1] - m2a_sim$qi$ev[,2], probs=c(0.025,0.975)))
                           , c(mean(m2c_sim$qi$ev[,1] - m2c_sim$qi$ev[,2])
                               , quantile(m2c_sim$qi$ev[,1] - m2c_sim$qi$ev[,2], probs=c(0.025,0.975)))
                           , c(mean(m2e_sim$qi$ev[,1] - m2e_sim$qi$ev[,2])
                               , quantile(m2e_sim$qi$ev[,1] - m2e_sim$qi$ev[,2], probs=c(0.025,0.975)))
                           , c(mean(m2g_sim$qi$ev[,1] - m2g_sim$qi$ev[,2])
                               , quantile(m2g_sim$qi$ev[,1] - m2g_sim$qi$ev[,2], probs=c(0.025,0.975)))
                           )
                     )
colnames(m2_sim) <- c("mean","cilo","cihi")
m2_sim$mft <- factor(x=1:4, labels=c("Harm / Care", "Fairness / Reciprocity", "Ingroup / Loyalty", "Authority / Respect"), ordered=T)
pdf("p4_models.pdf")
ggplot(m2_sim, aes(x = mean, y = factor(mft, levels = rev(levels(mft))))) +
  geom_point(size=4) + geom_errorbarh(aes(xmax=cihi,xmin=cilo),height=.1) +
  labs(y = "Moral Foundation", x= "Liberals - Conservatives") + geom_vline(xintercept=0) +
  theme_bw()
dev.off()

## models predicting including party identification for appendix
m3a <- zelig(harm_all ~ ideol + polint_c + pid + relig + educ + age + female + black, data=anes, model="logit",cite=F)
m3b <- zelig(fair_all ~ ideol + polint_c + pid + relig + educ + age + female + black, data=anes, model="logit",cite=F)
m3c <- zelig(ingr_all ~ ideol + polint_c + pid + relig + educ + age + female + black, data=anes, model="logit",cite=F)
m3d <- zelig(auth_all ~ ideol + polint_c + pid + relig + educ + age + female + black, data=anes, model="logit",cite=F)
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
