# LIBRARY LOAD ####
rm(list = ls())
library(readr)
library(dplyr)
library(psych)
library(sjPlot)
library(lme4)
library(stats)
library(ez)
library(ltm)
library(ggplot2)

# Functions ####

GetFactorName<-function (factorAnalysis, variableName)
{
  return(names(which.max(abs(factorAnalysis$loadings[variableName,]) )))
}

# Data ####
source("Utilities.R")
source("R_rainclouds.R")
source("summarySE.R")

dtIBI<-read_csv("dtRawIBI.csv",na=c("NA",""), col_types = cols(phase7 = col_double(), phase8 = col_double()))
summary(dtIBI$Phase)
dtIBI<-dtIBI %>%arrange(SUBJ_ID,timeUnix)
dtIBI<-dtIBI%>%mutate(deltaIBI=IBI-lag(IBI),samePhase= Phase==lag(Phase),unixTimeLag=timeUnix-lag(timeUnix) )
summary(dtIBI$samePhase)
dtIBI[!is.na(dtIBI$samePhase) & dtIBI$samePhase==FALSE,]$deltaIBI<-NA_real_
summary(dtIBI$deltaIBI)
dtIBI$deltaIBI_2<-dtIBI$deltaIBI*dtIBI$deltaIBI

dtIBIPhases<-filter(dtIBI,!is.na(samePhase))%>%group_by(SUBJ_ID,Phase)%>%
  summarise(Nibi=n(),RMSSD=sqrt(mean(deltaIBI_2,na.rm=T)),unixTimeLag=sum(unixTimeLag,na.rm=T) ) 

summary(dtIBIPhases[dtIBIPhases$Phase==1,]$unixTimeLag)

dtRaw<-read_csv("Emoticon2_Long.csv")

dtRaw$Valence<-NA_character_
dtRaw[!is.na(dtRaw$valence)&dtRaw$valence==1,]$Valence<-"P"
dtRaw[!is.na(dtRaw$valence)&dtRaw$valence==2,]$Valence<-"N"
dtRaw$Valence<-as.factor(dtRaw$Valence)

dtRaw$Feedback<-NA_character_
dtRaw[!is.na(dtRaw$fb)&dtRaw$fb==1,]$Feedback<-"Visual"
dtRaw[!is.na(dtRaw$fb)&dtRaw$fb==2,]$Feedback<-"VisuoTactile"
dtRaw$Feedback<-as.factor(dtRaw$Feedback)

dtRaw$subjId<-as.factor(dtRaw$subjId)
dtRaw$HRV <- dtRaw$HRV * 100
dtRaw$EDA <- dtRaw$EDA * 100

dtRaw$Relevance<-NA_real_
dtRaw$Identify<-NA_real_
dtRaw[!is.na(dtRaw$Feedback)&!is.na(dtRaw$valence)&dtRaw$Feedback=="Visual"&dtRaw$Valence=="P",]$Relevance<-
  dtRaw[!is.na(dtRaw$Feedback)&!is.na(dtRaw$valence)&dtRaw$Feedback=="Visual"&dtRaw$Valence=="P",]$RELEVANCE_VISUAL_POS
dtRaw[!is.na(dtRaw$Feedback)&!is.na(dtRaw$valence)&dtRaw$Feedback=="Visual"&dtRaw$Valence=="N",]$Relevance<-
  dtRaw[!is.na(dtRaw$Feedback)&!is.na(dtRaw$valence)&dtRaw$Feedback=="Visual"&dtRaw$Valence=="N",]$RELEVANCE_VISUAL_NEG
dtRaw[!is.na(dtRaw$Feedback)&!is.na(dtRaw$valence)&dtRaw$Feedback=="VisuoTactile"&dtRaw$Valence=="P",]$Relevance<-
  dtRaw[!is.na(dtRaw$Feedback)&!is.na(dtRaw$valence)&dtRaw$Feedback=="VisuoTactile"&dtRaw$Valence=="P",]$RELEVANCE_TACTILE_POS
dtRaw[!is.na(dtRaw$Feedback)&!is.na(dtRaw$valence)&dtRaw$Feedback=="VisuoTactile"&dtRaw$Valence=="N",]$Relevance<-
  dtRaw[!is.na(dtRaw$Feedback)&!is.na(dtRaw$valence)&dtRaw$Feedback=="VisuoTactile"&dtRaw$Valence=="N",]$RELEVANCE_TACTILE_NEG


dtRaw[!is.na(dtRaw$Feedback)&!is.na(dtRaw$valence)&dtRaw$Feedback=="Visual"&dtRaw$Valence=="P",]$Identify<-
  dtRaw[!is.na(dtRaw$Feedback)&!is.na(dtRaw$valence)&dtRaw$Feedback=="Visual"&dtRaw$Valence=="P",]$IDENTIFY_VISUAL_POS
dtRaw[!is.na(dtRaw$Feedback)&!is.na(dtRaw$valence)&dtRaw$Feedback=="Visual"&dtRaw$Valence=="N",]$Identify<-
  dtRaw[!is.na(dtRaw$Feedback)&!is.na(dtRaw$valence)&dtRaw$Feedback=="Visual"&dtRaw$Valence=="N",]$IDENTIFY_VISUAL_NEG
dtRaw[!is.na(dtRaw$Feedback)&!is.na(dtRaw$valence)&dtRaw$Feedback=="VisuoTactile"&dtRaw$Valence=="P",]$Identify<-
  dtRaw[!is.na(dtRaw$Feedback)&!is.na(dtRaw$valence)&dtRaw$Feedback=="VisuoTactile"&dtRaw$Valence=="P",]$IDENTIFY_TACTILE_POS
dtRaw[!is.na(dtRaw$Feedback)&!is.na(dtRaw$valence)&dtRaw$Feedback=="VisuoTactile"&dtRaw$Valence=="N",]$Identify<-
  dtRaw[!is.na(dtRaw$Feedback)&!is.na(dtRaw$valence)&dtRaw$Feedback=="VisuoTactile"&dtRaw$Valence=="N",]$IDENTIFY_TACTILE_NEG

#this merge keeps only phases 1,2,3,4 because these are the phases present in dtRaw before the merge
dtRaw<-merge(dtRaw,dtIBIPhases,by.x=c("subjId","Phase"),by.y=c("SUBJ_ID","Phase"), all.x=T)

summary(dtRaw$RMSSD)
# the old HRV measure which was a simple SD of the IBIs per phase, and the new meausre RMSSD, 
#have very high correlation as expected
cor.test(dtRaw$RMSSD,dtRaw$HRV)

summary(dtRaw)

# CRONBACH'S ALPHA #### 
STAI_TRAIT_TOTAL_crn <- dtRaw %>%
  dplyr::select(11:30) %>% 
  cronbach.alpha(na.rm = TRUE)
#0.62 for stai trait 

RSQ_TOTAL_crn <- dtRaw %>%
  dplyr::select(317:325) %>% 
  cronbach.alpha(na.rm = TRUE)
# 0.787 for rsq

STQ_cron<- dtRaw %>%
  dplyr::select(365:384) %>%
  cronbach.alpha(na.rm = TRUE)
# 0.804 for stq 


# DESCRIPTIVES #### 

Descriptive_stats <- dtRaw %>% # DESCRIPTIVES MEAN AND SD
  group_by(valence, fb) %>%
  summarise(HR_mean = mean(HR, na.rm = TRUE),
            HR_SD = sd(HR, na.rm = TRUE),
            EDA_mean = mean(EDA, na.rm = TRUE),
            EDA_SD = sd(EDA, na.rm = TRUE),
            HRV_mean = mean(HRV, na.rm = TRUE),
            HRV_SD = sd(HRV, na.rm = TRUE),
            measurement_mean = mean(measurement, na.rm = TRUE),
            measurement_SD = sd(measurement, na.rm = TRUE),
            SAFETY_TOTAL_CT6_mean = mean(SAFETY_TOTAL_CT6, na.rm = TRUE),
            SAFETY_TOTAL_CT6_SD = sd(SAFETY_TOTAL_CT6, na.rm = TRUE), 
            TRUST_TOTAL_CT6_mean = mean(TRUST_TOTAL_CT6, na.rm = TRUE),
            TRUST_TOTAL_CT6_SD = sd(TRUST_TOTAL_CT6, na.rm = TRUE),
            IDENTIFY_POSITIVE_mean = mean(IDENTIFY_POSITIVE, na.rm = TRUE),
            IDENTIFY_NEGATIVE_SD = sd(IDENTIFY_NEGATIVE, na.rm = TRUE),
            RELEVANCE_POSITIVE_mean = mean(RELEVANCE_POSITIVE, na.rm = TRUE),
            RELEVANCE_NEGATIVE_SD = sd(RELEVANCE_NEGATIVE, na.rm = TRUE)
  )


# H1: we look at the difference between types of feedback and then interaction w valence. ####
# feedback main effect
lm1<-lmer(measurement~Feedback+(1|subjId) + (1|Valence),data=dtRaw)
tab_model(lm1)
summary(lm1)
# sig. =.001 main effect, VT better than V 

# fb x valence
lm1<-lmer(measurement~Feedback*Valence+(1|subjId),data=dtRaw)
tab_model(lm1)
summary(lm1)
# trend of P over VN. and sig. VT x P interaction, .016. does not say much though bc interecept is VN

# each valence separately
lm1<-lmer(measurement~Feedback+(1|subjId),data=filter(dtRaw, Valence=="P"))
tab_model(lm1)
summary(lm1)
# VTP better than VP, <.001

lm1<-lmer(measurement~Feedback+(1|subjId),data=filter(dtRaw, Valence=="N"))
tab_model(lm1)
summary(lm1)
# VTN ns than VN, .545

# in summary we find a main effect of fb, VT better than V, and a sig interaction between valence and fb, 
# VTP better than intercept but this not v informative. when we break down to planned contrasts we see
# that VTP is better than VP but not the case between VTN and VN 

# H2: HR first ####
# here we look at the effects of fb on physiological measurements. then we add traits 
# heart rate first 
lm1<-lmer(HR~Feedback+(1|subjId)+(1|Phase),data=dtRaw)
tab_model(lm1, show.se=2)
summary(lm1)
# n.s. effect 

lm1<-lmer(HR~Feedback*Valence+(1|subjId)+(1|Phase),data=dtRaw)
tab_model(lm1, show.se=2)
summary(lm1)
# n.s. effect

lm1<-lmer(HR~Feedback+(1|subjId)+(1|Phase),data=filter(dtRaw, Valence=="P"))
tab_model(lm1, show.se=2)
summary(lm1)
# n.s. effect

lm1<-lmer(HR~Feedback+(1|subjId)+(1|Phase),data=filter(dtRaw, Valence=="N"))
tab_model(lm1, show.se=2)
summary(lm1)
# n.s. effect

# then we add traits on HR 
# both valences tgt 
lm1<-lmer(HR~Feedback*STAI_TRAIT+(1|subjId)+(1|Phase),data=dtRaw)
tab_model(lm1, show.se=2)
summary(lm1)
# n.s. effect

lm1<-lmer(HR~Feedback*RSQ_TOTAL+(1|subjId)+(1|Phase),data=dtRaw)
tab_model(lm1, show.se=2)
summary(lm1)
# n.s. effect

lm1<-lmer(HR~Feedback*ECR_ANX+(1|subjId)+(1|Phase),data=dtRaw)
tab_model(lm1, show.se=2)
summary(lm1)
# n.s. effect 

# pos valence 
lm1<-lmer(HR~Feedback*STAI_TRAIT+(1|subjId)+(1|Phase),data=filter(dtRaw, Valence=="P"))
tab_model(lm1, show.se=2)
summary(lm1)
# n.s. effect

lm1<-lmer(HR~Feedback*RSQ_TOTAL+(1|subjId)+(1|Phase),data=filter(dtRaw, Valence=="P"))
tab_model(lm1, show.se=2)
summary(lm1)
# n.s. effect

lm1<-lmer(HR~Feedback*ECR_ANX+(1|subjId)+(1|Phase),data=filter(dtRaw, Valence=="P"))
tab_model(lm1, show.se=2)
summary(lm1)
# n.s. effect 

# neg valence 
lm1<-lmer(HR~Feedback*STAI_TRAIT+(1|subjId)+(1|Phase),data=filter(dtRaw, Valence=="N"))
tab_model(lm1, show.se=2)
summary(lm1)
# n.s. effect

lm1<-lmer(HR~Feedback*RSQ_TOTAL+(1|subjId)+(1|Phase),data=filter(dtRaw, Valence=="N"))
tab_model(lm1, show.se=2)
summary(lm1)
# n.s. effect

lm1<-lmer(HR~Feedback*ECR_ANX+(1|subjId)+(1|Phase),data=filter(dtRaw, Valence=="N"))
tab_model(lm1, show.se=2)
summary(lm1)
# n.s. effect

# n.s. effects of fb on hr downregulation. but our ppts already had low HR and we didn't 
# induce stress. so maybe that's why. for discussion..n.s. when we add traits. normal range?

# H2: SCR ####

lm1<-lmer(EDA~Feedback+(1|subjId),data=dtRaw)
tab_model(lm1, show.se=2)
summary(lm1)
# n.s. effect 

lm1<-lmer(EDA~Feedback*Valence+(1|subjId)+(1|Phase),data=dtRaw)
tab_model(lm1, show.se=2)
summary(lm1)
# n.s. effect

lm1<-lmer(EDA~Feedback+(1|subjId)+(1|Phase),data=filter(dtRaw, Valence=="P"))
tab_model(lm1, show.se=2)
summary(lm1)
# n.s. effect

lm1<-lmer(EDA~Feedback+(1|subjId)+(1|Phase),data=filter(dtRaw, Valence=="N"))
tab_model(lm1, show.se=2)
summary(lm1)
# n.s. effect

# then we add traits on EDA 
# both valences tgt 
lm1<-lmer(EDA~Feedback*STAI_TRAIT+(1|subjId)+(1|Phase),data=dtRaw)
tab_model(lm1, show.se=2)
summary(lm1)
# n.s. effect

lm1<-lmer(EDA~Feedback*RSQ_TOTAL+(1|subjId)+(1|Phase),data=dtRaw)
tab_model(lm1, show.se=2)
summary(lm1)
# n.s. effect

lm1<-lmer(EDA~Feedback*ECR_ANX+(1|subjId)+(1|Phase),data=dtRaw)
tab_model(lm1, show.se=2)
summary(lm1)
# n.s. effect

# pos valence 
lm1<-lmer(EDA~Feedback*STAI_TRAIT+(1|subjId)+(1|Phase),data=filter(dtRaw, Valence=="P"))
tab_model(lm1, show.se=2)
summary(lm1)
# n.s. effect

lm1<-lmer(EDA~Feedback*RSQ_TOTAL+(1|subjId)+(1|Phase),data=filter(dtRaw, Valence=="P"))
tab_model(lm1, show.se=2)
summary(lm1)
# n.s. effect

lm1<-lmer(EDA~Feedback*ECR_ANX+(1|subjId)+(1|Phase),data=filter(dtRaw, Valence=="P"))
tab_model(lm1, show.se=2)
summary(lm1)
# VT fb better than V, sig, .010; n.s. main effect of ECR_ANX, but sig interaction VT x ECR_ANX, .009

# neg valence 
lm1<-lmer(EDA~Feedback*STAI_TRAIT+(1|subjId)+(1|Phase),data=filter(dtRaw, Valence=="N"))
tab_model(lm1, show.se=2)
summary(lm1)
# n.s. effect

lm1<-lmer(EDA~Feedback*RSQ_TOTAL+(1|subjId)+(1|Phase),data=filter(dtRaw, Valence=="N"))
tab_model(lm1, show.se=2)
summary(lm1)
# n.s effect

lm1<-lmer(EDA~Feedback*ECR_ANX+(1|subjId)+(1|Phase),data=filter(dtRaw, Valence=="N"))
tab_model(lm1, show.se=2)
summary(lm1)
# n.s. effect

# H2: HRV ####
lm1<-lmer(HRV~Feedback+(1|subjId)+(1|Phase) + (1|Valence),data=dtRaw)
tab_model(lm1, show.se=2)
summary(lm1)
# n.s. effect 

lm1<-lmer(HRV~Feedback*Valence+(1|subjId)+(1|Phase),data=dtRaw)
tab_model(lm1, show.se=2)
summary(lm1)
# n.s. effect

lm1<-lmer(HRV~Feedback+(1|subjId)+(1|Phase),data=filter(dtRaw, Valence=="P"))
tab_model(lm1, show.se=2)
summary(lm1)
# n.s. effect

lm1<-lmer(HRV~Feedback+(1|subjId)+(1|Phase),data=filter(dtRaw, Valence=="N"))
tab_model(lm1, show.se=2)
summary(lm1)
# n.s. effect

# then we add traits on HRV 
# both valences tgt 
lm1<-lmer(HRV~Feedback*STAI_TRAIT+(1|subjId)+(1|Phase),data=dtRaw)
tab_model(lm1, show.se=2)
summary(lm1)
# a sig. effect of VT, .035; n.s. stai, but sig VT x STAI, .033

lm1<-lmer(HRV~Feedback*RSQ_TOTAL+(1|subjId)+(1|Phase),data=dtRaw)
tab_model(lm1, show.se=2)
summary(lm1)
# n.s. effect

lm1<-lmer(HRV~Feedback*ECR_ANX+(1|subjId)+(1|Phase),data=dtRaw)
tab_model(lm1, show.se=2)
summary(lm1)
# n.s. effect

# pos valence 
lm1<-lmer(HRV~Feedback*STAI_TRAIT+(1|subjId)+(1|Phase),data=filter(dtRaw, Valence=="P"))
tab_model(lm1, show.se=2)
summary(lm1)
# n.s. effect

lm1<-lmer(HRV~Feedback*RSQ_TOTAL+(1|subjId)+(1|Phase),data=filter(dtRaw, Valence=="P"))
tab_model(lm1, show.se=2)
summary(lm1)
# n.s. effect

lm1<-lmer(HRV~Feedback*ECR_ANX+(1|subjId)+(1|Phase),data=filter(dtRaw, Valence=="P"))
tab_model(lm1, show.se=2)
summary(lm1)
# n.s. effect

# neg valence 
lm1<-lmer(HRV~Feedback*STAI_TRAIT+(1|subjId),data=filter(dtRaw, Valence=="N"))
tab_model(lm1, digits=3)
tab_model(lm1, show.se=2)
summary(dtRaw$HRV)
# sig. VT, .032; n.s. stait, but sig VT x STAI, .030
# removed phase bc it had 0 variance and conditional r sq was n/a

lm1<-lmer(HRV~Feedback*RSQ_TOTAL+(1|subjId)+(1|Phase),data=filter(dtRaw, Valence=="N"))
tab_model(lm1, show.se=2)
summary(lm1)
# n.s. effect

lm1<-lmer(HRV~Feedback*ECR_ANX+(1|subjId)+(1|Phase),data=filter(dtRaw, Valence=="N"))
tab_model(lm1, show.se=2)
summary(lm1)
# n.s. effect

# H3: relevance and identification ####
# these cannot be used bc v high colinearity, so we need to run a PCA
lm1<-lmer(measurement~Relevance+ (1|subjId)+(1|Valence)+(1|Feedback),data=dtRaw)
tab_model(lm1, show.se=2)
summary(lm1)

lm1<-lmer(measurement~Feedback*Relevance+ (1|subjId)+(1|Valence),data=dtRaw)
tab_model(lm1, show.se=2)
summary(lm1)

lm1<-lmer(measurement~Identify+ (1|subjId)+(1|Valence)+(1|Feedback),data=dtRaw)
tab_model(lm1, show.se=2)
summary(lm1)

lm1<-lmer(measurement~Feedback*Identify+ (1|subjId)+(1|Valence),data=dtRaw)
tab_model(lm1, show.se=2)
summary(lm1)

lm1<-lmer(measurement~Relevance + Identify+ (1|subjId)+(1|Valence)+(1|Feedback),data=dtRaw)
tab_model(lm1, show.se=2)
summary(lm1)
# strong colinearity

cor.test (dtRaw$Relevance, dtRaw$Identify)
#.65 v high 

# to deal w colinearity effects due to the high cor between the two IVs
# we performed PCA to orthogonalise the two variables

# PCA 
dtPCA<-dplyr::select(filter(dtRaw),"Relevance","Identify", "AGE")
fa1<-fa(dtPCA, nfactors=3, fm="pa" ,rotate="varimax",SMC=FALSE )
dtRaw$Relevance_pca<-fa1$scores[,GetFactorName(fa1, "Relevance")]
dtRaw$Identify_pca<-fa1$scores[,GetFactorName(fa1, "Identify")]
dtRaw$AGE_pca<-fa1$scores[,GetFactorName(fa1, "AGE")]

lm1<-lmer(measurement~Relevance_pca + Identify_pca+ (1|subjId)+(1|Valence)+(1|Feedback),data=dtRaw)
tab_model(lm1, show.se=2)
summary(lm1)

lm1<-lmer(measurement~Feedback*Relevance_pca + Feedback*Identify_pca+ (1|subjId)+(1|Valence),data=dtRaw)
tab_model(lm1, show.se=2)
summary(lm1)

# H4: safety and measurement ####
# main effect of safety 
dtVTonly<-filter(dtRaw,  Feedback %in% c("VisuoTactile"))
lm1<-lmer(measurement~SAFETY_TOTAL_CT6 + (1|subjId)+(1|Phase)+(1|Valence),data=dtVTonly)
tab_model(lm1, show.se=2)
summary(lm1)
# a trend, .061

# safety x valence 
lm1<-lmer(measurement~SAFETY_TOTAL_CT6 * Valence + (1|subjId),data=dtVTonly)
tab_model(lm1, show.se=2)
summary(lm1)
# main effect of safety, .021, main effect of valence, 0.005, but n.s. interaction 

# safety in pos only 
lm1<-lmer(measurement~SAFETY_TOTAL_CT6+(1|Phase),data=filter(dtVTonly, Valence=="P"))
tab_model(lm1, show.se=2)
summary(lm1)
# removed subjid bc only one row per ppt - same below 
# n.s. effect 
# if all random effects have n/a then change to lm

# safety in neg only
lm1<-lmer(measurement~SAFETY_TOTAL_CT6+(1|Phase),data=filter(dtVTonly, Valence=="N"))
tab_model(lm1, show.se=2)
summary(lm1)
# sig. .019 effect

# now we repeat w trust 
# main effect of trust 
dtVTonly<-filter(dtRaw,  Feedback %in% c("VisuoTactile"))
lm1<-lmer(measurement~TRUST_TOTAL_CT6 + (1|subjId)+(1|Phase)+(1|Valence),data=dtVTonly)
tab_model(lm1, show.se=2)
summary(lm1)
# sig. trust effect, 0.019

# trust x valence 
lm1<-lmer(measurement~TRUST_TOTAL_CT6 * Valence + (1|subjId)+(1|Phase),data=dtVTonly)
tab_model(lm1, show.se=2)
summary(lm1)
# trend in trust, .065 but n.s. main effect of valence and n.s. interaction 

# trust in pos only 
lm1<-lm(measurement~TRUST_TOTAL_CT6,data=filter(dtVTonly, Valence=="P"))
tab_model(lm1, show.se=2)
summary(lm1)
# sig. .012

# trust in neg only
lm1<-lmer(measurement~TRUST_TOTAL_CT6+(1|Phase),data=filter(dtVTonly, Valence=="N"))
tab_model(lm1, show.se=2)
summary(lm1)
# trend, .078

# Exploratory analyses ####
# measurement and weirdness of sleeve 
lm1<-lmer(measurement~WEIRD_TOTAL_CT6 + (1|subjId)+(1|Phase)+(1|Valence),data=dtVTonly)
tab_model(lm1, show.se=2)
summary(lm1)
# n.s. effect

# measurement and intensity of sleeve 
lm1<-lmer(measurement~INTENSITY_TOTAL_CT6 + (1|subjId)+(1|Phase)+(1|Valence),data=dtVTonly)
tab_model(lm1, show.se=2)
summary(lm1)
# a trend, .082

# measurement and ct6
lm1<-lmer(measurement~CT6_TOTAL + (1|subjId)+(1|Phase)+(1|Valence),data=dtVTonly)
tab_model(lm1, show.se=2)
summary(lm1)
# sig. effect of ct6, 0.019 

# measurement and anticipatory soft 
lm1<-lmer(measurement~ANTICIPATORY_SOFT + (1|subjId)+(1|Phase)+(1|Valence),data=dtVTonly)
tab_model(lm1, show.se=2)
summary(lm1)
# n.s. effect 

# measurement and anticipatory rough 
lm1<-lmer(measurement~ANTICIPATORY_ROUGH + (1|subjId)+(1|Phase)+(1|Valence),data=dtVTonly)
tab_model(lm1, show.se=2)
summary(lm1)
# n.s. effect

# GRAPHS H1 #### 
# RAINCLOUDS H1
packages <- c("cowplot", "readr", "ggplot2", "dplyr", "lavaan", "smooth", "Hmisc")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}

dtRaw$Valence<-as.character(dtRaw$Valence)
dtRaw[dtRaw$Valence=="N",]$Valence<-"Negative"
dtRaw[dtRaw$Valence=="P",]$Valence<-"Positive"
dtRaw$Valence<-as.factor(dtRaw$Valence)

sC_summaryStats <- summarySE(dtRaw, measurevar = "measurement",na.rm = TRUE,
                             groupvars=c("Feedback", "Valence"))

ggplot(dtRaw,aes(x=Feedback,y=measurement, fill = Valence, colour = Valence))+
  geom_flat_violin(position = position_nudge(x = .25, y = 0),adjust =2, trim = FALSE)+
  geom_point(position = position_jitter(width = .15), size = .25)+
  geom_point(position=position_dodge(width=0.12),data = sC_summaryStats, 
             aes(x =  as.numeric(Feedback),
                 y = measurement_mean,
                 group = Valence, colour = Valence),
             size = 3, shape = 18)+
  geom_errorbar(position=position_dodge(width=0.12),data = sC_summaryStats, 
                aes(x =  as.numeric(Feedback), y = measurement_mean,
                    group = Valence,   ymin = measurement_mean-ci,
                    ymax = measurement_mean+ci, colour = Valence), width = 0.2, size = 0.8)+
  geom_hline(yintercept=0, linetype = "dashed")+
  ylab('Perceived Social Intent')+
  xlab('Feedback Modality')+
  ylim(c(0, 100))+
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.caption = element_text(hjust = 0.5), text = element_text(size = 16))
ggsave('Hypothesis_1.png', width = 8, height = 6, dpi=1000)

