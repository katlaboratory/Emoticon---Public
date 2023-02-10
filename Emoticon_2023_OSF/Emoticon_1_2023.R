# LIBRARY AND DATASET LOAD ####
rm(list = ls())
library(readr)
library(dplyr)
library(sjPlot)
library(lme4)
library(stats)
library(ez)
library(ltm)
library(ggplot2)

source("Utilities.R")
source("R_rainclouds.R")
source("summarySE.R")

dtRaw<-read_csv("EMO1_LONG_20220601_AS.csv")
dtRaw2<-read_csv("EMO1_SHORT_20230110.csv")
summary(dtRaw)

summary(dtRaw$ID_LONG)

dtRaw$ID_LONG<-as.factor(dtRaw$ID_LONG)
summary(dtRaw)

Bigdata <- dtRaw %>% 
  merge(dtRaw2)

# H1: V vs T feedback in P vs N sentences when fb is Opt vs Subopt ####
# relevelling 
dtRaw$FB_MODALITY <- as.factor(dtRaw$FB_MODALITY)
dtRaw$FB_MODALITY <- relevel(dtRaw$FB_MODALITY, "Visual")

dtRaw$FB_OPTIMALITY <- as.factor(dtRaw$FB_OPTIMALITY)
dtRaw$FB_OPTIMALITY <- relevel(dtRaw$FB_OPTIMALITY, "Suboptimal")

dtRaw$VALENCE <- as.factor(dtRaw$VALENCE)
dtRaw$VALENCE <- relevel(dtRaw$VALENCE, "Negative")

# prelim analysis checking effects of context on social intent 
lm1<-lm(MAIN_SUPPORT~CONTEXT,data=dtRaw)
tab_model(lm1)
summary(lm1)

# main effect of fb - visual vs tactile
lm1<-lmer(MAIN_SUPPORT~FB_MODALITY+(1|ID_LONG)+(1|FB_OPTIMALITY) + (1|VALENCE),data=dtRaw)
tab_model(lm1)
summary(lm1)
# sig effect of feedback, <0.001

# main effect of optimality - high vs low
lm1<-lmer(MAIN_SUPPORT~FB_OPTIMALITY+(1|ID_LONG)+(1|FB_MODALITY) + (1|VALENCE),data=dtRaw)
tab_model(lm1, show.se=2)
summary(lm1)
# sig.001

# main effect of valence - pos vs neg
lm1<-lmer(MAIN_SUPPORT~VALENCE+(1|ID_LONG)+(1|FB_MODALITY) + (1|FB_OPTIMALITY),data=dtRaw)
tab_model(lm1)
summary(lm1)
# sig.<001 

# interaction between fb x and optimality
lm1<-lmer(MAIN_SUPPORT~FB_MODALITY*FB_OPTIMALITY+(1|ID_LONG)+ (1|VALENCE),data=dtRaw)
tab_model(lm1, show.se=2)
summary(lm1)

# interaction between fb mode x valence
lm1<-lmer(MAIN_SUPPORT~FB_MODALITY*VALENCE+(1|ID_LONG)+ (1|FB_OPTIMALITY),data=dtRaw)
tab_model(lm1, show.se=2)
summary(lm1)

# interaction between fb optimality x valence
lm1<-lmer(MAIN_SUPPORT~FB_OPTIMALITY*VALENCE+(1|ID_LONG)+ (1|FB_MODALITY),data=dtRaw)
tab_model(lm1, show.se=2)
summary(lm1)

# three-way interaction
lm1<-lmer(MAIN_SUPPORT~FB_MODALITY*FB_OPTIMALITY*VALENCE+(1|ID_LONG),data=dtRaw)
tab_model(lm1, show.se=2)
summary(lm1)

# Planned Contrasts, Set 1 ####
# visual vs tact in optimal only
dtOptOnly<-filter(dtRaw,  FB_OPTIMALITY %in% c("Optimal"))
 
# here we look at visual vs tact feedback in optimal support only
lm1<-lmer(MAIN_SUPPORT~FB_MODALITY+(1|ID_LONG) + (1|VALENCE),data=dtOptOnly)
tab_model(lm1, show.se=2)
summary(lm1)
# sig., 0.001

# if we look at each valence separately
lm1<-lmer(MAIN_SUPPORT~FB_MODALITY+(1|ID_LONG),data=filter(dtOptOnly, VALENCE=="Positive"))
tab_model(lm1, show.se=2)
summary(lm1)
# tact feedback sig more likes than vis fb

lm1<-lmer(MAIN_SUPPORT~FB_MODALITY+(1|ID_LONG),data=filter(dtOptOnly, VALENCE=="Negative"))
tab_model(lm1, show.se=2)
summary(lm1)
# same as above in the negative valence 

# Planned Contrasts Set 2 #### 
# then we look at the tactile feedback only. first opt vs subopt, then interaction w valence, then 
# each valence separately 
dtTactOnly<-filter(dtRaw,  FB_MODALITY %in% c("Tactile"))

lm1<-lmer(MAIN_SUPPORT~FB_OPTIMALITY+(1|ID_LONG) + (1|VALENCE),data=dtTactOnly)
tab_model(lm1, show.se=2)
summary(lm1)
# trend to sig, subopt tact worse than opt, 0.068

lm1<-lmer(MAIN_SUPPORT~FB_OPTIMALITY+(1|ID_LONG),data=filter(dtTactOnly, VALENCE == "Positive"))
tab_model(lm1, show.se=2)
summary(lm1)
# subopt sig worse than opt in post, 0.001

lm1<-lmer(MAIN_SUPPORT~FB_OPTIMALITY+(1|ID_LONG),data=filter(dtTactOnly, VALENCE == "Negative"))
tab_model(lm1, show.se=2)
summary(lm1)
# n.s. effect in negative 

# H2: Pre-likes on main task support and approval ####
lm1<-lmer(MAIN_SUPPORT~PRE_LIKES+ (1|ID_LONG)+ (1|VALENCE)+ (1|FB_MODALITY),data=dtRaw)
tab_model(lm1, show.se=2)
summary(lm1)
# the more likes, the higher the measurement during main task 

# H3: Post-likes after main task ####

lm1<-lmer(POST_LIKES~MAIN_SUPPORT + (1|ID_LONG)+(1| VALENCE) + (1|FB_MODALITY),data=dtRaw)
tab_model(lm1, show.se=2)
summary(lm1)
# the more social intent they perceived, the higher the likes post-task

# H4: Correlations and tests w qnaires #### 
cor.test(Bigdata$MAIN_SUPPORT,Bigdata$TAS_TOTAL)
cor.test(Bigdata$MAIN_SUPPORT,Bigdata$STQ_TOTAL)
# n.s
cor.test(Bigdata$MAIN_SUPPORT,Bigdata$SAQ_TOTAL)
cor.test(Bigdata$MAIN_SUPPORT,Bigdata$BAQ_TOTAL)
# sig.
cor.test(Bigdata$MAIN_SUPPORT,Bigdata$SNS_TOTAL)

cor.test(dtRaw2$SAQ_TOTAL,dtRaw2$STQ_TOTAL)
cor.test(dtRaw2$SAQ_TOTAL,dtRaw2$TAS_TOTAL)

# Graphs: H1 #### 
packages <- c("cowplot", "readr", "ggplot2", "dplyr", "lavaan", "smooth", "Hmisc")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}

# here we look at visual vs tact feedback in optimal support only
sC_summaryStats <- summarySE(dtOptOnly, measurevar = "MAIN_SUPPORT",na.rm = TRUE,
                             groupvars=c("FB_MODALITY", "VALENCE"))

ggplot(dtOptOnly,aes(x=FB_MODALITY,y=MAIN_SUPPORT, fill = VALENCE, colour = VALENCE))+
  geom_flat_violin(position = position_nudge(x = .25, y = 0),adjust =2, trim = FALSE)+
  geom_point(position = position_jitter(width = .15), size = .25)+
  geom_point(position=position_dodge(width=0.12),data = sC_summaryStats, 
             aes(x =  as.numeric(FB_MODALITY),
                 y = MAIN_SUPPORT_mean,
                 group = VALENCE, colour = VALENCE),
             size = 3, shape = 18)+
  geom_errorbar(position=position_dodge(width=0.12),data = sC_summaryStats, 
                aes(x =  as.numeric(FB_MODALITY), y = MAIN_SUPPORT_mean,
                    group = VALENCE,   ymin = MAIN_SUPPORT_mean-ci,
                    ymax = MAIN_SUPPORT_mean+ci, colour = VALENCE), width = 0.2, size = 0.8)+
  geom_hline(yintercept=0, linetype = "dashed")+
  ylab('Perceived Social Intent')+
  xlab('Feedback Modality')+
  ylim(c(0, 100))+
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.caption = element_text(hjust = 0.5), text = element_text(size = 16))
ggsave('TactVisOptimal.png', width = 8, height = 6, dpi=1000)

# here we look at tactile only
dtTactOnly$FB_OPTIMALITY<-as.character(dtTactOnly$FB_OPTIMALITY)
dtTactOnly[dtTactOnly$FB_OPTIMALITY=="Suboptimal",]$FB_OPTIMALITY<-"Low"
dtTactOnly[dtTactOnly$FB_OPTIMALITY=="Optimal",]$FB_OPTIMALITY<-"High"
dtTactOnly$FB_OPTIMALITY<-as.factor(dtTactOnly$FB_OPTIMALITY)

sC_summaryStats <- summarySE(dtTactOnly, measurevar = "MAIN_SUPPORT",na.rm = TRUE,
                             groupvars=c("FB_OPTIMALITY", "VALENCE"))

ggplot(dtTactOnly,aes(x=FB_OPTIMALITY,y=MAIN_SUPPORT, fill = VALENCE, colour = VALENCE))+
  geom_flat_violin(position = position_nudge(x = .25, y = 0),adjust =2, trim = FALSE)+
  geom_point(position = position_jitter(width = .15), size = .25)+
  geom_point(position=position_dodge(width=0.12),data = sC_summaryStats, 
             aes(x =  as.numeric(FB_OPTIMALITY),
                 y = MAIN_SUPPORT_mean,
                 group = VALENCE, colour = VALENCE),
             size = 3, shape = 18)+
  geom_errorbar(position=position_dodge(width=0.12),data = sC_summaryStats, 
                aes(x =  as.numeric(FB_OPTIMALITY), y = MAIN_SUPPORT_mean,
                    group = VALENCE,   ymin = MAIN_SUPPORT_mean-ci,
                    ymax = MAIN_SUPPORT_mean+ci, colour = VALENCE), width = 0.2, size = 0.8)+
  geom_hline(yintercept=0, linetype = "dashed")+
  ylab('Perceived Social Intent')+
  xlab('Feedback Support Level')+
  ylim(c(0, 100))+
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.caption = element_text(hjust = 0.5), text = element_text(size = 16))
ggsave('TactSubAndOptimal.png', width = 8, height = 6, dpi=1000)

