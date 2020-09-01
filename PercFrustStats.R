library(gsheet)
library(dplyr)
library(plotly)
library(coin)
require(lme4) # For ordinal logistic regression
library(ggplot2)
library(Rmisc) #For SummarySE function to get error bars
source("friedman.with.post.hoc.R")

#############
# GOALS ETC #
#############

# Game Frame: Run T-test with kiwi versus hand-strengthener, see if there are differences (?)
# In relation to game frame, we observe higher frustrations at lower perceived levels of control.


# Verify that we get similar kinds of curves and possibly compare them..

# Do we have a impact from giving people 100% on peoples rating?

# Check the counter-balancing, verify that it exists, and if not, how we can .... ??? make it so

# Plot average delay vs perceived control ratings, and compare to Greville.




# Motivation for separate study:
# - Include extra levels, to "complete the curve" and provide a better picture of its development.
# - Include a level with 100% recognition to provide some initial data on the corresponding "recognition curve", to contrast to the "fabrication curve"


#######################
# LOAD AND SETUP DATA #
#######################

url <- 'https://docs.google.com/spreadsheets/d/1k_bkdEwiXWoO5sZmxpKthuySS7T7151qzfQJ_SY7fdI/edit#gid=2137346269'
D <- gsheet2tbl(url)

# Load recognition rates, as logged by the game.
load("data_rates.rda")

# Load Kiwi Data from previous study.
load("kiwidata.rda")

D_rates <- D_rates %>%
  mutate(PID = as.factor(PID))

D_valid <- D %>%
  filter(!is.na(Frustration)) %>%
  mutate(Participant = as.factor(Participant),
         Condition = as.character(Condition))

D <- D_valid %>%
  inner_join(D_rates, by = c("Participant" = "PID", "Order" = "ConditionOrder"))

# Exclude trials that were bad 
D = bind_rows(D %>% filter(Condition == "0", Recog.Rate == 0.5, Fab.Rate == 0.0),
              D %>% filter(Condition == "100", Recog.Rate == 1, Fab.Rate == 0.0),
              D %>% filter(Condition == "15", Recog.Rate == 0.5, Fab.Rate == 0.15),
              D %>% filter(Condition == "30", Recog.Rate == 0.5, Fab.Rate == 0.30),
              D %>% filter(Condition == "50", Recog.Rate == 0.5, Fab.Rate == 0.50))

# Exclude participants who lack trials?
#D %>%
#  plot_ly() %>%
#  add_trace(x=~Participant, y=~Condition)

excludes <- c("1","13","16","17","21", "24")
D_excl <- D %>% filter(!Participant %in% excludes)

################################
# T-Test between Kiwi and Hand #
################################
kiwi_0 <- data %>% filter(shamRate == "0") %>% dplyr::select(FrustNormalized, controlNormalized)
kiwi_15 <- data %>% filter(shamRate == "15") %>% dplyr::select(FrustNormalized, controlNormalized)
kiwi_30 <- data %>% filter(shamRate == "30") %>% dplyr::select(FrustNormalized, controlNormalized)
kiwi_avg <- data %>% dplyr::group_by(PID) %>% dplyr::summarise(FrustAvg = mean(FrustNormalized), ContAvg = mean(controlNormalized))
hand_0 <- D_excl %>% filter(Condition == "0") %>% dplyr::select(FrustNormalized, PercNormalized)
hand_15 <- D_excl %>% filter(Condition == "15") %>% dplyr::select(FrustNormalized, PercNormalized)
hand_30 <- D_excl %>% filter(Condition == "30") %>% dplyr::select(FrustNormalized, PercNormalized)
hand_avg <- D_excl %>% filter(Condition %in% c("0","15","30")) %>% dplyr::group_by(Participant) %>% dplyr::summarise(FrustAvg = mean(FrustNormalized), PercAvg = mean(PercNormalized))

t.test(kiwi_0$controlNormalized, hand_0$PercNormalized)
t.test(kiwi_15$controlNormalized, hand_15$PercNormalized)
t.test(kiwi_30$controlNormalized, hand_30$PercNormalized)
t.test(kiwi_avg$ContAvg, hand_avg$PercAvg)

t.test(kiwi_0$FrustNormalized, hand_0$FrustNormalized)
t.test(kiwi_15$FrustNormalized, hand_15$FrustNormalized)
t.test(kiwi_30$FrustNormalized, hand_30$FrustNormalized)
t.test(kiwi_avg$FrustAvg, hand_avg$FrustAvg)

# Normal distribution test
density <- density(hand_avg$PercAvg)
plot_ly() %>% add_trace(x=~density$x, y=~density$y, type='scatter', mode='lines', fill='tozeroy')

########################################
# LINEAR MODEL CONDITION WITHOUT ORDER #
########################################
#first check with Linear model effect for sham rate but not order
D_excl$Condition = as.factor(D_excl$Condition)
D_excl = D_excl %>% arrange(Condition)
D_excl$conditionCont<-as.numeric(as.factor(D_excl$Condition))
# Question: Do I need to filter out the 100% Condition?
summary(lm(FrustrationEpisode~Condition,data=D_excl %>% filter(Condition != "100")))

#################################################
# SPLIT-UP LINEAR MODEL CONDITION WITHOUT ORDER #
#################################################
# In this version we split the dataset in two linear models - one modelling 0,15,30,50 sham, another modeling 0,100 true recognition.
D_fab = D %>% filter(Condition != 100)
D_rec = D %>% filter(Condition %in% c("0","100"))

# Question: Why are we using the conditionCont variable instead of just using Condition?
summary(lm(PerceivedControlEpisode~Condition,data=D_fab))
summary(lm(PerceivedControlEpisode~Condition,data=D_rec))

# Question: Why are we not doing the same for Frustration?
summary(lm(FrustrationEpisode~Condition,data=D_fab))
summary(lm(FrustrationEpisode~Condition,data=D_rec))

#####################################
# LINEAR MODEL CONDITION WITH ORDER #
#####################################
#first check with Linear model effect with shamChange
D = D %>% arrange(Participant,Order)
D = D %>% group_by(Participant) %>% mutate(ConditionChange = conditionCont-lag(conditionCont))
D[is.na(D$ConditionChange),]$ConditionChange<-0
summary(step(lm(PerceivedControlEpisode~ConditionChange,data=D)))

## stats analyis modelling  -------------
summary(step(lm(PerceivedControlEpisode~ConditionChange,data=D)))

###############################
# FRIEDMAN TEST WITH POST HOC # # NOT WORKING DUE TO 'SYMMETRY PROBLEM'
###############################
# NB: FRIEDMAN TESTS REQUIRE DATA FRAME OBJECT TYPES, CANNOT PARSE TIBBLE FACTORS!!!

# https://www.r-statistics.com/2010/02/post-hoc-analysis-for-friedmans-test-r-code/


#friedman.test.with.post.hoc(FrustrationEpisode ~ as.factor(Condition) | as.factor(Partcipant), D_excl %>% )
D_fry <- D_excl %>% dplyr::select(FrustrationEpisode, Condition, Participant) %>% filter(Condition != "100")
D_fry <- as.data.frame(D_fry)
D_fry$Condition = as.factor(D_fry$Condition)
str(D_fry)
friedman.test(FrustrationEpisode~Condition|Participant, D_fry)



#D_excl %>% dplyr::select(FrustrationEpisode, Condition, Participant) %>% filter(Condition != "100") %>% View()

#D_excl <- D_excl %>% group_by(Participant) %>%
#  mutate(Pattern = paste(Condition, collapse='-'))

#D_excl %>% dplyr::select(Pattern) %>% group_by(Pattern) %>%
#  summarize(cond = length(Pattern))

#t <- D_excl %>% dplyr::select(FrustrationEpisode, Condition, Participant)
#isSymmetric(t)

#D %>% plot_ly() %>%
#  add_trace(x=~Condition, y=~Count())

#symmetry_test(FrustrationEpisode ~ as.factor(Condition) | as.factor(Participant), data = D_excl)
#obj <- friedman_test(as.numeric(FrustrationEpisode) ~ as.factor(Condition) | as.factor(Participant), data = D_excl)

#################
# WILCOXON TEST #
#################

D_wil <- D_excl %>% dplyr::select(FrustrationEpisode,PerceivedControlEpisode, Condition, Participant) %>% filter(Condition != "100")

wilcoxsign_test(D_wil[D_wil$Condition=='0',]$FrustrationEpisode ~ D_wil[D_wil$Condition=='15',]$FrustrationEpisode, distribution="exact")
wilcoxsign_test(D_wil[D_wil$Condition=='0',]$FrustrationEpisode ~ D_wil[D_wil$Condition=='30',]$FrustrationEpisode, distribution="exact")
wilcoxsign_test(D_wil[D_wil$Condition=='0',]$FrustrationEpisode ~ D_wil[D_wil$Condition=='50',]$FrustrationEpisode, distribution="exact")
wilcoxsign_test(D_wil[D_wil$Condition=='15',]$FrustrationEpisode ~ D_wil[D_wil$Condition=='30',]$FrustrationEpisode, distribution="exact")
wilcoxsign_test(D_wil[D_wil$Condition=='15',]$FrustrationEpisode ~ D_wil[D_wil$Condition=='50',]$FrustrationEpisode, distribution="exact")
wilcoxsign_test(D_wil[D_wil$Condition=='30',]$FrustrationEpisode ~ D_wil[D_wil$Condition=='50',]$FrustrationEpisode, distribution="exact")
wilcoxsign_test(GroupA ~ GroupB, distribution="exact")
wilcoxsign_test(D_wil[D_wil$Condition=='0',]$PerceivedControlEpisode ~ D_wil[D_wil$Condition=='15',]$PerceivedControlEpisode, distribution="exact")
wilcoxsign_test(D_wil[D_wil$Condition=='0',]$PerceivedControlEpisode ~ D_wil[D_wil$Condition=='30',]$PerceivedControlEpisode, distribution="exact")
wilcoxsign_test(D_wil[D_wil$Condition=='0',]$PerceivedControlEpisode ~ D_wil[D_wil$Condition=='50',]$PerceivedControlEpisode, distribution="exact")
wilcoxsign_test(D_wil[D_wil$Condition=='15',]$PerceivedControlEpisode ~ D_wil[D_wil$Condition=='30',]$PerceivedControlEpisode, distribution="exact")
wilcoxsign_test(D_wil[D_wil$Condition=='30',]$PerceivedControlEpisode ~ D_wil[D_wil$Condition=='50',]$PerceivedControlEpisode, distribution="exact")
wilcoxsign_test(D_wil[D_wil$Condition=='30',]$PerceivedControlEpisode ~ D_wil[D_wil$Condition=='50',]$PerceivedControlEpisode, distribution="exact")



###############################
# ORDINAL LOGISTIC REGRESSION #
###############################

#check with ordinal logistic reqression - no significance?
summary(polr(as.factor(FrustrationEpisode) ~ Condition, data = D, Hess=TRUE))

percContr.null<- lmer(PerceivedControlEpisode~(1|Participant),data=D,REML=FALSE)
percContr.Condition <-lmer(PerceivedControlEpisode~Condition+(1|Participant),data=D,REML=FALSE)
percContr.ConditionChange <-lmer(PerceivedControlEpisode~ConditionChange+(1|Participant),data=D,REML=FALSE)
anova(percContr.null,percContr.ConditionChange)
anova(percContr.null,percContr.Condition)
percContr.ConditionOrder <-lmer(PerceivedControlEpisode~Condition+ConditionChange+(1|Participant),data=D,REML=FALSE)
anova(percContr.Condition,percContr.ConditionOrder)

# lmer for frustration
Frust.null<- lmer(FrustrationEpisode~(1|Participant),data=D,REML=FALSE)
Frust.Condition <-lmer(FrustrationEpisode~Condition+(1|Participant),data=D,REML=FALSE)
anova(Frust.null,Frust.Condition)
FrustContr.ConditionOrder <-lmer(FrustrationEpisode~Condition+ConditionChange+(1|Participant),data=D,REML=FALSE)
# friedmann test on sham amount
anova(Frust.Condition,FrustContr.ConditionOrder)

####################################################
# CHECK IF PERC. CONTROL IS INVERSE OF FRUSTRATION #
####################################################

#check if perceived control is just inverse of Frustration..
summary(step(lm(FrustrationEpisode~PerceivedControlEpisode*as.factor(Condition),D)))

###########
# GGPLOTS #
###########

# plotting fabrictaion Rate vs Frustration w/ Error Bars (95% Confidence Interval)  ----------- 
# Plot Sham Rate vs Frustration w/ Error Bars (95% Confidence Interval)
dsm = summarySE(D, measurevar="Frustration", groupvars=c("Condition"))
ggplot(dsm, aes(x=dsm$Condition, y=dsm$Frustration)) +
  geom_errorbar(aes(ymin=dsm$Frustration-dsm$ci, ymax=dsm$F+dsm$ci), width=.1, size=.5) +
  geom_point(size=3.25) +
  geom_line(aes(group=1), size=1.25) +
  ylim(0,1) +
  theme_bw() +
  theme(text = element_text(size = 13)) +
  xlab("Fabrication Rate (%)") +
  ylab("Frustration")