library(tidyr)
library(plotly)
library(lubridate)
library(gsheet)
library(dplyr)

###############################
# Feedback Delay Plots #
###############################

# Load the data
load("data_feedbackdelay.rda")

# Plot Feedback Delay compared to Participant Number
D_fabfeedbackdelay <- D_feedbackdelay %>%
  filter(Event == "GameDecision", CurrentInputDecision == "FabInput")

D_validfeedbackdelay <- D_feedbackdelay %>%
  filter(Event == "GameDecision", CurrentInputDecision != "RejectAllInput")

FabDelayCurve <- supsmu(D_fabfeedbackdelay$PID, as.numeric(D_fabfeedbackdelay$FeedbackDelay))
DelayCurve <- supsmu(D_validfeedbackdelay$PID, as.numeric(D_validfeedbackdelay$FeedbackDelay))

D_feedbackdelay %>%
  filter(Event == "GameDecision", CurrentInputDecision != "RejectAllInput") %>%
  plot_ly() %>%
  add_trace(x=~PID, y=~FeedbackDelay, color=~CurrentInputDecision, colors=c('black','gray'), symbol=~CurrentInputDecision, symbols=c('circle', 'circle-open'), mode='markers', type='scatter') %>%
  add_trace(name="Fab. Input", data=FabDelayCurve, x=~x, y=~y, type='scatter', mode='lines+markers', color=I('dimgray')) %>%
  add_trace(name="All Input", data=DelayCurve, x=~x, y=~y, type='scatter', mode='lines+markers', color=I('black')) %>%
  layout(xaxis=list(title="Participant Number"), yaxis=list(title="Feedback Delay"), showlegend=FALSE)

orca(fig, "hand-feedbackdelay.pdf", width=350, height=350)

###################################
# Creating the D_formula Dataset #
###################################

# Plot Feedback Delay compared to Perceived Control.
url <- 'https://docs.google.com/spreadsheets/d/1k_bkdEwiXWoO5sZmxpKthuySS7T7151qzfQJ_SY7fdI/edit#gid=2137346269'
D_ratings <- gsheet2tbl(url)

D_ratings <- D_ratings %>% select(Participant, Order, Condition, FrustNormalized, PercNormalized)

D_delays <- D_validfeedbackdelay %>% dplyr::group_by(PID, ConditionOrder) %>% summarise(AvgDelay = mean(FeedbackDelay, na.rm=T))

D_comb <- D_ratings %>%
  inner_join(D_validfeedbackdelay, by = c("Participant" = "PID", "Order" = "ConditionOrder"))

D_avgD <- D_ratings %>%
  inner_join(D_delays, by = c("Participant" = "PID", "Order" = "ConditionOrder")) %>%
  arrange(AvgDelay) %>%
  filter(!is.na(AvgDelay))

D_comb %>%
  plot_ly() %>%
  add_trace(x=~jitter(as.numeric(FeedbackDelay), amount=.02), y=~jitter(as.numeric(PercNormalized), amount=.02), color=~CurrentInputDecision, colors=c('black','black'),
            symbol=~CurrentInputDecision, symbols=c('circle', 'circle-open'),
            mode='markers', type='scatter') %>%
  layout(xaxis=list(title="Feedback Delay (s)"), yaxis=list(title="Perceived Control"), showlegend=FALSE)


PercAvgDelayCurve <- lm(as.numeric(D_avgD$AvgDelay) ~ as.numeric(D_avgD$PercNormalized))
base::summary(PercAvgDelayCurve)


D_avgD %>%
  plot_ly() %>%
  add_trace(x=~AvgDelay, y=~PercNormalized, color=I('dimgray'),
            mode='markers', type='scatter') %>%
  add_trace(x=~AvgDelay, y=~fitted(PercAvgDelayCurve), mode='lines+markers', type='scatter') %>%
  #add_trace(data=PercAvgDelayCurve, x=~x, y=~y, type='scatter', mode='lines+markers', color=I('black')) %>%
  layout(xaxis=list(title="Feedback Delay (s)"), yaxis=list(title="Perceived Control"), showlegend=FALSE)

FrustAvgDelayCurve <- supsmu(D_fabfeedbackdelay$PID, as.numeric(D_fabfeedbackdelay$FeedbackDelay))

D_avgD %>%
  plot_ly() %>%
  add_trace(x=~jitter(as.numeric(AvgDelay),amount=.005), y=~jitter(as.numeric(FrustNormalized), amount=.02), color=I('black'),
            mode='markers', type='scatter') %>%
  layout(xaxis=list(title="Feedback Delay (s)"), yaxis=list(title="Frustration"), showlegend=FALSE)


orca(fig, "hand-feedbackdelay-frustration.pdf", width=350, height=350)

correctBlinkCount <- D_feedbackdelay %>% group_by(PID, ConditionOrder) %>%
  filter(CurrentInputDecision == "AcceptAllInput") %>%
  summarize(nblinks = length(Event[Event == 'GameDecision']))

blink_count <- D_feedbackdelay %>% group_by(PID, ConditionOrder) %>%
  select(Timestamp,CurrentInputDecision,PID,TrialID,Event) %>%
  summarize(nblinks = length(Event[Event == 'EyeOpening']))

D_blinkcount <- D_up %>% left_join(blink_count)

D_D <- D_avgD %>%
  inner_join(blink_count, by = c("Participant" = "PID", "Order" = "ConditionOrder")) %>%
  inner_join(correctBlinkCount, by = c("Participant" = "PID", "Order" = "ConditionOrder"))
  
D_failrate <- D_D %>% group_by(Participant, Order) %>%
  summarize(fail_rate = 1-(nblinks.y / nblinks.x))

D_D <- D_D %>% inner_join(D_failrate)

D_formula = D_D %>% inner_join(D_ratings)

###################################
# lm() perc ~ failrate * AvgDelay #
###################################
# Load the data
load("data_action_analysis.rda")

ddd <- lm(as.numeric(PercNormalized*100) ~ as.numeric(fail_rate) * as.numeric(AvgDelay), data=D_formula)
