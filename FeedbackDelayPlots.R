library(tidyr)
library(plotly)
library(lubridate)
library(gsheet)
library(ltm)
library(lm.beta)
library(dplyr)
library(beeswarm)

# Visualization template for plotly - removes unnecessary defaults
# Visualization template for plotly - removes unnecessary defaults
vistemplate <- plot_ly() %>%
  config(scrollZoom = TRUE, displaylogo = FALSE, modeBarButtonsToRemove = c("pan2d","select2d","hoverCompareCartesian", "toggleSpikelines","zoom2d","toImage", "sendDataToCloud", "editInChartStudio", "lasso2d", "drawclosedpath", "drawopenpath", "drawline", "drawcircle", "eraseshape", "autoScale2d", "hoverClosestCartesian","toggleHover", "")) %>%
  layout(dragmode = "pan", showlegend=FALSE,
         xaxis=list(mirror=T, ticks='outside', showline=T,titlefont = list(size=16),tickfont = list(size=14),dtick = 0.2),
         yaxis=list(mirror=T, ticks='outside', showline=T,titlefont = list(size=16),tickfont = list(size=14),dtick = 0.2)
  )

###############################
# Feedback Delay Plots #
###############################

# Load the data
load("data_feedbackdelay.rda")

# Plot Feedback Delay compared to Participant Number
D_fabfeedbackdelay <- D_feedbackdelay %>%
  filter(Event == "GameDecision", CurrentInputDecision == "FabInput")

# Remove blinks in relation to RejectAllInput?
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

D_formula$taskPercent<-ifelse(D_formula$Condition==100,100,D_formula$Condition+50)
D_formula$delayBin<-as.factor(ifelse(D_formula$AvgDelay<median(D_formula$AvgDelay),"lo","hi"))
D_formula <- D_formula %>% mutate(activation_rate = as.numeric(1-fail_rate))

ddd <- lm(as.numeric(PercNormalized*100) ~ activation_rate, data=D_formula)
lm.beta(ddd)
step(ddd)
summary(ddd)

ddd <- lm(as.numeric(PercNormalized*100) ~ as.numeric(AvgDelay), data=D_formula)
step(ddd)
summary(ddd)

ddd <- lm(as.numeric(PercNormalized*100) ~ activation_rate * as.numeric(AvgDelay), data=D_formula)
summary(ddd)

ddd <- lm(as.numeric(FrustNormalized*100) ~ activation_rate * as.numeric(AvgDelay), data=D_formula)
lm.beta(ddd)
step(ddd)
summary(ddd)

ddd <- lm(as.numeric(PercNormalized*100) ~ as.numeric(taskPercent / 100), data=D_formula)
lm.beta(ddd)
step(ddd)
summary(ddd)

ddd <- lm(as.numeric(PercNormalized*100) ~ as.numeric(taskPercent / 100) * as.numeric(AvgDelay), data=D_formula)
summary(ddd)

D_formula %>% group_by(Condition) %>%
  summarize(meanControl = mean(PercNormalized),
            meanFrust = mean(FrustNormalized))

###################################
# lm() perc ~ frustration #
###################################

# Is there a correlation between preceived control and frustration?
# A: Yes, there is a negative correlation.
model <- lm(FrustNormalized ~ PercNormalized, data=D_formula)
summary(model)
lm.beta(model)
model$a <- model$coefficients[2]
model$b <- model$coefficients[1]

#summary(lm(FrustNormalized~poly(PercNormalized,1,raw=TRUE), data=D_formula))

line = D_formula %>% ungroup() %>% select(PercNormalized) %>% arrange(PercNormalized) %>% distinct(PercNormalized) %>%
  mutate(
    x = PercNormalized,
    y = PercNormalized * model$a + model$b
  ) %>% select(x,y)

D_error <- D_formula %>% group_by(PercNormalized) %>%
  dplyr::summarize(perc_error = qnorm(0.975)*sd(FrustNormalized)/sqrt(26))

line = line %>% left_join(D_error, by=c("x" = "PercNormalized")) %>%
  mutate(left_error = y-perc_error,
         right_error = y+perc_error)

vistemplate %>%
  add_trace(data=line, x=~x, y=~right_error, mode='lines', type='scatter', line=list(color='transparent')) %>%
  add_trace(data=line, x=~x, y=~left_error, mode='lines', type='scatter', fill='tonexty', fillcolor='rgba(50,50,50,0.2)', line=list(color='transparent')) %>%
  add_trace(data=D_formula, x=~jitter(PercNormalized,amount=.02), y=~jitter(FrustNormalized,amount=.02), color=I('darkgrey'),type='scatter', mode='markers') %>%
  add_trace(data=line, x=~x, y=~y, type='scatter',mode='lines',color=I('black')) %>%
  layout(xaxis=list(range=c(0,1), title="Perceived Control"), yaxis=list(range=c(0,1), title="Frustration")) 

orca(fig, "perceived-frustration.pdf", width=350, height=350)

ggplot(D_formula,aes(x=PercNormalized,y=FrustNormalized))+geom_point()+geom_smooth(method = "lm", fill = NA)+geom_jitter(width=0.02,height=0.02)

#Is there a correlation between perceived control and frustration based on the condition?

plot_ly() %>%
  add_trace(data=D_formula, x=~jitter(PercNormalized,amount=.03), y=~jitter(FrustNormalized,amount=.03), color=~Condition, type='scatter', mode='markers') %>%
  add_trace(data=line, x=~x, y=~y, type='scatter',mode='lines') 

#################################
# lm() Color / Order influence? #
#################################

###################################
# ICC:Intra Class Correlation - Check rating consistency.
###################################

# First, we have two condition "30" for Participant 21, so remove one of them.
D_formula <- D_formula %>% filter( !(Participant == 21 & Order == 3))

D_icc <- D_formula %>% select(Participant, PercNormalized, Condition) %>% pivot_wider(names_from = Participant, values_from = PercNormalized) %>%
         ungroup() %>% select(-Condition)
psych::ICC(D_icc)

D_icc <- D_formula %>% select(Participant, FrustNormalized, Condition) %>% pivot_wider(names_from = Participant, values_from = FrustNormalized) %>%
  ungroup() %>% select(-Condition)
psych::ICC(D_icc)


####################################################
# Plot 1: delayBin as median across all conditions #
####################################################

ggplot(D_formula,aes(x=taskPercent,y=FrustNormalized,colour=delayBin,group=delayBin,shape=factor(Condition)))+geom_point()+ geom_smooth(method = "lm", fill = NA)
summary(lm(formula = as.numeric(PercNormalized * 100) ~ as.numeric(fail_rate) * 
     as.numeric(AvgDelay), data = D_formula[D_formula$fail_rate>.75,]))

####################################################
# Plot 2: delayBin as median across each condition #
####################################################

D_formula <- D_formula %>% group_by(Condition) %>%
  mutate(medianprCondition = median(AvgDelay))

D_formula %<>% group_by(Participant, Condition) %>%
  mutate(delayBin = ifelse(AvgDelay < medianprCondition,"lo","hi"))

D_formula %>% group_by(delayBin) %>%
  dplyr::summarise(mean_delay = mean(AvgDelay)) %>% View()

ggplot(D_formula,aes(x=1-fail_rate,y=PercNormalized,colour=delayBin,group=delayBin,shape=factor(Condition)))+geom_jitter(height=0.05)+ geom_smooth(method = "lm", fill = NA)
summary(lm(formula = as.numeric(PercNormalized * 100) ~ as.numeric(fail_rate) * 
             as.numeric(AvgDelay), data = D_formula[D_formula$fail_rate>.75,]))

############################################
# Violin Plots of input-based recognition  #
############################################

D_excl %>% dplyr::select(Participant, Condition, Fab.Rate, Recog.Rate) %>% 
  dplyr::mutate(Condition = as.character(Condition),
                Condition = ifelse(Condition == "100", "Ref.", Condition))


# Violin Plot Level of Control vs Perceived Control
#D_formula$Condition <- as.character(D_formula$Condition)
#D_formula$Condition[D_formula$Condition == "100"] <- "Ref."
#D_formula$Condition <- as.factor(D_formula$Condition)
D_excl = D_excl %>%
  dplyr::mutate(PureControl = ifelse(Fab.Rate == 0,Recog.Rate,NA),
         FabControl = ifelse(Fab.Rate > 0,Fab.Rate + Recog.Rate, NA))

PercPureCurve = supsmu(D_excl$PureControl, D_excl$PercNormalized)
PercFabCurve = supsmu(D_excl$FabControl, D_excl$PercNormalized)
PercPureCurve$x <- as.character(PercPureCurve$x)
PercFabCurve$x <- as.character(PercFabCurve$x)
PercPureCurve$x[PercPureCurve$x == "0.5"] <- "0"
PercPureCurve$x[PercPureCurve$x == "1"] <- "Ref."
PercFabCurve$x[PercFabCurve$x == "0.65"] <- "15"
PercFabCurve$x[PercFabCurve$x == "0.8"] <- "30"
PercFabCurve$x[PercFabCurve$x == "1"] <- "50"
PercCurve <- PercPureCurve %>% bind_rows(PercFabCurve)
PercCurve$x <- factor(PercCurve$x, levels=c("0", "15", "30", "50", "Ref."))
PercCurve <- PercCurve %>% arrange(x)

D_error <- D_excl %>% dplyr::group_by(Condition) %>%
  dplyr::summarise(numberOfParticipants = length(unique(D_excl$Participant)),
                   perc_error = qnorm(0.975)*sd(PercNormalized)/sqrt(numberOfParticipants),
                   perc_error_left = 5-perc_error,
                   perc_error_right = 5+perc_error) 
D_error$Condition <- factor(D_error$Condition, levels=c("0", "15", "30", "50", "Ref."))

PercCurve <- PercCurve %>% merge(D_error, by.x=c("x"), by.y=c("Condition"))

D_excl$Condition <- as.character(D_excl$Condition)
D_exclbee <- beeswarm::beeswarm(PercNormalized ~ Condition, data=D_excl, method="swarm", priority="density",corral="wrap")

fig <- vistemplate %>%
  add_trace(x=factor(D_excl$Condition, levels=c("0", "15", "30", "50", "Ref.")), y=jitter(D_excl$PercNormalized,amount=.02),
            scalemode='width',points=NULL, outliers=NULL, pointpos=0,name='C', jitter=.5, color=I('rgba(0,0,0,0)'),
            scalegroup='C', type="violin", spanmode="soft", width=1, bandwidth=.06, line=list(color='rgba(0.3,0.3,0.3,1)',width=1),
            marker=list(color='rgba(1,1,1,0)', line=list(color='rgba(0.3,0.3,0.3,0)',width=1))) %>%
  add_trace(data=PercCurve, x=~x, y=~y, type='scatter', mode='lines+markers', color=I('black'), line=list(width=1.5),
            error_y= list(array=~perc_error, thickness=1.3), marker=list(size=8)) %>%
  add_trace(x=D_exclbee$x, y=D_exclbee$y, type='scatter', mode='markers', color=I('black'),
            marker=list(color='rgba(0,0,0,0)',size=5, line=list(color='black',width=1)), xaxis='x2', yaxis='y2') %>%
  layout(yaxis = list(range=c(0,1.1), title="Perceived Control", violinmode = 'overlay', violingap = 0), xaxis=list(title="Fabrication Rate (%)"),
         xaxis2 = list(range=c(0.2,5.8), domain=c(0,1), anchor='y2', showticklabels = FALSE, showgrid = FALSE, zeroline=FALSE), yaxis2=list(domain = c(0,1), anchor='x2', showticklabels = FALSE, showgrid = FALSE, zeroline=FALSE),
         plot_bgcolor  = "rgba(0, 0, 0, 0)",
         paper_bgcolor = "rgba(0, 0, 0, 0)",
         fig_bgcolor   = "rgba(0, 0, 0, 0)")



orca(fig, "level-of-control-perceived.pdf", width=350, height=350)


