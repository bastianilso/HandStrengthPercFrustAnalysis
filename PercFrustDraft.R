library(gsheet)
library(dplyr)
library(plotly)

# Interaction features
# - Toggle, ColorSize=Delta
# - Toggle, Color=Condition
# - Jitter, Amount

# How to export plots to PDF from Plotly
orca(fig, "surface-plot.pdf", width=350, height=350)

# How to export HTML plots from Plotly
htmlwidgets::saveWidget(config(p, displayModeBar = FALSE), "graph.html")

#######################
# LOAD AND SETUP DATA #
#######################

# Load data from participant surveys.
url <- 'https://docs.google.com/spreadsheets/d/1k_bkdEwiXWoO5sZmxpKthuySS7T7151qzfQJ_SY7fdI/edit#gid=2137346269'
D <- gsheet2tbl(url)

# Load recognition rates, as logged by the game.
load("data_rates.rda")

# PID as factor.
D_rates <- D_rates %>%
  mutate(PID = as.factor(PID))

D_valid <- D %>%
  filter(!is.na(Frustration)) %>%
  mutate(Participant = as.factor(Participant),
         Condition = as.character(Condition))
         
# Version A: Assume that each condition had the actual rate it has.
#D_valid$PureControl[D_valid$Condition == "0"] = 0.50
#D_valid$PureControl[D_valid$Condition == "100"] = 1
#D_valid$FabControl[D_valid$Condition == "15"] = 0.65
#D_valid$FabControl[D_valid$Condition == "30"] = 0.80
#D_valid$FabControl[D_valid$Condition == "50"] = 1

# Version B: Use calculations based on what actually happened in terms of task-outcomes.
D_real <- D_valid %>%
  inner_join(D_rates, by = c("Participant" = "PID", "Order" = "ConditionOrder"))

# Exclude trials that were bad 

D_R0 = D_real %>% filter(Condition == "0", Recog.Rate == 0.5)
D_R100 = D_real %>% filter(Condition == "100", Recog.Rate == 1)
D_15 = D_real %>% filter(Condition == "15", Recog.Rate == 0.5, Fab.Rate == 0.15)
D_30 = D_real %>% filter(Condition == "30", Recog.Rate == 0.5, Fab.Rate == 0.30)
D_50 = D_real %>% filter(Condition == "50", Recog.Rate == 0.5, Fab.Rate == 0.50)

D_comb <- bind_rows(D_R0, D_R100, D_15, D_30, D_50)

excludes <- c(135, 164, 165, 212, 243, 245)

######################
# VERIFICATION PLOTS #
######################

# Check recognition rates across participants
D_comb %>%
  plot_ly() %>%
  add_trace(x=~Participant, y=~FabRecogRate, mode='markers', type='scatter')

# Check to what extent trying one condition after another has an impact.
D_comb <- D_comb %>% group_by(Participant) %>%
  mutate(ConditionLag = lag(FabRecogRate))%>%
  rowwise() %>%
  mutate(ConditionDelta = if (!is.na(ConditionLag)) FabRecogRate-ConditionLag else NA)


# Plot all ratings and see what happens when people score 100 compared to fab15%.
D_comb$tm <- 1:nrow(D_comb)
fig <- D_comb %>%
  plot_ly() %>%
  group_by(Participant) %>%
  add_trace(x=~tm, y=~PerceivedControl, mode='lines+markers', type='scatter', color=I('gray')) %>%
  filter(Condition == '100') %>%
  add_trace(x=~tm, y=~PerceivedControl, mode='markers', type='scatter', color=I('black'))
fig <- fig %>%
  add_trace(x=Dcomb30$tm, y=Dcomb30$PerceivedControl, mode='markers', type='scatter', color=I('SkyBlue'))
fig


###################################
# PERCEIVED CONTROL - FRUSTRATION #
###################################

# Check that each condition has proper rates.
D_comb %>%
  plot_ly() %>%
  add_trace(x=~Condition, y=~FabRecogRate, mode='markers', type='scatter')


# Plot Level of Control vs Frustration Level
FrustPercCurve.All = supsmu(D_comb$PercNormalized, D_comb$FrustNormalized)
#FrustPercCurve.15 = supsmu(D_15$PerceivedControl, D_15$Frustration)
#FrustPercCurve.30 = supsmu(D_30$PerceivedControl, D_30$Frustration)
#FrustPercCurve.50 = supsmu(D_50$PerceivedControl, D_50$Frustration)
#FrustPercCurve.R100 = supsmu(D_R100$PerceivedControl, D_R100$Frustration)
#FrustPercCurve.R0 = supsmu(D_R0$PerceivedControl, D_R0$Frustration)

qfit15 <- lm(Frustration ~ PerceivedControl, data = D_15) 
qfit30 <- lm(Frustration ~ PerceivedControl, data = D_30) 
qfit50 <- lm(Frustration ~ PerceivedControl, data = D_50) 
qfitR100 <- lm(Frustration ~ PerceivedControl, data = D_R100) 
qfitR0 <- lm(Frustration ~ PerceivedControl, data = D_R0) 
FrustPercCurve.15 <- data.frame(x = (1:100)/100, y = predict(qfit15, data.frame(PerceivedControl = (1:100)/100)))
FrustPercCurve.30 <- data.frame(x = (1:100)/100, y = predict(qfit30, data.frame(PerceivedControl = (1:100)/100)))
FrustPercCurve.50 <- data.frame(x = (1:100)/100, y = predict(qfit50, data.frame(PerceivedControl = (1:100)/100)))
FrustPercCurve.R100 <- data.frame(x = (1:100)/100, y = predict(qfitR100, data.frame(PerceivedControl = (1:100)/100)))
FrustPercCurve.R0 <- data.frame(x = (1:100)/100, y = predict(qfitR0, data.frame(PerceivedControl = (1:100)/100)))


col = c('Tan', 'Maroon', 'Peru', 'Chocolate', 'SaddleBrown')

D_comb %>%
  mutate(PerceivedControl = jitter(PercNormalized, amount=.02),
         Frustration = jitter(FrustNormalized, amount=.02)) %>%
  arrange(Participant, Order) %>%
  plot_ly() %>%
  add_trace(x=~PerceivedControl, y=~Frustration, color=I('Gray'), mode='markers') %>%
  add_trace(x=~FrustPercCurve.All$x, y=~FrustPercCurve.All$y, color=I('Black'), mode='lines+markers') %>%
  #add_trace(x=~PercNormalized, y=~FrustNormalized, mode='markers+lines', color=~Participant, size=~as.numeric(Condition), transforms = list(type = 'groupby',groups = ~Condition)) %>%
  #add_trace(name="15% Fab", x=FrustPercCurve.15$x, y=FrustPercCurve.15$y, mode='lines', color=I('Peru')) %>%
  #add_trace(name="30% Fab", x=FrustPercCurve.30$x, y=FrustPercCurve.30$y, mode='lines', color=I('Chocolate')) %>%
  #add_trace(name="50% Fab", x=FrustPercCurve.50$x, y=FrustPercCurve.50$y, mode='lines', color=I('SaddleBrown')) %>%
  #add_trace(name="100% Recog", x=FrustPercCurve.R100$x, y=FrustPercCurve.R100$y, mode='lines', color=I('Maroon')) %>%
  #add_trace(name="0% Fab", x=FrustPercCurve.R0$x, y=FrustPercCurve.R0$y, mode='lines',color=I('Tan')) %>%
  layout(xaxis = list(range=c(0,1.1)), yaxis = list(range=c(0,1.1), scaleanchor = "x"))
orca(fig, "hand-perceived-control-frustration.pdf", width=350, height=350)
  
#################################
# CALCULATE TASK-OUTCOME RATES  #
#################################
D_comb = D_comb %>%
  rowwise() %>%
  mutate(PureControl = if (Fab.Rate == 0) Recog.Rate else NA,
         FabControl = if (Fab.Rate > 0) Fab.Rate + Recog.Rate else NA)


# Plot Level of Control vs Perceived Control
PercPureCurve = supsmu(D_comb$PureControl, D_comb$PerceivedControl)
PercPureCurve$x <- c(0,100)
PercPureCurve$x <- as.factor(PercPureCurve$x)
PercFabCurve = supsmu(D_comb$FabControl, D_comb$PerceivedControl)
PercFabCurve$x <- c(15,30,50)
PercFabCurve$x <- as.factor(PercFabCurve$x)
fig1 <-D_comb %>%
  mutate(PerceivedControl = jitter(PerceivedControl, amount=.03),
         PureControl = jitter(PureControl, amount=.03),
         FabControl = jitter(FabControl, amount=.03)) %>%
  plot_ly() %>%
  add_trace(type='scatter', mode='markers', name="Pure Control", x=~PureControl, y=~PerceivedControl, color=I('darkgray'), symbol=I('circle')) %>%
  add_trace(type='scatter', mode='markers', name="Fab Control", x=~FabControl, y=~PerceivedControl, color=I('gray'),symbol=I('circle-open')) %>%
  add_trace(type='scatter', name="Pure Control", x=PercPureCurve$x, y=PercPureCurve$y, mode='lines+markers', color=I('Black'), symbol=I('circle')) %>%
  add_trace(type='scatter', name="Fab Control", x=PercFabCurve$x, y=PercFabCurve$y, mode='lines+markers', color=I('Black'), line=list(dash='dashdot')) %>%
  layout(xaxis = list(range=c(0,1.1), title="Level of Control"), yaxis = list(range=c(0,1.1),scaleanchor="x"),showlegend=FALSE)
orca(fig1, "hand-level-of-control-perceived-control.pdf", width=350, height=350)

D_comb$Condition <- as.factor(D_comb$Condition)

# Plot Level of Control vs Frustration
FrustPureCurve = supsmu(D_comb$PureControl, D_comb$Frustration)
FrustFabCurve = supsmu(D_comb$FabControl, D_comb$Frustration)

fig <- D_comb %>%
  mutate(Frustration = jitter(Frustration, amount=.03),
         PureControl = jitter(PureControl, amount=.03),
         FabControl = jitter(FabControl, amount=.03)) %>%
  plot_ly() %>%
  add_trace(x=~PureControl, name="Pure Control", y=~Frustration, color=I('darkgray'), symbol=I('circle')) %>%
  add_trace(x=~FabControl, name="Fab Control", y=~Frustration, color=I('gray'), symbol=I('circle-open')) %>%
  add_trace(name="Pure Control", x=FrustPureCurve$x, y=FrustPureCurve$y, mode='lines+markers', color=I('black')) %>%
  add_trace(name="Fab Control", x=FrustFabCurve$x, y=FrustFabCurve$y, mode='lines+markers', color=I('black'), line=list(dash='dashdot')) %>%
  layout(xaxis = list(title="Level of Control", range=c(0,1.1)), yaxis = list(range=c(0,1.1), scaleanchor="x"), showlegend=FALSE)
orca(fig, "hand-level-of-control-frustration.pdf", width=350, height=350)

###################################
# CALCULATE ACTION-OUTCOME RATES  #
###################################

load("data_blinkrate.rda")

D_blinkrates <- D_blinkrates %>%
  mutate(PID = as.factor(PID))

D_comb <- D_comb %>%
  inner_join(D_blinkrates, by = c("Participant" = "PID", "Order" = "ConditionOrder") )

#BlinkCount Verification
D_comb %>%
  plot_ly() %>%
  add_trace(x=~FabRecogRate, y=~BlinkCount, hoverinfo='text', hovertext=paste('BlinkCount:',D_comb$BlinkCount), mode='markers', type='scatter')

# Plot Level of Control vs Perceived Control
PercPureCurve = supsmu(D_comb$PureControl, D_comb$PerceivedControl)
PercFabCurve = supsmu(D_comb$FabControl, D_comb$PerceivedControl)
D_comb %>%
  mutate(PerceivedControl = jitter(PerceivedControl, amount=.03),
         PureControl = jitter(PureControl, amount=.03),
         FabControl = jitter(FabControl, amount=.03)) %>%
  plot_ly() %>%
  add_trace(name="Pure Control", x=~PureControl, y=~PerceivedControl) %>%
  add_trace(name="Fab Control", x=~FabControl, y=~PerceivedControl) %>%
  add_trace(name="Pure Control", x=PercPureCurve$x, y=PercPureCurve$y, mode='lines+markers', color=I('SkyBlue')) %>%
  add_trace(name="Fab Control", x=PercFabCurve$x, y=PercFabCurve$y, mode='lines+markers', color=I('Salmon')) %>%
  layout(xaxis = list(title="Level of Control", range=c(0,1.1)), yaxis = list(range=c(0,1.1)))

# Plot Level of Control vs Frustration
FrustPureCurve = supsmu(D_comb$PureControl, D_comb$Frustration)
FrustFabCurve = supsmu(D_comb$FabControl, D_comb$Frustration)
D_comb %>%
  mutate(Frustration = jitter(Frustration, amount=.03),
         PureControl = jitter(PureControl, amount=.03),
         FabControl = jitter(FabControl, amount=.03)) %>%
  plot_ly() %>%
  add_trace(x=~PureControl, name="Pure Control", y=~Frustration) %>%
  add_trace(x=~FabControl, name="Fab Control", y=~Frustration) %>%
  add_trace(name="Pure Control", x=FrustPureCurve$x, y=FrustPureCurve$y, mode='lines+markers', color=I('SkyBlue')) %>%
  add_trace(name="Fab Control", x=FrustFabCurve$x, y=FrustFabCurve$y, mode='lines+markers', color=I('Salmon')) %>%
  layout(xaxis = list(title="Level of Control", range=c(0,1.1)), yaxis = list(range=c(0,1.1)))
