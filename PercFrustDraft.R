library(gsheet)
library(dplyr)
library(plotly)
library(tidyr)

# Interaction features
# - Toggle, ColorSize=Delta
# - Toggle, Color=Condition
# - Jitter, Amount

# Visualization template for plotly - removes unnecessary defaults
vistemplate <- plot_ly() %>%
  config(scrollZoom = TRUE, displaylogo = FALSE, modeBarButtonsToRemove = c("pan2d","select2d","hoverCompareCartesian", "toggleSpikelines","zoom2d","toImage", "sendDataToCloud", "editInChartStudio", "lasso2d", "drawclosedpath", "drawopenpath", "drawline", "drawcircle", "eraseshape", "autoScale2d", "hoverClosestCartesian","toggleHover", "")) %>%
  layout(dragmode = "pan", showlegend=FALSE,
         xaxis=list(mirror=T, ticks='outside', showline=T,titlefont = list(size=16),tickfont = list(size=14),dtick = 0.2),
         yaxis=list(mirror=T, ticks='outside', showline=T,titlefont = list(size=16),tickfont = list(size=14),dtick = 0.2)
         )

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

# Check for counter balance on the raw data
D_valid %>% group_by(Order, Condition) %>%
  summarise(count = length(Order)) %>% pivot_wider(names_from = Order, values_from = count) %>% View()
# TODO: check whether the imbalance 
# Check for counter balance in colors
D_valid %>% group_by(Order, Color) %>%
  summarise(count = length(Order)) %>% pivot_wider(names_from = Order, values_from = count) %>% View()


# Version A: Assume that each condition had the actual rate it has.
#D_valid$PureControl[D_valid$Condition == "0"] = 0.50
#D_valid$PureControl[D_valid$Condition == "100"] = 1
#D_valid$FabControl[D_valid$Condition == "15"] = 0.65
#D_valid$FabControl[D_valid$Condition == "30"] = 0.80
#D_valid$FabControl[D_valid$Condition == "50"] = 1

# Version B: Use calculations based on what actually happened in terms of task-outcomes.
D_real <- D_valid %>%
  left_join(D_rates, by = c("Participant" = "PID", "Order" = "ConditionOrder"))

# Exclude trials that were bad 

D_R0 = D_real %>% filter(Condition == "0", Recog.Rate == 0.5)
D_R0_B = D_real %>% filter(Condition == "0")
D_R100 = D_real %>% filter(Condition == "100", Recog.Rate == 1)
D_R100_B = D_real %>% filter(Condition == "100")
D_15 = D_real %>% filter(Condition == "15", Recog.Rate == 0.5, Fab.Rate == 0.15)
D_15_B = D_real %>% filter(Condition == "15")
D_30 = D_real %>% filter(Condition == "30", Recog.Rate == 0.5, Fab.Rate == 0.30)
D_30_B = D_real %>% filter(Condition == "30")
D_50 = D_real %>% filter(Condition == "50", Recog.Rate == 0.5, Fab.Rate == 0.50)
D_50_B = D_real %>% filter(Condition == "50")

D_R100_B %>% anti_join(D_R100) %>% View()
D_15_B %>% anti_join(D_15) %>% View()
D_15_B %>% anti_join(D_15) %>% View()


D_comb <- bind_rows(D_R0, D_R100, D_15, D_30, D_50)

D_comb %>% group_by(Condition) %>%
  summarise(count = length(Condition)) %>% View() 

D_real %>% anti_join(D_comb) %>% dplyr::select(Condition, Participant, Recog.Rate, Fab.Rate) %>% View()

excludes <- c(135, 164, 165, 212, 243, 245)

summary(lm(PerceivedControl ~ Order, data=D_comb))

lm(PerceivedControl ~ FabRecogRate, data=D_comb)
lm(Frustration ~ FabRecogRate, data=D_comb)

p_lin <- function(df, response, term) {
  min = min(df[[term]]) * 100
  max = max(df[[term]]) * 100
  fm <- lm(as.formula(paste(response, "~", term)), data = df)
  df_p <- data.frame(x = (min:max)/100)
  df_p[[term]] <- (min:max)/100
  curve <- data.frame(x = (min:max)/100, y = predict(fm, df_p))
  return(curve)
}

export <- p_lin(D_comb, "PerceivedControl", "FabRecogRate")
hand_frust <- p_lin(D_comb, "Frustration", "FabRecogRate")
save(export, file = 'hand.rda')
save(hand_frust, file = 'hand_frust.rda')

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

# Violin Plot Level of Control vs Perceived Control
D_excl$Condition <- as.character(D_excl$Condition)
D_excl$Condition[D_excl$Condition == "100"] <- "Ref."
D_excl$Condition <- as.factor(D_excl$Condition)
D_excl = D_excl %>%
  rowwise() %>%
  mutate(PureControl = ifelse(Fab.Rate == 0,Recog.Rate,NA),
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
PercPureCurve <- PercCurve %>% filter(x %in% c("0","Ref."))
PercFabCurve <- PercCurve %>% filter(x %in% c("0","15","30","50"))

fig <- vistemplate %>%
  add_trace(x=factor(D_excl$Condition, levels=c("0", "15", "30", "50", "Ref.")), y=jitter(D_excl$PercNormalized,amount=.02),
            scalemode='width',points='all', pointpos=0,name='C', jitter=.3,
            scalegroup='C', type="violin", spanmode="soft", width=1, fillcolor = "rgba(0, 0, 0, 0)", bandwidth=.08, color=I('darkgray')) %>%
  add_trace(data=PercPureCurve, x=~x, y=~y, type='scatter', line=list(dash='dot'), symbol=I('square-x-open'), mode='lines+markers', color=I('black'),marker=list(size=10),
            error_y= list(array=~perc_error)) %>%
  add_trace(data=PercFabCurve, x=~x, y=~y, type='scatter', mode='lines+markers', color=I('black'),marker=list(size=10),
            error_y= list(array=~perc_error)) %>%
  layout(yaxis = list(range=c(0,1.1), title="Perceived Control", violinmode = 'overlay', violingap = 0), xaxis=list(title="Fabrication Rate (%)"))

orca(fig, "level-of-control-perceived.pdf", width=350, height=350)

# Violin Plot Level of Control vs Frustration
FrustPureCurve = supsmu(D_excl$PureControl, D_excl$FrustNormalized)
FrustFabCurve = supsmu(D_excl$FabControl, D_excl$FrustNormalized)
FrustPureCurve$x <- as.character(FrustPureCurve$x)
FrustFabCurve$x <- as.character(FrustFabCurve$x)
FrustPureCurve$x[FrustPureCurve$x == "0.5"] <- "0"
FrustPureCurve$x[FrustPureCurve$x == "1"] <- "Ref."
FrustFabCurve$x[FrustFabCurve$x == "0.65"] <- "15"
FrustFabCurve$x[FrustFabCurve$x == "0.8"] <- "30"
FrustFabCurve$x[FrustFabCurve$x == "1"] <- "50"
FrustCurve <- FrustPureCurve %>% bind_rows(FrustFabCurve)
FrustCurve$x <- factor(FrustCurve$x, levels=c("0", "15", "30", "50", "Ref."))
FrustCurve <- FrustCurve %>% arrange(x)

D_error <- D_excl %>% dplyr::group_by(Condition) %>%
  dplyr::summarise(numberOfParticipants = length(unique(D_excl$Participant)),
                   frust_error = qnorm(0.975)*sd(FrustNormalized)/sqrt(numberOfParticipants),
                   frust_error_left = 5-frust_error,
                   frust_error_right = 5+frust_error) 
D_error$Condition <- factor(D_error$Condition, levels=c("0", "15", "30", "50", "Ref."))

FrustCurve <- FrustCurve %>% merge(D_error, by.x=c("x"), by.y=c("Condition"))
FrustPureCurve <- FrustCurve %>% filter(x %in% c("0","Ref."))
FrustFabCurve <- FrustCurve %>% filter(x %in% c("0","15","30","50"))


fig <- vistemplate %>%
  add_trace(x=factor(D_excl$Condition, levels=c("0", "15", "30", "50", "Ref.")), y=jitter(D_excl$FrustNormalized,amount=.02),
            scalemode='width',points='all', pointpos=0,name='C', fillcolor = "rgba(0, 0, 0, 0)", jitter=.3,
            scalegroup='C', type="violin", spanmode="soft", width=1, bandwidth=.08, color=I('darkgray')) %>%
  add_trace(x=FrustPureCurve$x, y=FrustPureCurve$y, type='scatter', symbol=I('square-x-open'),line=list(dash='dot'), mode='lines+markers', color=I('black'),marker=list(size=10),
            error_y= list(array=FrustPureCurve$frust_error)) %>%
  add_trace(x=FrustFabCurve$x, y=FrustFabCurve$y, type='scatter', mode='lines+markers', color=I('black'),marker=list(size=10),
            error_y= list(array=FrustFabCurve$frust_error)) %>%
  layout(yaxis = list(range=c(0,1.1), title="Frustration", violinmode = 'overlay', violingap = 0), xaxis=list(title="Fabrication Rate (%)"))

orca(fig, "level-of-control-frustration.pdf", width=350, height=350)

plot_ly() %>% add_trace(x=iris$Sepal.Length, y=iris$Sepal.Width, symbol=I('square-x-open'))
###################################
# CALCULATE ACTION-OUTCOME RATES  #
###################################

load("data_blinkratev2.rda")

# Calculate number of blinks vs number of accepted blinks per condition
D_formula %>% group_by(Condition) %>%
  summarize(n_blinks = sum(TotalBlinks),
            c_blinks = sum(CorrectBlinks),
            ratio = c_blinks / n_blinks) %>% View()
 
D_blinkrates <- D_formula %>%
  mutate(Participant = as.factor(Participant))

# Exclude trials that were bad 
D_real <- D_blinkrates %>%
  inner_join(D_rates, by = c("Participant" = "PID", "Order" = "ConditionOrder"))
D_R0 = D_real %>% filter(Condition == "0", Recog.Rate == 0.5)
D_R100 = D_real %>% filter(Condition == "100", Recog.Rate == 1)
D_15 = D_real %>% filter(Condition == "15", Recog.Rate == 0.5, Fab.Rate == 0.15)
D_30 = D_real %>% filter(Condition == "30", Recog.Rate == 0.5, Fab.Rate == 0.30)
D_50 = D_real %>% filter(Condition == "50", Recog.Rate == 0.5, Fab.Rate == 0.50)
D_blinkrates <- bind_rows(D_R0, D_R100, D_15, D_30, D_50)

# calculate averages per condition
D_blinkrates <- D_blinkrates %>% group_by(Condition) %>%
  mutate(avgActivation = mean(activation_rate)) 

# Violin Plot Level of Control vs Perceived Control
D_blinkrates$Condition <- as.character(D_blinkrates$Condition)
D_blinkrates$Condition[D_blinkrates$Condition == "100"] <- "Ref."
D_blinkrates$Condition <- as.factor(D_blinkrates$Condition)
D_blinkrates = D_blinkrates %>%
  rowwise() %>%
  mutate(PureControl = ifelse(Fab.Rate == 0,avgActivation,NA),
         FabControl = ifelse(Fab.Rate > 0 | Fab.Rate == 0 & Recog.Rate == 0.5,avgActivation, NA))



PercPureCurve = supsmu(D_blinkrates$PureControl, D_blinkrates$PercNormalized)
PercFabCurve = supsmu(D_blinkrates$FabControl, D_blinkrates$PercNormalized)
#PercPureCurve$x <- as.character(PercPureCurve$x)
#PercFabCurve$x <- as.character(PercFabCurve$x)
#PercPureCurve$x[PercPureCurve$x == "0.5"] <- "0"
#PercPureCurve$x[PercPureCurve$x == "1"] <- "Ref."
#PercFabCurve$x[PercFabCurve$x == "0.65"] <- "15"
#PercFabCurve$x[PercFabCurve$x == "0.8"] <- "30"
#PercFabCurve$x[PercFabCurve$x == "1"] <- "50"
#PercCurve <- PercPureCurve %>% bind_rows(PercFabCurve)
#PercCurve$x <- factor(PercCurve$x, levels=c("0", "15", "30", "50", "Ref."))
#PercCurve <- PercCurve %>% arrange(x)

D_error <- D_blinkrates %>% dplyr::group_by(avgActivation) %>%
  dplyr::summarise(numberOfParticipants = length(unique(D_blinkrates$Participant)),
                   perc_error = qnorm(0.975)*sd(PercNormalized)/sqrt(numberOfParticipants),
                   perc_error_left = 5-perc_error,
                   perc_error_right = 5+perc_error) 
PercPureCurve <- PercPureCurve %>% merge(D_error, by.x=c("x"), by.y=c("avgActivation"))
PercFabCurve <- PercFabCurve %>% merge(D_error, by.x=c("x"), by.y=c("avgActivation"))
#PercCurve <- PercCurve %>% merge(D_error, by.x=c("x"), by.y=c("Condition"))
#D_FabCurve = D_blinkrates %>% dplyr::filter(!is.na(FabControl))
#D_PureCurve = D_blinkrates %>% dplyr::filter(!is.na(PureControl))
#PercPureCurve = ksmooth(D_PureCurve$activation_rate, D_PureCurve$PercNormalized, "normal", 0.25, x.points=D_PureCurve$activation_rate)
#PercFabCurve = ksmooth(D_FabCurve$activation_rate, D_FabCurve$PercNormalized, "normal", 0.25, x.points=D_FabCurve$activation_rate)
#PercCurve = ksmooth(D_blinkrates$activation_rate, D_blinkrates$PercNormalized, "normal", 0.25, x.points=D_blinkrates$activation_rate)

fig <- vistemplate %>%
  #add_trace(x=D_blinkrates$avgActivation, y=D_blinkrates$PercNormalized,
  #          scalemode='width',points=FALSE, pointpos=0,name='C', jitter=.1,
  #          scalegroup='C', type="violin", spanmode="soft", width=0.4, bandwidth=.08, fillcolor = "rgba(0, 0, 0, 0)", color=I('darkgray'), symbols=c('circle','circle-open','square-open','cross-open','circle')) %>%
  #add_trace(x=jitter(D_blinkrates$activation_rate,amount=.03), y=jitter(D_blinkrates$PercNormalized,amount=.03), type="scatter",mode="text", color=I('darkgray'),
  #          text=D_blinkrates$Condition, textfont = list(size = 9)) %>%
  add_trace(x=jitter(D_blinkrates$activation_rate,amount=.03), y=jitter(D_blinkrates$PercNormalized,amount=.03), type="scatter",mode="markers", color=I('darkgray'),
            symbol=D_blinkrates$Condition,marker=list(size=7)) %>%
  add_trace(data=PercPureCurve, x=~x, y=~y, type='scatter', mode='lines+markers', line=list(dash='dot'), symbol=I('square-x-open'), color=I('black'),marker=list(size=10),error_y= list(array=~perc_error)) %>%
  add_trace(data=PercFabCurve, x=~x, y=~y, type='scatter', mode='lines+markers', color=I('black'),marker=list(size=10),
            error_y= list(array=~perc_error)) %>%
  layout(yaxis = list(range=c(0,1.1), title="Perceived Control", violinmode = 'overlay', violingap = 0), xaxis=list(range=c(0,1.1),dtick = 0.2, title="Level of Control (Input-based)"))

orca(fig, "input-level-of-control-perceived.pdf", width=350, height=350)

#Violin Plot Level of Control to Frustration
FrustPureCurve = supsmu(D_blinkrates$PureControl, D_blinkrates$FrustNormalized)
FrustFabCurve = supsmu(D_blinkrates$FabControl, D_blinkrates$FrustNormalized)

D_error <- D_blinkrates %>% dplyr::group_by(avgActivation) %>%
  dplyr::summarise(numberOfParticipants = length(unique(D_blinkrates$Participant)),
                   frust_error = qnorm(0.975)*sd(FrustNormalized)/sqrt(numberOfParticipants),
                   frust_error_left = 5-frust_error,
                   frust_error_right = 5+frust_error) 
FrustPureCurve <- FrustPureCurve %>% merge(D_error, by.x=c("x"), by.y=c("avgActivation"))
FrustFabCurve <- FrustFabCurve %>% merge(D_error, by.x=c("x"), by.y=c("avgActivation"))

fig <- vistemplate %>%
  #add_trace(x=D_blinkrates$avgActivation, y=D_blinkrates$FrustNormalized,
#            scalemode='width',points=FALSE, pointpos=0,name='C', jitter=.1,
#            scalegroup='C', type="violin", spanmode="soft", width=0.4, bandwidth=.08, fillcolor = "rgba(0, 0, 0, 0)", color=I('darkgray'), symbols=c('circle','circle-open','square-open','cross-open','circle')) %>%
  add_trace(x=jitter(D_blinkrates$activation_rate,amount=.03), y=jitter(D_blinkrates$FrustNormalized,amount=.03), type="scatter",mode="markers", color=I('darkgray'),
            symbol=D_blinkrates$Condition, marker=list(size=7)) %>%
  add_trace(data=FrustPureCurve, x=~x, y=~y, type='scatter', mode='lines+markers', line=list(dash='dot'), symbol=I('square-x-open'), color=I('black'),marker=list(size=10),error_y= list(array=~frust_error)) %>%
  add_trace(data=FrustFabCurve, x=~x, y=~y, type='scatter', mode='lines+markers', color=I('black'), marker=list(size=10),
  error_y= list(array=~frust_error)) %>%
  layout(yaxis = list(range=c(0,1.1), title="Frustration", violinmode = 'overlay', violingap = 0), xaxis=list(range=c(0,1.1), title="Level of Control (Input-based)"))

orca(fig, "input-level-of-control-frustration.pdf", width=350, height=350)

# OLD CODE
# Plot Level of Control vs Perceived Control
#PercPureCurve = supsmu(D_comb$PureControl, D_comb$PerceivedControl)
#PercFabCurve = supsmu(D_comb$FabControl, D_comb$PerceivedControl)
#D_comb %>%
#  mutate(PerceivedControl = jitter(as.numeric(PerceivedControl), amount=.03),
#         PureControl = jitter(as.numeric(PureControl), amount=.03),
#         FabControl = jitter(as.numeric(FabControl), amount=.03)) %>%
#  plot_ly() %>%
#  add_trace(name="Pure Control", x=~BlinkRecogAcc, y=~PerceivedControl) %>%
#  add_trace(name="Fab Control", x=~BlinkFabAccRecog, y=~PerceivedControl) %>%
#add_trace(name="Pure Control", x=PercPureCurve$x, y=PercPureCurve$y, mode='lines+markers', color=I('SkyBlue')) %>%
  #add_trace(name="Fab Control", x=PercFabCurve$x, y=PercFabCurve$y, mode='lines+markers', color=I('Salmon')) %>%
#  layout(xaxis = list(title="Level of Control", range=c(0,1.1)), yaxis = list(range=c(0,1.1)))

# Plot Level of Control vs Frustration
#FrustPureCurve = supsmu(D_comb$PureControl, D_comb$Frustration)
#FrustFabCurve = supsmu(D_comb$FabControl, D_comb$Frustration)
#D_comb %>%
#  mutate(Frustration = jitter(Frustration, amount=.03),
#         PureControl = jitter(PureControl, amount=.03),
#         FabControl = jitter(FabControl, amount=.03)) %>%
#  plot_ly() %>%
#  add_trace(x=~PureControl, name="Pure Control", y=~Frustration) %>%
#  add_trace(x=~FabControl, name="Fab Control", y=~Frustration) %>%
#  add_trace(name="Pure Control", x=FrustPureCurve$x, y=FrustPureCurve$y, mode='lines+markers', color=I('SkyBlue')) %>%
#  add_trace(name="Fab Control", x=FrustFabCurve$x, y=FrustFabCurve$y, mode='lines+markers', color=I('Salmon')) %>%
#  layout(xaxis = list(title="Level of Control", range=c(0,1.1)), yaxis = list(range=c(0,1.1)))
