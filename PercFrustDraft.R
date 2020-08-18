library(gsheet)
library(dplyr)
library(plotly)

url <- 'https://docs.google.com/spreadsheets/d/1k_bkdEwiXWoO5sZmxpKthuySS7T7151qzfQJ_SY7fdI/edit#gid=2137346269'
D <- gsheet2tbl(url)

D_valid <- D %>%
  filter(!is.na(Frustration)) %>%
  mutate(Participant = as.factor(Participant),
         Condition = as.character(Condition))

D_valid$PureControl[D_valid$Condition == "0"] = 0.50
D_valid$PureControl[D_valid$Condition == "100"] = 1
D_valid$FabControl[D_valid$Condition == "15"] = 0.65
D_valid$FabControl[D_valid$Condition == "30"] = 0.80
D_valid$FabControl[D_valid$Condition == "50"] = 1

D_R0 = D_valid %>% filter(Condition == "0")
D_R100 = D_valid %>% filter(Condition == "100")
D_15 = D_valid %>% filter(Condition == "15")
D_30 = D_valid %>% filter(Condition == "30")
D_50 = D_valid %>% filter(Condition == "50")


# Plot Level of Control vs Frustration Level
FrustPercCurve.All = supsmu(D_valid$PerceivedControl, D_valid$Frustration)
FrustPercCurve.15 = supsmu(D_15$PerceivedControl, D_15$Frustration)
FrustPercCurve.30 = supsmu(D_30$PerceivedControl, D_30$Frustration)
FrustPercCurve.50 = supsmu(D_50$PerceivedControl, D_50$Frustration)
FrustPercCurve.R100 = supsmu(D_R100$PerceivedControl, D_R100$Frustration)
FrustPercCurve.R0 = supsmu(D_R0$PerceivedControl, D_R0$Frustration)
col = c('Tan', 'Maroon', 'Peru', 'Chocolate', 'SaddleBrown')

D_valid %>%
  mutate(PerceivedControl = jitter(PerceivedControl, amount=.03),
         Frustration = jitter(Frustration, amount=.03)) %>%
  plot_ly() %>%
  add_trace(x=~PerceivedControl, y=~Frustration, color=~Condition, colors=col, mode='markers') %>%
  add_trace(name="Smooth Line", x=FrustPercCurve.All$x, y=FrustPercCurve.All$y, mode='lines')
  #add_trace(name="15% Fab", x=FrustPercCurve.15$x, y=FrustPercCurve.15$y, mode='lines', color=I('Peru')) %>%
  #add_trace(name="30% Fab", x=FrustPercCurve.30$x, y=FrustPercCurve.30$y, mode='lines', color=I('Chocolate')) %>%
  #add_trace(name="50% Fab", x=FrustPercCurve.50$x, y=FrustPercCurve.50$y, mode='lines', color=I('SaddleBrown')) %>%
  #add_trace(name="100% Recog", x=FrustPercCurve.R100$x, y=FrustPercCurve.R100$y, mode='lines', color=I('Maroon')) %>%
  #add_trace(name="0% Fab", x=FrustPercCurve.R0$x, y=FrustPercCurve.R0$y, mode='lines',color=I('Tan'))


# Plot Level of Control vs Perceived Control
PercPureCurve = supsmu(D_valid$PureControl, D_valid$PerceivedControl)
PercFabCurve = supsmu(D_valid$FabControl, D_valid$PerceivedControl)
D_valid %>%
  mutate(PerceivedControl = jitter(PerceivedControl, amount=.03),
         PureControl = jitter(PureControl, amount=.03),
         FabControl = jitter(FabControl, amount=.03)) %>%
  plot_ly() %>%
  add_trace(name="Pure Control", x=~PureControl, y=~PerceivedControl, color=I('LightSkyBlue')) %>%
  add_trace(name="Fab Control", x=~FabControl, y=~PerceivedControl, color=I('LightSalmon')) %>%
  add_trace(name="Pure Control", x=PercPureCurve$x, y=PercPureCurve$y, mode='lines+markers', color=I('SkyBlue')) %>%
  add_trace(name="Fab Control", x=PercFabCurve$x, y=PercFabCurve$y, mode='lines+markers', color=I('Salmon')) %>%
  layout(xaxis = list(title="Level of Control", range=c(0,1.1)), yaxis = list(range=c(0,1.1)))

# Plot Level of Control vs Frustration
FrustPureCurve = supsmu(D_valid$PureControl, D_valid$Frustration)
FrustFabCurve = supsmu(D_valid$FabControl, D_valid$Frustration)
D_valid %>%
  mutate(Frustration = jitter(Frustration, amount=.03),
         PureControl = jitter(PureControl, amount=.03),
         FabControl = jitter(FabControl, amount=.03)) %>%
  plot_ly() %>%
  add_trace(x=~PureControl, name="Pure Control", y=~Frustration) %>%
  add_trace(x=~FabControl, name="Fab Control", y=~Frustration) %>%
  add_trace(name="Pure Control", x=FrustPureCurve$x, y=FrustPureCurve$y, mode='lines+markers', color=I('SkyBlue')) %>%
  add_trace(name="Fab Control", x=FrustFabCurve$x, y=FrustFabCurve$y, mode='lines+markers', color=I('Salmon')) %>%
  layout(xaxis = list(title="Level of Control", range=c(0,1.1)), yaxis = list(range=c(0,1.1)))
