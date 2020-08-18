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

# Plot Level of Control vs Frustration Level  
D_valid %>%
  mutate(PerceivedControl = jitter(PerceivedControl, amount=.03),
         Frustration = jitter(Frustration, amount=.03)) %>%
  plot_ly() %>%
  add_trace(x=~PerceivedControl, y=~Frustration, color=~Participant)

# Plot Level of Control vs Perceived Control
D_valid %>%
  mutate(PerceivedControl = jitter(PerceivedControl, amount=.03),
         PureControl = jitter(PureControl, amount=.03),
         FabControl = jitter(FabControl, amount=.03)) %>%
  plot_ly() %>%
  add_trace(x=~PureControl, name="Pure Control", y=~PerceivedControl) %>%
  add_trace(x=~FabControl, name="Fab Control", y=~PerceivedControl) %>%
  layout(xaxis = list(title="Level of Control", range=c(0,1.1)), yaxis = list(range=c(0,1.1)))

# Plot Level of Control vs Frustration
D_valid %>%
  mutate(Frustration = jitter(Frustration, amount=.03),
         PureControl = jitter(PureControl, amount=.03),
         FabControl = jitter(FabControl, amount=.03)) %>%
  plot_ly() %>%
  add_trace(x=~PureControl, name="Pure Control", y=~Frustration) %>%
  add_trace(x=~FabControl, name="Fab Control", y=~Frustration) %>%
  layout(xaxis = list(title="Level of Control", range=c(0,1.1)), yaxis = list(range=c(0,1.1)))
