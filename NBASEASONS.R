library(dplyr)
NBA <- read.csv("C:/RPROJECTS/NBADATASET/Seasons_Stats.csv", header = TRUE, encoding = "latin1")

tail(NBA)

options(max.print = 999999)

##Top Scorer per Year
topscorers <- NBA %>%
  filter(!is.na(PTS)) %>%
  group_by(Year) %>%
  slice_max(PTS, n = 1) %>%
  select(Year, Player, PTS)

#Correlation between Scoring and Turnovers
plot(NBA$PTS, NBA$TOV,
     xlab = "Points Scored (PTS)",
     ylab = "Turnovers (TOV)",
     main = "PTS vs TOV",
     pch = 19, col = "black",
     bg = "black")
abline(lm(TOV ~ PTS, data = NBA), col = "red")  # adds a trend line

print(topscorers, n = nrow(topscorers))
  
AveAge <- mean(NBA$Age[NBA$PTS > 30], na.rm = TRUE)
PtsToAst <- cor(NBA$PTS,NBA$AST, use = "complete.obs")
PtsToStls <- cor(NBA$PTS, NBA$STL, use = "complete.obs")
PtsToTRB <- cor(NBA$PTS, NBA$TRB., use = "complete.obs")
print(PointDiff)
print(AveAge)
cat("Points to Assists",PtsToAst)
cat("Points to Steals",PtsToStls)

##Top Turnovers per Year
topturnovers <- NBA %>%
  filter(!is.na(TOV)) %>%
  group_by(Year) %>%
  slice_max(TOV, n = 1) %>%
  select(Year, Player, TOV)

print(topturnovers, n = nrow(topturnovers))

##Top Assists per Year
topassists <- NBA %>%
  filter(!is.na(AST)) %>%
  group_by(Year) %>%
  slice_max(AST, n = 1) %>%
  select(Year, Player, AST)

print(topassists, n = nrow(topassists))

##Player Search:
PlayerSearch <- 

