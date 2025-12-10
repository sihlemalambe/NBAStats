packages <- c("dplyr", "ggplot2", "shiny")
lapply(packages, library, character.only = TRUE)

NBA <- read.csv("C:/RPROJECTS/NBADATASET/Seasons_Stats.csv", header = TRUE, encoding = "latin1")

tail(NBA)

options(max.print = 999999)

career_stats <- NBA %>%
  group_by(Player) %>%
  summarise(
    CareerPTS = sum(PTS, na.rm = TRUE),
    CareerTOV = sum(TOV, na.rm = TRUE)
  )

#Correlation between Scoring and Turnovers
ggplot(career_stats, aes(x = CareerPTS, y = CareerTOV)) +
  geom_point(color = "white") +
  geom_smooth(method = "lm", color = "red") +
  theme_minimal(base_size = 14) +
  theme(panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black"),
        axis.text = element_text(color = "white"),
        axis.title = element_text(color = "white"),
        plot.title = element_text(color = "white")) +
  labs(title = "PTS vs TOV", x = "Points Scored", y = "Turnovers Committed")

##Top Scorer per Year
topscorers <- NBA %>%
  filter(!is.na(PTS)) %>%
  group_by(Year) %>%
  slice_max(PTS, n = 1) %>%
  select(Year, Player, PTS)
cat("====TOP SCORERS PER SEASON====")
print(topscorers, n = nrow(topscorers))

##Top Turnovers per Year
topturnovers <- NBA %>%
  filter(!is.na(TOV)) %>%
  group_by(Year) %>%
  slice_max(TOV, n = 1) %>%
  select(Year, Player, TOV)
cat("====TOP TURNOVERS PER SEASON====")
print(topturnovers, n = nrow(topturnovers))

##Top Assists per Year
topassists <- NBA %>%
  filter(!is.na(AST)) %>%
  group_by(Year) %>%
  slice_max(AST, n = 1) %>%
  select(Year, Player, AST)
cat("====TOP ASSISTS PER SEASON====")
print(topassists, n = nrow(topassists))

AveAge <- mean(NBA$Age[NBA$PTS > 30], na.rm = TRUE)
PtsToAst <- cor(NBA$PTS,NBA$AST, use = "complete.obs")
PtsToStls <- cor(NBA$PTS, NBA$STL, use = "complete.obs")
PtsToTRB <- cor(NBA$PTS, NBA$TRB., use = "complete.obs")

cat("Correlation of Points to Assists",PtsToAst)
cat("Correlation of Points to Steals",PtsToStls)
cat("Average Age of NBA Players",round(AveAge,))
