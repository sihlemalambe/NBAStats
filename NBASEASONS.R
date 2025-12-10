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
  labs(title = "Career Points vs Career Turnovers", x = "Careers Points", y = "Career Turnovers Committed")

##Top Scorer per Year
top_scorers <- NBA %>%
  filter(!is.na(PTS)) %>%
  group_by(Year) %>%
  slice_max(PTS, n = 1) %>%
  select(Year, Player, PTS)
cat("====TOP SCORERS PER SEASON====")
print(top_scorers, n = nrow(top_scorers))

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

cat("Average Age of NBA Players",round(AveAge,))

##Correlation Matrix
cor_Matrix <- NBA %>% 
  select(PTS, AST, TOV, STL, TRB) %>%
  cor(use = "complete.obs")
cat("====Correlation Matrix====")
print(cor_Matrix)

##Era Comparison
NBA <- NBA %>%
  mutate(Era = case_when(
    Year <= 1969 ~ "50s-60s",
    Year >= 1970 & Year <= 1979 ~ "70s",
    Year >= 1980 & Year <= 1989 ~ "80s",
    Year >= 1990 & Year <= 1999 ~ "90s",
    Year >= 2000 & Year <= 2009 ~ "2000s",
    Year >= 2010 & Year <= 2019 ~ "2010s"
  )
         )

##Summarise Stats by Era
Era_Stats <- NBA %>% 
  group_by(Era) %>%
  summarise(
    AvgPTS = mean(PTS, na.rm = TRUE),
    AvgAST = mean(AST, na.rm = TRUE)
  )

print(Era_Stats)
