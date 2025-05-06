#import data
NBA_revenue <-read.csv("~/Downloads/Team_Revenues.csv")
names(NBA_revenue) <- substr(names(NBA_revenue), 2, nchar(names(NBA_revenue)))
names(NBA_revenue)[names(NBA_revenue) == ""] <- "team"
NBA_revenue[-1] <- lapply(NBA_revenue[-1], function(x) as.numeric(gsub("[$,]", "", x)))


trans <- as.data.frame(t(NBA_revenue))
colnames(trans) <- as.character(unlist(trans[1, ]))  
trans <- trans[-1, ]  
trans$year <- c("2011","2012","2013","2014","2015","2016","2017","2018","2019", "2020","2021" )
trans <- trans[1:(nrow(trans)-2),]
heights <- as.numeric(trans$Average)
labels <- trans$year

# average revenue bar plot
bp <- barplot(
  heights,
  col = "skyblue",
  main = "Average Revenue per Year",
  ylim = c(0, max(heights) * 1.2),
  ylab = "Average Revenue (in million USD)",
  xlab = "Year",
)

text(
  x = bp,
  y = par("usr")[3] - 0.05 * max(heights),  
  labels = labels,
  srt = 45,      
  adj = 1,      
  xpd = TRUE,    
  cex = 0.8
)
text(x = bp, y = heights, labels = heights, pos = 3, cex = 0.9)


# team record data

records <- read.csv("~/Downloads/Team_Records.csv")
records <- records[records$Season >= "2008-2009",]
records$Team <- gsub("\\*", "", records$Team)
records$Team[records$Team == "New Jersey Nets"] <- "Brooklyn Nets"
records$Team[records$Team == "Charlotte Bobcats"] <- "Charlotte Hornets"
records$Team[records$Team == "New Orleans Hornets"] <- "New Orleans Pelicans"
records <- records[ ,c("Season", "Team", "W.L.")]

new_rows <- data.frame(
  Season = rep("2018–19", 30),
  Team = c("Boston Celtics", "Toronto Raptors", "New York Knicks", "Philadelphia 76ers",
           "Brooklyn Nets", "Minnesota Timberwolves", "Oklahoma City Thunder",
           "Denver Nuggets", "Portland Trail Blazers", "Utah Jazz",
           "Cleveland Cavaliers", "Detroit Pistons", "Milwaukee Bucks",
           "Indiana Pacers", "Chicago Bulls", "Golden State Warriors", 
           "Los Angeles Clippers", "Phoenix Suns", "Sacramento Kings",
           "Los Angeles Lakers", "Washington Wizards", "Miami Heat", "Charlotte Hornets",
           "Orlando Magic", "Atlanta Hawks", "Houston Rockets",
           "San Antonio Spurs", "New Orleans Pelicans", "Dallas Mavericks",
           "Memphis Grizzlies"),
  W.L. = c(0.598, 0.707, 0.207, 0.622, 0.512, 0.439, 0.598,
           0.659, 0.646, 0.610, 0.232, 0.500, 0.732, 0.585,
           0.268, 0.695, 0.585, 0.232, 0.476, 0.451, 0.390,
           0.476, 0.476, 0.512, 0.354, 0.646, 0.585, 0.402,
           0.402, 0.402)
  
)

records <- rbind(records, new_rows)

champions <- c(
  "2018-19" = "Toronto Raptors",
  "2017–18" = "Golden State Warriors",
  "2016–17" = "Golden State Warriors",
  "2015–16" = "Cleveland Cavaliers",
  "2014-15" = "Golden State Warriors",
  "2013-14" = "San Antonio Spurs",
  "2012-13" = "Miami Heat",
  "2011-12" = "Miami Heat",
  "2010–11" = "Dallas Mavericks",
  "2009-10" = "Los Angeles Lakers"
)

champion_df <- data.frame(
  Season = names(champions),
  Team = unname(champions),
  stringsAsFactors = FALSE
)

fix_dashes <- function(x) gsub("–", "-", x)

records$Season <- fix_dashes(records$Season)
new_rows$Season <- fix_dashes(new_rows$Season)
names(champions) <- fix_dashes(names(champions))
records$Season <- as.character(records$Season)
records$Champion <- ifelse(
  paste(records$Season, records$Team) %in% paste(names(champions), champions),1,0
)


# clean and merge data
library(tidyverse)  
revenue_long <- trans %>%
  pivot_longer(-year, names_to = "Team", values_to = "Revenue")
records <- records %>%
  mutate(
    season_end_year = as.numeric(str_sub(Season, -2, -1)) + 2000,
    season_end_year = ifelse(season_end_year < 2010, season_end_year + 100, season_end_year),
    Team = str_trim(Team)
  )
revenue_long$year <- as.numeric(revenue_long$year)

nba_merged <- revenue_long %>%
  mutate(Team = str_trim(Team)) %>%
  left_join(records, by = c("Team", "year" = "season_end_year"))

nba <- nba_merged %>%
  select(Season, year, Team, Revenue, `W.L.`, Champion) %>%
  rename(WinPct = `W.L.`)

records_2009 <- records %>%
  filter(Season == "2009-10") %>%
  mutate(
    year = 2010,  
    Revenue = NA 
  )
nba_extended <- bind_rows(nba, records_2009 %>%
                            select(Season, Team, WinPct = `W.L.`, year, Revenue, Champion)) %>%
  arrange(Team, year)

#lagged revenue
nba_extended <- nba_extended %>%
  arrange(Team, year) %>%
  group_by(Team) %>%
  mutate(NextRevenue = lead(Revenue)) %>%
  ungroup()


# successful seasons
nba_extended$Revenue <- as.numeric(gsub(",", "", nba_extended$Revenue))
nba_extended$Revenue <- as.numeric(gsub("[$,]", "", nba_extended$Revenue))

nba_extended <- nba_extended %>%
  arrange(Team, year) %>%
  group_by(Team) %>%
  mutate(
    NextRevenue = lead(Revenue),
    RevenueChange = NextRevenue - Revenue,
    SuccessfulSeason = ifelse(WinPct > 0.600, 1, 0)
  ) %>%
  ungroup()

# winning season
nba_extended <- nba_extended %>%
  arrange(Team, year) %>%
  group_by(Team) %>%
  mutate(
    NextRevenue = lead(Revenue),
    RevenueChangePct = (NextRevenue - Revenue) / Revenue * 100,
    WinningSeason = ifelse(WinPct > 0.5, 1, 0)
  ) %>%
  ungroup()

team_avg_pct_change <- nba_extended %>%
  filter(WinningSeason == 1, !is.na(RevenueChangePct)) %>%
  group_by(Team) %>%
  summarise(AveragePctChange = mean(RevenueChangePct, na.rm = TRUE)) %>%
  arrange(desc(AveragePctChange))

#graph of revenue changes 
nba_extended <- nba_extended %>%
  arrange(Team, year) %>%
  group_by(Team) %>%
  mutate(
    NextRevenue = lead(Revenue),
    RevenueChangePct = (NextRevenue - Revenue) / Revenue * 100,
    WinningSeason = ifelse(WinPct > 0.5, 1, 0)
  ) %>%
  ungroup()

team_avg_pct_change <- nba_extended %>%
  filter(WinningSeason == 1, !is.na(RevenueChangePct)) %>%
  group_by(Team) %>%
  summarise(AveragePctChange = mean(RevenueChangePct, na.rm = TRUE)) %>%
  arrange(desc(AveragePctChange))

ggplot(team_avg_pct_change, aes(x = reorder(Team, -AveragePctChange), y = AveragePctChange)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(
    aes(label = round(AveragePctChange, 1)), 
    vjust = -0.5, 
    angle = 45, 
    size = 3
  ) +
  theme_minimal() +
  labs(
    title = "Average % Revenue Change After a Winning Season",
    x = "Team",
    y = "Avg % Revenue Change (Next Season)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




# graph of championship revenue effects
champion_avg_change <- nba_extended %>%
  filter(Champion == 1, !is.na(RevenueChangePct)) %>%
  group_by(Team) %>%
  summarise(AveragePctChange = mean(RevenueChangePct, na.rm = TRUE)) %>%
  arrange(desc(AveragePctChange))


ggplot(champion_avg_change, aes(x = reorder(Team, -AveragePctChange), y = AveragePctChange)) +
  geom_bar(stat = "identity", fill = "goldenrod") +
  theme_minimal() +
  labs(
    title = "Avg % Revenue Change After Winning a Championship",
    x = "Team",
    y = "Avg % Revenue Change (Next Season)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#revenu percent changes

nba_extended <- nba_extended %>%
  arrange(Team, year) %>%
  group_by(Team) %>%
  mutate(
    PrevRevenue = lag(Revenue),
    NextRevenue = lead(Revenue),
    RevenueChangeCurrentPct = (Revenue - PrevRevenue) / PrevRevenue * 100,
    RevenueChangeNextPct = (NextRevenue - Revenue) / Revenue * 100
  ) %>%
  ungroup()
champ_current_change <- nba_extended %>%
  filter(Champion == 1, !is.na(RevenueChangeCurrentPct)) %>%
  group_by(Team) %>%
  summarise(AveragePctChange = mean(RevenueChangeCurrentPct, na.rm = TRUE)) %>%
  arrange(desc(AveragePctChange))


# linear regression models 
model_current <- lm(Revenue ~ WinPct, data = nba_extended)
summary(model_current)

model_next <- lm(NextRevenue ~ WinPct, data = nba_extended)
summary(model_next)

poly_next <- lm(NextRevenue ~ poly(WinPct, 3, raw = TRUE), data = nba_extended)
summary(poly_next)

model_next <- lm(log(NextRevenue) ~ WinPct, data = nba_extended)
summary(model_next)


percent_model <- lm(RevenueChangeCurrentPct ~ WinPct, data = nba_extended)
summary(percent_model)


# first regression graph 
ggplot(nba_extended, aes(x = WinPct, y = RevenueChangeCurrentPct)) +
  geom_point(color = "orange", alpha = 0.7) +  # scatter points
  geom_smooth(method = "lm", se = TRUE, color = "red") +  # regression line
  labs(
    title = "Percentage of Revenue Change vs Win Percentage",
    x = "Win Percentage",
    y = "Percentage of Revenue Change"
  ) +
  theme_minimal(base_size = 14)


# hypotheses tests
t.test(Revenue ~ Champion, data = nba_extended)

t.test(RevenueChangeNextPct ~ SuccessfulSeason, data = nba_extended)

avg_champ <- nba_extended %>%
  filter(Champion == 1, !is.na(RevenueChangeNextPct)) %>%
  summarise(AveragePctChange = mean(RevenueChangeNextPct))

t.test(RevenueChangePct ~ SuccessfulSeason, data = nba_extended)
t.test(RevenueChangePct ~ Champion, data = nba_extended)

# hypotheses tests by team 
library(dplyr)
nba_extended <- nba_extended %>%
  mutate(WinningSeason = ifelse(WinPct > 0.5, "Winning", "Losing"))

results <- data.frame(
  Team = character(),
  p_value = numeric(),
  significant = logical(),
  mean_winning = numeric(),
  mean_losing = numeric(),
  stringsAsFactors = FALSE
)


for (team in unique(nba_extended$Team)) {
  team_data <- nba_extended %>% filter(Team == team)
  

  rev_win <- team_data$Revenue[team_data$WinningSeason == "Winning"]
  rev_lose <- team_data$Revenue[team_data$WinningSeason == "Losing"]
  
 
  rev_win <- rev_win[!is.na(rev_win)]
  rev_lose <- rev_lose[!is.na(rev_lose)]
  
  if (length(rev_win) >= 2 && length(rev_lose) >= 2) {
    t_test <- t.test(rev_win, rev_lose)
    
    results <- rbind(results, data.frame(
      Team = team,
      p_value = t_test$p.value,
      significant = t_test$p.value < 0.05,
      mean_winning = mean(rev_win),
      mean_losing = mean(rev_lose)
    ))
  }
}


results <- results %>% arrange(p_value)
significant_teams <- results %>% filter(significant == TRUE)

print(significant_teams)


# final graph 
nba_extended_net <- nba_extended %>%
  filter(Team != "Brooklyn Nets")

percent_model <- lm(RevenueChangeCurrentPct ~ WinPct, data = nba_extended_net)
summary(percent_model)

ggplot(nba_extended_net, aes(x = WinPct, y = RevenueChangeCurrentPct)) +
  geom_point(color = "orange", alpha = 0.7) +  # scatter points
  geom_smooth(method = "lm", se = TRUE, color = "red") +  # regression line
  labs(
    title = "Percentage of Revenue Change vs Win Percentage",
    x = "Win Percentage",
    y = "Percentage of Revenue Change"
  ) +
  theme_minimal(base_size = 14)

