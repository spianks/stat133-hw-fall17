# ===============================================================================
# title: nba2017 teams table
# description: csv file that contains the required variables in ranking analysis
# input: teams table in csv format 
# output: graphics and image files in pdf format 
# ===============================================================================

# packages

library(readr)    # importing data
library(dplyr)    # data wrangling
library(ggplot2)  # graphics



roster <- read.csv("stat133-hws-fall17/hw03/data/nba2017-roster.csv", stringsAsFactors = FALSE)
stats <- read.csv("stat133-hws-fall17/hw03/data/nba2017-stats.csv", stringsAsFactors = FALSE)

stats <- mutate(stats,
                "missed_fg" = points3_atts - points3_made + points2_atts - points2_made,
                "missed_ft" = stats$"points1_atts" - stats$"points1_made",
                points = 3*stats$"points3_made" + 2*stats$"points2_made" + stats$"points1_made",
                rebounds = stats$"off_rebounds" + stats$"def_srebounds")


stats <- mutate(stats,
                efficiency = (points + rebounds + assists + steals + blocks - missed_fg - missed_ft - turnovers)/games_played)
stats

sink("stat133-hws-fall17/hw03/output/efficiency-summary.txt")
summary(stats$efficiency)
sink()


a <- merge(roster, stats)


# Creating nba2017-teams.csv

teams <- a %>% group_by(team) %>% summarize(
  experience = round(sum(experience),2),
  salary = round(sum(salary)/1000000, 2),
  points3 = sum(points3_made),
  points2 = sum(points2_made),
  free_throws = sum(points1_made),
  points = sum(points3_made + points2_made + points1_made),
  off_rebounds = sum(off_rebounds),
  def_rebounds = sum(def_rebounds),
  assists = sum(assists),
  steals = sum(steals),
  blocks = sum(blocks),
  turnovers = sum(turnovers),
  fouls = sum(fouls),
  efficiency = sum(efficiency)
)

teams

sink("stat133-hws-fall17/hw03/output/teams-summary.txt")
summary(teams)
sink()

write.csv(teams, file = "stat133-hws-fall17/hw03/data/nba2017-teams.csv")








#create and save star plot of the teams 
pdf("stat133-hws-fall17/hw03/images/teams_star_plot.pdf", width = 7, height = 5)
stars(teams[, -1], labels = teams$team)
dev.off()

#create and save a scatterplot of experienc and salary 
sctplt <- ggplot(teams, aes(experience, salary)) + geom_point() + geom_text(label = teams$team)
ggsave(filename = "stat133-hws-fall17/hw03/images/experience_salary.pdf", plot = sctplt, width = 7, height = 5)



