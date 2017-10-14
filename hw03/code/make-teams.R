## Make Teams Table

# This script prepares data for a CSV file to be used in the homework.

github <- "https://github.com/ucb-stat133/stat133-fall-2017/raw/master/"
file <- "data/nba2017-roster.csv"
csv <- paste0(github, file)
download.file(url = csv, destfile = 'nba2017-roster.csv')
roster <- read.csv('nba2017-roster.csv')

github <- "https://github.com/ucb-stat133/stat133-fall-2017/raw/master/"
file <- "data/nba2017-stats.csv"
csv <- paste0(github, file)
download.file(url = csv, destfile = 'nba2017-stats.csv')
stats <- read.csv('nba2017-stats.csv')

stats <- mutate(stats, missed_fg = stats$field_goals_atts - stats$field_goals_made)
stats <- mutate(stats, missed_ft = stats$points1_atts - stats$points1_made)
stats <- mutate(stats, points = stats$points1_made + stats$points2_made * 2 + stats$points3_made * 3)
stats <- mutate(stats, rebounds = stats$off_rebounds + stats$def_rebounds)
stats <- mutate(stats, efficiency = (stats$points + stats$rebounds + stats$assists + stats$steals + stats$blocks - stats$missed_fg -stats$missed_ft - stats$turnovers) / stats$games_played)

sink('efficiency-summary.txt')
summary(stats$efficiency)
sink()

team <- merge(roster, stats)
teams <- summarise(group_by(team, team),
          experience = sum(experience),
          salary = round((sum(salary) / 1000000), 2),
          points3 = sum(points3_made),
          points2 = sum(points2_made),
          free_throws = sum(points1_made),
          points = sum(points),
          off_rebounds = sum(off_rebounds),
          def_rebounds = sum(def_rebounds),
          assists = sum(assists),
          steals = sum(steals),
          blocks = sum(blocks),
          turnovers = sum(turnovers),
          fouls = sum(fouls),
          efficiency = sum(efficiency))
summary(teams)          

sink('teams-summary.txt')
summary(teams)
sink()

write.csv(teams, file = 'nba2017-teams.csv', row.names = FALSE)

jpeg(filename = 'teams_star_plot.jpg', width = 600, height = 400)
stars(teams[ , -1], labels = as.character(teams$team))
dev.off()
pdf(file = 'teams_star_plot.pdf', width = 7, height = 5, 'teams_star_plot.jpg')

gg_pts_salary <- ggplot(data = teams) + geom_point(aes(x = experience, y = salary))
ggsave('experience_salary.pdf', width = 7, height = 5, units = 'in')

