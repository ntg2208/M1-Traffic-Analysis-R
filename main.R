# Q10
library(dplyr)
traffic = read.csv('q10_data.csv')

traffic$date <- as.Date(traffic$date, "%d/%m/%Y")

tmp <- traffic %>% group_by(bound, day_part) 
df <-tmp %>% summarise(speed = mean(speed))

m_north <- subset(traffic, bound=='north' & day_part == 0)
e_north <- subset(traffic, bound=='north' & day_part == 1)
# View(m_north)

north_morning <- m_north %>% group_by(date) 
df_m <-north_morning %>% summarise(speed = mean(speed))

north_eve <- e_north %>% group_by(date) 
df_e <-north_eve %>% summarise(speed = mean(speed))

var.test(df_m$speed, df_e$speed)

t.test(df_m$speed, df_e$speed, var.equal = TRUE, alternative = 'greater')

m_south <- subset(traffic, bound=='south' & day_part == 0)
e_south <- subset(traffic, bound=='south' & day_part == 1)

south_morning <- m_south %>% group_by(date) 
df_m <-south_morning %>% summarise(speed = mean(speed))

south_eve <- e_south %>% group_by(date) 
df_e <-south_eve %>% summarise(speed = mean(speed))

var.test(df_m$speed, df_e$speed)

t.test(df_m$speed, df_e$speed, var.equal = FALSE, alternative = 'greater')


north <- subset(traffic, bound=='north')

north <- north %>% group_by(date) 
df <-north %>% summarise(speed = mean(speed))

t.test(df$speed, mu=60, alternative = "greater")

south <- subset(traffic, bound=='south')

south <- south %>% group_by(date) 
df <-south %>% summarise(speed = mean(speed))
View(df)

t.test(df$speed, mu=60, alternative = "greater")

library(ggplot2)
ggplot(df,aes(speed)) +
  geom_histogram() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5))

