# Q1
set.seed(0)
exercise <- c(rnorm(44, mean = 1.8, sd = 1.03))
gym <- c(rnorm(60, mean = 2.5, sd = 1.33))

var.test(exercise, gym)

t.test(exercise, gym, var.equal = TRUE,  alternative = "less")


# Q3
library(readr)
cystfibr <- read_table("q3_data.txt")
cystfibr <- as.data.frame(cystfibr)
attach(cystfibr)

summary(cystfibr)
# Q4a
library(PerformanceAnalytics) 
chart.Correlation(cystfibr, histogram = TRUE)

library(ggplot2)

ggplot(cystfibr, aes(x=age, y=height)) + geom_point() + 
  ggtitle("Scatter plot between age and height") + xlab('Age (year-old)') + ylab('Height (cm)') + 
  theme_minimal()

cor(cystfibr$age, cystfibr$height)

# Q4b
cystfibr$sex <- as.factor(cystfibr$sex)
ggplot(cystfibr, aes(x=sex, y=pemax, group=sex)) +
  geom_boxplot(aes(fill=sex)) +
  ggtitle("Stratified boxplot between pemax and sex") +
  theme_minimal()


boxplot(height~sex)
boxplot(weight~sex)
boxplot(bmp~sex)
boxplot(fev1~sex)
boxplot(rv~sex)
boxplot(frc~sex)
boxplot(tlc~sex)
boxplot(pemax~sex)
# Q6
dpois(7, 5)
# Q5
dbinom(5, 8, 0.88)
# Q7
pnorm(10000, mean=14500, sd=2500, lower.tail = FALSE) #P(X>10000)
# Q8
data <- data.frame(y=c(8.1, 7.8, 8.5, 9.8, 9.5, 8.9, 8.6, 10.2, 9.3, 9.2, 10.5),
                   x=c(1.0, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2.0))
model <- lm(y~x, data=data)
summary(model)

ggplot(data,aes(x, y)) +
  geom_point() +
  geom_smooth(method='lm')  +
  theme_minimal() +
  labs(x='X Values', y='Y Values', title='Linear Regression Plot') +
  theme(plot.title = element_text(hjust=0.5, size=20, face='bold')) 

p <-  as.data.frame(1.76)
colnames(p) <- "x"

predict(model, newdata = p)


# Q9
x = c(0,6,4,5,2,7,3,10)
y = c(4,8,5,10,1,3,4,12)
cor(x,y)
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

