# M1 Traffic Analysis Using R
## Introduction
GNT Consultant is a consulting company which is a group of Data Scientist use data to help client making data-driven decision to improve their business. One of their biggest clients who based in London have delivery lorries need to go through the length of M1, they want to have some advice about what the best time is to start from J1 and best time to go back from J48. Below is the report to advice client “Best time to start and best time to go back, daily” when deliver lorries going through M1.
## Analysis
### M1 motorway
The M1 motorway connects London to Leeds, where it joins the A1(M) near Aberford, to connect to Newcastle. It was the first inter-urban motorway to be completed in the UK, the first motorway in the country was the Preston By-pass, which later became part of the M6.
The motorway is 193 miles (311 km) long and was constructed in four phases. Most of the motorway was opened between 1959 and 1968. The southern end was extended in 1977 and the northern end was extended in 1999.
### Background research
According to (Highways, 2021) (Catapult, 2019) they have been investigating the effects that different speeds and driving styles have on vehicle emissions across a range of vehicle types. Our initial assessments show that managing speed at 60mph has the most significant impact, resulting in an average 17% reduction in emissions and so can potentially brought forward compliance by one to two years. That is why they are now implementing these speed limits at several locations across the network. 
It is a 24h speed limit because we need to improve air quality along M1 as soon as possible. Emissions levels are calculated on an annual average basis, and so having the speed limits in place 24 hours a day will bring down the annual averages in the shortest timescale possible. Also, vehicles travelling late at night often travel at higher average speeds resulting in high levels of harmful emissions. All vehicles’ emissions at any time of day contribute to the total. (Highways, 2021)
### Sampling method
Our client has 2 storages, 1 in London and 1 in Leeds, according to research, the average speed on M1 is 60mph (Highways, 2021), we will need 3.22 hour to travel through M1. But in the real-world scenario, there are many factors will affect the average speed: congestion, roadblock, road work, incident, ... To make sure we can deliver package on time we will need to start in appropriate time in the morning and evening to go through M1 as fast as possible. Since we only need to estimate the appropriate starting time and backing time, we will use “Stratified sampling” method to collect average speed.
Stratified sampling involves dividing the population into subpopulations that may differ in important ways. It allows you draw more precise conclusions by ensuring that every subgroup is properly represented in the sample. To use this sampling method, we divide the population into subgroups (called strata) based on the relevant characteristic (e.g., gender, age range, income bracket, job role). Based on the overall proportions of the population, you calculate how many people should be sampled from each subgroup. Then you use random or systematic sampling to select a sample from each subgroup (McCombes, 2022). 
Systematic sampling is like simple random sampling, but it is usually slightly easier to conduct. Every member of the population is listed with a number, but instead of randomly generating numbers, individuals are chosen at regular intervals (McCombes, 2022).
In our problem, we will consider speed between every junction during the day is a population, we will define 2 strata based on day part: Morning and Evening then we will use systematic sampling which is selecting hour to collect: between 9am-11am and 5pm-7pm every day.
In our problem, we will consider speed between every junction during the day is a population, we will define 2 strata based on day part: Morning and Evening then we will use systematic sampling which is selecting hour to collect between 2 intervals: 9am-11am and 5pm-7pm every day. We will use http://www.trafficengland.com/traffic-report to collect speed between every junction on M1. In other to create large enough sample size, we will collect more than 30 days of data. Our final sampling strategy is: “Collecting speed between every junction of M1 motorway between 9am-11am and 5pm-7pm every day in 34 days from 22/02/2022 to 27/03/2022”.
On M1 motorway, there are 54 junctions and 2 bound: northbound, southbound which will create 104 smaller road between every junction in both bound. To collect data, we will need to design a table that can be used to collect our data. Our table will have 8 columns as below:
Table 1: Data dictionary
![image](https://github.com/ntg2208/M1-Traffic-Analysis-R/assets/25520448/2a21790c-3e87-4be7-baf7-09f5d8bb6a49)

Every day from 22/02/2022 to 27/03/2022 we will collect speed and traffic information and filled as table below:
<img width="1000" alt="image" src="https://github.com/ntg2208/M1-Traffic-Analysis-R/assets/25520448/aee2e540-6e18-4ccc-8a6a-bd558f6a955d">

After 32 days of collecting data, we have a dataset of 7208 observations with 8 columns as attached file “q10_data.csv”
### Hypotheses 
We will conduct 4 hypotheses:
```
Is it faster to go from London to Leeds in morning than going in the evening?
Is it faster to go from Leeds to London in morning than going in the evening?
Is the average speed being the same with reported speed 60mph on north bound?
Is the average speed being the same with reported speed 60mph on south bound?
```
First we read in the dataset and parsing date on the date columns
```
library(dplyr)
traffic = read.csv('q10_data.csv')

traffic$date <- as.Date(traffic$date, "%d/%m/%Y")
```
### First hypothesis
We can go through M1 faster in the morning than in the evening on the south bound
On the north bound, let s_m and s_e is average speed on the morning and evening of M1 respectively. Let H=mean(s_m )-mean(s_e) we have the following hypothesis
H_0=0: There are no significant differences in speed when going through M1 in the morning or in the evening
H_a>0: Going through M1 in the morning is faster than going through M1 on the evening
First, we will subset our data, only keep speed on the north, and divide into 2 dataset which are morning speed and evening speed, then use aggregate function to calculate average speed through every junction for 34 days
```
m_north <- subset(traffic, bound=='north' & day_part == 0) 
e_north <- subset(traffic, bound=='north' & day_part == 1)

north_morning <- m_north %>% group_by(date)
df_m <-north_morning %>% summarise(speed = mean(speed)) 
north_eve <- e_north %>% group_by(date) 
df_e <-north_morning %>% summarise(speed = mean(speed))
```
Then we do the t-test between morning speed and evening speed
```
t.test(df_m$speed, df_e$speed, var.equal = TRUE, alternative = 'greater')
```
Because p-value=0.5197 is greater than critical value alpha = 0.05 so we cannot reject the Null hypothesis. There is no significant different when going through M1 in the morning or in the evening.

### Second hypothesis
We can go through M1 faster in the morning than in the evening on the south bound
On the south bound, let s_m and s_e is average speed on the morning and evening of M1 respectively. Let H=mean(s_m )-mean(s_e) we have the following hypothesis
H_0=0: There are no significant differences in speed when going through M1 in the morning or in the evening
H_a>0: Going through M1 in the morning is faster than going through M1 on the evening

First, we will subset our data, only keep speed on the south, and divide into 2 dataset which are morning speed and evening speed, then use aggregate function to calculate average speed through every junction for 34 days
```
m_south <- subset(traffic, bound=='south' & day_part == 0)
e_south <- subset(traffic, bound=='south' & day_part == 1)

m_south <- subset(traffic, bound=='south' & day_part == 0)
e_south <- subset(traffic, bound=='south' & day_part == 1)

south_morning <- m_south %>% group_by(date) 
df_m <-south_morning %>% summarise(speed = mean(speed))

south_eve <- e_south %>% group_by(date) 
df_e <-south_eve %>% summarise(speed = mean(speed))
```

Then we do the t-test between morning speed and evening speed
```
t.test(df_m$speed, df_e$speed, var.equal = TRUE, alternative = 'greater')
```
The p-value = 0.001669 is very small compared to significant value alpha = 0.05 so we can reject the Null hypothesis H_0 and accept the alternative H_a that going through M1 in the morning are faster than going through M1 on the evening on south bound of the highway.

### Third hypothesis:
Can we go faster than 60mph on the north bound of the M1?
Let the average speed on north bound of M1 is s_nand the average speed of M1 is 60mph because it is producing less CO2 which is better for the environment. Because we have more than 30 samples so we can assume this has a large sample size and have a normal distribution
Let H=mean(s_n )-60
H_0=0: The average speed of north bound is 60mph
H_a>0: The average speed of north bound is greater than 60mph
Next, we will use RStudio to calculate the average speed of north bound and use one-sample t-test
```
north <- subset(traffic, bound=='north')

north <- north %>% group_by(date) 
df <-north %>% summarise(speed = mean(speed))

t.test(df$speed, mu=60, alternative = "greater")
```
The p-value = 2.941e-15 is very small compared to significant value alpha = 0.05 so we can reject the Null hypothesis H_0 and accept the alternative H_a that going through M1 in the north bound can be faster than 60 mph

### Fourth hypothesis
Can we go faster than 60mph on the south bound of the M1?
	Let the average speed on north bound of M1 is s_n and the average speed of M1 is 60mph because it is producing less CO2 which is better for the environment. Because we have more than 30 samples so we can assume this has a large sample size and have a normal distribution
Let H=mean(s_n )-60
H_0=0: The average speed of south bound is 60mph
H_a>0: The average speed of south bound is greater than 60mph
Next, we will use RStudio to calculate the average speed of south bound and use one-sample t-test
```
south <- subset(traffic, bound=='south')

south <- south %>% group_by(date) 
df <-south %>% summarise(speed = mean(speed))

t.test(df$speed, mu=60, alternative = "greater")
```
The p-value = 2.941e-15 is very small compared to significant value alpha = 0.05 so we can reject the Null hypothesis H_0 and accept the alternative H_a that going through M1 in the north bound can be faster than 60 mph
### Summary
	There are no significantly differences between going in the morning or the evening of the north bound M1
	Going in the morning is faster than going in the evening on the south bound of M1
	You can go faster than 60mph on north and south bound of M1
## Conclusion
Based on collected data and 4 hypotheses above, we can give our client the following:
	We can travel from London to Leeds in the evening and going back on the south bound in the next morning to save time
	We can go faster than 60mph on M1, it is going to take less than 3 hours per each trip.
## Bibliography
Catapult, 2019. Light vehicle motorway NOx exhaust emissions. [Online] 
Available at: https://assets.highwaysengland.co.uk/CPC_Speed_Band_17092019_v2.pdf
[Accessed 12 05 2022].
Highways, N., 2021. Air quality speed limit trials - National Highways. [Online] 
Available at: https://nationalhighways.co.uk/our-work/environment/air-quality-and-noise/air-quality/air-quality-speed-limit-trials/
[Accessed 12 05 2022].
McCombes, S., 2022. Sampling Methods | Types and Techniques Explained. [Online] 
Available at: https://www.scribbr.com/methodology/sampling-methods/
[Accessed 12 05 2022].

