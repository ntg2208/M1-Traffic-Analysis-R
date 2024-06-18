# M1 Traffic Analysis Using R

## Introduction

GNT Consultant is a consulting company composed of data scientists who use data to help clients make data-driven decisions to improve their businesses. One of our biggest clients, based in London, has delivery lorries that need to traverse the length of the M1. They require advice on the best times to start from J1 and return from J48. This report provides recommendations on the "Best time to start and best time to go back, daily" for delivery lorries traveling along the M1.

![M1 Traffic](https://github.com/ntg2208/M1-Traffic-Analysis-R/assets/25520448/b5530677-7ea3-46ef-9277-f2954b4cf7f0)

M1 traffic on [trafficengland.com](https://www.trafficengland.com/traffic-report)

## Analysis

### M1 Motorway

The M1 motorway connects London to Leeds, where it joins the A1(M) near Aberford to connect to Newcastle. It was the first inter-urban motorway completed in the UK, with construction occurring in four phases between 1959 and 1968. The southern end was extended in 1977 and the northern end in 1999.

### Background Research

According to Highways England (2021) and Catapult (2019), managing speeds at 60mph has the most significant impact on reducing vehicle emissions by an average of 17%. Consequently, speed limits are implemented at several locations to improve air quality along the M1. These speed limits are enforced 24/7 to bring down annual average emissions quickly.

### Sampling Method

Our client has two storage locations, one in London and one in Leeds. The average speed on the M1 is 60mph (Highways England, 2021), requiring 3.22 hours to travel the M1. However, real-world factors such as congestion, roadblocks, and incidents affect this speed. To ensure timely deliveries, we need to determine appropriate start times in the morning and evening.

We used "Stratified sampling" to collect average speed data, dividing the population into subpopulations (morning and evening) and using systematic sampling to select hours for data collection (9am-11am and 5pm-7pm daily). Data was collected over 34 days from 22/02/2022 to 27/03/2022 using [trafficengland.com](http://www.trafficengland.com/traffic-report).

### Data Collection

The M1 motorway has 54 junctions and two bounds (northbound and southbound), creating 104 segments. Data was collected and stored in a table with 8 columns:

![Data Dictionary](https://github.com/ntg2208/M1-Traffic-Analysis-R/assets/25520448/2a21790c-3e87-4be7-baf7-09f5d8bb6a49)

Over 32 days, we collected 7208 observations. The dataset is available in the attached file “q10_data.csv”.

### Hypotheses

We conducted four hypotheses tests:

1. Is it faster to go from London to Leeds in the morning than in the evening?
2. Is it faster to go from Leeds to London in the morning than in the evening?
3. Is the average speed on the northbound the same as the reported speed of 60mph?
4. Is the average speed on the southbound the same as the reported speed of 60mph?

First, we read in the dataset and parse the date column:

```r
library(dplyr)
traffic = read.csv('q10_data.csv')

traffic$date <- as.Date(traffic$date, "%d/%m/%Y")
```
### First Hypothesis

We can go through the M1 faster in the morning than in the evening on the northbound.

Hypothesis:
- \(H_0\): There are no significant differences in speed when going through the M1 in the morning or in the evening.
- \(H_a\): Going through the M1 in the morning is faster than going through the M1 in the evening.

First, we subset our data, only keep speed on the north, and divide it into 2 datasets: morning speed and evening speed. Then, we use the `aggregate` function to calculate the average speed through every junction for 34 days:

```r
m_north <- subset(traffic, bound=='north' & day_part == 0) 
e_north <- subset(traffic, bound=='north' & day_part == 1)

north_morning <- m_north %>% group_by(date)
df_m <-north_morning %>% summarise(speed = mean(speed)) 
north_eve <- e_north %>% group_by(date) 
df_e <-north_eve %>% summarise(speed = mean(speed))
```

Then we do the t-test between morning speed and evening speed:

```r
t.test(df_m$speed, df_e$speed, var.equal = TRUE, alternative = 'greater')
```
Because p-value = 0.5197 is greater than the critical value alpha = 0.05, we cannot reject the Null hypothesis. There is no significant difference when going through the M1 in the morning or in the evening.

### Second Hypothesis

We can go through the M1 faster in the morning than in the evening on the southbound.

Hypothesis:
- \(H_0\): There are no significant differences in speed when going through the M1 in the morning or in the evening.
- \(H_a\): Going through the M1 in the morning is faster than going through the M1 in the evening.

First, we will subset our data, only keep speed on the south, and divide it into 2 datasets: morning speed and evening speed. Then, we use the `aggregate` function to calculate the average speed through every junction for 34 days:

```r
m_south <- subset(traffic, bound=='south' & day_part == 0)
e_south <- subset(traffic, bound=='south' & day_part == 1)

south_morning <- m_south %>% group_by(date) 
df_m <-south_morning %>% summarise(speed = mean(speed))

south_eve <- e_south %>% group_by(date) 
df_e <-south_eve %>% summarise(speed = mean(speed))
```

Then we do the t-test between morning speed and evening speed:

```r
t.test(df_m$speed, df_e$speed, var.equal = TRUE, alternative = 'greater')
```
The p-value = 0.001669 is very small compared to the significant value alpha = 0.05, so we can reject the Null hypothesis  \(H_0\) and accept the alternative \(H_a\) that going through the M1 in the morning is faster than going through the M1 in the evening on the southbound of the highway.

### Third Hypothesis

Can we go faster than 60mph on the northbound of the M1?

Hypothesis:
- \(H_0\): The average speed of the northbound is 60mph.
- \(H_a\): The average speed of the northbound is greater than 60mph.

Calculate average speed and perform the one-sample t-test:

```r
north <- subset(traffic, bound=='north')

north <- north %>% group_by(date) 
df <-north %>% summarise(speed = mean(speed))

t.test(df$speed, mu=60, alternative = "greater")
```
The p-value = 2.941e-15 is very small compared to the significant value alpha = 0.05, so we can reject the Null hypothesis \(H_0\) and accept the alternative \(H_a\) that going through the M1 in the northbound can be faster than 60 mph.

### Fourth Hypothesis

Can we go faster than 60mph on the southbound of the M1?

Hypothesis:
- \(H_0\): The average speed of the southbound is 60mph.
- \(H_a\): The average speed of the southbound is greater than 60mph.

Next, we will use RStudio to calculate the average speed of the southbound and use a one-sample t-test:

```r
south <- subset(traffic, bound=='south')

south <- south %>% group_by(date) 
df <-south %>% summarise(speed = mean(speed))

t.test(df$speed, mu=60, alternative = "greater")
```
The p-value = 2.941e-15 is very small compared to the significant value alpha = 0.05, so we can reject the Null hypothesis \(H_0\) and accept the alternative \(H_a\) that going through the M1 in the southbound can be faster than 60 mph.

### Summary

- There are no significant differences between going in the morning or the evening on the northbound M1.
- Going in the morning is faster than going in the evening on the southbound M1.
- You can go faster than 60mph on both the northbound and southbound M1.

## Conclusion

Based on the collected data and hypotheses, we recommend:
- Travel from London to Leeds in the evening and return on the southbound in the next morning to save time.
- It is possible to travel faster than 60mph on the M1, taking less than 3 hours per trip.

## Bibliography

- Catapult, 2019. Light vehicle motorway NOx exhaust emissions. Available at: https://assets.highwaysengland.co.uk/CPC_Speed_Band_17092019_v2.pdf [Accessed 12 05 2022].
- Highways, N., 2021. Air quality speed limit trials - National Highways. Available at: https://nationalhighways.co.uk/our-work/environment/air-quality-and-noise/air-quality/air-quality-speed-limit-trials/ [Accessed 12 05 2022].
- McCombes, S., 2022. Sampling Methods | Types and Techniques Explained. Available at: https://www.scribbr.com/methodology/sampling-methods/ [Accessed 12 05 2022].
