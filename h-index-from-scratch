#Start session ----
rm(list=ls())
library(tidyverse)
library(dplyr)

# 1 - Import data ----

data = read.table("https://meek-parfait-60672c.netlify.app/docs/High_tech_Employment_Eurostat.txt", header=TRUE)
colnames(data)[3] ="employment"

# 2 - Keep regional observations only ----

#keep only the observations whose number of letters of "geo" is equal to 4,
#since this corresponds to the NUTS2 observations 
data = subset(data, nchar(data$geo)==4)

#remove NAs
data = drop_na(data)

# 3 - Compute h-index ----

data$year = as.factor(data$year) #needed for the "past" function
data$geo = str_sub(data$geo, 1, 2) #needed for "group_by" function

#create a feature for each moment
data = data %>% 
  group_by(year, geo) %>%
  mutate(category = paste(year, geo))

#compute sum of employment
data = data %>%
  group_by(category) %>%
  mutate(sum_cate = sum(employment))

#compute percent of employment/moment
data = data %>%
  group_by(category) %>%
  mutate(percent = round(employment/sum_cate, 2))

#compute h-index
data = data %>%
  group_by(category) %>%
  mutate(herfin = sum(percent**2))

# 4 - Create plot for 5 countrys ----
EU5 = c("DE", "BE", "IT", "FR", "NL")
data$year = as.numeric(data$year) #needed for the plot

#select for each country the right obs
FR = subset(data, data$geo=='FR')
DE = subset(data, data$geo=='DE')
BE = subset(data, data$geo=='BE')
IT = subset(data, data$geo=='IT')
NL = subset(data, data$geo=='NL')

#ploting the h-index throught time
par(mfrow = c(2,3))
#par(mfrow = c(1,1Plot ))
plot(FR$year, FR$herfin, ylim = c(0, 0.3),
     main="France", ylab = "H-index", xlab = "From 1999 to 2008", col="blue", pch=1)
plot(DE$year, DE$herfin, ylim = c(0, 0.3),
     main="Germany", ylab = "H-index", xlab = "From 1999 to 2008", col="green", pch=2)
plot(BE$year, BE$herfin, ylim = c(0, 0.3),
     main="United Kingdom", ylab = "H-index", xlab = "From 1999 to 2008", col="red", pch=3)
plot(IT$year, IT$herfin, ylim = c(0, 0.3),
     main="Italy", ylab = "H-index", xlab = "From 1999 to 2008", col="yellow", pch=4)
plot(NL$year, NL$herfin, ylim = c(0, 0.3),
     main="Spain", ylab = "H-index", xlab = "From 1999 to 2008", col="grey", pch=5)
