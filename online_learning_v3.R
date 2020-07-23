#########################################
#Detrending analysis of Google trends
#online learning
#US
#Programmer: Miguel Acevedo
#9/7/20
#########################################

library(ggplot2)
library(dplyr)
library(patchwork) # To display 2 charts together
library(hrbrthemes)

#Format data
ol.data<- read.csv("online_learning_US.csv",skip=2,header=TRUE)
names(ol.data) <- c("date","ol") 

us.covid_cases <- read.csv("covid_us_cases.csv",header=TRUE)

#merge into a single data frame
df.m <- merge(x=ol.data, y=us.covid_cases,by="date",all.x=TRUE)
df.m$date <- as.Date(df.m$date)

########PLOT####################

#To scale the secondary y axis
coeff <- 25000

# setup colors
covidColor <- "#69b3a2"
olColor <- rgb(0.2, 0.6, 0.9, 1)

ggplot(df.m, aes(x=date)) +
  geom_line( aes(y=ol), size=2, color=olColor) + 
  geom_line( aes(y=United.States / coeff), size=2, color=covidColor) +
  scale_y_continuous(
    # Features of the first axis
    name = "Relative interest in online learning",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Number of Conformed COVID-19 cases")
  ) + 
  theme_ipsum() +
  theme(
    axis.title.y = element_text(color = olColor, size=13),
    axis.title.y.right = element_text(color = covidColor, size=13)
  ) +
  ggtitle("")