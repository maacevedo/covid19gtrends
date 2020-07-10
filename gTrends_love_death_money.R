#########################################
#Detrending analysis of Google trends
#death,love,money
#Global analysis
#Programer: Miguel Acevedo
#9/7/20
#########################################

library(gtable)
library(grid)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(dplyr)
library(patchwork) # To display 2 charts together
library(hrbrthemes)
library(ggpubr)

#Import data for all variables in 2020
data.20 <- read.csv("data_jan_jun_2020.csv",skip=1,header=TRUE)
names(data.20) <- c("Day","Money","Death","Love") #rename columns

data.19 <- read.csv("data_jan_jun_2019.csv",skip=1,header=TRUE)
names(data.19) <- c("Day","Money","Death","Love")

#These are global covid cases from Jan 1--Jun 30, 2020
#Downloaded from: https://ourworldindata.org/coronavirus-source-data
covid_cases <- read.csv("covid_total_cases.csv",header=TRUE)

#####################################################
#Decompose time series to extract trend
#####################################################

#Using a frquency of 7 for a week and start position of 1
#which is the first julian day (January 1)

trend <- function(data)
{
  n <- dim(data)[2]
  de <- decompose(ts(data[,2:n],frequency=7,start=1))
  de.df <- as.data.frame(de$trend)
  names(de.df) <- names(data)[2:n]
  return(de.df)
}

trends20 <- trend(data.20)
trends19 <- trend(data.19)
names(trends20)=c("Money.t","Death.t","Love.t")
names(trends19)=c("Money.t","Death.t","Love.t")

##################################PLOTS###################



##ggplot2###########
#change to long format
data.20.c <- cbind(data.20,trends20,covid_cases)
data.19.c <- cbind(data.19,trends19)

#ensure date format
data.20.c$Day <- as.Date(data.20.c$Day)
data.19.c$Day <- as.Date(data.19.c$Day)



#################PLOT############################
coeff <- 100000

covidColor <- "gray77"
moneyColor <- "#99FF99"
deathColor <- "#CCFFFf"
loveColor <- "#FFCCCC"
money.tColor <- "#336633"
death.tColor <- "#003366"
love.tColor <- "#990000"

p20 <- ggplot(data.20.c, aes(x=Day)) +
  geom_bar( aes(y=total_cases/coeff), stat="identity", size=.1, fill=covidColor, color=covidColor, alpha=.4) + 
  geom_line( aes(y=Money), size=2, color=moneyColor) +
  geom_line( aes(y=Money.t), size=1, color=money.tColor) +
  geom_line( aes(y=Death), size=2, color=deathColor) +
  geom_line( aes(y=Death.t), size=1, color=death.tColor) +
  geom_line( aes(y=Love), size=2, color=loveColor) +
  geom_line( aes(y=Love.t), size=1, color=love.tColor) +
  scale_y_continuous(
    # Features of the first axis
    name = "",lim=c(0,100),
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Global number of Covid19 cases")
  ) + 
  theme_ipsum() +
  theme(
    axis.title.y = element_blank(),
    axis.title.y.right = element_text(color = "black", size=13),
      plot.margin=unit(c(1,1,1,-0.01),"cm")
  ) +
  ggtitle(2020)+
  scale_x_date(date_labels="%b",date_breaks="1 month")



p19 <- ggplot(data.19.c, aes(x=Day)) +
  geom_line( aes(y=Money), size=2, color=moneyColor) +
  geom_line( aes(y=Money.t), size=1, color=money.tColor) +
  geom_line( aes(y=Death), size=2, color=deathColor) +
  geom_line( aes(y=Death.t), size=1, color=death.tColor) +
  geom_line( aes(y=Love), size=2, color=loveColor) +
  geom_line( aes(y=Love.t), size=1, color=love.tColor) +
  scale_y_continuous(
    # Features of the first axis
    name = "Relative interest",lim=c(0,100)
  ) + 
  theme_ipsum() +
  theme(
    axis.title.y = element_text(color = "black", size=13),
    axis.title.y.right = element_text(color = "black", size=13),
    plot.margin=unit(c(1,-0.001,1,2.6),"cm")
  ) +
  ggtitle(2019)+
  scale_x_date(date_labels="%b",date_breaks="1 month")+
  annotate("text",x=as.Date("2019-01-07"), y=50, label= "Love",color="darkred")+
  annotate("text",x=as.Date("2019-01-07"), y=37, label= "Death",color="blue")+
  annotate("text",x=as.Date("2019-01-07"), y=28, label= "Money",color="darkgreen")
 
#####Arrange plots into a single panel
plots <- arrangeGrob(p19,p20,ncol=2)
as_ggplot(plots)                                


#  draw_plot_label(label = c("(A)", "(B)"),
              #    x=c(0.05,0.5),y=c(0.88,0.88))

