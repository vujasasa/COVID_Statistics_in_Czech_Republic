

#following packages must be installed in order for the script to work. Please un-comment if not installed

install.packages("ggplot2")
install.packages("tidyverse")
library(ggplot2)
library(tidyverse)


#links to COVID data from mzcr.cz
COVID_Tested_link <- "https://onemocneni-aktualne.mzcr.cz/api/v1/covid-19/testy.csv"
COVID_Infected_link <- "https://onemocneni-aktualne.mzcr.cz/api/v1/covid-19/nakaza.csv"


#load datasets
library(readr)
COVID_Tested <- read_csv(COVID_Tested_link, 
                         col_types = cols(datum = col_date(format = "%Y-%m-%d")))

COVID_Infected <- read_csv(COVID_Infected_link, 
                           col_types = cols(datum = col_date(format = "%Y-%m-%d")))

#combine datasets for infected and tested
COVID_Combined <- merge(COVID_Infected, COVID_Tested)
View(COVID_Combined)

#add columns percent_of_infected and percent_increase
COVID_Combined$percent_of_infected <- round((COVID_Combined$pocet_den/COVID_Combined$testy_den)*100,2)
COVID_Combined$percent_increase<- round((COVID_Combined$pocet_den/COVID_Combined$pocet_celkem)*100,2)



#PLOT GRAPHS#

#COMPLEX Daily growth rate of confirmed cases
ggplot(COVID_Combined, aes(x=pocet_celkem, y=pocet_den, label = datum)) + geom_smooth() + geom_point() + 
  scale_x_log10() + scale_y_log10() + 
  labs(title = "COVID-19 Czech Republic - Daily growth rate of confirmed cases") + 
  geom_text(check_overlap = TRUE, size = 3, nudge_y = 0.03) + 
  geom_vline(xintercept = 189, linetype = "dashed", color="red") + 
  geom_vline(xintercept = 63, linetype = "dashed", color="red") + 
  geom_abline(intercept = -1, slope = 1, linetype = 'dotted') + 
  geom_abline(intercept = -0.76, slope = 1, linetype = 'dotted') + 
  geom_abline(intercept = -0.53, slope = 1, linetype = 'dotted') + 
  geom_abline(intercept = -1.26, slope = 1, linetype = 'dotted') + 
  xlab("Total number of COVID reported cases over time") + ylab("Daily number of COVID reported cases") + 
  annotate("text", x=40, y=13,label = "30% growth rate", angle = 35) + 
  annotate("text", x=60, y=11.5,label = "20% growth rate", angle = 35, size=4) + 
  annotate("text", x=40, y=4.5,label = "10% growth rate", angle = 35, size=4) + 
  annotate("text", x=80, y=4.9,label = "5% growth rate", angle = 34, size=4) + 
  annotate("text", x=189, y=300,label = "Restaurants and \n shops closed", size=4, color = 'red') + 
  annotate("text", x=63, y=100,label = "Social events \n over 100 banned", size=4, color = 'red')


#Daily infected percentage out of tested cases 
ggplot(COVID_Combined, aes(x=datum, y=percent_of_infected, label = percent_of_infected)) + geom_smooth() + geom_point() + 
  geom_line(size=0.1) + 
  labs(title = "COVID-19 Czech Republic - Daily percentage of COVID cases (compared to number of daily tests)") + 
  geom_text(check_overlap = TRUE, size = 3, nudge_y = 1) + 
  geom_vline(xintercept = COVID_Combined$datum[48], linetype = "dashed", color="red") + 
  geom_vline(xintercept = COVID_Combined$datum[44], linetype = "dashed", color="red") + 
  annotate("text", x=COVID_Combined$datum[50], y=23,label = "Restaurants and \n shops closed", size=4, color = 'red') + 
  annotate("text", x=COVID_Combined$datum[42], y=20,label = "Social events \n over 100 banned", size=4, color = 'red')


#remove first 40 columns for next two graphs (outliers that make no sense)
outliers <- c(1:40)
COVID_Combined <- COVID_Combined[-outliers,]

#Daily growth rate of confirmed cases in percentages
ggplot(COVID_Combined, aes(x=pocet_celkem, y=percent_increase, label = datum)) + geom_smooth() + geom_point() + 
  scale_x_log10() + scale_y_log10() + 
  labs(title = "COVID-19 Czech Republic - Daily growth rate of confirmed cases in percentages") + 
  geom_text(check_overlap = TRUE, size = 3, nudge_y = 0.03) + 
  geom_vline(xintercept = 189, linetype = "dashed", color="red") + 
  geom_vline(xintercept = 63, linetype = "dashed", color="red") +
  annotate("text", x=189, y=80,label = "Restaurants and \n shops closed", size=4, color = 'red') + 
  annotate("text", x=63, y=100,label = "Social events \n over 100 banned", size=4, color = 'red')






