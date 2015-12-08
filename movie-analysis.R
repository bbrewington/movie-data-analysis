# Load Packages
library(lubridate)
library(dplyr)
library(ggplot2)

# Get Consumer Price Index from stlouisfed.org website
# Thanks to stackoverflow user brash-equilibrium for this code
# Link to source: http://stackoverflow.com/questions/12590180/inflation-adjusted-prices-package

cpi.yearly.adjust <- function(){
     monthly_cpi <- read.csv("http://research.stlouisfed.org/fred2/data/CPIAUCSL.csv", header = TRUE)
     names(monthly_cpi) <- c("date", "value")
     monthly_cpi$cpi_year <- year(monthly_cpi$date)
     yearly_cpi <- monthly_cpi %>% group_by(cpi_year) %>% summarize(cpi = mean(value))
     yearly_cpi$adj_factor <- yearly_cpi$cpi / yearly_cpi$cpi[yearly_cpi$cpi_year == 2005]
     names(yearly_cpi) <- c("year", "cpi", "adj_factor")
     yearly_cpi
}

cpi.yearly <- cpi.yearly.adjust()

# Load movie dataset (comes with package ggplot2)
data(movies)

# Set up movie yearly summary data
movies.yearlysummary <- movies %>% filter(Short == 0 & mpaa!="") %>% group_by(year) %>% 
     summarise(movies.count = n(), 
               length.mean = mean(length, na.rm=T), length.sd = sd(length, na.rm=T), 
               rating.mean = mean(rating, na.rm=T), rating.sd = sd(rating, na.rm=T), 
               votes.mean = mean(votes, na.rm=T), votes.sd = sd(votes, na.rm=T),
               budget.mean = mean(budget, na.rm=T), budget.sd = sd(budget, na.rm=T)) %>%
     mutate(length.rel = length.sd/length.mean, rating.rel = rating.sd / rating.mean,
            votes.rel = votes.sd / votes.mean, budget.rel = budget.sd / budget.mean,
            budget.per.rating = budget.mean / rating.mean)
movies.yearlysummary <- left_join(movies.yearlysummary, cpi.yearly, by="year") %>%
     mutate(budget.mean.adj = budget.mean * adj_factor, budget.sd.adj = budget.sd * adj_factor)

# Plot: Mean adjusted budget by year, with error bars of adjusted budget standard deviation
ggplot(movies.yearlysummary, 
       aes(year, budget.mean.adj))+geom_point()+
     geom_pointrange(aes(year, 
                         ymin=budget.mean.adj-.5*budget.sd.adj,
                         ymax=budget.mean.adj+.5*budget.sd.adj))
