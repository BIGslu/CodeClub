library(tidytuesdayR)
library(tidyverse)

#### Data ####
dat <- tidytuesdayR::tt_load("2022-05-03")

#Explore the data
head(dat$capacity)
head(dat$wind)
head(dat$solar)
head(dat$average_cost)

#### Solar plot ####
#Combine capacity and cost data
dat2 <- full_join(dat$wind, dat$solar) %>%
  #Get year from date
  separate(date, into="year", sep = "-", extra="drop") %>% 
  #Averages per year
  group_by(year) %>% 
  summarise(across(c(wind_mwh:solar_capacity), 
                   list(mean=~mean(., na.rm=TRUE), 
                        sd=~sd(., na.rm=TRUE))), 
            .groups = "drop") %>% 
  #remove year missing wind data
  filter(year != 2021) %>%
  #make year numeric
  mutate(year = as.numeric(year)) %>% 
  #merge with cost data
  inner_join(dat$average_cost)

#Solar capacity vs cost
ggplot(dat2, aes(x=solar_capacity_mean, 
                 y=solar_mwh)) +
  geom_point(aes(color=as.character(year)))

#Add error bars and swap xy axes
ggplot(dat2, aes(y=solar_capacity_mean, 
                 x=solar_mwh)) +
  geom_point(aes(color=as.character(year))) +
  geom_errorbar(aes(ymin=solar_capacity_mean-solar_capacity_sd,
                    ymax=solar_capacity_mean+solar_capacity_sd))

#add trend lines
ggplot(dat2, aes(y=solar_capacity_mean, 
                 x=solar_mwh)) +
  
  geom_errorbar(aes(ymin=solar_capacity_mean-solar_capacity_sd,
                    ymax=solar_capacity_mean+solar_capacity_sd),
                color="grey") +
  geom_point(aes(color=as.character(year))) +
  geom_smooth(aes(group = solar_mwh < 75),
              method="lm", color="black",
              se=FALSE)

#curved trendline instead
ggplot(dat2, aes(y=solar_capacity_mean, 
                 x=solar_mwh)) +
  
  geom_errorbar(aes(ymin=solar_capacity_mean-solar_capacity_sd,
                    ymax=solar_capacity_mean+solar_capacity_sd),
                color="grey") +
  geom_point(aes(color=as.character(year))) +
  geom_smooth(method="loess", color="black",
              se=FALSE)

# beautify labels, etc
ggplot(dat2, aes(y=solar_capacity_mean, 
                 x=solar_mwh)) +
  
  geom_errorbar(aes(ymin=solar_capacity_mean-solar_capacity_sd,
                    ymax=solar_capacity_mean+solar_capacity_sd),
                color="grey") +
  geom_point(aes(color=as.character(year))) +
  geom_smooth(method="loess", color="black",
              se=FALSE) +
  theme_bw() +
  labs(x = "Cost ($/mwh)",
       y="Mean capacity (mwh)",
       title="Solar",
       color="Year")

#### Solar and wind plot ####
#combine wind and solar
##Rename columns so can bind
dat3a <- dat$wind %>% 
  rename(mwh=wind_mwh, capacity=wind_capacity) %>% 
  mutate(source = "wind")

dat3b <- dat$solar %>% 
  rename(mwh=solar_mwh, capacity=solar_capacity) %>% 
  mutate(source = "solar")

#Bind and calculate mean + sd
dat3 <- bind_rows(dat3a, dat3b) %>% 
  #Get year from date
  separate(date, into="year", sep = "-", 
           extra="drop") %>% 
  #Averages per year
  group_by(year, source) %>% 
  summarise(across(c(mwh:capacity), 
                   list(mean=~mean(., na.rm=TRUE), 
                        sd=~sd(., na.rm=TRUE))), 
            .groups = "drop") %>% 
  #remove year missing wind data
  filter(year != 2021) %>%
  #make year numeric
  mutate(year = as.numeric(year)) 

#Facet plot to make sure it's doing what we want
ggplot(dat3, aes(y=capacity_mean, 
                 x=mwh_mean)) +
  
  geom_errorbar(aes(ymin=capacity_mean-capacity_sd,
                    ymax=capacity_mean+capacity_sd),
                color="grey") +
  geom_point(aes(fill=as.character(year)), 
             pch=21) +
  geom_smooth(method="loess", color="black",
              se=FALSE) +
  facet_wrap(~source)

#combine plots into 1
ggplot(dat3, aes(y=capacity_mean, 
                 x=mwh_mean)) +
  
  geom_errorbar(aes(ymin=capacity_mean-capacity_sd,
                    ymax=capacity_mean+capacity_sd),
                color="grey") +
  geom_point(aes(fill=as.character(year)), 
             pch=21) +
  geom_smooth(aes(group = source=="solar", 
                  color=source),
              method="loess",
              se=FALSE) +
  theme_bw() +
  labs(x = "Cost ($/mwh)",
       y="Mean capacity (mwh)",
       color="Year") +
  scale_color_manual(values=c("red","black"))
