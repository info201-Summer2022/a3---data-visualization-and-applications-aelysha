#Load Necessary Packages
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(plotly)
library(usmap)
library(tidyr)
library(lubridate)
library(patchwork)
#Read in the Data and Assign to Variable
incarc_trends <- read.csv("incarceration_trends.csv")

#View Data
View(incarc_trends)

#possible vars of interest - juv populations vs adult pop, admit/release rates, private jails

#Get data only from years 2000-2018
trends_after_1999 <- filter(incarc_trends, year >= 2000)
trends_2000_2018 <- mutate(trends_after_1999,
                           state_fips = fips(trends_after_1999$state))

#Create a loop that takes the sum of all population data (ages 15 to 64)
#For each county within a state and returns those sums for each year of interest

#Specify dimensions of matrix to fit data into (number of states and years)
pop_sum_by_state <- matrix(0, nrow = 51, ncol = 19)

for(i in 2000:2018){
  
  for(j in 1:51){
    temporary_data <- trends_2000_2018 %>% 
      filter(year == i & state_fips == unique(trends_2000_2018$state_fips)[j])
    
    pop_sum_by_state[j, i-1999] <- sum(temporary_data$total_pop_15to64)
  }
  
}

#Coerce into the dataframe type
pop_sum_by_state <- as.data.frame(pop_sum_by_state)
#Modify column and row names into years and states
colnames(pop_sum_by_state) <- as.character(c(2000:2018))
rownames(pop_sum_by_state) <- unique(trends_after_1999$state)
#Add a column which calculates the average population from the last 19 years
state_populations_after_1999 <- pop_sum_by_state %>% mutate(average_pop = rowSums(pop_sum_by_state)/19)

#Only pull data from 6 states with the largest populations
top_6_states <- head(state_populations_after_1999[order(state_populations_after_1999$average_pop, decreasing = TRUE),])

#Create separate dataframes for each state within our years of interest to
#Individually analyze which categories have excessive null values to guide research
just_ny <- filter(trends_2000_2018, state == "NY")
just_ca <- filter(trends_2000_2018, state == "CA")
just_tx <- filter(trends_2000_2018, state == "TX")
just_il <- filter(trends_2000_2018, state == "IL")
just_pa <- filter(trends_2000_2018, state == "PA")
just_fl <- filter(trends_2000_2018, state == "FL")

#Create a loop that takes the sum of all black jail population data
#For each county within a state and returns those sums for each year of interest
#THIS WILL TREAT NULL VALUES AS ZEROS!!

#Specify dimensions of matrix to fit data into (number of states and years)
black_jail_pop_sum <- matrix(0, nrow = 51, ncol = 19)

for(i in 2000:2018){
  
  for(j in 1:51){
    temporary_data_2 <- trends_2000_2018 %>% 
      filter(year == i & state_fips == unique(trends_2000_2018$state_fips)[j])
    
    black_jail_pop_sum[j, i-1999] <- sum(temporary_data_2$black_jail_pop, na.rm = TRUE)
  }
  
}

#Coerce into the dataframe type
black_jail_pop_sum <- as.data.frame(black_jail_pop_sum)
#Modify column and row names into years and states
colnames(black_jail_pop_sum) <- as.character(c(2000:2018))
rownames(black_jail_pop_sum) <- unique(trends_after_1999$state)

#Only pull black jail population data from the 6 states with the largest populations
top_6_black_jail <- black_jail_pop_sum[c('CA', 'TX', 'NY', 'FL', 'IL', 'PA'),]
#Reverse layout of rows and columns, swap them
long_top_6_black_jail <- as.data.frame(t(top_6_black_jail))

#Create a vector for the (roughly estimated) dates when the figures were reported
years_of_interest <- c("2000-06-30", "2001-06-30", "2002-06-30", "2003-06-30", "2004-06-30", "2005-06-30",
                                    "2006-06-30", "2007-06-30", "2008-06-30", "2009-06-30", "2010-06-30", "2011-06-30",
                                    "2012-06-30", "2013-06-30", "2014-06-30", "2015-12-31", "2016-12-31", "2017-06-30",
                                    "2018-06-30")
#Add a column to the dataframe with these dates specifically as the date type
t6_black_jail_dates <- mutate(long_top_6_black_jail,
                              date = ymd(years_of_interest, truncated = 2L))
#Rename the rows as numbers instead of years
rownames(t6_black_jail_dates) <- c("1", "2", "3", "4", "5", "6",
                                   "7", "8", "9", "10", "11", "12",
                                   "13", "14", "15", "16", "17", "18",
                                   "19")

#Create a loop that takes the sum of all white jail population data
#For each county within a state and returns those sums for each year of interest
#THIS WILL TREAT NULL VALUES AS ZEROS!!

#Specify dimensions of matrix to fit data into (number of states and years)
white_jail_pop_sum <- matrix(0, nrow = 51, ncol = 19)

for(i in 2000:2018){
  
  for(j in 1:51){
    temporary_data_3 <- trends_2000_2018 %>% 
      filter(year == i & state_fips == unique(trends_2000_2018$state_fips)[j])
    
    white_jail_pop_sum[j, i-1999] <- sum(temporary_data_3$white_jail_pop, na.rm = TRUE)
  }
  
}

#Coerce into the dataframe type
white_jail_pop_sum <- as.data.frame(white_jail_pop_sum)
#Modify column and row names into years and states
colnames(white_jail_pop_sum) <- as.character(c(2000:2018))
rownames(white_jail_pop_sum) <- unique(trends_after_1999$state)

#Only pull black jail population data from the 6 states with the largest populations
top_6_white_jail <- white_jail_pop_sum[c('CA', 'TX', 'NY', 'FL', 'IL', 'PA'),]
#Reverse layout of rows and columns, swap them
long_top_6_white_jail <- as.data.frame(t(top_6_white_jail))

#Add a column to the dataframe with these dates specifically as the date type
t6_white_jail_dates <- mutate(long_top_6_white_jail,
                    date = ymd(years_of_interest, truncated = 2L))
#Rename the rows as numbers instead of years
rownames(t6_white_jail_dates) <- c("1", "2", "3", "4", "5", "6",
                                     "7", "8", "9", "10", "11", "12",
                                     "13", "14", "15", "16", "17", "18",
                                     "19")

#Create a Time Series which includes lines for the 6 states with the largest populations
#And presents data on total black jail populations by year from 2000 to 2018
black_jail_pop_viz <- plot_ly(t6_black_jail_dates, type = 'scatter', mode = 'lines')%>%
  add_trace(x = ~date, y = ~CA, name = 'California')%>%
  add_trace(x = ~date, y = ~TX, name = 'Texas')%>%
  add_trace(x = ~date, y = ~NY, name = 'New York')%>%
  add_trace(x = ~date, y = ~FL, name = 'Florida')%>%
  add_trace(x = ~date, y = ~IL, name = 'Illinois')%>%
  add_trace(x = ~date, y = ~PA, name = 'Pennsylvania')
black_jail_pop_viz <- black_jail_pop_viz %>%
  layout(title = list(text ='Total Black Jail Population in Top 6 Populous States',
                      y = 0.99),
    xaxis = list(title = 'Date',
                 zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff', 
                 width = 500),
    yaxis = list(title = 'Number of Individuals',
                 zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff',
                 tick0 = 0,
                 dtick= 5000, 
                 height = 500),
    legend = list(title = list(text = 'State')),
    plot_bgcolor='#e5ecf6')
#View the plot
black_jail_pop_viz

#Create a Time Series which includes lines for the 6 states with the largest populations
#And presents data on total white jail populations by year from 2000 to 2018
white_jail_pop_viz <- plot_ly(t6_white_jail_dates, type = 'scatter', mode = 'lines')%>%
  add_trace(x = ~date, y = ~CA, name = 'California')%>%
  add_trace(x = ~date, y = ~TX, name = 'Texas')%>%
  add_trace(x = ~date, y = ~NY, name = 'New York')%>%
  add_trace(x = ~date, y = ~FL, name = 'Florida')%>%
  add_trace(x = ~date, y = ~IL, name = 'Illinois')%>%
  add_trace(x = ~date, y = ~PA, name = 'Pennsylvania')
white_jail_pop_viz <- white_jail_pop_viz %>%
  layout(title = list(text ='Total White Jail Population in Top 6 Populous States',
                      y = 0.99),
         xaxis = list(title = 'Date',
                      zerolinecolor = '#ffff',
                      zerolinewidth = 2,
                      gridcolor = 'ffff', 
                      width = 500),
         yaxis = list(title = 'Number of Individuals',
                      zerolinecolor = '#ffff',
                      zerolinewidth = 2,
                      gridcolor = 'ffff',
                      tick0 = 0,
                      dtick= 5000, 
                      height = 500),
         legend = list(title = list(text = 'State')),
         plot_bgcolor='#e5ecf6')
#View the plot
white_jail_pop_viz

################################### SECOND PLOT 

#Create a loop that takes the sum of all black prison admissions data
#For each county within a state and returns those sums for each year of interest
#THIS WILL TREAT NULL VALUES AS ZEROS!!

#Specify dimensions of matrix to fit data into (number of states and years)
black_adm_sums <- matrix(0, nrow = 51, ncol = 19)

for(i in 2000:2018){
  
  for(j in 1:51){
    temporary_data_4 <- trends_2000_2018 %>% 
      filter(year == i & state_fips == unique(trends_2000_2018$state_fips)[j])
    
    black_adm_sums[j, i-1999] <- sum(temporary_data_4$black_prison_adm, na.rm = TRUE)
  }
  
}

#Coerce into the dataframe type
black_adm_sums <- as.data.frame(black_adm_sums)
#Modify column and row names into years and states
colnames(black_adm_sums) <- as.character(c(2000:2018))
rownames(black_adm_sums) <- unique(trends_after_1999$state)

#Add a column which calculates the average black prison admissions from the last 19 years
avg_black_adm <- black_adm_sums %>% mutate(average_adm = rowSums(black_adm_sums)/19)

#Create a loop that takes the sum of all black prison admissions data
#For each county within a state and returns those sums for each year of interest
#THIS WILL TREAT NULL VALUES AS ZEROS!!

#Specify dimensions of matrix to fit data into (number of states and years)
white_adm_sums <- matrix(0, nrow = 51, ncol = 19)

for(i in 2000:2018){
  
  for(j in 1:51){
    temporary_data_5 <- trends_2000_2018 %>% 
      filter(year == i & state_fips == unique(trends_2000_2018$state_fips)[j])
    
    white_adm_sums[j, i-1999] <- sum(temporary_data_5$white_prison_adm, na.rm = TRUE)
  }
  
}

#Coerce into the dataframe type
white_adm_sums <- as.data.frame(white_adm_sums)
#Modify column and row names into years and states
colnames(white_adm_sums) <- as.character(c(2000:2018))
rownames(white_adm_sums) <- unique(trends_after_1999$state)
#Add a column which calculates the average white prison admissions from the last 19 years
avg_white_adm <- white_adm_sums %>% mutate(average_adm = rowSums(white_adm_sums)/19)

#Create a loop that takes the sum of all black population data ages 15 to 64
#For each county within a state and returns those sums for each year of interest

#Specify dimensions of matrix to fit data into (number of states and years)
black_pop_sum_by_state <- matrix(0, nrow = 51, ncol = 19)

for(i in 2000:2018){
  
  for(j in 1:51){
    temporary_data_7 <- trends_2000_2018 %>% 
      filter(year == i & state_fips == unique(trends_2000_2018$state_fips)[j])
    
    black_pop_sum_by_state[j, i-1999] <- sum(temporary_data_7$black_pop_15to64)
  }
  
}

#Coerce into the dataframe type
black_pop_sum_by_state <- as.data.frame(black_pop_sum_by_state)
#Modify column and row names into years and states
colnames(black_pop_sum_by_state) <- as.character(c(2000:2018))
rownames(black_pop_sum_by_state) <- unique(trends_after_1999$state)

#Add a column which calculates the average black population from the last 19 years
blk_state_populations_after_1999 <- black_pop_sum_by_state %>% mutate(average_pop = rowSums(black_pop_sum_by_state)/19)

#Create a loop that takes the sum of all white population data ages 15 to 64
#For each county within a state and returns those sums for each year of interest

#Specify dimensions of matrix to fit data into (number of states and years)
white_pop_sum_by_state <- matrix(0, nrow = 51, ncol = 19)

for(i in 2000:2018){
  
  for(j in 1:51){
    temporary_data_8 <- trends_2000_2018 %>% 
      filter(year == i & state_fips == unique(trends_2000_2018$state_fips)[j])
    
    white_pop_sum_by_state[j, i-1999] <- sum(temporary_data_8$white_pop_15to64)
  }
  
}

#Coerce into the dataframe type
white_pop_sum_by_state <- as.data.frame(white_pop_sum_by_state)
#Modify column and row names into years and states
colnames(white_pop_sum_by_state) <- as.character(c(2000:2018))
rownames(white_pop_sum_by_state) <- unique(trends_after_1999$state)

#Add a column which calculates the average white population from the last 19 years
whi_state_populations_after_1999 <- white_pop_sum_by_state %>% mutate(average_pop = rowSums(white_pop_sum_by_state)/19)

#Create a loop that takes the sum of all prison admissions data 
#For each county within a state and returns those sums for each year of interest

#Specify dimensions of matrix to fit data into (number of states and years)
total_prison_adm <- matrix(0, nrow = 51, ncol = 19)

for(i in 2000:2018){
  
  for(j in 1:51){
    temporary_data_9 <- trends_2000_2018 %>% 
      filter(year == i & state_fips == unique(trends_2000_2018$state_fips)[j])
    
    total_prison_adm[j, i-1999] <- sum(temporary_data_9$total_prison_adm, na.rm = TRUE)
  }
  
}

#Coerce into the dataframe type
total_prison_adm <- as.data.frame(total_prison_adm)
#Modify column and row names into years and states
colnames(total_prison_adm) <- as.character(c(2000:2018))
rownames(total_prison_adm) <- unique(trends_after_1999$state)
#Add a column which calculates the average prison admissions from the last 19 years
average_prison_adm <- total_prison_adm %>% mutate(average_adm = rowSums(total_prison_adm)/19)

#Create vectors for state names, average population figures, and average prison admission figures
#In order to create a new dataframe to be used for plotting
States <- unique(trends_after_1999$state)
Average_White_Population <- whi_state_populations_after_1999$average_pop
Average_Black_Population <- blk_state_populations_after_1999$average_pop
Black_Prison_Admissions <- avg_black_adm$average_adm
White_Prison_Admissions <- avg_white_adm$average_adm
Total_Admissions <- average_prison_adm$average_adm

#Create a dataframe with the vectors from above
pop_and_admissions <- data.frame(States, Average_Black_Population,
                                 Average_White_Population, Black_Prison_Admissions,
                                 White_Prison_Admissions,
                                 Total_Admissions)
#Add columns for black and white proportions of admissions to race-specific population and admissions to total admissions
proportional_admissions <- mutate(pop_and_admissions,
                                  Popn_Proportional_White_Admissions = White_Prison_Admissions/Average_White_Population,
                                  Popn_Proportional_Black_Admissions = Black_Prison_Admissions/Average_Black_Population,
                                  Adm_Prop_White = White_Prison_Admissions/Total_Admissions,
                                  Adm_Prop_Black = Black_Prison_Admissions/Total_Admissions)
#Create a dataframe that removes Arkansas, as it is an extreme outlier in plot #3
prop_adm_no_ak <- proportional_admissions[-2,]
#Create a dataframe that removes null values, so only real proportions (not NaN) are used for plot #6
prop_adm_no_null <- na.omit(proportional_admissions)

# #Find the linear model for plot 1 in order to get summary stats on the linear regression and find an R2 value
# lin_reg_plot1 <- lm(proportional_admissions$Average_Black_Population ~ proportional_admissions$Black_Prison_Admissions)
# lin_reg_summary_plot1 <- summary(lin_reg_plot1)
# R2_value_plot1 <- summary(lin_reg_plot1)$r.squared

# #Create an interactive scatterplot with a best-fit line to compare black admissions to black population
# black_adm_vs_pop <- ggplot(proportional_admissions, aes(x= Black_Prison_Admissions, y= Average_Black_Population)) + 
#   geom_point(aes(col=States)) +
#   labs(title = "Average Black Prison Admissions vs. Average Black Population by State (2000-2018)",
#        x= "Average Black Prison Admissions (# Individuals)",
#        y= "Average Black Population (15-64)") +
#   theme(plot.title = element_text(hjust = 0.5)) +
#   annotate("text", x=20000, y=2750000, label = "R2= 0.7448512") +
#   geom_smooth(method=lm , color="black", se=FALSE)
# #View plot
# black_adm_vs_pop_interactive <- ggplotly(black_adm_vs_pop)

# #Find the linear model for plot 2 in order to get summary stats on the linear regression and find an R2 value
# lin_reg_plot2 <- lm(proportional_admissions$Average_White_Population ~ proportional_admissions$White_Prison_Admissions)
# lin_reg_summary_plot2 <- summary(lin_reg_plot2)
# R2_value_plot2 <- summary(lin_reg_plot2)$r.squared

# #Create an interactive scatterplot with a best-fit line to compare white admissions to white population
# white_adm_vs_pop <- ggplot(proportional_admissions, aes(x= White_Prison_Admissions, y= Average_White_Population)) + 
#   geom_point(aes(col=States)) +
#   labs(title = "Average White Prison Admissions vs. Average White Population by State (2000-2018)",
#        x= "Average White Prison Admissions (# Individuals)",
#        y= "Average White Population (15-64)") +
#   theme(plot.title = element_text(hjust = 0.5)) +
#   annotate("text", x=20000, y=12000000, label = "R2= 0.6521701") +
#   geom_smooth(method=lm , color="black", se=FALSE)
# #View plot
# white_adm_vs_pop_interactive <- ggplotly(white_adm_vs_pop)

#Find the linear model for plot 3 in order to get summary stats on the linear regression and find an R2 value
lin_reg_plot3 <- lm(prop_adm_no_ak$Popn_Proportional_Black_Admissions ~ prop_adm_no_ak$Popn_Proportional_White_Admissions)
lin_reg_summary_plot3 <- summary(lin_reg_plot3)
R2_value_plot3 <- summary(lin_reg_plot3)$r.squared

#Create an interactive scatterplot with a best-fit line to compare white admissions proportional to
#White population, and black admissions proportional to black population
white_vs_black_adm_prop <- ggplot(prop_adm_no_ak, aes(x= Popn_Proportional_White_Admissions, y= Popn_Proportional_Black_Admissions)) + 
  geom_point(aes(col=States)) +
  labs(title = "White Admissions/White Population vs. Black Admissions/Black Population (2000-2018)",
       x= "White Proportional Admissions",
       y= "Black Proportional Admissions") +
  theme(plot.title = element_text(hjust = 0.5)) +
  annotate("text", x=0.003, y=0.0, label = "R2= 0.5728713, Slope = 3.4694030") +
  geom_smooth(method=lm , color="black", se=FALSE)
white_vs_black_adm_prop_interactive <- ggplotly(white_vs_black_adm_prop)
#View plot
white_vs_black_adm_prop_interactive

# #Find the linear model for plot 4 in order to get summary stats on the linear regression and find an R2 value
# lin_reg_plot4 <- lm(proportional_admissions$Total_Admissions ~ proportional_admissions$Black_Prison_Admissions)
# lin_reg_summary_plot4 <- summary(lin_reg_plot4)
# R2_value_plot4 <- summary(lin_reg_plot4)$r.squared
# 
# #Create an interactive scatterplot with a best-fit line to compare black admissions to total admissions
# black_adm_vs_total <- ggplot(proportional_admissions, aes(x= Black_Prison_Admissions, y= Total_Admissions)) + 
#   geom_point(aes(col=States)) +
#   labs(title = "Average Black Prison Admissions vs. Total Admissions (2000-2018)",
#        x= "Black Prison Admissions",
#        y= "Total Prison Admissions") +
#   theme(plot.title = element_text(hjust = 0.5)) +
#   annotate("text", x=20000, y=75000, label = "R2= 0.8332937") +
#   geom_smooth(method=lm , color="black", se=FALSE)
# black_adm_vs_total_interactive <- ggplotly(black_adm_vs_total)
# #View plot
# black_adm_vs_total_interactive

# #Find the linear model for plot 5 in order to get summary stats on the linear regression and find an R2 value
# lin_reg_plot5 <- lm(proportional_admissions$Total_Admissions ~ proportional_admissions$White_Prison_Admissions)
# lin_reg_summary_plot5 <- summary(lin_reg_plot5)
# R2_value_plot5 <- summary(lin_reg_plot5)$r.squared
# 
# #Create an interactive scatterplot with a best-fit line to compare white admissions to total admissions
# white_adm_vs_total <- ggplot(proportional_admissions, aes(x= White_Prison_Admissions, y= Total_Admissions)) + 
#   geom_point(aes(col=States)) +
#   labs(title = "Average White Prison Admissions vs. Total Admissions (2000-2018)",
#        x= "White Prison Admissions",
#        y= "Total Prison Admissions") +
#   theme(plot.title = element_text(hjust = 0.5)) +
#   annotate("text", x=20000, y=76000, label = "R2= 0.8854215") +
#   geom_smooth(method=lm , color="black", se=FALSE)
# white_adm_vs_total_interactive <- ggplotly(white_adm_vs_total)

# #View plot
# white_adm_vs_total_interactive 

#Find the linear model for plot 6 in order to get summary stats on the linear regression and find an R2 value
lin_reg_plot6 <- lm(prop_adm_no_null$Adm_Prop_White ~ prop_adm_no_null$Adm_Prop_Black)
lin_reg_summary_plot6 <- summary(lin_reg_plot6)
R2_value_plot6 <- summary(lin_reg_plot6)$r.squared

#Create an interactive scatterplot with a best-fit line to compare black admissions proportional to
#total admissions, and white admissions proportional to total admissions
black_prop_vs_white_prop <- ggplot(prop_adm_no_null, aes(x= Adm_Prop_Black, y= Adm_Prop_White)) + 
  geom_point(aes(col=States)) +
  labs(title = "Black Admissions/Total Admissions vs. White Admissions/Total Admissions (2000-2018)",
       x= "Black Prison Admissions",
       y= "White Prison Admissions") +
  theme(plot.title = element_text(hjust = 0.5)) +
  annotate("text", x=0.6, y=0.6, label = "R2= 0.08952752, Slope= -0.241162") +
  geom_smooth(method=lm , color="black", se=FALSE)
black_prop_vs_white_prop_interactive <- ggplotly(black_prop_vs_white_prop)

#View plot
black_prop_vs_white_prop_interactive

################################### THIRD PLOT 

#Create a loop that takes the sum of all black prison population data
#For each county within a state and returns those sums for each year of interest
#THIS WILL TREAT NULL VALUES AS ZEROS!!
black_prison_pop_sum <- matrix(0, nrow = 51, ncol = 19)

for(i in 2000:2018){
  
  for(j in 1:51){
    temporary_data_10 <- trends_2000_2018 %>% 
      filter(year == i & state_fips == unique(trends_2000_2018$state_fips)[j])
    
    black_prison_pop_sum[j, i-1999] <- sum(temporary_data_10$black_prison_pop, na.rm = TRUE)
  }
  
}
#Coerce into the dataframe type
black_prison_pop_sum <- as.data.frame(black_prison_pop_sum)
#Modify column and row names into years and states
colnames(black_prison_pop_sum) <- as.character(c(2000:2018))
rownames(black_prison_pop_sum) <- unique(trends_after_1999$state)
#Add a column which calculates the average black prison population from the last 19 years
avg_black_prison_pop <- black_prison_pop_sum %>% mutate(average_black_prison = rowSums(black_prison_pop_sum)/19)

#Create a loop that takes the sum of all white prison population data
#For each county within a state and returns those sums for each year of interest
#THIS WILL TREAT NULL VALUES AS ZEROS!!
white_prison_pop_sum <- matrix(0, nrow = 51, ncol = 19)

for(i in 2000:2018){
  
  for(j in 1:51){
    temporary_data_10 <- trends_2000_2018 %>% 
      filter(year == i & state_fips == unique(trends_2000_2018$state_fips)[j])
    
    white_prison_pop_sum[j, i-1999] <- sum(temporary_data_10$white_prison_pop, na.rm = TRUE)
  }
  
}
#Coerce into the dataframe type
white_prison_pop_sum <- as.data.frame(white_prison_pop_sum)
#Modify column and row names into years and states
colnames(white_prison_pop_sum) <- as.character(c(2000:2018))
rownames(white_prison_pop_sum) <- unique(trends_after_1999$state)
#Add a column which calculates the average white prison population from the last 19 years
avg_white_prison_pop <- white_prison_pop_sum %>% mutate(average_white_prison = rowSums(white_prison_pop_sum)/19)

#Create vectors for fips codes and average prison populations to create dataframes for plotting
fips <- unique(trends_2000_2018$state_fips)
Average_Black_Prison_Population <- avg_black_prison_pop$average_black_prison
Average_White_Prison_Population <- avg_white_prison_pop$average_white_prison
#Create dataframes for regional data on black and white prison populations (averaged from 2000 to 2018)
black_prison_pop_us <- data.frame(fips, Average_Black_Prison_Population)
white_prison_pop_us <- data.frame(fips, Average_White_Prison_Population)

#Create an interactive plot which displays values for black prison populations across the U.S.
avg_blk_prison_us_map <- ggplotly(plot_usmap(data = black_prison_pop_us, 
           values = "Average_Black_Prison_Population",
           color = "white") +
  scale_fill_continuous(name = "Number of Individuals", 
                        label = scales::comma) +
  theme(plot.title = element_text(hjust = 0.5)) +
    labs(title = "Average Black Prison Population, Between 2000 to 2018",
        ))
  
#View Plot
avg_blk_prison_us_map


#Create an interactive plot which displays values for white prison populations across the U.S.
avg_white_prison_us_map <- ggplotly(plot_usmap(data = white_prison_pop_us,
           values = "Average_White_Prison_Population",
           color = "white") + 
  scale_fill_continuous(name = "Number of Individuals", 
                        label = scales::comma) +
  theme(plot.title = element_text(hjust = 0.5)) + 
    labs(title = "Average White Prison Population, Between 2000 to 2018",
         ))
#View Plot
avg_white_prison_us_map







