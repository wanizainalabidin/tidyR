#Install the Dataset to be cleaned
install.packages("tidytuesdayR")

transit <- tidytuesdayR::tt_load('2021-01-05')
transit<- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-05/transit_cost.csv')
transit <- transit %>% rename(ID = e, RailRoad = rr, SourceData= source1, CostSourceData= source2)

#Finding the NAs and removing the NAs
transit <- transit[!is.na(transit$country),]
transit[!complete.cases(transit$end_year) & !complete.cases(transit$start_year),] 
transit <- na.omit(transit)


#Remove reference columns that are not necessary for analysis. 
transit$reference <- NULL

#Converting the data type of data frame
transit$cost_km_millions <- as.numeric(signif(transit$cost_km_millions,3))
transit$year <- format(transit$year, "%Y")
transit$year <- as.Date(transit$year, "%Y")
transit$length <- signif(as.numeric(transit$length),)
transit$cost <- as.numeric(transit$cost)
transit$ppp_rate <- round(as.numeric(transit$ppp_rate),2)
transit$real_cost <- round(as.numeric(transit$real_cost))
transit$stations <- as.numeric(transit$stations)
transit$tunnel <- signif(as.numeric(transit$tunnel),1)

#installing and loading more packages to format percentages
library(scales)
install.packages("formattable")
library("formattable")

transit$tunnel_per <- percent(transit$tunnel_per, 2)

#use glimpse() to get a sense of the data and the data type of data set
transit %>% glimpse()

#Exploratory Data Analysis 
TransitCostCountries <- transit %>% group_by(country, cost_km_millions, length, line) %>% select(country, cost_km_millions, length,line) %>% 
  arrange(country, cost_km_millions)
view(TransitCostCountries)

#Finding the min cost for each Km and the maximum length
min(transit$cost_km_millions)
max(transit$length)
max(cost_km_millions)

#Visualisation of the cost_km_millions
boxplot(transit$cost_km_millions)

#Finding the countries with the max of lengh or min of cost for each mkm
transit %>% filter(cost_km_millions == 7.79 | length == 200) %>% select(country, cost_km_millions, length)

#US has the highest cost_km_million for a short length
transit %>% filter(cost_km_millions == 3930) %>% select(country, cost_km_millions, length)

#Finding the cost for the length of train 
transit %>% filter(country=="US")%>% select(country, cost_km_millions,length, real_cost) %>%
  arrange(cost_km_millions, real_cost,length)

#outliers detection of values
outlier <-boxplot.stats(transit$cost_km_millions)$out

#Identifying rows with outliers
which(transit$cost_km_millions %in% outlier)

#finding the values of the boxplot by calling the summary function
summary(transit$cost_km_millions)

#Identifying which values are the outliers
transit[c(1,3,4,5,6,7,9,10,11,12,13,16,22,33, 45, 71, 72, 73,
          76,96, 107, 109, 133, 134, 135, 136, 137, 147, 160, 161, 172,
          175, 176, 183, 206, 214,227, 408, 409, 414), 
        c("country", "cost_km_millions","length")]

outlierCountry <- transit %>% slice(c(1,3,4,5,6,7,9,10,11,12,13,16,22,33, 45, 71, 72, 73,
                     76,96, 107, 109, 133, 134, 135, 136, 137, 147, 160, 161, 172,
                     175, 176, 183, 206, 214,227, 408, 409, 414)) %>% 
  select(country, real_cost, length) %>% arrange(country, real_cost)

#Exploring which country has the highest value for money through real cost value
min(transit$real_cost)
max(transit$real_cost)

#Finding the country with the highest real cost value
transit[transit$real_cost == 45604, c("country", "real_cost", "length")]

#Visualising the lenght through a boxplot
summary(transit$length)
boxplot(length)

#Finding the country with the smallest length and the associated cost
transit[transit$length == 0.6,c("real_cost", "country", "length", "cost_km_millions")]

#Finding the country with the lowest cost and the associated length
transit[transit$real_cost == 66, c("country","length", "real_cost", "cost_km_millions")]

#Outlier detection of the value, length
outlierLength <- boxplot.stats(transit$length)$out
which(transit$length %in% outlierLength)

#Finding the countries, real cost, length and cost_km_millions for outlier values
OutlierLength <- transit %>% slice(c(27,51,95,115,116,120,193,199,202,324,338,370,406,410)) %>% 
  select(country, real_cost, length, cost_km_millions) %>% arrange(country,real_cost, length)

#Grouping the length to analyse inter and intra group comparison of length and cost.
LengthGroup1 <- transit[transit$length >=0.5 & transit$length <=6, c("country", "length", "real_cost", "cost_km_millions")]
LengthGroup2 <- transit[transit$length >6 & transit$length <= 15, c("country", "length", "real_cost", "cost_km_millions")]
LengthGroup3 <- transit[transit$length > 15 & transit$length <=28, c("country", "length","real_cost", "cost_km_millions")]
LengthGroup4 <- transit[transit$length >28 & transit$length <= 50, c("country","length","real_cost","cost_km_millions")]
LengthGroup5 <- transit[transit$length >50 & transit$length <= 75, c("country","length","real_cost","cost_km_millions")]
LengthGroup6 <- transit[transit$length>75 & transit$length <= 90, c("country","length","real_cost","cost_km_millions")]
LengthGroup7 <- transit[transit$length > 90 & transit$length <= 150, c("country","length","real_cost","cost_km_millions")]
LengthGroup8 <- transit[transit$length >150, c("country","length","real_cost","cost_km_millions")]

