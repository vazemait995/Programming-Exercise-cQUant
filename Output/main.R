library(caret)
library(dplyr)
library(reshape2)
library(lubridate)
library(tidyverse)

setwd("C:/Maitreya/R/")

#Task 1
histdata2017<-read.csv("Input/historicalPriceData_ERCOT_DA_Prices_2017.csv", sep=",", header=TRUE, stringsAsFactors=FALSE)
histdata2016 <- read.csv("Input/historicalPriceData_ERCOT_DA_Prices_2016.csv", sep=",", header=TRUE, stringsAsFactors=FALSE)
histdata2018 <- read.csv("Input/historicalPriceData_ERCOT_DA_Prices_2018.csv", sep=",", header=TRUE, stringsAsFactors=FALSE)
histdata2019 <- read.csv("Input/historicalPriceData_ERCOT_DA_Prices_2019.csv", sep=",", header=TRUE, stringsAsFactors=FALSE)

histdata_allyears <- rbind(histdata2016,histdata2017,histdata2018,histdata2019)

#Locational Nature of Power Prices
#Task 2

histdata_allyears$Date<-as.POSIXct(histdata_allyears$Date, format = "%Y-%m-%d %H:%M:%S")

settlement_pt<- histdata_allyears %>% 
                select(SettlementPoint,Price,Date) %>% mutate(Year = year(Date), Month = month(Date)) %>% group_by(SettlementPoint,Year,Month) %>% summarise(AveragePrice = mean(Price))

#Task 3
write.csv(settlement_pt,"Output/AveragePriceByMonth.csv", row.names = FALSE)


#Price Volatility
#Task 4

hourly_vol <- histdata_allyears %>% filter(startsWith(SettlementPoint, 'HB') & Price > 0) %>% mutate(Year = year(Date), Hour = hour(Date)) %>% group_by(SettlementPoint, Year) %>% 
  summarise( HourlyVolatility = sd(c(0, diff(log(Price), lag = 1)))) 

#Task 5  
write.csv(hourly_vol,"Output/HourlyVolatilityByYear.csv", row.names = FALSE)

#Task 6
maxvol <- hourly_vol %>% group_by(Year) %>%  summarise(MaxHourlyVolatility = max(HourlyVolatility)) 

MaxVolatilityByYear <- left_join(maxvol,hourly_vol, by = c("Year", "MaxHourlyVolatility" = "HourlyVolatility"))
write.csv(MaxVolatilityByYear,"Output/MaxVolatilityByYear.csv", row.names = FALSE)


#Data Translation and Formatting
#Task 7
histdata <- histdata_allyears %>%  mutate(date = date(Date), Hour = hour(Date)) %>% group_by(SettlementPoint, date , Hour) %>% select(SettlementPoint, date , Hour, Price)

hist1 <- dcast(histdata, SettlementPoint + date ~ Hour, mean)

df2<- c("Variable","Date")
df3 <- list()

for(a in 1:24){
  
  df3 = c(df3,paste0("X",a))
  
}

df2 <- c(df2,df3)

names(hist1) <- df2
    
df1 <- unique(hist1$SettlementPoint)

for( i in df1){
  temp <- hist1 %>% filter(Variable == i)
  write.csv(temp,paste0("Output/formattedSpotHistory/spot_",i,".csv"), row.names = FALSE )
}



#Bonus Task
hubspot <-  histdata_allyears %>% 
  select(SettlementPoint,Price,Date) %>% mutate(date = date(Date), Year = year(Date), Month = month(Date)) %>% group_by(SettlementPoint,Year,Month) %>% summarise(AveragePrice = mean(Price)) %>% filter(startsWith(SettlementPoint, 'HB')) 
  
ggplot(data=hubspot , aes(x=, y=, group=1)) +
  geom_line()

