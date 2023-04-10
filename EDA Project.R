## Libraries
library(tidyverse)
library(ggplot2)
library(grid)
library(gridExtra)
library(RColorBrewer)
library(ggthemes)
library(dplyr)
library(sp)
library(rworldmap)
library(fpp2)
library(tseries)
library(prophet)

## Global Temperatures of countries world map between 1850 - 2012

convert<-function(x){
  westEast<-substr(x,nchar(x),nchar(x))
  xx<-substr(x,1,nchar(x)-1)
  val<-as.numeric(char2dms(paste0(strsplit(xx,'[.]')[[1]][1],'d',strsplit(xx,'[.]')[[1]][1],"'",westEast)))
  return(val)
}

worldMap <- fortify(map_data("world"), region = "region")
m <- ggplot() + 
  geom_map(data = worldMap, map = worldMap,aes(x = long, y = lat, map_id = region, group = group),fill = "white", color = "black", size = 0.1)
allCities<-read.csv('GlobalLandTemperaturesByCity.csv')
allCities<-na.omit(allCities)
allCities$date<-as.Date(allCities$dt)
allCities$year<-as.numeric(format(allCities$date,'%Y'))
allCities <-as.data.frame(allCities %>% filter(year>=1850))
allCities$month<-as.numeric(format(allCities$date,'%m'))
allCities$Longitude<-as.character(allCities$Longitude)
allCities$Latitude<-as.character(allCities$Latitude)

start<-allCities %>% filter(year==1850)
start$LONG<-sapply(start$Longitude,convert) 
start$LAT<-sapply(start$Latitude,convert)
start<-as.data.frame(start %>% group_by(Country, City) %>% select(AverageTemperature, City ,LAT, LONG, Country) %>% summarise(avgTemp_start = mean(AverageTemperature), long = mean(LONG), lat = mean(LAT)))

end<-as.data.frame(allCities %>% filter(year==2012))
end<-as.data.frame(end %>% group_by(Country,City) %>% select(AverageTemperature,City) %>% summarise(avgTemp_end = mean(AverageTemperature)))

res2<-as.data.frame(merge(start,end, by=c('Country','City')))

m + 
  geom_point(data=res2,aes(x=long, y=lat, size=avgTemp_end - avgTemp_start,color=avgTemp_end - avgTemp_start),alpha=.2) + 
  theme_fivethirtyeight() + 
  ggtitle('Temperature difference between 1850 and 2012') + theme(axis.text = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + labs(size = '') + 
  scale_color_gradientn(name='Degrees Celsius',colors=rev(brewer.pal(10,'Spectral'))) + scale_size(guide = 'none')



#### Inference 
* The darker the color of a country, the more variation in temperature was recorded.   Eastern countries of US show red color indicating heavy change in temperature but    India has not shown any drastic change when the temperatures were compared.


## Functions
rm(list = ls())

mean_median = function(df){
  df %>%
    summarize(avg = mean(df$AverageTemperature,na.rm = T), md = median(df$AverageTemperature, na.rm= T))
} ## prints mean and median

check_na = function(df,col){
  print(paste("Col name: ", col))
  sum(is.na(df[col]))
}## checks if any na value exists


## Data pre-processing
df_country = read.csv("GlobalLandTemperaturesByCountry.csv")
head(df_country)


IND_data = df_country %>%
  filter(Country=="India")  %>%
  separate(col = dt, into = c("Year", "Month", "Day"), convert = TRUE) ->cData

## rows having India are only extracted
## splitting the dt column into Year,Month and Day

summary(IND_data)
str(IND_data)

#unique(df_country$Country)
rownames(IND_data) = 1:nrow(IND_data)

## checking if any values exist in the columns

check_na(IND_data, "AverageTemperature")
check_na(IND_data, "Year")

mean_median(IND_data)

IND_data = IND_data %>% ## Data only in and after 1850. Range 1850-2013
  filter(Year >= 1850)

check_na(IND_data, "AverageTemperature")
check_na(IND_data, "Year")
check_na(IND_data, "Month")
mean_median(IND_data)

##Imputating the missing temperature values with median

IND_data = IND_data %>%
  mutate(AverageTemperature = replace(AverageTemperature,is.na(AverageTemperature), median(AverageTemperature,na.rm = T) ))
#### Here the column dt is seperated into year, month and date and the rows with na values in Average Temperature Column are imputated with median
## this stores the rows where year >=2000
Ind_2000 = IND_data %>%
  filter(Year >=2000)
str(IND_data)

Y = ts(Ind_2000$AverageTemperature,start = c(2000,1),frequency = 12)

## Stores yearly avg
Ind_avgYr = IND_data %>% 
  group_by(Year) %>% 
  summarise(Temp = mean(AverageTemperature)) 

## Stores monthly avg
Ind_avgMonth = IND_data %>% 
  group_by(Month) %>% 
  summarise(Temp = mean(AverageTemperature)) 

## Stores the max temp for each year
Ind_maxYr1 = IND_data %>% 
  group_by(Year) %>% 
  summarise(Temp = max(AverageTemperature)) 

## Stores the min temp for each year

Ind_minYr1 = IND_data %>% 
  group_by(Year) %>% 
  summarise(Temp = min(AverageTemperature)) 

Ind_maxYr = Ind_maxYr1[!(Ind_maxYr1$Year=='1862' | Ind_maxYr1$Year=='1863'| Ind_maxYr1$Year=='1864'),]
Ind_minYr = Ind_minYr1[!(Ind_minYr1$Year=='1862' | Ind_minYr1$Year=='1863'| Ind_minYr1$Year=='1864'),]

check_na(IND_data, "AverageTemperature")
check_na(IND_data, "Year")

print(paste("Max avg temp: ",max(IND_data$AverageTemperature)))
print(paste("Min avg temp: ",min(IND_data$AverageTemperature)))


tsdata_full = ts(IND_data$AverageTemperature, frequency = 12)
ddata_full = decompose(tsdata_full, "multiplicative")



## Plots and Analysis 


plot(ddata_full) ## Checking trens, seasonality and randomness of the data. Trend can clearly be seen



### Here we can see the rend clearly but seasonality is not visible properly hence further investigation in that is required. Trend shows that the temperature is increasing 
plot(IND_data$Year,IND_data$AverageTemperature,xlab="Year",ylab="Average Temp",type = "l",ylim = c(15.27,31.329),main = "Avg Monthy Temperature vs Year") #Monthly Avg Temp
## No trend or seasonality is visible bcz of many data points

### Here all the datapoints are ploted hence no seasonality or trend is observed.
plot(10 *diff(log(IND_data$AverageTemperature)), xlab = "year", ylab = "Avg Temp", type="l",lwd=2,ylim=c(-5,5),main = "Avg Temp Garph")
plot(Ind_2000$Year,Ind_2000$AverageTemperature,xlab="Year",ylab="Average Temp",type = "l",ylim = c(15.27,31.329))

qplot(Year, Temp, data=Ind_avgYr, main="India Average Temperature 1850-2013 by yr",geom=c("point","smooth"))+ aes(colour = Temp) + scale_color_gradient(low="blue", high="red") ## Temperature Increasing trend can be seen here

### Here the avg tempearture of all the years are plotted and the trends becomes clearly visible.

qplot(Month, Temp, data=Ind_avgMonth, main="India Average Temperature 1850-2013 by month",geom=c("point","smooth"))+ aes(colour = Temp) + scale_color_gradient(low="blue", high="red") ## monthly seasonality where temp increases till May which is its peek and then decreases


### Monthly seasonality where temp increases till May which is its peek and then decreases

ggseasonplot(window(Y,2000,2013)) + ggtitle("Seasonal plot [2000-2013]") ## COnfirming seasonality for years from 2000 to 2013


### Seasonality remains same for all years from 2000 to 2013 where temp increases till may and then decreases

plot(Ind_avgYr$Year,Ind_avgYr$Temp,xlab="Year",ylab="Average Temp",type = "l",main = "Avg Yearly Temperature vs Year") #Yearly Avg Temp ## Increasing trend



### Here the avg tempearture of all the years are plotted and the trends becomes clearly visible which is increasing trend.

plot(Ind_maxYr$Year,Ind_maxYr$Temp,xlab="Year",ylab="Max Temp",type = "l",main = "Max Yearly Temperature vs Year") #Yearly Max Temp

plot(Ind_minYr$Year,Ind_minYr$Temp,xlab="Year",ylab="Min Temp",type = "l",main = "Max Yearly Temperature vs Year") #Yearly Min Temp

qplot(Year, Temp, data=Ind_maxYr, main="India Max Yearly Temperature 1850-2013",geom=c("point","smooth"))+ aes(colour = Temp) + scale_color_gradient(low="blue", high="red")
qplot(Year, Temp, data=Ind_minYr, main="India Min yearly Temperature 1850-2013",geom=c("point","smooth"))+ aes(colour = Temp) + scale_color_gradient(low="blue", high="red")

### Here the yearly max and min temperatures are plotted and even they show same trend which means that our inference on the avg temperature above was correct.
# 40 yr splits of years

btw_1850_1890 = IND_data %>%
  filter(Year >= 1850 & Year<1890)

btw_1890_1930 = IND_data %>%
  filter(Year >=1890 & Year<1930)

btw_1930_1970 = IND_data %>%
  filter(Year >=1930 & Year<1970)

btw_1970_2013 = IND_data %>%
  filter(Year >=1970 & Year<2013)



plot(btw_1850_1890$AverageTemperature,xlab="Year",ylab="Average Temp",type = "l",main = "Avg Monthy Temperature vs Year between 1850 and 1890")

plot(btw_1890_1930$AverageTemperature,xlab="Year",ylab="Average Temp",type = "l",main = "Avg Monthy Temperature vs Year between 1890 and 1930")

plot(btw_1930_1970$AverageTemperature,xlab="Year",ylab="Average Temp",type = "l",main = "Avg Monthy Temperature vs Year between 1930 and 1970")

plot(btw_1970_2013$AverageTemperature,xlab="Year",ylab="Average Temp",type = "l",main = "Avg Monthy Temperature vs Year between 1970 and 2013")

## We can observe from the 40 yr splits that same pattern is being observed. The temperature increases to a peak which can be considered as the month of May and then decreases. This continues.

### Here 40 yr splits are done
* The temperature range gradually increases as the year reaches 1930.
* This is also seen by seeing range of the graph
* Initially the lower is 15 but then it changes to 20 indicating rise in temperature.

# Seeing boxplots on different yrs
splitYr = IND_data %>% 
  filter(Year==1850 | Year==1890 | Year==1930 | Year==1970 | Year==2013)


splitYr$Year <- as.factor(splitYr$Year)
qplot(x =  Year, y = AverageTemperature, data = splitYr) +
  ggtitle("Average Temperature for 40 Year Intervals")+geom_boxplot(fill="blue1")
## Box plots show temperature steadily has increased

### here 5 years are taken as a difference of 40 years 
* It is observable that that both the range and median increases
* This further confirms the trend

# Taking particular yr - 2000

temp_2000 = IND_data %>%
  filter(Year == 2000)

plot(temp_2000$Month,temp_2000$AverageTemperature,xlab="months",ylab="Average Temp",type = "l",main = "Avg Monthy Temperature of 2000", ylim = c(min(temp_2000$AverageTemperature), max(temp_2000$AverageTemperature)))


### Here seasonality is checked on the yr 2000 and same thing is observed.

### BoxPlots

## Yearly
quantile(IND_data$AverageTemperature, probs = c(0,0.25,0.5,0.75,1))
boxplot(IND_data$AverageTemperature ~ IND_data$Year, main = "Average Temperature",
        ylab = "Avg Temp", xlab = "Years", las = 1)

boxplot(Ind_2000$AverageTemperature ~ Ind_2000$Year, main = "Average Temperature",
        ylab = "Avg Temp", xlab = "Years", las = 1)



### Boxplots are made on Yearly Average Temperatures and the median increases over the years



## Monthly
boxplot(IND_data$AverageTemperature ~ IND_data$Month, main = "Average Temperature",
        ylab = "Avg Temp", xlab = "Months", las = 1) ## again may has the hight temperature range



### Monthly boxplots

#Yearly - Differencing for converting to stationary time series
avg_time = ts(Ind_avgYr$Temp,start = min(Ind_avgYr$Year), end=max(Ind_avgYr$Year), frequency = 1)
plot(avg_time) ## Non stationary time series as mean and variance does not remain same for any 2 periods
plot(log(avg_time),type = "l")
plot(diff(log(avg_time)),type = "l") ## converted to stationary time series

## The initial time series is not stationary meaning the mean and variance any 2 time periods is not same.
## We convert it into stationary time series by differencing the log values.


## We will use Yearly average tempearature to make the model and forecast values
## ARIMA Model 

## AR I  MA
## q  d  p

# p = acf
# q = pacf

acf(avg_time) ## As non stationary time series, all lines are above the blue limit line

pacf(avg_time)

acf(diff(log(avg_time))) ## Most of the lines between the line after converting to stationary

pacf(diff(log(avg_time)))



### For non stationary time series-
* All the lines in acf graph are above the blue line

### For stationary time series-
* Most of the lines fit between the blue lines in both acf and padf graph

## Model
model1 = auto.arima(avg_time, ic = "aic", trace = T)
model1     

acf(ts(model1$residuals))
pacf(ts(model1$residuals))

model2 = auto.arima(IND_data$AverageTemperature, ic = "aic", trace = T)



auto.arima(Ind_avgYr$Temp, ic = "aic", trace= T)

plot.ts(model1$residuals)




### Here using the auto.arima model, the best model obtained is ARIMA(3,1,1). The 1 in center denotes the seasonality observed. It has the least AIC and BIC Values and maximum log liklihood confirming it to be most suitable model.

### The residuals when plotted by acf and pacf show very less auto-correlation betwenn them.

## Forecast next 10 years


forecast1 = forecast(model1, level=c(95),h = 10)
plot(forecast1) ## the trend continues as the avg temperature continues to increase on yearly basis
print(forecast1)
f_df = as.data.frame(forecast1)
print(paste("Avg Temperature in 2020: ",f_df$`Point Forecast`[7]))
print(paste("Avg Temperature in 2021: ",f_df$`Point Forecast`[8]))

print(paste("Avg Temperature in 2022: ",f_df$`Point Forecast`[9]))



### The forecasts although not 100% accurate, are pretty close to real values.
### The forecast for 2021 only varies by 1 degree.
### The forecasted values in plot staify the trend that the temperature increases steadily.

## Validation


Box.test(model1$residuals, lag=5, type="Ljung-Box")

Box.test(model1$residuals, lag=10, type="Ljung-Box")

Box.test(model1$residuals, lag=15, type="Ljung-Box")

## p-values here are all above 0.05 hence good model


### p-values here are all above 0.05 hence good model


## FBs Prophet Model


# column : ds,y

df_y_ts = data.frame(ds=IND_data$Year,y=IND_data$AverageTemperature)

df_y_ts$ds = as.character(df_y_ts$ds)
df_y_ts$ds <- as.Date(df_y_ts$ds, format="%Y")

prop_fit <- prophet(df_y_ts)
Future1 = make_future_dataframe(prop_fit,periods =5, freq = "year")
tail(Future1)

forecast1 = predict(prop_fit,Future1)
tail(forecast1[c('ds','yhat')])
prop_fit$component.modes

dyplot.prophet(prop_fit,forecast1)
prophet_plot_components(prop_fit,forecast1)

df_y_ts = data.frame(ds=Ind_avgYr$Year,y=Ind_avgYr$Temp)

df_y_ts$ds = as.character(df_y_ts$ds)
df_y_ts$ds <- as.Date(df_y_ts$ds, format="%Y")

prop_fit <- prophet(df_y_ts)
Future1 = make_future_dataframe(prop_fit,periods =5, freq = "year")
tail(Future1)

forecast1 = predict(prop_fit,Future1)
tail(forecast1[c('ds','yhat')])
prop_fit$component.modes

dyplot.prophet(prop_fit,forecast1)
prophet_plot_components(prop_fit,forecast1)

# Prophet model gives a forecast which is almost similar to ARIMA model's forecast.

# The interactive graph plot can be used to see the forecast values and trend for the future.

# Plot component function showcases the trend and yearly additive component of the time series.

