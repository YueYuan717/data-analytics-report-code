packages =  c("ggplot2", "dplyr", "tidyr", "data.table", 'corrplot', 'gridExtra', 'forecast', 'tseries', 'TSA', 'tibble', 'TTR')

my.install <- function(pkg, ...){
  if (!(pkg %in% installed.packages()[,1])) {
    install.packages(pkg)
  }
  return (library(pkg, ...))
}

purrr::walk(packages, my.install, character.only = TRUE, warn.conflicts = FALSE)



s_data <- read.csv("all_stocks_5yr.csv")
head(s_data)
head(table(s_data$Name), n=100) 


symbol_to_path <- function(symbol, base_dir="../yueyuan/Dropbox/rpi2020spring/data%20analytics/project/sandp500/individual_stocks_5yr/individual_stocks_5yr"){
  return(paste(base_dir, symbol,'_data.csv', sep='')) # complete file name of symbol with path 
}
# It will give us 100 companies name and their number of traded days
summary(s_data)
str(s_data)
s_data[is.na(s_data)] <- 0
s_data$Date <- as.Date(s_data$date, format = "%Y-%m-%d")
summary(s_data)
str(s_data)



#univariate distribution
options(repr.plot.width=12, repr.plot.height=12) 

p1=ggplot(s_data, aes(open)) + geom_histogram(bins = 50, aes(y = ..density..), col = "red", fill = "red", alpha = 0.3) + geom_density()

p2=ggplot(s_data, aes(high)) + geom_histogram(bins = 50, aes(y = ..density..), col = "red", fill = "red", alpha = 0.3) + geom_density()

p3=ggplot(s_data, aes(low)) + geom_histogram(bins = 50, aes(y = ..density..), col = "red", fill = "red", alpha = 0.3) + geom_density()

p4=ggplot(s_data, aes(close)) + geom_histogram(bins = 50, aes(y = ..density..), col = "red", fill = "red", alpha = 0.3) + geom_density()

grid.arrange(p1,p2,p3,p4, nrow=2,ncol=2)

##time series analysis
tmp <- filter(s_data, high > 100) 
sample(tmp$Name, 10)

i_stock <- filter(s_data, Name == "PSA")
str(i_stock)


##create time series
## Create a daily Date object
inds <- seq(as.Date("2013-02-08"), as.Date("2018-02-07"), by = "day")

create_ts <- function(col_idx){
  i_ts <- as.numeric(i_stock[,col_idx]) %>%
    tsclean(replace.missing = TRUE, lambda = NULL) %>%
    ts(start = c(2013, as.numeric(format(inds[1], "%j"))),
       frequency = 365.5)
  return(i_ts)
}
par(mfrow=c(1,1)) 
i_ts = create_ts(which(colnames(i_stock) == "high"))
plot.ts(i_ts, xlab = "Time", ylab = "High value", main = "Time Series", col = "red")

adf.test(i_stock[,which(colnames(i_stock) == "high")], alternative = "stationary", k = 0)

#plot close price of citygroup
# Load libraries
library(tidyverse)
install.packages("ggthemes")
library(ggthemes)
library(forecast)
library(tseries)
library(gridExtra)
install.packages("rugarch")
library(rugarch)
stocks<-read_csv('all_stocks_5yr.csv')
stocks = stocks %>% select(date , close , Name)

# One column for each stock
stocks = stocks %>% spread(key = Name , value = close)

qplot(x = 1:1259 , y = stocks$AMZN , geom = 'line') + geom_line(color = 'darkblue') + 
  labs(x = '' , y = 'Price' , title = "Amazon") + geom_hline(yintercept = mean(stocks$AMZN) ,color='red')


##decomposing time series
i_tscomponents <- decompose(i_ts)
plot(i_tscomponents, col = "red")

#Differencing a Time Series
i_tsdiff1 <- diff(i_ts, differences=1)
plot.ts(i_tsdiff1, col = "red")

##arima model
acf(i_tsdiff1, lag.max=60)             # plot a correlogram

acf(i_tsdiff1, lag.max=60, plot=FALSE) # get the autocorrelation values

pacf(i_tsdiff1, lag.max=60)             # plot a partial correlogram
pacf(i_tsdiff1, lag.max=60, plot=FALSE) # get the partial autocorrelation values

#fitting an arima model
i_tsarima <- auto.arima(i_ts, max.p = 3, max.q = 3, max.d = 3)
i_tsarima

##forecasting using an arima model
i_tsforecasts <- forecast(i_tsarima, h = 365)
plot(i_tsforecasts, col = "red")
help("forecast")

plot.ts(i_tsforecasts$residuals)            # make time plot of forecast errors

ggplot(data.frame(residuals = i_tsforecasts$residuals), aes(residuals)) + geom_histogram(bins = 50, aes(y = ..density..), col = "red", fill = "red", alpha = 0.3) + geom_density()# make a histogram


##garch model
rets = diff(stocks$AMZN) / stocks$C[-length(stocks$AMZN)]

p1 = qplot(x = 1:length(rets) , y = rets , geom = 'line') + geom_line(color = 'darkblue') + 
  geom_hline(yintercept = mean(rets) , color = 'red' , size = 1) + 
  labs(x = '' , y = 'Daily Returns')

p2 = qplot(rets , geom = 'density') + coord_flip() + geom_vline(xintercept = mean(rets) , color = 'red' , size = 1) +
  geom_density(fill = 'lightblue' , alpha = 0.4) + labs(x = '')

grid.arrange(p1 , p2 , ncol = 2)

adf.test(rets)
model.arima = auto.arima(rets , max.order = c(3 , 0 ,3) , stationary = TRUE , trace = T , ic = 'aicc')
model.arima
model.arima$residuals %>% ggtsdisplay(plot.type = 'hist' , lag.max = 14)
ar.res = model.arima$residuals
Box.test(model.arima$residuals , lag = 14 , fitdf = 2 , type = 'Ljung-Box')
tsdisplay(ar.res^2 , main = 'Squared Residuals')

# Model specification
model.spec = ugarchspec(variance.model = list(model = 'sGARCH' , garchOrder = c(1 , 1)) , 
                        mean.model = list(armaOrder = c(0 , 0)))

model.fit = ugarchfit(spec = model.spec , data = ar.res , solver = 'solnp')

options(scipen = 999)
model.fit@fit$matcoef

quantile(rets , 0.05)
qplot(rets , geom = 'histogram') + geom_histogram(fill = 'lightblue' , bins = 30) +
  geom_histogram(aes(rets[rets < quantile(rets , 0.05)]) , fill = 'red' , bins = 30) +
  labs(x = 'Daily Returns')

jarque.bera.test(rets)

p2_1 = qplot(rets , geom = 'density') + geom_density(fill = 'blue' , alpha = 0.4) + 
  geom_density(aes(rnorm(200000 , 0 , sd(rets))) , fill = 'red' , alpha = 0.25) + 
  labs(x = '')

p2_2 = qplot(rets , geom = 'density') + geom_density(fill = 'blue' , alpha = 0.4) + 
  geom_density(aes(rnorm(200000 , 0 , sd(rets))) , fill = 'red' , alpha = 0.25) + 
  coord_cartesian(xlim = c(-0.07 , -0.02) , ylim = c(0 , 10)) + 
  geom_vline(xintercept = c(qnorm(p = c(0.01 , 0.05) , mean = mean(rets) , sd = sd(rets))) , 
             color = c('darkgreen' , 'green') , size = 1) + labs(x = 'Daily Returns')

grid.arrange(p2_1 , p2_2 , ncol = 1)

fitdist(distribution = 'std' , x = rets)$pars

cat("For a = 0.05 the quantile value of normal distribution is: " , 
    qnorm(p = 0.05) , "\n" ,
    "For a = 0.05 the quantile value of t-distribution is: " ,
    qdist(distribution = 'std' , shape = 3.7545967917 , p = 0.05) , "\n" , "\n" , 
    'For a = 0.01 the quantile value of normal distribution is: ' , 
    qnorm(p = 0.01) , "\n" , 
    "For a = 0.01 the quantile value of t-distribution is: " , 
    qdist(distribution = 'std' , shape = 3.7545967917 , p = 0.01) , sep = "")

model.roll = ugarchroll(spec = model.spec , data = rets , n.start = 758 , refit.every = 50 ,
                        refit.window = 'moving')

# Test set 500 observations
VaR95_td = mean(rets) + model.roll@forecast$density[,'Sigma']*qdist(distribution='std', shape=3.7545967917, p=0.05)

model.roll = ugarchroll(spec = model.spec , data = rets , n.start = 758 , refit.every = 50 ,
                        refit.window = 'moving')

p = c()
p[1] = pbinom(q = 0 , size = 500 , prob = 0.05)
for(i in 1:50){
  p[i] = (pbinom(q = (i-1) , size = 500 , prob = 0.05) - pbinom(q = (i-2) , size = 500 , prob = 0.05))
}
qplot(y = p , x = 1:50 , geom = 'line') + scale_x_continuous(breaks = seq(0 , 50 , 2)) + 
  annotate('segment' , x = c(16 , 35) , xend = c(16 , 35) , y = c(0 , 0) , yend = p[c(16 , 35)] , color = 'red' , 
           size = 1) + labs(y = 'Probability' , x = 'Number of Exceptions') + theme_light()

qplot(y = VaR95_td , x = 1:500 , geom = 'line') +
  geom_point(aes(x = 1:500 , y = rets[759:1258] , color = as.factor(rets[759:1258] < VaR95_td)) , size = 2) + scale_color_manual(values = c('gray' , 'red')) + 
  labs(y = 'Daily Returns' , x = 'Test set Observation') + theme_light() + 
  theme(legend.position = 'none')

