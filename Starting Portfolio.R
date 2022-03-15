####################################
######## Created by: Nidhi Mendon
######## Hult MFIN - Cohort 1
####################################
######## Modeling and Analytics - Starting Portfolio
####################################

### Monthly returns per stock in the portfolio
library(quantmod)

stock1 <- getSymbols("AAPL", auto.assign = FALSE)
stock2 <- getSymbols("MSFT", auto.assign = FALSE)
stock3 <- getSymbols("AMZN", auto.assign = FALSE)
stock4 <- getSymbols("GOOGL", auto.assign = FALSE)
stock5 <- getSymbols("ADBE", auto.assign = FALSE)
stock6 <- getSymbols("ASML", auto.assign = FALSE)
stock7 <- getSymbols("NFLX", auto.assign = FALSE)
stock8 <- getSymbols("HD", auto.assign = FALSE)
stock9 <- getSymbols("DIS", auto.assign = FALSE)
stock10 <- getSymbols("WMT", auto.assign = FALSE)

#Creating a new table with all the stock reutrns
joined_prices <- merge.xts(stock1, stock2, stock3, stock4, stock5, stock6, stock7, stock8,
                           stock9, stock10)

##### We just need the adjusted prices: [only stands for adjusted price only]
##### Dataframe has 2 dimensions. You can also use the names of the columns but you will have to mention the ticker name if you do this
joined_prices_only <- joined_prices[,seq(from = 6 , to = 60 , by = 6)]

joined_prices_only_normal <- as.data.frame(joined_prices_only)
joined_prices_only_normal$date <- index(joined_prices_only)

joined_prices_only_normal$AAPL.Adjusted
joined_prices_only_normal$MSFT.Adjusted
joined_prices_only_normal$AMZN.Adjusted
joined_prices_only_normal$GOOGL.Adjusted
joined_prices_only_normal$ADBE.Adjusted
joined_prices_only_normal$ASML.Adjusted
joined_prices_only_normal$NFLX.Adjusted
joined_prices_only_normal$HD.Adjusted
joined_prices_only_normal$DIS.Adjusted
joined_prices_only_normal$WMT.Adjusted

### Step 4 - Adding desc_stats function
desc_stats <- function(x){
  my_min <- min(x)
  my_mean <- mean(x)
  my_median <- median(x)
  my_sd <- sd(x)
  my_max <- max(x)
  my_range <- range(x)
  return(c(my_min, my_median, my_sd, my_max, my_range))
}
descdata <- data.frame(
desc_stats(joined_prices_only$AAPL.Adjusted),
desc_stats(joined_prices_only$MSFT.Adjusted),
desc_stats(joined_prices_only$AMZN.Adjusted),
desc_stats(joined_prices_only$GOOGL.Adjusted),
desc_stats(joined_prices_only$ADBE.Adjusted),
desc_stats(joined_prices_only$ASML.Adjusted),
desc_stats(joined_prices_only$NFLX.Adjusted),
desc_stats(joined_prices_only$HD.Adjusted),
desc_stats(joined_prices_only$DIS.Adjusted),
desc_stats(joined_prices_only$WMT.Adjusted)
)
#massaging our prices into rate of returns using log:
joined_returns_loop <- as.data.frame(joined_prices_only)
joined_returns_loop$log_ret <- c()
joined_returns_loop$log_ret[1] <- NA

### creating a loop for AAPL
joined_returns_loop$logret_AAPL <- c()
joined_returns_loop$logret_AAPL[1] <- NA

for(i in 2:nrow(joined_returns_loop)){
  joined_returns_loop$logret_AAPL[i] <- log(joined_returns_loop$AAPL.Adjusted[i]/joined_returns_loop$AAPL.Adjusted[i-1])
}

### creating a loop for MSFT
joined_returns_loop$logret_MSFT <- c()
joined_returns_loop$logret_MSFT[1] <- NA

for(i in 2:nrow(joined_returns_loop)){
  joined_returns_loop$logret_MSFT[i] <- log(joined_returns_loop$MSFT.Adjusted[i]/joined_returns_loop$MSFT.Adjusted[i-1])
}

### creating a loop for AMZN
joined_returns_loop$logret_AMZN <- c()
joined_returns_loop$logret_AMZN[1] <- NA

for(i in 2:nrow(joined_returns_loop)){
  joined_returns_loop$logret_AMZN[i] <- log(joined_returns_loop$AMZN.Adjusted[i]/joined_returns_loop$AMZN.Adjusted[i-1])
}

### creating a loop for GOOGL
joined_returns_loop$logret_GOOGL <- c()
joined_returns_loop$logret_GOOGL[1] <- NA

for(i in 2:nrow(joined_returns_loop)){
  joined_returns_loop$logret_GOOGL[i] <- log(joined_returns_loop$GOOGL.Adjusted[i]/joined_returns_loop$GOOGL.Adjusted[i-1])
}

### creating a loop for ADBE
joined_returns_loop$logret_ADBE <- c()
joined_returns_loop$logret_ADBE[1] <- NA

for(i in 2:nrow(joined_returns_loop)){
  joined_returns_loop$logret_ADBE[i] <- log(joined_returns_loop$ADBE.Adjusted[i]/joined_returns_loop$ADBE.Adjusted[i-1])
}

### creating a loop for ASML
joined_returns_loop$logret_ASML <- c()
joined_returns_loop$logret_ASML[1] <- NA

for(i in 2:nrow(joined_returns_loop)){
  joined_returns_loop$logret_ASML[i] <- log(joined_returns_loop$ASML.Adjusted[i]/joined_returns_loop$ASML.Adjusted[i-1])
}

### creating a loop for NFLX
joined_returns_loop$logret_NFLX <- c()
joined_returns_loop$logret_NFLX[1] <- NA

for(i in 2:nrow(joined_returns_loop)){
  joined_returns_loop$logret_NFLX[i] <- log(joined_returns_loop$NFLX.Adjusted[i]/joined_returns_loop$NFLX.Adjusted[i-1])
}

### creating a loop for HD
joined_returns_loop$logret_HD <- c()
joined_returns_loop$logret_HD[1] <- NA

for(i in 2:nrow(joined_returns_loop)){
  joined_returns_loop$logret_HD[i] <- log(joined_returns_loop$HD.Adjusted[i]/joined_returns_loop$HD.Adjusted[i-1])
}

### creating a loop for DIS
joined_returns_loop$logret_DIS <- c()
joined_returns_loop$logret_DIS[1] <- NA

for(i in 2:nrow(joined_returns_loop)){
  joined_returns_loop$logret_DIS[i] <- log(joined_returns_loop$DIS.Adjusted[i]/joined_returns_loop$DIS.Adjusted[i-1])
}

### creating a loop for WMT
joined_returns_loop$logret_WMT <- c()
joined_returns_loop$logret_WMT[1] <- NA

for(i in 2:nrow(joined_returns_loop)){
  joined_returns_loop$logret_WMT[i] <- log(joined_returns_loop$WMT.Adjusted[i]/joined_returns_loop$WMT.Adjusted[i-1])
}

#designing a UDF to account for different securities and time windows
### creating an empty object to host the result
window_returns <- function(x, t){
  compounded <- rep(NA, each = (t-1))
  for(i in t:length(x)){
    compounded[i] <- log(x[i]/x[i-t+1])
  }
  return(compounded)
}#closing window_returns


joined_returns_loop$AAPL_monthly <- window_returns(x=joined_returns_loop$AAPL.Adjusted, t=20)
joined_returns_loop$MSFT_monthly <- window_returns(x=joined_returns_loop$MSFT.Adjusted, t=20)
joined_returns_loop$AMZN_monthly <- window_returns(x=joined_returns_loop$AMZN.Adjusted, t=20)
joined_returns_loop$GOOGL_monthly <- window_returns(x=joined_returns_loop$GOOGl.Adjusted, t=20)
joined_returns_loop$ADBE_monthly <- window_returns(x=joined_returns_loop$ADBE.Adjusted, t=20)
joined_returns_loop$ASML_monthly <- window_returns(x=joined_returns_loop$ASML.Adjusted, t=20)
joined_returns_loop$NFLX_monthly <- window_returns(x=joined_returns_loop$NFLX.Adjusted, t=20)
joined_returns_loop$HD_monthly <- window_returns(x=joined_returns_loop$HD.Adjusted, t=20)
joined_returns_loop$DIS_monthly <- window_returns(x=joined_returns_loop$DIS.Adjusted, t=20)
joined_returns_loop$WMT_monthly <- window_returns(x=joined_returns_loop$WMT.Adjusted, t=20)

#Step 5:
#calculating returns using dplyr library with time windows
library(dplyr)

t <- 20

joined_returns <- as.data.frame(joined_prices_only) %>%
  mutate(AAPL_ROR = log(AAPL.Adjusted/log(AAPL.Adjusted, t))) %>%
  mutate(MSFT_ROR = log(MSFT.Adjusted/log(MSFT.Adjusted, t))) %>%
  mutate(AMZN_ROR = log(AMZN.Adjusted/log(AMZN.Adjusted, t))) %>%
  mutate(GOOGL_ROR = log(GOOGL.Adjusted/log(GOOGL.Adjusted, t))) %>%
  mutate(ADBE_ROR = log(ADBE.Adjusted/log(ADBE.Adjusted, t))) %>%
  mutate(ASML_ROR = log(ASML.Adjusted/log(ASML.Adjusted, t))) %>%
  mutate(NFLX_ROR = log(NFLX.Adjusted/log(NFLX.Adjusted, t))) %>%
  mutate(HD_ROR = log(HD.Adjusted/log(HD.Adjusted, t))) %>%
  mutate(DIS_ROR = log(DIS.Adjusted/log(DIS.Adjusted, t))) %>%
  mutate(WMT_ROR = log(WMT.Adjusted/log(WMT.Adjusted, t)))

### step 7: other version to calculate step 4,5 and 6

######## daily returns
stock1_returns_daily <- dailyReturn(getSymbols("AAPL", auto.assign = FALSE))
stock2_returns_daily <- dailyReturn(getSymbols("MSFT", auto.assign = FALSE))
stock3_returns_daily <- dailyReturn(getSymbols("AMZN", auto.assign = FALSE))
stock4_returns_daily <- dailyReturn(getSymbols("GOOGL", auto.assign = FALSE))
stock5_returns_daily <- dailyReturn(getSymbols("ADBE", auto.assign = FALSE))
stock6_returns_daily <- dailyReturn(getSymbols("ASML", auto.assign = FALSE))
stock7_returns_daily <- dailyReturn(getSymbols("NFLX", auto.assign = FALSE))
stock8_returns_daily <- dailyReturn(getSymbols("HD", auto.assign = FALSE))
stock9_returns_daily <- dailyReturn(getSymbols("DIS", auto.assign = FALSE))
stock10_returns_daily <- dailyReturn(getSymbols("WMT", auto.assign = FALSE))

joined_daily_returns <- merge.xts(stock1_returns_daily, stock2_returns_daily, stock3_returns_daily, stock4_returns_daily, 
                                  stock5_returns_daily, stock6_returns_daily, stock7_returns_daily, stock8_returns_daily, stock9_returns_daily, stock10_returns_daily)


######## monthly returns
stock1_returns_monthly <- monthlyReturn(getSymbols("AAPL", auto.assign = FALSE))
stock2_returns_monthly <- monthlyReturn(getSymbols("MSFT", auto.assign = FALSE))
stock3_returns_monthly <- monthlyReturn(getSymbols("AMZN", auto.assign = FALSE))
stock4_returns_monthly <- monthlyReturn(getSymbols("GOOGL", auto.assign = FALSE))
stock5_returns_monthly <- monthlyReturn(getSymbols("ADBE", auto.assign = FALSE))
stock6_returns_monthly <- monthlyReturn(getSymbols("ASML", auto.assign = FALSE))
stock7_returns_monthly <- monthlyReturn(getSymbols("NFLX", auto.assign = FALSE))
stock8_returns_monthly <- monthlyReturn(getSymbols("HD", auto.assign = FALSE))
stock9_returns_monthly <- monthlyReturn(getSymbols("DIS", auto.assign = FALSE))
stock10_returns_monthly <- monthlyReturn(getSymbols("WMT", auto.assign = FALSE))

joined_monthly_returns <- merge.xts(stock1_returns_monthly, stock2_returns_monthly, stock3_returns_monthly, stock4_returns_monthly, 
                                    stock5_returns_monthly, stock6_returns_monthly, stock7_returns_monthly, stock8_returns_monthly, stock9_returns_monthly, stock10_returns_monthly)

######## yearly returns
stock1_returns_yearly <- yearlyReturn(getSymbols("AAPL", auto.assign = FALSE))
stock2_returns_yearly <- yearlyReturn(getSymbols("MSFT", auto.assign = FALSE))
stock3_returns_yearly <- yearlyReturn(getSymbols("AMZN", auto.assign = FALSE))
stock4_returns_yearly <- yearlyReturn(getSymbols("GOOGL", auto.assign = FALSE))
stock5_returns_yearly <- yearlyReturn(getSymbols("ADBE", auto.assign = FALSE))
stock6_returns_yearly <- yearlyReturn(getSymbols("ASML", auto.assign = FALSE))
stock7_returns_yearly <- yearlyReturn(getSymbols("NFLX", auto.assign = FALSE))
stock8_returns_yearly <- yearlyReturn(getSymbols("HD", auto.assign = FALSE))
stock9_returns_yearly <- yearlyReturn(getSymbols("DIS", auto.assign = FALSE))
stock10_returns_yearly <- yearlyReturn(getSymbols("WMT", auto.assign = FALSE))

joined_yearly_returns <- merge.xts(stock1_returns_yearly, stock2_returns_yearly, stock3_returns_yearly, stock4_returns_yearly, 
                                   stock5_returns_yearly, stock6_returns_yearly, stock7_returns_yearly, stock8_returns_yearly, stock9_returns_yearly, stock10_returns_yearly)


### Adding a benchmark:
benchmark_returns <- monthlyReturn(getSymbols("ITOT", auto.assign = FALSE))

benchmark_returns_yearly <- yearlyReturn(getSymbols("ITOT", auto.assign = FALSE))

joined_monthly_returns <- merge.xts(joined_monthly_returns,benchmark_returns)

### Stock Allocation:
AAPL_alloc <- 0.1
MSFT_alloc <- 0.1
AMZN_alloc <- 0.1
GOOGL_alloc <- 0.1
ADBE_alloc <- 0.1
ASML_alloc <- 0.1
NFLX_alloc <- 0.1
HD_alloc <- 0.1
DIS_alloc <- 0.1
WMT_alloc <- 0.1

library(dplyr)
joined_monthly_returns <- as.data.frame(joined_monthly_returns)%>%
  mutate(portfolio = AAPL_alloc*joined_monthly_returns$monthly.returns+
           MSFT_alloc*joined_monthly_returns$monthly.returns.1+
           AMZN_alloc*joined_monthly_returns$monthly.returns.2+
           GOOGL_alloc*joined_monthly_returns$monthly.returns.3+
           ADBE_alloc*joined_monthly_returns$monthly.returns.4+
           ASML_alloc*joined_monthly_returns$monthly.returns.5+
           NFLX_alloc*joined_monthly_returns$monthly.returns.6+
           HD_alloc*joined_monthly_returns$monthly.returns.7+
           DIS_alloc*joined_monthly_returns$monthly.returns.8+
           WMT_alloc*joined_monthly_returns$monthly.returns.9)

joined_yearly_returns <- as.data.frame(joined_yearly_returns)%>%
  mutate(portfolio = AAPL_alloc*joined_yearly_returns$yearly.returns+
           MSFT_alloc*joined_yearly_returns$yearly.returns.1+
           AMZN_alloc*joined_yearly_returns$yearly.returns.2+
           GOOGL_alloc*joined_yearly_returns$yearly.returns.3+
           ADBE_alloc*joined_yearly_returns$yearly.returns.4+
           ASML_alloc*joined_yearly_returns$yearly.returns.5+
           NFLX_alloc*joined_yearly_returns$yearly.returns.6+
           HD_alloc*joined_yearly_returns$yearly.returns.7+
           DIS_alloc*joined_yearly_returns$yearly.returns.8+
           WMT_alloc*joined_yearly_returns$yearly.returns.9)


### calculation compounded ROR
my_ROR <- prod(1+joined_monthly_returns$monthly.returns.1[1:24])
## for the oldest 24 months
### annualize and -1
my_ROR^min((12/24),1)-1
## if there were only 9 months of data will be only (12/24)-1\

############# UDF for compounding
compounded <- function(x, t){# x is vector of monthly ROR, t is in months
  compounded <- rep(NA, each=(t-1))
  for(i in t:length(x)){
    compounded[i] <- prod(1+x[i:(i-t+1)])
  }
  annualized<- compounded^min((12/t),1)-1
  return(annualized)
}#closing compounded

compounded(x=joined_monthly_returns$portfolio, t=12)

### To be worked on:
time_index <- nrow(joined_monthly_returns)
joined_monthly_returns <- as.data.frame(joined_monthly_returns)
AAPL_sigma <- sd(joined_monthly_returns$monthly.returns[time_index : (time_index-11)])*sqrt(12)
MSFT_sigma <- sd(joined_monthly_returns$monthly.returns.1[time_index : (time_index-11)])*sqrt(12)
AMZN_sigma <- sd(joined_monthly_returns$monthly.returns.2[time_index : (time_index-11)])*sqrt(12)
GOOGL_sigma <- sd(joined_monthly_returns$monthly.returns.3[time_index : (time_index-11)])*sqrt(12)
ADBE_sigma <- sd(joined_monthly_returns$monthly.returns.4[time_index : (time_index-11)])*sqrt(12)
ASML_sigma <- sd(joined_monthly_returns$monthly.returns.5[time_index : (time_index-11)])*sqrt(12)
NFLX_sigma <- sd(joined_monthly_returns$monthly.returns.6[time_index : (time_index-11)])*sqrt(12)
HD_sigma <- sd(joined_monthly_returns$monthly.returns.7[time_index : (time_index-11)])*sqrt(12)
DIS_sigma <- sd(joined_monthly_returns$monthly.returns.8[time_index : (time_index-11)])*sqrt(12)
WMT_sigma <- sd(joined_monthly_returns$monthly.returns.9[time_index : (time_index-11)])*sqrt(12)
portfolio_sigma <- sd(joined_monthly_returns$portfolio[time_index : (time_index-11)])*sqrt(12)

### market risk
# calculating tracking error on active returns 
### To be worked on
AAPL_te <- sd(joined_monthly_returns$monthly.returns[time_index : (time_index-11)]-
                joined_monthly_returns$monthly.returns.10[time_index : (time_index-11)])*sqrt(12)
MSFT_te <- sd(joined_monthly_returns$monthly.returns.1[time_index : (time_index-11)]-
                joined_monthly_returns$monthly.returns.10[time_index : (time_index-11)])*sqrt(12)
AMZN_te <- sd(joined_monthly_returns$monthly.returns.2[time_index : (time_index-11)]-
                joined_monthly_returns$monthly.returns.10[time_index : (time_index-11)])*sqrt(12)
GOOGL_te <- sd(joined_monthly_returns$monthly.returns.3[time_index : (time_index-11)]-
                 joined_monthly_returns$monthly.returns.10[time_index : (time_index-11)])*sqrt(12)
ADBE_te <- sd(joined_monthly_returns$monthly.returns.4[time_index : (time_index-11)]-
                joined_monthly_returns$monthly.returns.10[time_index : (time_index-11)])*sqrt(12)
ASML_te <- sd(joined_monthly_returns$monthly.returns.5[time_index : (time_index-11)]-
                joined_monthly_returns$monthly.returns.10[time_index : (time_index-11)])*sqrt(12)
NFLX_te <- sd(joined_monthly_returns$monthly.returns.6[time_index : (time_index-11)]-
                joined_monthly_returns$monthly.returns.10[time_index : (time_index-11)])*sqrt(12)
HD_te <- sd(joined_monthly_returns$monthly.returns.7[time_index : (time_index-11)]-
              joined_monthly_returns$monthly.returns.10[time_index : (time_index-11)])*sqrt(12)
DIS_te <- sd(joined_monthly_returns$monthly.returns.8[time_index : (time_index-11)]-
               joined_monthly_returns$monthly.returns.10[time_index : (time_index-11)])*sqrt(12)
WMT_te <- sd(joined_monthly_returns$monthly.returns.9[time_index : (time_index-11)]-
               joined_monthly_returns$monthly.returns.10[time_index : (time_index-11)])*sqrt(12)
portfolio_te <- sd(joined_monthly_returns$portfolio[time_index : (time_index-11)]-
                     joined_monthly_returns$monthly.returns.10[time_index : (time_index-11)])*sqrt(12)

##### Sharpe ratio

riskfree <- 0.0001

AAPL_sharpe <- (mean(joined_monthly_returns$monthly.returns[time_index:(time_index-11)])-riskfree)/sd(joined_monthly_returns$monthly.returns[time_index:(time_index-11)])
MSFT_sharpe <- (mean(joined_monthly_returns$monthly.returns.1[time_index:(time_index-11)])-riskfree)/sd(joined_monthly_returns$monthly.returns.1[time_index:(time_index-11)])
AMZN_sharpe <- (mean(joined_monthly_returns$monthly.returns.2[time_index:(time_index-11)])-riskfree)/sd(joined_monthly_returns$monthly.returns.2[time_index:(time_index-11)])
GOOGL_sharpe <- (mean(joined_monthly_returns$monthly.returns.3[time_index:(time_index-11)])-riskfree)/sd(joined_monthly_returns$monthly.returns.3[time_index:(time_index-11)])
ADBE_sharpe <- (mean(joined_monthly_returns$monthly.returns.4[time_index:(time_index-11)])-riskfree)/sd(joined_monthly_returns$monthly.returns.4[time_index:(time_index-11)])
ASML_sharpe <- (mean(joined_monthly_returns$monthly.returns.5[time_index:(time_index-11)])-riskfree)/sd(joined_monthly_returns$monthly.returns.5[time_index:(time_index-11)])
NFLX_sharpe <- (mean(joined_monthly_returns$monthly.returns.6[time_index:(time_index-11)])-riskfree)/sd(joined_monthly_returns$monthly.returns.6[time_index:(time_index-11)])
HD_sharpe <- (mean(joined_monthly_returns$monthly.returns.7[time_index:(time_index-11)])-riskfree)/sd(joined_monthly_returns$monthly.returns.7[time_index:(time_index-11)])
DIS_sharpe <- (mean(joined_monthly_returns$monthly.returns.8[time_index:(time_index-11)])-riskfree)/sd(joined_monthly_returns$monthly.returns.8[time_index:(time_index-11)])
WMT_sharpe <- (mean(joined_monthly_returns$monthly.returns.9[time_index:(time_index-11)])-riskfree)/sd(joined_monthly_returns$monthly.returns.9[time_index:(time_index-11)])
Portfolio_sharpe <- (mean(joined_monthly_returns$portfolio[time_index:(time_index-11)])-riskfree)/sd(joined_monthly_returns$portfolio[time_index:(time_index-11)])

#looking at covariance and correlation
cov(joined_monthly_returns, use="complete.obs")
cor(joined_monthly_returns, use="complete.obs")

last_12_months <- joined_monthly_returns[(time_index-11):time_index, ]
last_24_months <- joined_monthly_returns[(time_index-23):time_index, ]

#### CAPM
AAPL_reg <- lm(monthly.returns ~ monthly.returns.10, data=last_24_months) #alternatively data = joined_monthly_returns
summary(AAPL_reg)
MSFT_reg <- lm(monthly.returns.1 ~ monthly.returns.10, data=last_24_months)
summary(MSFT_reg)
AMZN_reg <- lm(monthly.returns.2 ~ monthly.returns.10, data=last_24_months)
summary(AMZN_reg)
GOOGL_reg <- lm(monthly.returns.3 ~ monthly.returns.10, data=last_24_months)
summary(GOOGL_reg)
ADBE_reg <- lm(monthly.returns.4 ~ monthly.returns.10, data=last_24_months)
summary(ADBE_reg)
ASML_reg <- lm(monthly.returns.5 ~ monthly.returns.10, data=last_24_months)
summary(ASML_reg)
NFLX_reg <- lm(monthly.returns.6 ~ monthly.returns.10, data=last_24_months)
summary(NFLX_reg)
HD_reg <- lm(monthly.returns.7 ~ monthly.returns.10, data=last_24_months)
summary(HD_reg)
DIS_reg <- lm(monthly.returns.8 ~ monthly.returns.10, data=last_24_months)
summary(DIS_reg)
WMT_reg <- lm(monthly.returns.9 ~ monthly.returns.10, data=last_24_months)
summary(WMT_reg)
Portfolio_reg <- lm(portfolio~ monthly.returns.10, data=last_24_months) 
summary(Portfolio_reg)

#### Using the model to predict not to forecast
testing_sample_index <- sample(1:nrow(last_24_months), size=5)
testing_sample_data <- last_24_months[testing_sample_index,]

predict(AAPL_reg, testing_sample_data)
predict(MSFT_reg, testing_sample_data)
predict(AMZN_reg, testing_sample_data)
predict(GOOGL_reg, testing_sample_data)
predict(ADBE_reg, testing_sample_data)
predict(ASML_reg, testing_sample_data)
predict(NFLX_reg, testing_sample_data)
predict(HD_reg, testing_sample_data)
predict(DIS_reg, testing_sample_data)
predict(WMT_reg, testing_sample_data)
predict(Portfolio_reg, testing_sample_data)

##### Looking at treynor ratio - Use 24 months
##### Treynor ratio is usually lower than the Sharpe ratio since Beta is higher as compared to SD (denominators)
AAPL_Treynor <- (mean(joined_monthly_returns$monthly.returns[(time_index-23):time_index])-riskfree)/
  AAPL_reg$coefficients[2]
MSFT_Treynor <- (mean(joined_monthly_returns$monthly.returns.1[(time_index-23):time_index])-riskfree)/
  MSFT_reg$coefficients[2]
AMZN_Treynor <- (mean(joined_monthly_returns$monthly.returns.2[(time_index-23):time_index])-riskfree)/
  AMZN_reg$coefficients[2]
GOOGL_Treynor <- (mean(joined_monthly_returns$monthly.returns.3[(time_index-23):time_index])-riskfree)/
  GOOGL_reg$coefficients[2]
ADBE_Treynor <- (mean(joined_monthly_returns$monthly.returns.4[(time_index-23):time_index])-riskfree)/
  ADBE_reg$coefficients[2]
ASML_Treynor <- (mean(joined_monthly_returns$monthly.returns.5[(time_index-23):time_index])-riskfree)/
  ASML_reg$coefficients[2]
NFLX_Treynor <- (mean(joined_monthly_returns$monthly.returns.6[(time_index-23):time_index])-riskfree)/
  NFLX_reg$coefficients[2]
HD_Treynor <- (mean(joined_monthly_returns$monthly.returns.7[(time_index-23):time_index])-riskfree)/
  HD_reg$coefficients[2]
DIS_Treynor <- (mean(joined_monthly_returns$monthly.returns.8[(time_index-23):time_index])-riskfree)/
  DIS_reg$coefficients[2]
WMT_Treynor <- (mean(joined_monthly_returns$monthly.returns.9[(time_index-23):time_index])-riskfree)/
  WMT_reg$coefficients[2]
Portfolio_Treynor <- (mean(joined_monthly_returns$portfolio[(time_index-23):time_index])-riskfree)/
  Portfolio_reg$coefficients[2]

#### CHARTS ####
## Creating charts for our securities prices
chartSeries(stock1)
chartSeries(stock2)
chartSeries(stock3)
chartSeries(stock4)
chartSeries(stock5)
chartSeries(stock6)
chartSeries(stock7)
chartSeries(stock8)
chartSeries(stock9)
chartSeries(stock10)


#install.packages("ggplot2")
library(ggplot2)
#First looking at the shape of pricing time series? 
ggplot(data=joined_prices_only)+
  geom_line(aes(x=index(joined_prices_only), y=AAPL.Adjusted))
ggplot(data=joined_prices_only)+
  geom_line(aes(x=index(joined_prices_only), y=MSFT.Adjusted))
ggplot(data=joined_prices_only)+
  geom_line(aes(x=index(joined_prices_only), y=AMZN.Adjusted))
ggplot(data=joined_prices_only)+
  geom_line(aes(x=index(joined_prices_only), y=GOOGL.Adjusted))
ggplot(data=joined_prices_only)+
  geom_line(aes(x=index(joined_prices_only), y=ADBE.Adjusted))
ggplot(data=joined_prices_only)+
  geom_line(aes(x=index(joined_prices_only), y=ASML.Adjusted))
ggplot(data=joined_prices_only)+
  geom_line(aes(x=index(joined_prices_only), y=NFLX.Adjusted))
ggplot(data=joined_prices_only)+
  geom_line(aes(x=index(joined_prices_only), y=HD.Adjusted))
ggplot(data=joined_prices_only)+
  geom_line(aes(x=index(joined_prices_only), y=DIS.Adjusted))
ggplot(data=joined_prices_only)+
  geom_line(aes(x=index(joined_prices_only), y=WMT.Adjusted))

#let's take a look at all securities together.....
compare_chart <- ggplot(data=joined_prices_only)+
  geom_line(aes(x=index(joined_prices_only), y=AAPL.Adjusted), color="blue")+
  geom_line(aes(x=index(joined_prices_only), y=MSFT.Adjusted), color="red")+
  geom_line(aes(x=index(joined_prices_only), y=AMZN.Adjusted), color="green")+
  geom_line(aes(x=index(joined_prices_only), y=GOOGL.Adjusted), color="orange")+
  geom_line(aes(x=index(joined_prices_only), y=ADBE.Adjusted), color="yellow")+
  geom_line(aes(x=index(joined_prices_only), y=ASML.Adjusted), color="black")+
  geom_line(aes(x=index(joined_prices_only), y=NFLX.Adjusted), color="grey")+
  geom_line(aes(x=index(joined_prices_only), y=HD.Adjusted), color="purple")+
  geom_line(aes(x=index(joined_prices_only), y=DIS.Adjusted), color="brown")+
  geom_line(aes(x=index(joined_prices_only), y=WMT.Adjusted), color="pink")
print(compare_chart)

#we can also make this chart interactive by using plotly: 
#install.packages("plotly")
library(plotly)
ggplotly(compare_chart)

#First looking at the shape of ROR time series? 
ggplot(data=joined_monthly_returns)+
  geom_line(aes(x=index(joined_monthly_returns), y=monthly.returns))
ggplot(data=joined_monthly_returns)+
  geom_line(aes(x=index(joined_monthly_returns), y=monthly.returns.1))
ggplot(data=joined_monthly_returns)+
  geom_line(aes(x=index(joined_monthly_returns), y=monthly.returns.2))
ggplot(data=joined_monthly_returns)+
  geom_line(aes(x=index(joined_monthly_returns), y=monthly.returns.3))
ggplot(data=joined_monthly_returns)+
  geom_line(aes(x=index(joined_monthly_returns), y=monthly.returns.4))
ggplot(data=joined_monthly_returns)+
  geom_line(aes(x=index(joined_monthly_returns), y=monthly.returns.5))
ggplot(data=joined_monthly_returns)+
  geom_line(aes(x=index(joined_monthly_returns), y=monthly.returns.6))
ggplot(data=joined_monthly_returns)+
  geom_line(aes(x=index(joined_monthly_returns), y=monthly.returns.7))
ggplot(data=joined_monthly_returns)+
  geom_line(aes(x=index(joined_monthly_returns), y=monthly.returns.8))
ggplot(data=joined_monthly_returns)+
  geom_line(aes(x=index(joined_monthly_returns), y=monthly.returns.9))
ggplot(data=joined_monthly_returns)+
  geom_line(aes(x=index(joined_monthly_returns), y=portfolio))


#let's take a look at all securities together.....
compare_ror <- ggplot(data=joined_monthly_returns)+
  geom_line(aes(x=index(joined_monthly_returns), y=monthly.returns), color="blue")+
  geom_line(aes(x=index(joined_monthly_returns), y=monthly.returns.1), color="red")+
  geom_line(aes(x=index(joined_monthly_returns), y=monthly.returns.2), color="green")+
  geom_line(aes(x=index(joined_monthly_returns), y=monthly.returns.3), color="orange")+
  geom_line(aes(x=index(joined_monthly_returns), y=monthly.returns.4), color="yellow")+
  geom_line(aes(x=index(joined_monthly_returns), y=monthly.returns.5), color="black")+
  geom_line(aes(x=index(joined_monthly_returns), y=monthly.returns.6), color="grey")+
  geom_line(aes(x=index(joined_monthly_returns), y=monthly.returns.7), color="purple")+
  geom_line(aes(x=index(joined_monthly_returns), y=monthly.returns.8), color="brown")+
  geom_line(aes(x=index(joined_monthly_returns), y=monthly.returns.9), color="pink")
print(compare_ror)

source("~/Downloads/8. Fama French multi-factor models source this file before running the next one (1).R")

#calling the Fama French 3F model UDF for AAPL
AAPL_FF3F <- fama_french_3F(ticker="AAPL", from_date='2010-01-31', to_date='2019-02-20')
summary(AAPL_FF3F[[2]])#looking at factor loading - are any statisitcally significant
#now let's visualize the model error and the cumulative stock returns
ggplot(data=AAPL_FF3F[[1]])+
  geom_line(aes(x=Date, y=rr_spf), color="red4")+
  geom_line(aes(x=Date, y=tr_cum), color="blue") #red is the error and blue is the stock return

#calling the Fama French 3F model UDF for MSFT
MSFT_FF3F <- fama_french_3F(ticker="MSFT", from_date='2010-01-31', to_date='2021-02-20')
summary(MSFT_FF3F[[2]])

ggplot(data=MSFT_FF3F[[1]])+
  geom_line(aes(x=Date, y=rr_spf), color="red4")+
  geom_line(aes(x=Date, y=tr_cum), color="blue")

#calling the Fama French 3F model UDF for AMZN
AMZN_FF3F <- fama_french_3F(ticker="AMZN", from_date='2010-01-31', to_date='2021-02-20')
summary(AMZN_FF3F[[2]])

ggplot(data=AMZN_FF3F[[1]])+
  geom_line(aes(x=Date, y=rr_spf), color="red4")+
  geom_line(aes(x=Date, y=tr_cum), color="blue")

#calling the Fama French 3F model UDF for GOOGL
GOOGL_FF3F <- fama_french_3F(ticker="GOOGL", from_date='2010-01-31', to_date='2021-02-20')
summary(GOOGL_FF3F[[2]])

ggplot(data=GOOGL_FF3F[[1]])+
  geom_line(aes(x=Date, y=rr_spf), color="red4")+
  geom_line(aes(x=Date, y=tr_cum), color="blue")

#calling the Fama French 3F model UDF for ADBE
ADBE_FF3F <- fama_french_3F(ticker="ADBE", from_date='2010-01-31', to_date='2021-02-20')
summary(ADBE_FF3F[[2]])

ggplot(data=ADBE_FF3F[[1]])+
  geom_line(aes(x=Date, y=rr_spf), color="red4")+
  geom_line(aes(x=Date, y=tr_cum), color="blue")

#calling the Fama French 3F model UDF for ASML
ASML_FF3F <- fama_french_3F(ticker="ASML", from_date='2010-01-31', to_date='2021-02-20')
summary(ASML_FF3F[[2]])

ggplot(data=ASML_FF3F[[1]])+
  geom_line(aes(x=Date, y=rr_spf), color="red4")+
  geom_line(aes(x=Date, y=tr_cum), color="blue")

#calling the Fama French 3F model UDF for NFLX
NFLX_FF3F <- fama_french_3F(ticker="NFLX", from_date='2010-01-31', to_date='2021-02-20')
summary(NFLX_FF3F[[2]])

ggplot(data=NFLX_FF3F[[1]])+
  geom_line(aes(x=Date, y=rr_spf), color="red4")+
  geom_line(aes(x=Date, y=tr_cum), color="blue")

#calling the Fama French 3F model UDF for HD
HD_FF3F <- fama_french_3F(ticker="HD", from_date='2010-01-31', to_date='2021-02-20')
summary(HD_FF3F[[2]])

ggplot(data=HD_FF3F[[1]])+
  geom_line(aes(x=Date, y=rr_spf), color="red4")+
  geom_line(aes(x=Date, y=tr_cum), color="blue")

#calling the Fama French 3F model UDF for DIS
DIS_FF3F <- fama_french_3F(ticker="DIS", from_date='2010-01-31', to_date='2021-02-20')
summary(DIS_FF3F[[2]])

ggplot(data=DIS_FF3F[[1]])+
  geom_line(aes(x=Date, y=rr_spf), color="red4")+
  geom_line(aes(x=Date, y=tr_cum), color="blue")

#calling the Fama French 3F model UDF for WMT
WMT_FF3F <- fama_french_3F(ticker="WMT", from_date='2010-01-31', to_date='2021-02-20')
summary(WMT_FF3F[[2]])

ggplot(data=WMT_FF3F[[1]])+
  geom_line(aes(x=Date, y=rr_spf), color="red4")+
  geom_line(aes(x=Date, y=tr_cum), color="blue")

# based on code from: https://www.r-bloggers.com/2016/05/a-gentle-introduction-to-finance-using-r-efficient-frontier-and-capm-part-1/
library(data.table)
library(scales)
library(ggplot2)
library(quantmod)
library(reshape2)
ticker1 <- "AAPL"
ticker2<- "AMZ"
ticker3<- "MSFT"
mydf1 <- as.data.frame(getSymbols(ticker1, auto.assign=FALSE))
mydf2 <- as.data.frame(getSymbols(ticker2, auto.assign=FALSE))
mydf3 <- as.data.frame(getSymbols(ticker3, auto.assign=FALSE))

combined_df <- cbind(mydf1[,4], mydf2[,4], mydf3[,4])

dt <- as.data.frame(combined_df)
colnames(dt) <- c(ticker1, ticker2, ticker3)
dt$date = as.Date(rownames(mydf1))
dt <- melt(dt, id="date")
colnames(dt) <- c("date", "ticker","price")
dt <- data.table(dt)
# create indexed values
dt[, idx_price := price/price[1], by = ticker]
# plot the indexed values
ggplot(dt, aes(x = date, y = idx_price, color = ticker)) +
  geom_line() +
  # Miscellaneous Formatting
  theme_bw() + ggtitle("Price Developments") +
  xlab("Date") + ylab("Pricen(Indexed 2000 = 1)") +
  scale_color_discrete(name = "Company")

# calculate the arithmetic returns
dt[, ret := price / shift(price, 1) - 1, by = ticker]

# summary table
# take only non-na values
tab <- dt[!is.na(ret), .(ticker, ret)]

# calculate the expected returns (historical mean of returns) and volatility (standard deviation of returns)
tab <- tab[, .(er = round(mean(ret), 4),
               sd = round(sd(ret), 4)),
           by = "ticker"]

ggplot(tab, aes(x = sd, y = er, color = ticker)) +
  geom_point(size = 5) +
  # Miscellaneous Formatting
  theme_bw() + ggtitle("Risk-Return Tradeoff") +
  xlab("Volatility") + ylab("Expected Returns") +
  scale_y_continuous(label = percent, limits = c(0, 0.03)) +
  scale_x_continuous(label = percent, limits = c(0, 0.1))

################################################
#### Two risky assets portfolio
################################################
# load the data
ticker1_select <- "AAPL"
ticker2_select <- "MSFT"
ticker3_select <- "GOOGL"

mydf1 <- as.data.frame(monthlyReturn(getSymbols(ticker1_select, auto.assign=FALSE)))
mydf2 <- as.data.frame(monthlyReturn(getSymbols(ticker2_select, auto.assign=FALSE)))
mydf3 <- as.data.frame(monthlyReturn(getSymbols(ticker3_select, auto.assign=FALSE)))


combined_df <- cbind(mydf1[,1], mydf2[,1], mydf3[,1])

df <- as.data.frame(combined_df)
colnames(df) <- c("x","y", "z")
df$date = as.Date(rownames(mydf1))
# calculate the necessary values:
# I) expected returns for the two assets
er_x <- mean(df$x)
er_y <- mean(df$y)

# II) risk (standard deviation) as a risk measure
sd_x <- sd(df$x)
sd_y <- sd(df$y)

# III) covariance
cov_xy <- cov(df$x, df$y)

# create 1000 portfolio weights (omegas)
x_weights <- seq(from = 0, to = 1, length.out = 1000)

# create a data.table that contains the weights for the two assets
two_assets <- data.table(wx = x_weights,
                         wy = 1 - x_weights)

# calculate the expected returns and standard deviations for the 1000 possible portfolios
two_assets[, ':=' (er_p = wx * er_x + wy * er_y,
                   sd_p = sqrt(wx^2 * sd_x^2 +
                                 wy^2 * sd_y^2 +
                                 2 * wx * (1 - wx) * cov_xy))]
two_assets



# lastly plot the values
ggplot() +
  geom_point(data = two_assets, aes(x = sd_p, y = er_p, color = wx)) +
  geom_point(data = data.table(sd = c(sd_x, sd_y), mean = c(er_x, er_y)),
             aes(x = sd, y = mean), color = "red", size = 3, shape = 18) +
  # Miscellaneous Formatting
  theme_bw() + ggtitle("Possible Portfolios with Two Risky Assets") +
  xlab("Volatility") + ylab("Expected Returns") +
  scale_y_continuous(label = percent, limits = c(0, max(two_assets$er_p) * 1.2)) +
  scale_x_continuous(label = percent, limits = c(0, max(two_assets$sd_p) * 1.2)) +
  scale_color_continuous(name = expression(omega[x]), labels = percent)


######################################################
### Three risky assets portfolio:
#########################################################
# load the data

# calculate the necessary values:
# I) expected returns for the two assets
er_x <- mean(df$x)
er_y <- mean(df$y)
er_z <- mean(df$z)

# II) risk (standard deviation) as a risk measure
sd_x <- sd(df$x)
sd_y <- sd(df$y)
sd_z <- sd(df$z)

# III) covariance
cov_xy <- cov(df$x, df$y)
cov_xz <- cov(df$x, df$z)
cov_yz <- cov(df$y, df$z)

# create portfolio weights (omegas)
x_weights <- seq(from = 0, to = 1, length.out = 1000)

# create a data.table that contains the weights for the three assets
three_assets <- data.table(wx = rep(x_weights, each = length(x_weights)),
                           wy = rep(x_weights, length(x_weights)))

three_assets[, wz := 1 - wx - wy]


# calculate the expected returns and standard deviations for the 1000 possible portfolios
three_assets[, ':=' (er_p = wx * er_x + wy * er_y + wz * er_z,
                     sd_p = sqrt(wx^2 * sd_x^2 +
                                   wy^2 * sd_y^2 +
                                   wz^2 * sd_z^2 +
                                   2 * wx * wy * cov_xy +
                                   2 * wx * wz * cov_xz +
                                   2 * wy * wz * cov_yz))]

# take out cases where we have negative weights (shortselling)
three_assets <- three_assets[wx >= 0 & wy >= 0 & wz >= 0]
three_assets

# lastly plot the values
ggplot() +
  geom_point(data = three_assets, aes(x = sd_p, y = er_p, color = wx - wz)) +
  geom_point(data = data.table(sd = c(sd_x, sd_y, sd_z), mean = c(er_x, er_y, er_z)),
             aes(x = sd, y = mean), color = "red", size = 3, shape = 18) +
  # Miscellaneous Formatting
  theme_bw() + ggtitle("Possible Portfolios with Three Risky Assets") +
  xlab("Volatility") + ylab("Expected Returns") +
  scale_y_continuous(label = percent, limits = c(0, max(three_assets$er_p) * 1.2)) +
  scale_x_continuous(label = percent, limits = c(0, max(three_assets$sd_p) * 1.2)) +
  scale_color_gradientn(colors = c("red", "blue", "yellow"),
                        name = expression(omega[x] - omega[z]), labels = percent)

##### Portfolio Optimization

library(quantmod)
library(tseries)
library(stats)
library(quadprog) ####Quadratic optimize - Second degree optimization

##### End Date upto a selected date - Why not till recent date? We don't run it in the middle of the month (End of month, quarter or year and not a random date)
enddate <- "2021-12-31" #### You can use "2021-12-31"
t<-1350 #The first time you run this, you'll see the error so adjust the t with the requested number
##### in case there is an error, copy the error and post it in palce of "1180"

myvector <- c()
nstocks <- 7
pricinglist <- as.data.frame(matrix(ncol=nstocks, nrow=t))
colnames(pricinglist) <- c("AAPL","MSFT", "GOOGL","NFLX", "HD", "DIS", "WMT")
#the pricinglist data starts form 2016-8-22 - this is the first row

for (i in 1:(ncol(pricinglist))){ #### pricing list 10 because 10 tickers, time series will be 1389
  current_ticker <- colnames(pricinglist)[i]
  newtable <- getSymbols(current_ticker, src = "yahoo", from="2016-8-22", to=enddate, auto.assign=FALSE)
  pricinglist[,i] <- newtable[,6]
}

#forecasting the next price using a backpropagation training algorithm in a neural network. 
# a Autoregressive Model of fourth order AR4 was used.


newpricingdataset <- pricinglist

#creating a dataset with monthly ROR for each day using continuous compounding
dailyROR <- as.data.frame(matrix(ncol=ncol(newpricingdataset), nrow=nrow(newpricingdataset)-25))
colnames(dailyROR) <- colnames(pricinglist)
for (c in 1:(ncol(newpricingdataset))){
  for (r in 1:(nrow(newpricingdataset)-25)){
    dailyROR[r,c] <- log(as.numeric(newpricingdataset[(r+25),c])/as.numeric(newpricingdataset[r,c]))
  }
}
#The most current expected return for n+25 (n is today) is in the last row of the above dataset

#calculating Expected(R) for all securities 
averet <- as.matrix(dailyROR[nrow(dailyROR),], nrow=1)
#calculating covariance matrix
rcov <- cov(dailyROR[(nrow(dailyROR)-125):(nrow(dailyROR)),]) #125 stands for 6 trading months
target.r <- 1/1000
#using solver to get to optimal weights


effFrontier = function(averet, rcov, nports, shorts, wmax, wmin)
{
  mxret <- max(averet)
  mnret <- -mxret
  n.assets <- ncol(averet)
  reshigh <- rep(wmax, n.assets)
  reslow <- rep(wmin, n.assets)
  min.rets <- seq(mnret, mxret, length.out=nports)
  vol <- rep(NA, nports)
  ret <- rep(NA, nports)
  pw <- data.frame(matrix(ncol=nports, nrow=n.assets))
  for (i in 1:nports)
  {
    port.sol <- NULL
    try(port.sol <- portfolio.optim(x=averet, pm=min.rets[i], covmat=rcov,   reshigh = reshigh, reslow= reslow, shorts=F)
        , silent=T)
    if(!is.null(port.sol))
    {
      vol[i] <- sqrt(as.vector(port.sol$pw %*% rcov %*% port.sol$pw))  #### Std Dev
      ret[i] <- averet %*% port.sol$pw #### Avg Portfolio Return
      pw[,i] <- port.sol$pw #### Portfolio Weights is more important than Std Dev
    }
  }
  return(list(vol=vol, ret = ret, weights = pw))
  
}

maxSharpe <- function(averet, rcov, shorts=F, wmax=0.2, min.weight=0.01)
{
  optim.callback=function(param, averet, rcov, reshigh, reslow, shorts)
  { 
    port.sol = NULL
    try(port.sol <- portfolio.optim(x=averet, pm=param, covmat=rcov,
                                    reshigh=reshigh, reslow=reslow, shorts=shorts),silent=T)
    if(is.null(port.sol)) { ratio= 10^9} else 
    {
      m.return <- averet %*% port.sol$pw
      m.risk <- sqrt(as.vector(port.sol$pw %*% rcov %*% port.sol$pw))
      ratio <- m.return/m.risk #### Sharpe Ratio #### Modify this code to calculate the Sortino ratio
      assign("w", port.sol$pw, inherits=T)
    }
    return(ratio)
  }
  
  ef <- effFrontier(averet=averet, rcov=rcov, shorts=shorts, wmax=wmax, nports = 100, wmin=min.weight)
  n <- ncol(averet)
  reshigh <- rep(wmax, n)
  reslow <- rep(min.weight, n)
  
  max.sh <- which.max(ef$ret/ef$vol)
  
  
  if(is.na(ef$ret[max.sh-1])){lowerinterval<-ef$ret[max.sh]}else{lowerinterval <- ef$ret[max.sh-1]}
  if(is.na(ef$ret[max.sh+1])){upperinterval<-ef$ret[max.sh]}else{upperinterval <- ef$ret[max.sh+1]}
  
  w <- rep(0, ncol(averet))
  xmin <- optimize(f=optim.callback, interval = c(lowerinterval, upper=upperinterval), 
                   averet=averet, rcov=rcov, reshigh=reshigh, reslow=reslow, shorts=shorts)
  return(w)
  return(xmin)
}

z <- maxSharpe(averet, rcov, shorts=F, wmax=0.2)
print(z)

print(colnames(pricinglist))

#### Optimizer suggests that Stocks having an allocation of 1% should be removed
##sharpe, treynor, efficient front, SD, Returns, graphs