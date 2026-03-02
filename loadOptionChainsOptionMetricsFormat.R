# save option chains using quantmod in OptionMetrics format
library(quantmod)

tckr = "SPY"
asofDate = "2026-02-27"

# get historical stock prices, and save to csv

getSymbols("SPY", src="yahoo", from="2026-01-01", to=asofDate)

time <- index(SPY)
priceData <- data.frame("Date"=time, "Price"=SPY$SPY.Close)
head(priceData)

# save price data
write.csv(priceData, file="prices.csv")



##### load all option maturities
data.optall <- getOptionChain(tckr, NULL)
#head(data.optall$Feb.11.2026)
#class(jpm.optall[[1]])

# the index of the list labels the expiration
#extract all expiries

n.exp <- length(data.optall)
n.exp

expiries <- c()

for (i in 1:n.exp) {
  expi <- data.optall[[i]]$calls$Expiration[1]
  expid <- as.Date(expi)  
  print(expid)
  expiries <- c(expiries, expid)
}
expiries <- as.Date(expiries)

expiries

##################################################
# iterate through expiries and write calls/puts to master data.frame
# loop through all expiries

expiries
n.exp

cp.data <- data.frame(
  date = character(),
  symbol= character(),
  exdate = numeric(),
  cp_flag=character(),
  strike_price = numeric(),
  best_bid=numeric(),
  best_offer=numeric(),
  volume=numeric(),
  volume=numeric(),
  open_interest=numeric(),
  impl_volatility=numeric()
)

str(cp.data)

#cp.data <- rbind(cp.data, calls.index)
#head(cp.data)

for (ix in 1:n.exp){
  print(expiries[ix])
  optindex <- data.optall[[ix]]
  optindex.C <- optindex$calls
  optindex.P <- optindex$puts
  
  n.C <- length(optindex.C$Strike)
  print(n.C)
  
  n.P <- length(optindex.P$Strike)
  print(n.P)
  
  calls.index <- data.frame("date"=asofDate, 
                            "symbol" = tckr,
                            "exdate"=expiries[ix],
                            "cp_flag"="C",
                            "strike_price"=optindex.C$Strike,
                            "best_bid"=optindex.C$Bid,
                            "best_offer"=optindex.C$Ask,
                            "volume"=optindex.C$Vol,
                            "open_interest"=optindex.C$OI,
                            "impl_volatility"=optindex.C$IV)
  
#  head(calls.index)
  
  puts.index <- data.frame("date"=asofDate, 
                           "symbol" = tckr,
                           "exdate"=expiries[ix],
                           "cp_flag"="P",
                           "strike_price"=optindex.P$Strike,
                           "best_bid"=optindex.P$Bid,
                           "best_offer"=optindex.P$Ask,
                           "volume"=optindex.P$Vol,
                           "open_interest"=optindex.P$OI,
                           "impl_volatility"=optindex.P$IV)
  
#  head(puts.index)
  cp.data <- rbind(cp.data, calls.index)
  cp.data <- rbind(cp.data, puts.index)
  
#  cp.index <- rbind(calls.index,puts.index)
  
}

head(cp.data)
tail(cp.data)
length(cp.data$best_bid)

cp.data.clean <- na.omit(cp.data)
head(cp.data.clean)
length(cp.data.clean$best_bid)


###################################################

# writing the result to csv
write.csv(data.frame(cp.data.clean), file="SPY_options_27Feb2026.csv")

# saved option chains 26.Feb at 16:27


