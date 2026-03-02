# List call/put prices with given maturity
# filters: keep only options with volume >= 1 and bid>0
# code produced in Feb 2026
rm(list = ls())
source("dataFunctionsYFOMFixedTImpVol.R")
#source("getPlotsOMFixedTAmerican.R")
# spot close 

S0 <- 686.29

# load SPY options data on 18-Feb-2026
allData <- read.csv(file = 'mktdata/SPY_options_18Feb2026.csv')


#View(allData)
names(allData)
length(allData$exdate)
#head(allData)

summary(allData$date) # all pricing dates in the data
#=====================================================================

oneDay <- '2026-02-18'

dataOneDay <- allData[ which(allData$date==oneDay),]

#==================================================================
# extract all expirations
summary(dataOneDay$exdate)
table(dataOneDay$exdate)

#dataOneDay$exdate <- droplevels(dataOneDay$exdate)
datesExpiry <- unique(dataOneDay$exdate)

head(datesExpiry)

#datesExpiry <- levels(datesExpiry)
datesExpiry <- as.Date(datesExpiry, "%Y-%m-%d")
datesExpiry <- sort(datesExpiry)
head(datesExpiry)
tail(datesExpiry)
n.exp <- length(datesExpiry)
n.exp

#Tgrid <- seq(0,length(datesExpiry))

Tdays <- numeric()
for (i in 1:n.exp) Tdays[i] <- as.numeric(datesExpiry[i]) - as.numeric(datesExpiry[1])
Tdays

# maturities in units of 5 days
#hist(Tdays, breaks=seq(0,1050,5), xlim=c(0,100))

# === compute the forwards and traded volumes ===========
n.KC <- c()
n.KP <- c()
call.vol <- 0
put.vol <- 0
Fwd <- c()

#dataCall(dataOneDay, datesExpiry[2])

for (i in 1:n.exp){
  dataCi <- dataCall(dataOneDay,datesExpiry[i])
  dataPi <- dataPut(dataOneDay,datesExpiry[i])
  fwdPrice <- getForward(dataCi,dataPi)
  n.KC <- c(n.KC,length(dataCi$K))
  n.KP <- c(n.KP,length(dataPi$K))
  vol.data <- getTradedVolumes(dataCi,dataPi)
  call.vol <- call.vol + vol.data$volC
  put.vol <- put.vol + vol.data$volP
  Fwd <- c(Fwd, fwdPrice)
  
}

sum(n.KC)
sum(n.KP)
call.vol
put.vol
#write the forwards to a csv file c(date,expiration,ForwardPrice)
Fwd.OM <- data.frame("date"=oneDay,"expiration"=datesExpiry,
                     "ForwardPrice"=Fwd)
head(Fwd.OM)
#write.csv(Fwd.OM, file="SPY_forwards_11Feb2026.csv")

################################################
# extract call/put prices with given maturity
source("dataFunctionsYFOMFixedT.R")
indExp <- 4
datesExpiry[indExp]
Tdays[indExp]

dataC1 <- dataCall(dataOneDay,datesExpiry[indExp])
dataP1 <- dataPut(dataOneDay,datesExpiry[indExp])

fwdPrice1 <- getForward(dataC1,dataP1)
fwdPrice1

head(dataC1,10)
head(dataP1,10)

S0

tv <- getTradedVolumes(dataC1,dataP1)
tv$volC


# plot the implied volatility

# the implied volatility smile
plot(dataC1$K, dataC1$IV, type="p", pch=20, 
     xlim=c(500,800), ylim=c(0,1))
lines(dataP1$K, dataP1$IV, type="p", pch=20, col="red")
####################################################
