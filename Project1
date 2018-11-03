DATA <- read.csv("RandomPortfolio.csv")
DATA[is.na(DATA)] <- 0
DATA$mcap <- DATA$PRC*DATA$SHROUT
DATA$date <- as.Date(DATA$date,"%d%b%Y")
DATA$year <- as.numeric(format(DATA$date,"%Y"))
DATA$month <- as.numeric(format(DATA$date,"%m"))
#DATA <- subset(DATA,mcap!=0)

nstocks=20
nports = 70

#Baseline Year & Month
nyear = 1980
nmonth = 2


investamt = 100/nstocks

nperiods <- c(nyear+1,nyear+3,nyear+5,nyear+10,nyear+15, nyear+20, nyear+25)

returnmatrix <- data.frame(matrix(ncol = length(nperiods), nrow = nports ))
returnmatrix[is.na(returnmatrix)] <- 0

stockmatrix <- data.frame(matrix(ncol = nstocks, nrow = nports))
stockmatrix[is.na(stockmatrix)] <- 0

#Creating a new dataset containing data from the baseline year
DATA2 <- DATA[which(DATA$year == nyear),]
DATA2$month <- as.numeric(format(DATA2$date,"%m"))
#Pick random stocks for all the given number of portfolios
for (j in 1:nports)
{
stockmatrix[j,] <- sample(DATA2$PERMNO,nstocks)
}

#Defining linear vectors for calculations of return matrix
startcap <- vector(mode="numeric",length=nstocks)
sc <- vector(mode="numeric",length=nstocks)
portcap <- vector(mode="numeric",length=nstocks)

#Defining dummy variables for use
mak =0
dummyvar = 0

#For Loop for choosing the portfolio one by one
for (k in 1:nports)
{

  #For Loop for calculating the market cap & ownership percentage during the baseline year
  for (m in 1:nstocks)
  {
    #We get the market capitalization for each stock in the portfolio during baseline year
   dummyvar  <- DATA2[which( (DATA2$month == nmonth) & (DATA2$PERMNO == stockmatrix[k,m]) ), ]$mcap
   #If the value returned is empty then it will be stored as zero
   startcap[m] <- ifelse(length(dummyvar)==0,0,dummyvar)    
   
     #This variable contains the ownership percentage of each company in the portfolio
     sc[m] <- (investamt/startcap[m])
  }
  
  #This FOR LOOP calculates the returns for each period of the chosen portfolio
  for (l in 1:length(nperiods))
  {
  
    #This FOR LOOP computes the return of the portfolio for that particular period
    for (n in 1:nstocks)
    {
      dummyvar <- DATA[which((DATA$year== nperiods[l] & DATA$month == nmonth) & (DATA$PERMNO == stockmatrix[k,n]) ), ]$mcap
      portcap[n] <- ifelse(length(dummyvar) ==0,0,dummyvar)
      returnmatrix[k,l] = returnmatrix[k,l] + (sc[n]*portcap[n])
    }    
  }
}
View(returnmatrix)
