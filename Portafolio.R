#Cruz Mendoza Edwin Alejandro

library(fPortfolio)
library(BatchGetSymbols)
library(timeSeries)

last.date <- as.Date("2020-11-30")+1 
first.date <- as.Date("2015-12-07")
freq.data <- "daily"

tickers<-c("MSFT", "NVDA", "AMD")
data<- BatchGetSymbols(tickers = tickers, first.date = first.date,last.date = last.date, freq.data = freq.data)

vad=which(colnames(data$df.tickers)=="price.close")
data$df.tickers[data$df.tickers$ticker==tickers[1],vad]

data3=data$df.tickers[data$df.tickers$ticker==tickers[1],vad]

fechas=data$df.tickers[data$df.tickers$ticker==tickers[1],]$ref.date

for(h in 2:length(tickers)){
  
data3=cbind(data3, data$df.tickers[data$df.tickers$ticker==tickers[h],vad])
  
}

colnames(data3)=tickers

data2=timeSeries(data3, charvec = fechas)

vec_peso=rbind()

rend=returns(data2)

fecha_rend=getTime(rend)
fecha_rend=fecha_rend@Data

for(j in 1:length(fecha_rend)){
n=7*j
rend=returns(data2)

fecha_rend=getTime(rend)
fecha_rend=fecha_rend@Data
if (n >= 1  && n <= 1224){
rend1=window(rend, fecha_rend[n], fecha_rend[n+30])
pm_st=portfolioSpec()
setRiskFreeRate(pm_st)=0.00035584
  
pst=tangencyPortfolio(data = rend1,
                      spec=pm_st,
                      constraints = "LongOnly")
weights=t(as.vector(getWeights(pst))*100)
vec_peso=rbind(vec_peso, weights)
}
}

View(vec_peso)
