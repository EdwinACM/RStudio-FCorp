library(fPortfolio)
library(BatchGetSymbols)
library(timeSeries)

last.date <- as.Date("2020-02-28")+1 #as.Date("2016-11-11") #
first.date <- as.Date("2015-12-08") # as.Date(last.date - datos, format = "%Y-%m-%d", origin='2011-01-01')
freq.data <- "weekly"

tickers<-c("ASURB.MX", "OMAB.MX", "GAPB.MX", "TLEVISACPO.MX", "CUERVO.MX")
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

#View(data2)
#class(data2)

rend=returns(data2)

fecha_rend=getTime(rend)
fecha_rend=fecha_rend@Data
vec_peso=cbind()

length(fecha_rend)

for(h in 1:length(fecha_rend)){
  
rend=window(rend, fecha_rend[h], fecha_rend[h+30])
pm_st=portfolioSpec()
setRiskFreeRate(pm_st)=0.0889

pst=tangencyPortfolio(data = rend,
                      spec=pm_st,
                      constraints = "LongOnly")
print(pst)

weights=t(as.vector(getWeights(pst))*100)
vec_peso=cbind(vec_peso, weights)
}

vec_peso

nombres=colnames(rend)
barplot(height = weights, names.arg = nombres, col=seq(1,3))
title("Pesos Optimos de Portafolio ST")

