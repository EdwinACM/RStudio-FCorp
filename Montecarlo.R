library(BatchGetSymbols)
library(matrixStats)

datos<-360
# set dates, 
last.date <- Sys.Date()+1 #as.Date("2016-11-11") #
first.date <- last.date - datos # as.Date(last.date - datos, format = "%Y-%m-%d", origin='2011-01-01')
freq.data <- "daily"


tickers<-c("^MXX", "CUERVO.MX", "TLEVISACPO.MX", "ALSEA.MX")
data<- BatchGetSymbols(tickers = tickers[4], first.date = first.date,last.date = last.date, freq.data = freq.data)
data_c<-data$df.tickers$price.close
View(data_c)

train<-.80
data_c_tr<-data_c[(1:round(train*length(data_c)))]
data_c_test<-data_c[(length(data_c_tr):length(data_c))]

rend_m<-mean(data$df.tickers$ret.closing.prices[(1:round(train*length(data_c)))], na.rm = TRUE)*360
rend_sd<-sd(data$df.tickers$ret.closing.prices[(1:round(train*length(data_c)))], na.rm = TRUE)*(360^.5)

plot(data$df.tickers$ref.date, data$df.tickers$price.close, type = "l", col="red")

dt<-1/250
tray<-10000
pasos<-length(data_c)-length(data_c_tr)+1
s_vect<-cbind()

for(k in 1:tray){

s0<-data_c_tr[length(data_c_tr)]
s<-c(s0)

for(j in 2:pasos){
ds<-(rend_m*s[length(s)]*dt)+(rend_sd*s[length(s)]*rnorm(1, 0, dt^.5))
s_t<-s[length(s)]+ds
s<-c(s,s_t)

}

s_vect<-cbind(s_vect, s)

}


plot(data_c_test, type="l", col="red", ylim = c(min(s_vect), max(s_vect)))

for(k in 1:ncol(s_vect)){
lines(s_vect[,k], col="grey")
}

lines(data_c_test, col="red")

q_95 <- rowQuantiles(s_vect, probs = .95)
q_75 <- rowQuantiles(s_vect, probs = .75)
q_5 <- rowQuantiles(s_vect, probs = .5)
q_25 <- rowQuantiles(s_vect, probs = .25)
q_05 <- rowQuantiles(s_vect, probs = .05)
quat_base<-cbind(q_95,q_75,q_5,q_25,q_05)

plot(data_c_test, type="l", col="red", ylim=c(min(quat_base), max(quat_base)))
lines(q_95)
lines(q_75)
lines(q_5)
lines(q_25)
lines(q_05)