library(matrixStats)

rend<-as.numeric(unlist(Tlvisa$`Rendimientos aritmeticos`))
#View(rend)

train=.90
tv_entrena<-rend[(1:round(train*length(rend)))]
tv_prueba<-rend[(length(tv_entrena):length(rend))]
#View(tv_prueba)
#View(tv_entrena)

rend_m<-mean(rend[(1:round(train*length(tv_entrena)))], na.rm = TRUE)
rend_sd<-sd(rend[(1:round(train*length(tv_entrena)))], na.rm = TRUE)^.5

#plot(Tlvisa$Fecha, rend, type="l", col="red")

#Simulación Montecarlo

dt<-1
tray<-10000
pasos<-length(rend)-length(tv_entrena)+1
s_vect<-cbind()

for(k in 1:tray){
  
s0<-tv_entrena[length(tv_entrena)]
s<-c(s0)
  
for(j in 2:pasos){
ds<-(rend_m*s[length(s)]*dt)+(rend_sd*s[length(s)]*rnorm(1, 0, dt^.5))
s_t<-s[length(s)]+ds
s<-c(s,s_t)
    
}
  
s_vect<-cbind(s_vect, s)
  
}

#View(s_vect)
plot(tv_prueba, type="l", col="darksalmon", ylim = c(min(s_vect), max(s_vect)))

for(k in 1:ncol(s_vect)){
  lines(s_vect[,k], col="gainsboro")
}

lines(tv_prueba, col="darksalmon")

q_95 <- rowQuantiles(s_vect, probs = .95)
q_75 <- rowQuantiles(s_vect, probs = .75)
q_5 <- rowQuantiles(s_vect, probs = .5)
q_25 <- rowQuantiles(s_vect, probs = .25)
q_05 <- rowQuantiles(s_vect, probs = .05)
quat_base<-cbind(q_95,q_75,q_5,q_25,q_05)

plot(tv_prueba, type="l", col="darksalmon", ylim=c(min(quat_base), max(quat_base)))
lines(q_95)
lines(q_75)
lines(q_5)
lines(q_25)
lines(q_05)

#Cruz Mendoza Edwin Alejandro