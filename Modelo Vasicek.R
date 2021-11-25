library(yuima)
library(matrixStats)

train=.90
db=as.numeric(unlist(EJERTASAS[,2]))/100
is.numeric(db)
View(db)

plot(db, type="l", col="green")
db=db[250:length(db)]


db_entrena=db[1:round(length(db)*train)]
db_prueba=db[round(length(db)*train):length(db)]

# View(db_entrena)
# View(db_prueba)

# ESTIMACIÓN

dbe=db_entrena[2:length(db_entrena)]
dbe_1=db_entrena[1:(length(db_entrena)-1)]

DB=cbind(dbe, dbe_1)
View(DB)

mod1=lm(dbe~dbe_1)
summary(mod1)

names(mod1)

a0=1-mod1$coefficients[2]
b0=mod1$coefficients[1]/a0
s0=sd(mod1$residuals)

modtasa=setModel(drift="a*(b-x)", diffusion = "sigma", solve.variable = "x")
estima1=setYuima(data=setData(db_entrena, delta=1/360), model=modtasa)

param.init<-list(a=a0, b=b0, sigma=s0)
low.par<-list(a=-10,b=.01, sigma=.00001)
upp.var<-list(a=10, b=1, sigma=1)
mle1<-qmle(estima1, start=param.init, method="L-BFGS-B", lower=low.par, upper = upp.var)

pasos=length(db_prueba)
samp<-setSampling(n=pasos, delta = 1/360)
tray=1000
r_vect=cbind()

for(k in (1:tray)){

r<-simulate(modtasa, xinit = db_prueba[1], sampling=samp, 
            true.parameter = list(a=mle1@coef[2], b=mle1@coef[3], sigma=mle1@coef[1]))

r1=as.vector(get.zoo.data(r)[[1]])

r_vect=cbind(r_vect, r1)

}



plot(db_prueba,type="l", ylim=c(min=min(r_vect), max=max(r_vect)))

for(k in (1:tray)){
  
lines(r_vect[, k], col="grey")  
  
}

lines(db_prueba, col="red")

q_95 <- rowQuantiles(r_vect, probs = .95)
q_75 <- rowQuantiles(r_vect, probs = .75)
q_5 <- rowQuantiles(r_vect, probs = .5)
q_25 <- rowQuantiles(r_vect, probs = .25)
q_05 <- rowQuantiles(r_vect, probs = .05)
quat_base<-cbind(q_95,q_75,q_5,q_25,q_05)

plot(db_prueba, type="l", col="red", ylim=c(min(quat_base), max(quat_base)))
lines(q_95)
lines(q_75)
lines(q_5)
lines(q_25)
lines(q_05)


plot(db_prueba, type="l", col="red", ylim=c(min(quat_base), max(quat_base)))
q_20 <- rowQuantiles(r_vect, probs = .20)
lines(q_20)

rt=q_20[length(q_20)]
rt


