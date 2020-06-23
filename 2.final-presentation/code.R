data<-read.csv('data/GAM.csv')
library(nlme)
library(mgcv)
library(dlnm)
library(splines)

#对确诊人数的影响

#构建交叉基
cbPM2.5<-crossbasis(data$PM2.5,lag=c(0,7),argvar=list(fun="lin"),arglag=list(fun="integer"))
cbSO2<-crossbasis(data$SO2,lag=c(0,7),argvar=list(fun="lin"),arglag=list(fun="integer"))
cbO3<-crossbasis(data$O3,lag=c(0,7),argvar=list(fun="lin"),arglag=list(fun="integer"))

model1<-gam(DIAG~ cbPM2.5+cbSO2+cbO3+s(TIME)+s(AT)+s(DTR)+s(RH)+as.factor(DOW),family=quasipoisson,data=data)
summary(model1)

#气象对确诊人数的暴露-反应图
par(mfrow=c(2,2)) 
plot(model1)


#气象交互作用对确诊人数的影响，其中（AT,RH）不显著
model2<-gam(DIAG~ s(TIME)+PM2.5+SO2+O3+s(AT,DTR)+as.factor(DOW),family=quasipoisson,data=data)
model3<-gam(DIAG~ s(TIME)+PM2.5+SO2+O3+s(DTR,RH)+as.factor(DOW),family=quasipoisson,data=data)
model4<-gam(DIAG~ s(TIME)+PM2.5+SO2+O3+s(AT,RH)+as.factor(DOW),family=quasipoisson,data=data)

summary(model2)
summary(model3)
summary(model4)

par(mfrow=c(1,2)) 
vis.gam(model2,view = c("AT", "DTR"),ticktype="detailed",color="heat",theta=-150)
vis.gam(model3,view = c("DTR", "RH"),ticktype="detailed",color="heat",theta=125)


#气象对确诊人数的滞后

cbAT <- crossbasis(data$AT, lag=c(0,7), argvar=list(fun="lin"),arglag=list(fun="integer"))
cbDTR <- crossbasis(data$DTR, lag=c(0,7), argvar=list(fun="lin"),arglag=list(fun="integer"))
cbRH <- crossbasis(data$RH, lag=c(0,7), argvar=list(fun="lin"),arglag=list(fun="integer"))

model5<-gam(DIAG~ cbAT+cbDTR+cbRH+s(TIME)+s(PM2.5)+s(SO2)+s(O3)+as.factor(DOW),data=data,family=quasipoisson)
summary(model5)

par(mfrow=c(1,3))
#AT
pred1.AT<-crosspred(cbAT,model5,at = 1)
plot(pred1.AT,var=1,type="p",ci="bars",col=2,pch=19,
     xlab="Lag Days of AT",ylab="RR(95%CI)") 

tablag1.AT<-with(pred1.AT,t(rbind(matRRfit,matRRlow,matRRhigh)))
colnames(tablag1.AT)<-c("RR","ci.low","ci.hi")
tablag1.AT

#DTR
pred1.DTR<-crosspred(cbDTR,model5,at = 1)
plot(pred1.DTR,var=1,type="p",ci="bars",col=2,pch=19,
     xlab="Lag Days of DTR",ylab="RR(95%CI)") 

tablag1.DTR<-with(pred1.DTR,t(rbind(matRRfit,matRRlow,matRRhigh)))
colnames(tablag1.DTR)<-c("RR","ci.low","ci.hi")
tablag1.DTR

#RH
pred1.RH<-crosspred(cbRH,model5,at = 1)
plot(pred1.RH,var=1,type="p",ci="bars",col=2,pch=19,
     xlab="Lag Days of RH",ylab="RR(95%CI)") 

tablag1.RH<-with(pred1.RH,t(rbind(matRRfit,matRRlow,matRRhigh)))
colnames(tablag1.RH)<-c("RR","ci.low","ci.hi")
tablag1.RH


#污染对确诊人数的滞后

par(mfrow=c(1,3)) 
#PM2.5绘图
pred1.PM2.5<-crosspred(cbPM2.5,model1,at=10)
plot(pred1.PM2.5,var=10,type="p",ci="bars",col=2,pch=19,
     xlab="Lag Days of PM2.5",ylab="RR(95%CI)") 

#PM2.5提取RR值及可信区间
tablag1.PM2.5<-with(pred1.PM2.5,t(rbind(matRRfit,matRRlow,matRRhigh)))
colnames(tablag1.PM2.5)<-c("RR","ci.low","ci.hi")
tablag1.PM2.5

#SO2绘图
pred1.SO2<-crosspred(cbSO2,model1,at = 10)
plot(pred1.SO2,var=10,type="p",ci="bars",col=2,pch=19,
     xlab="Lag Days of SO2",ylab="RR(95%CI)") 

#SO2提取RR值及可信区间
tablag1.SO2<-with(pred1.SO2,t(rbind(matRRfit,matRRlow,matRRhigh)))
colnames(tablag1.SO2)<-c("RR","ci.low","ci.hi")
tablag1.SO2

#O3绘图
pred1.O3<-crosspred(cbO3,model1,at = 10)
plot(pred1.O3,var=10,type="p",ci="bars",col=2,pch=19,
     xlab="Lag Days of O3",ylab="RR(95%CI)") 

#O3提取RR值及可信区间
tablag1.O3<-with(pred1.O3,t(rbind(matRRfit,matRRlow,matRRhigh)))
colnames(tablag1.O3)<-c("RR","ci.low","ci.hi")
tablag1.O3


#对死亡人数的影响


#构建交叉基
cbPM2.5<-crossbasis(data$PM2.5,lag=c(0,7),argvar=list(fun="lin"),arglag=list(fun="integer"))
cbSO2<-crossbasis(data$SO2,lag=c(0,7),argvar=list(fun="lin"),arglag=list(fun="integer"))
cbO3<-crossbasis(data$O3,lag=c(0,7),argvar=list(fun="lin"),arglag=list(fun="integer"))

#死亡人数模型
model6<-gam(DEATH~ s(TIME)+cbPM2.5+cbSO2+cbO3+s(AT)+s(SUN)+s(RH)+as.factor(DOW),family=quasipoisson,data=data)

par(mfrow=c(2,2)) 
plot(model6)#平滑项绘图
summary(model6)#p值是否显著
par(mfrow=c(2,2)) 
gam.check(model6)#模型检验


#交互作用对死亡人数的影响
model7<-gam(DEATH~ s(TIME)+PM2.5+SO2+O3+s(AT,SUN)+as.factor(DOW),family=quasipoisson,data=data)
model8<-gam(DEATH~ s(TIME)+PM2.5+SO2+O3+s(AT,RH)+as.factor(DOW),family=quasipoisson,data=data)
model9<-gam(DEATH~ s(TIME)+PM2.5+SO2+O3+s(RH,SUN)+as.factor(DOW),family=quasipoisson,data=data)

summary(model7)
summary(model8)
summary(model9)


par(mfrow=c(1,3)) 
#绘制二维图，mgcv包里有vis.gam的具体教程
vis.gam(model7,view = c("AT", "SUN"),ticktype="detailed",color="heat",theta=-35)
vis.gam(model8,view = c("AT", "RH"),ticktype="detailed",color="heat",theta=-35)
vis.gam(model9,view = c("RH", "SUN"),ticktype="detailed",color="heat",theta=-130)


#气象对死亡人数的滞后

cbAT <- crossbasis(data$AT, lag=c(0,7), argvar=list(fun="lin"),arglag=list(fun="integer"))
cbRH <- crossbasis(data$RH, lag=c(0,7), argvar=list(fun="lin"),arglag=list(fun="integer"))
cbSUN <- crossbasis(data$SUN, lag=c(0,7), argvar=list(fun="lin"),arglag=list(fun="integer"))

model10<-gam(DEATH~cbAT+cbRH+cbSUN+s(PM2.5)+s(SO2)+s(O3)+s(TIME)+as.factor(DOW),data=data,family=quasipoisson)
summary(model10)

par(mfrow=c(1,3))
#AT
pred2.AT<-crosspred(cbAT,model10,at = 1)
plot(pred2.AT,var=1,type="p",ci="bars",col=2,pch=19,
     xlab="Lag Days of AT",ylab="RR(95%CI)") 

tablag2.AT<-with(pred1.AT,t(rbind(matRRfit,matRRlow,matRRhigh,matse)))
colnames(tablag2.AT)<-c("RR","ci.low","ci.hi","se")
tablag2.AT

#RH
pred2.RH<-crosspred(cbRH,model10,at = 1)
plot(pred2.RH,var=1,type="p",ci="bars",col=2,pch=19,
     xlab="Lag Days of RH",ylab="RR(95%CI)") 

tablag2.RH<-with(pred1.RH,t(rbind(matRRfit,matRRlow,matRRhigh,matse)))
colnames(tablag2.RH)<-c("RR","ci.low","ci.hi","se")
tablag1.RH

#SUN
pred2.SUN<-crosspred(cbSUN,model10,at = 1)
plot(pred2.SUN,var=1,type="p",ci="bars",col=2,pch=19,
     xlab="Lag Days of SUN",ylab="RR(95%CI)")

tablag2.SUN<-with(pred1.SUN,t(rbind(matRRfit,matRRlow,matRRhigh,matse)))
colnames(tablag2.SUN)<-c("RR","ci.low","ci.hi","se")
tablag2.SUN


#污染对死亡人数的滞后

par(mfrow=c(1,3)) 
#PM2.5绘图
pred2.PM2.5<-crosspred(cbPM2.5,model6,at = 10)
plot(pred2.PM2.5,var=10,type="p",ci="bars",col=2,pch=19,
     xlab="Lag Days of PM2.5",ylab="RR(95%CI)") 

#PM2.5提取RR值及可信区间
tablag2.PM2.5<-with(pred2.PM2.5,t(rbind(matRRfit,matRRlow,matRRhigh)))
colnames(tablag2.PM2.5)<-c("RR","ci.low","ci.hi")
tablag2.PM2.5


#SO2绘图
pred2.SO2<-crosspred(cbSO2,model6,at = 10)
plot(pred2.SO2,var=10,type="p",ci="bars",col=2,pch=19,
     xlab="Lag Days of SO2",ylab="RR(95%CI)") 

#SO2提取RR值及可信区间
tablag2.SO2<-with(pred2.SO2,t(rbind(matRRfit,matRRlow,matRRhigh)))
colnames(tablag2.SO2)<-c("RR","ci.low","ci.hi")
tablag2.SO2


#O3绘图
pred2.O3<-crosspred(cbO3,model6,at = 10)
plot(pred2.O3,var=10,type="p",ci="bars",col=2,pch=19,
     xlab="Lag Days of O3",ylab="RR(95%CI)") 

#O3提取RR值及可信区间
tablag2.O3<-with(pred2.O3,t(rbind(matRRfit,matRRlow,matRRhigh)))
colnames(tablag2.O3)<-c("RR","ci.low","ci.hi")
tablag2.O3

