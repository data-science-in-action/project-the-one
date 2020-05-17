
#一、描述性分析

#武汉市各区域累计确诊人数变化折线图
library(ggplot2)
library(reshape2)
library(forcats)
data<-read.csv('data/area_data.csv')
data<-melt(data,id.vars = 'day',
           variable.name = "area",value.name = "num") 
data$day<-fct_inorder(data$day)
ggplot(data = data, mapping = aes(x = day, y = num, colour = area,group = area )) +
  geom_line(size = 0.5) +
  geom_point(size = 1) +
  labs(title = "武汉市各区域累计确诊人数变化折线图:",x = '时间',y = '人数/人') + 
  theme(axis.text.x = element_text(angle = 60, hjust = 0.5, vjust =0.5 ,size = 8))

#3月31日武汉市各区累计确诊人数占比环形图
library(ggplot2)
data<-read.csv('data/area_count.csv')
data$proportion<-round(data$count/sum(data$count),3)
data$ymax<-cumsum(data$proportion) 
data$ymin<-c(0,head(data$ymax,n = -1))  
data$labelPosition<-(data$ymax + data$ymin)/2
data$label<-paste0(data$area,"\n",data$proportion)
ggplot(data)+
  geom_rect(aes(ymax = ymax,ymin = ymin,xmax = 0,xmin = 1,fill = id))+
  xlim(-1,1)+
  coord_polar(theta = "y")+
  theme_void()+
  geom_label(x = 0.7,y = data$labelPosition,label = data$label,size = 3)+
  labs(title = "3月31日武汉市各区域累计确诊人数占比环形图:",fill = '地区')+
  theme(legend.position = "right")

#气象因子描述统计
library(ggplot2)
GAM<-read.csv('data/GAM.csv')
par(mfrow=c(2,3))
hist(GAM$AT, breaks = 15, col = "yellow", freq = FALSE)
lines(dens<-density(GAM$AT))
hist(GAM$DTR, breaks = 15, col = "yellow", freq = FALSE)
lines(dens<-density(GAM$DTR))
hist(GAM$RH, breaks = 15, col = "yellow", freq = FALSE)
lines(dens<-density(GAM$RH))
hist(GAM$SUN, breaks = 15, col = "yellow", freq = FALSE)
lines(dens<-density(GAM$SUN))
hist(GAM$WIND, breaks = 15, col = "yellow", freq = FALSE)
lines(dens<-density(GAM$WIND))

#空气污染因子描述统计
library(ggplot2)
GAM<-read.csv('data/GAM.csv')
par(mfrow=c(2,3))
hist(GAM$PM2.5, breaks = 15, col = "red", freq = FALSE)
lines(dens<-density(GAM$PM2.5))
hist(GAM$PM10, breaks = 15, col = "red", freq = FALSE)
lines(dens<-density(GAM$PM10))
hist(GAM$SO2, breaks = 15, col = "red", freq = FALSE)
lines(dens<-density(GAM$SO2))
hist(GAM$CO, breaks = 15, col = "red", freq = FALSE)
lines(dens<-density(GAM$CO))
hist(GAM$NO2, breaks = 15, col = "red", freq = FALSE)
lines(dens<-density(GAM$NO2))
hist(GAM$O3, breaks = 15, col = "red", freq = FALSE)
lines(dens<-density(GAM$O3))

#二、相关分析

## 气象数据、空气污染数据与当日新增确诊数相关性
data_GAM1 <- read.csv('data/GAM.csv')
install.packages("ggplot2")
library(ggplot2)
ggplot(data_GAM1,mapping =aes(x=PM2.5,y=DIAG))+geom_point()
ggplot(data_GAM1,mapping =aes(x=PM10,y=DIAG))+geom_point()
ggplot(data_GAM1,mapping =aes(x=SO2,y=DIAG))+geom_point()
ggplot(data_GAM1,mapping =aes(x=CO,y=DIAG))+geom_point()
ggplot(data_GAM1,mapping =aes(x=NO2,y=DIAG))+geom_point()
ggplot(data_GAM1,mapping =aes(x=O3,y=DIAG))+geom_point()
ggplot(data_GAM1,mapping =aes(x=AT,y=DIAG))+geom_point()
ggplot(data_GAM1,mapping =aes(x=DTR,y=DIAG))+geom_point()
ggplot(data_GAM1,mapping =aes(x=RH,y=DIAG))+geom_point()
ggplot(data_GAM1,mapping =aes(x=SUN,y=DIAG))+geom_point()
ggplot(data_GAM1,mapping =aes(x=WIND,y=DIAG))+geom_point()


cov1 <-cov(data_GAM1)
s.xg1 <-cor(data_GAM1,method = "spearman")
install.packages("psych")
library(psych)
corr.test(data_GAM1,use="complete",method = "spearman")

install.packages("corrplot")
library(corrplot)
corrplot(s.xg1,type="lower",method="number",addCoef.col="red",pin=c(10,10))

## 气象数据、空气污染数据与当日新增死亡数相关性
data_GAM2 <- read.csv('data/GAM.csv')
library(ggplot2)
ggplot(data_GAM2,mapping =aes(x=PM2.5,y=DEATH))+geom_point()
ggplot(data_GAM2,mapping =aes(x=PM10,y=DEATH))+geom_point()
ggplot(data_GAM2,mapping =aes(x=SO2,y=DEATH))+geom_point()
ggplot(data_GAM2,mapping =aes(x=CO,y=DEATH))+geom_point()
ggplot(data_GAM2,mapping =aes(x=NO2,y=DEATH))+geom_point()
ggplot(data_GAM2,mapping =aes(x=O3,y=DEATH))+geom_point()
ggplot(data_GAM2,mapping =aes(x=AT,y=DEATH))+geom_point()
ggplot(data_GAM2,mapping =aes(x=DTR,y=DEATH))+geom_point()
ggplot(data_GAM2,mapping =aes(x=RH,y=DEATH))+geom_point()
ggplot(data_GAM2,mapping =aes(x=SUN,y=DEATH))+geom_point()
ggplot(data_GAM2,mapping =aes(x=WIND,y=DEATH))+geom_point()

cov2 <-cov(data_GAM2)
s.xg2 <-cor(data_GAM2,method = "spearman")

library(psych)
corr.test(data_GAM2,use="complete",method = "spearman")

library(corrplot)
corrplot(s.xg2,type="lower",method="number",addCoef.col="red",pin=c(10,10))


#三、GAM模型


mydata<-read.csv('data/GAM.csv')
library(mgcv)

#建立确诊人数的GAM的模型
mod.gam1<-gam(DIAG~ s(TIME)+PM2.5+PM10+SO2+CO+NO2+O3+s(AT)+s(DTR)+s(RH)+s(SUN)+s(WIND)+as.factor(DOW),family=poisson,data=mydata)
summary(mod.gam1)
par(mfrow=c(2,2)) 
gam.check(mod.gam1)#模型检验通不过

#建立死亡人数的GAM的模型
mod.gam2<-gam(DEATH~ s(TIME)+PM2.5+PM10+SO2+CO+NO2+O3+s(AT)+s(DTR)+s(RH)+s(SUN)+s(WIND)+as.factor(DOW),family=poisson,data=mydata)
summary(mod.gam2)#有的变量不显著

#删除WIND变量
mod.gam3<-gam(DEATH~ s(TIME)+PM2.5+PM10+SO2+CO+NO2+O3+s(AT)+s(DTR)+s(RH)+s(SUN)+as.factor(DOW),family=poisson,data=mydata)
summary(mod.gam3)
coef(mod.gam3)#coef()函数提取所有的模型系数，这些系数是构成平滑的每个基函数的系数。
par(mfrow=c(2,2)) 
gam.check(mod.gam3)#模型检验通过
par(mfrow=c(2,3))
plot(mod.gam3)

#交互作用的影响
mod.gam4<-gam(DEATH~ s(TIME)+PM2.5+PM10+SO2+CO+NO2+O3+s(AT,DTR)+as.factor(DOW),family=poisson,data=mydata)
mod.gam5<-gam(DEATH~ s(TIME)+PM2.5+PM10+SO2+CO+NO2+O3+s(AT,RH)+as.factor(DOW),family=poisson,data=mydata)
mod.gam6<-gam(DEATH~ s(TIME)+PM2.5+PM10+SO2+CO+NO2+O3+s(AT,SUN)+as.factor(DOW),family=poisson,data=mydata)
mod.gam7<-gam(DEATH~ s(TIME)+PM2.5+PM10+SO2+CO+NO2+O3+s(DTR,RH)+as.factor(DOW),family=poisson,data=mydata)
mod.gam8<-gam(DEATH~ s(TIME)+PM2.5+PM10+SO2+CO+NO2+O3+s(DTR,SUN)+as.factor(DOW),family=poisson,data=mydata)
mod.gam9<-gam(DEATH~ s(TIME)+PM2.5+PM10+SO2+CO+NO2+O3+s(RH,SUN)+as.factor(DOW),family=poisson,data=mydata)
summary(mod.gam4)
summary(mod.gam5)
summary(mod.gam6)
summary(mod.gam7)
summary(mod.gam8)
summary(mod.gam9)

par(mfrow=c(2,3)) 
#绘制二维图，mgcv包里有vis.gam的具体教程
vis.gam(mod.gam4,view = c("AT", "DTR"),ticktype="detailed",color="heat",theta=-35)
vis.gam(mod.gam5,view = c("AT", "RH"),ticktype="detailed",color="heat",theta=-35)
vis.gam(mod.gam6,view = c("AT", "SUN"),ticktype="detailed",color="heat",theta=-35)
vis.gam(mod.gam7,view = c("DTR", "RH"),ticktype="detailed",color="heat",theta=-35)
vis.gam(mod.gam8,view = c("DTR", "SUN"),ticktype="detailed",color="heat",theta=-35)
vis.gam(mod.gam9,view = c("RH", "SUN"),ticktype="detailed",color="heat",theta=-35)


