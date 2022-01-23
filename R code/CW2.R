belfastdata <- read.csv("8096data.csv", header = TRUE, sep = ",")
# lm. belfastdata<-lm(humidity~PM2.5+NO2+O3+temperature.,data = belfastdata)
# lm. belfastdata$coefficients

# modelhumidity <- lm(humidity~PM2.5 + NO2 + O3 + temperature, data = belfastdata)
modelPM2.5 <- lm(PM2.5~humidity + NO2 + O3 + temperature, data = belfastdata)
modelNO2 <- lm(NO2~PM2.5 + humidity + O3 + temperature, data = belfastdata)
modelO3 <- lm(O3~PM2.5 + NO2 + humidity + temperature, data = belfastdata)
# modeltemperature <- lm(humidity~PM2.5 + NO2 + O3 + humidity, data = belfastdata)

# Show the model.
# print(model)
# Get the Intercept and coefficients as vector elements.
# cat("# # # # The Coefficient Values # # # ", "\n")
a <- coef(model)[1]
# print(a)

XPM2.5 <- coef(model)[2]
XNO2 <- coef(model)[3]
XO3 <- coef(model)[4]
Xtemperature <- coef(model)[5]

# print(XPM2.5)
# print(XNO2)
# print(XO3)
# print(Xtemperature)

# Y = a+XPM2.5.x1+XNO2.x2+XO3.x3+Xtemperature.x4

states <- as.data.frame(belfastdata[,c('PM2.5','NO2','O3','temperature','humidity')])
cor(states)#查看变量相关系数
library(car)
pdf("D:/Rstudio4.1.2/workspace/overall.pdf",width=4,height=4)
scatterplotMatrix(states,spread=FALSE)  
dev.off()

# library(leaps)
# leaps<-regsubsets(humidity~PM2.5 + NO2 + O3 + temperature, data = belfastdata,nbest = 4)
# pdf("D:/Rstudio4.1.2/workspace/adjr2 humidity.pdf",width=8,height=8)
# plot(leaps,scale = 'adjr2') 
# dev.off()

leaps<-regsubsets(PM2.5~humidity + NO2 + O3 + temperature, data = belfastdata,nbest = 4)
pdf("D:/Rstudio4.1.2/workspace/adjr2 PM2.5.pdf",width=8,height=8)
plot(leaps,scale = 'adjr2') 
dev.off()

leaps<-regsubsets(NO2~humidity + PM2.5 + O3 + temperature, data = belfastdata,nbest = 4)
pdf("D:/Rstudio4.1.2/workspace/adjr2 NO2.pdf",width=8,height=8)
plot(leaps,scale = 'adjr2') 
dev.off()

leaps<-regsubsets(O3~humidity + PM2.5 + NO2 + temperature, data = belfastdata,nbest = 4)
pdf("D:/Rstudio4.1.2/workspace/adjr2 O3.pdf",width=8,height=8)
plot(leaps,scale = 'adjr2') 
dev.off()

# leaps<-regsubsets(temperature~humidity + PM2.5 + NO2 + O3, data = belfastdata,nbest = 4)
# pdf("D:/Rstudio4.1.2/workspace/adjr2 temperature.pdf",width=8,height=8)
# plot(leaps,scale = 'adjr2') 
# dev.off()

pdf("D:/Rstudio4.1.2/workspace/qqplotPM2.5.pdf",width=8,height=8)
qqPlot(modelPM2.5,labels = row.names(states),id.method = 'identify',simulate = T)
dev.off()

pdf("D:/Rstudio4.1.2/workspace/qqplotNO2.pdf",width=8,height=8)
qqPlot(modelNO2,labels = row.names(states),id.method = 'identify',simulate = T)
dev.off()

pdf("D:/Rstudio4.1.2/workspace/qqplotO3.pdf",width=8,height=8)
qqPlot(modelO3,labels = row.names(states),id.method = 'identify',simulate = T)
dev.off()