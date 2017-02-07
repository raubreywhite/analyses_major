library("stabs")
library("lars")
## make data set available
data("bodyfat", package = "TH.data")
## set seed
set.seed(1234)

## lasso
lars.lasso(x=bodyfat[,-2],y=bodyfat[,2])
x <- model.matrix(~ . - 1, bodyfat[,-2])
lars::lars(x, bodyfat[,2])
(stab.lasso <- stabsel(x = bodyfat[, -2], y = bodyfat[,2],
                       fitfun = lars.lasso, cutoff = 0.75,
                       PFER = 1))

## stepwise selection
(stab.stepwise <- stabsel(x = bodyfat[, -2], y = bodyfat[,2],
                          fitfun = lars.stepwise, cutoff = 0.75,
                          PFER = 1))

## plot results
par(mfrow = c(2, 1))
plot(stab.lasso, main = "Lasso")
plot(stab.stepwise, main = "Stepwise Selection")


n <- names(d)
n <- n[stringr::str_detect(n,"^c")]
data <- d[variable=="Colour",]
y <- data[,"value",with=F]
x <- data.table(cbind(model.matrix(~-1+id, data=data),model.matrix(~-1+month, data=data),data[,n,with=F]))
x <- data.table(cbind(model.matrix(~-1+month, data=data),data[,n,with=F]))
full <- na.omit(cbind(y,x))
y <- full[[1]]
x <- full[,-1,with=F]

(stab.lasso <- stabsel(x = x, y = y,
                       fitfun = lars.lasso, cutoff = 0.75,
                       PFER = 1))

data[,year:=NULL]
data[,week:=NULL]
data[,waterwork:=NULL]
data[,point:=NULL]
data[,units:=NULL]
data[,type:=NULL]
data[,variable:=NULL]
data[,waterType:=NULL]
data[,season:=NULL]
data[,watersource:=NULL]

install.packages("lmmlasso")
library(lmmlasso)



n <- names(d)
n <- n[stringr::str_detect(n,"^c")]
data <- na.omit(d[variable=="Colour",])
y <- data$value
x <- data.table(cbind(model.matrix(~-1+id, data=data),model.matrix(~-1+month, data=data),data[,n,with=F]))
x <- as.matrix(cbind(model.matrix(~1+month, data=data),data[,n,with=F]))
z <- model.matrix(~id, data=data)
data[,id:=as.numeric(as.factor(id))]

lmmlasso(x,y,z,grp=data$id,nonpen=1,lambda=1)
