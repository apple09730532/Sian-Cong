library(openxlsx)
library(vars)
library(urca)
library(tsDyn)
library(lmtest)
library(MSBVAR)
cu<-read.xlsx("C:\\Users\\user1\\Documents\\SHFE_HISDATA.xlsx",sheet = 'cu',detectDates=TRUE,skipEmptyRows=TRUE)
cu_ur<-matrix(nrow=2,ncol=5)
for (i in c(3:7)){
cu_ur[1,i-2]=(adf.test(cu[,i]))$p.value
cu_ur[2,i-2]=(pp.test(cu[,i]))$p.value
}
x<-cu[2:1598,c(4,5)]
k<-VARselect(x,lag.max = 10, type = c("const"),
             season = NULL, exogen = NULL)
y<-VAR(x, p =k$selection[3], type = c("const"))
z<-causality(y,names(cu[5]))

e<-egcm(cu[,4],cu[,3])
plot(e)
summary(e)
is.cointegrated(e)







x<-test[,c(3:4)]

VARselect(x,lag.max = 20, type = c("const"),
          season = NULL, exogen = NULL)
y<-VAR(x, p =k$selection[1], type = c("const"))
z<-causality(y, cause =2)
vecm<-VECM(x, lag = 2, r=1,include = c("const"),
             beta = NULL, estim = c("ML"), LRinclude = c("const")
