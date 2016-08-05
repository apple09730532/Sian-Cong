library(openxlsx)
library(vars)
library(urca)
library(tsDyn)
library(lmtest)
test<-read.xlsx("C:\\Users\\user1\\Documents\\testcu.xlsx",detectDates=TRUE,skipEmptyRows=TRUE)
x<-test[,c(8,12)]
z<-ca.jo(x, type = c("eigen"), ecdet = c("const"), K = 2,
                    spec=c("longrun"), season = NULL, dumvar = NULL)
y<-vec2var(z, r = 1)
l<-0
for (i in c(3:7)){ 
x<-test[,c(2,i)]
k<-VARselect(x,lag.max = 20, type = c("const"),
          season = NULL, exogen = NULL)
y<-VAR(x, p =k$selection[1], type = c("const"))
z<-causality(y, cause =2)
l[i]<-z$Granger$p.value
}
vecm<-VECM(x, lag = 2, r=1,include = c("const"),
             beta = NULL, estim = c("ML"), LRinclude = c("const"), exogen = NULL)
