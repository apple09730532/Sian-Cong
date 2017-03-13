library(stringr) 
library(RODBC) 
sqlConnString = "driver={SQL Server};server=10.216.8.81;database=TWMM;uid={itreport};pwd={@itreport}" 
conn <- odbcDriverConnect(sqlConnString)
x <- sqlQuery(conn, " SELECT*FROM tblTaifexTickData where pid='CDFC7'or pid='CDFD7' or pid='CDFD7'or pid='CDFC7/D7' order by txTime") 
odbcClose(conn)
x$Pid<-str_trim(x$Pid)
count=numeric(length(x$txTime))
bp=0
ap=0
tp=0
for (i in c(3:length(x$txTime))){
  if    (
    (x$Pid[i]=='CDFC7/D7')&&
    (x$Pid[i-1]!=x$Pid[i-2])&&
    (x$txTime[i-1]==x$txTime[i-2])&&
    (is.na(x$Dv[i])==TRUE)&&
    (is.na(x$Dv[i-1])==FALSE)&&
    (is.na(x$Dv[i-2])==FALSE)
  )
  {
    count[i]=i
    count[i-1]=i-1
    count[i-2]=i-2
    
    if(abs(x$Dp[i-1]-x$Dp[i-2])==ceiling(abs(x$Bp1[i])*10)/10)
    {
      bp=bp+min(c(x$Dv[i-1],x$Dv[i-2]))
    }
    else 
    {
      ap=ap+min(c(x$Dv[i-1],x$Dv[i-2]))
      
    }
    
    
    
  }
}
y<-x[count[count!=0],]
tp=max(x[x$pid=='CDFC7/D7']$Tvolume)
z<-x[x$Pid=='CDFC7/D7',]$Tvolume
z[is.na(z)]<-0
max(z) 
