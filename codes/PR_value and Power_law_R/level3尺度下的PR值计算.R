#本文件为计算研究示例中最小尺度下不同层级subspce的PR值
library(tidyverse)
d=0.85
n=9 #”9“为该尺度下subspce的总数，可根据具体情况进行更改

#计算没有从属的subspce的PR值
PR<-c()
for (i in 1:n){
 h<-which(fc3[,2]==i) #”fc3“为arcgis中导出的邻域分析表
 for(j in h){
      a=FALSE
      if(fc3[j,4]<fc3[j,5])
      {a=TRUE
      PR[i]=(1-d)/n}
      else
      {a=FALSE
      PR[i]=NA
      break;}
    }
  }

#统计每个subspce被同一层级subspace所指向的总数
e<-c()
for (i in 1:n){
  m=0
  for(j in 1:34){
    if(fc3[j,2]==i){
      if(fc3[j,4]<fc3[j,5])
      {m<-m+1}
    }
  }
  e<-c(e,m)
}

#计算每个subspce的PR值     
PR<-c()      
for (i in 1:n){
  b<-c()
  for(j in 1:34){
    if(fc3[j,2]==i){
      if(fc3[j,4]>fc3[j,5])
      {b<-c(b,j)}
    }
  }
  if(length(b)!=0){
    aa=0
  for(k in 1:length(b))
  {
    aa<-aa+d*(PR[fc3[[b[k],3]]])/(e[fc3[[b[k],2]]])
  }
    PR[i]<-(1-d)/n+aa
  }
  else{PR[i]=(1-d)/n}
}
#函数形式
cal <- function(z){
  if(z==7){
    return ((1-d)/n)
  }
  else{
      b<-c()
      for(j in 1:34){
        if(fc3[j,2]==z){
          if(fc3[j,4]>fc3[j,5])
          {b<-c(b,j)}
        }
      }
      if(length(b)!=0){
        aa=0
        for(k in 1:length(b))
        {
          aa<-aa+d*(cal(fc3[[b[k],3]]))/(e[fc3[[b[k],3]]])
        }
      }
    }
    return((1-d)/n+aa)
  }

#输出PR值
prscore<-c()
for(i in 1:9)
{prscore<-c(prscore,cal(i))
cat(i,cal(i),'\n')}

write.table (prscore, file ="C:/Users/dell/Desktop/prscore.xls",row.names =FALSE, col.names =FALSE)





