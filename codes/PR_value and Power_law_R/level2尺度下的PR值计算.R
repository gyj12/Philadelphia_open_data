#本文件为计算费城尺度下最大的subspce中不同层级subspce的PR值
library(tidyverse)
d=0.85
n=37  #”n“为level1层级subspce的总数


#计算level1层级没有从属的subspce的PR值
PR<-c()
for (i in 1:n){
  h<-which(fc2[,2]==i)
  for(j in h){ 
    a=FALSE
    if(fc2[j,4]<fc2[j,5])
    {a=TRUE
    PR[i]=(1-d)/n}
    else
    {a=FALSE
    PR[i]=NA
    break;}
  }
}

f1<-is.na(PR)
f2<-c()
for(i in 1:37){
  if(f1[i]==FALSE)
  {f2<-c(f2,i)}
}

#统计每个level1层级的subspce被同一层级subspace所指向的总数
e<-c()
for (i in 1:n){
  m=0
  for(j in 1:250){
    if(fc2n[j,2]==i){
      if((fc2n[j,6]<fc2n[j,7])|(fc2n[j,3]>37))
      {m<-m+1}
    }
  }
  e<-c(e,m)
}

#计算level1层级的subspce的PR值
cal <- function(z){
  if(z %in% f2)
  {
    return ((1-d)/n)
  }
  else{
    b<-which(fc2[,2]==z & fc2[,4]>fc2[,5])
    if(length(b)!=0){
      aa=0
      for(k in 1:length(b))
      {
        aa<-aa+d*(cal(fc2[[b[k],3]]))/(e[fc2[[b[k],3]]])
      }
    }
  }
  return((1-d)/n+aa)
  }

prscore<-c()
for(i in 1:37)
{prscore<-c(prscore,cal(i))
  cat(i,cal(i),'\n')}

#计算level2层级每个subspce来自于其从属level1层级subspace获得的PR分数
PR_part<-c()
for(i in 38:46){
  bb=0
  for(j in 1:102){
    if(fc23[j,2]==i & fc23[j,3]<38)
    {
      bb<-bb+d*((prscore[fc23[[j,3]]])/(e[fc23[[j,3]]]))
    }
  }
  PR_part[i]<-bb
}


#统计每个level2层级的subspce被同一层级subspace所指向的总数
e1<-c()
for (i in 38:46){
  m1=0
  for(j in 1:102){
    if(fc23[j,2]==i & fc23[j,3]>37){
      if((fc23[j,6]<fc23[j,7]))
      {m1<-m1+1}
    }
  }
  e1<-c(e1,m1)
}
e1<-e1+1

#计算level2层级的subspce的PR值
cal1 <- function(z){
  g<-which(fc23[,2]==z & fc23[,3]>37 & fc23[,6]>fc23[,7])
    if(length(g)!=0){
      cc=0
      for(k in 1:length(g))
      {
        cc<-cc+d*(cal1(fc23[[g[k],3]]))/(e1[fc23[[g[k],3]]-37]) 
      }
    }
    else{cc<-0}
  return((1-d)/9+cc+PR_part[z])
}

prscore1<-c()
for(i in 38:46)
{prscore1<-c(prscore1,cal1(i))
cat(i,cal1(i),'\n')}

#输出level1的PR值 
write.table (prscore, file ="C:/Users/dell/Desktop/prscore1.xls",row.names =FALSE, col.names =FALSE)
#输出level2的PR值
write.table (prscore1, file ="C:/Users/dell/Desktop/prscore2.xls",row.names =FALSE, col.names =FALSE)