#本文件为计算整个费城尺度下不同层级subspce的PR值，其余尺度下计算方法相同
library(tidyverse)
d=0.85
n=547  #”n“为level1层级subspce的总数

#计算level1层级没有从属的subspce的PR值
PR<-c()
for (i in 1:n){
  h<-which(FC1[,2]==i)  #”FC1“为arcgis中导出的邻域分析表
  for(j in h){ 
      a=FALSE
      if(FC1[j,10]<FC1[j,11])
      {a=TRUE
      PR[i]=(1-d)/634}  #”634“为费城尺度下subspce的总数，可根据具体情况进行更改
      else
      {a=FALSE
      PR[i]=NA
      break;}
    }
  }

f1<-is.na(PR)
f2<-c()
for(i in 1:n){
  if(f1[i]==FALSE)
  {f2<-c(f2,i)}
}

#统计每个level1层级的subspce被同一层级subspace所指向的总数
e<-c()
for (i in 1:n){
  m<-length(which(FC12[,2]==i & (FC12[,10]<FC12[,11]|FC12[,3]>547)))
  e<-c(e,m)
}

#计算level1层级的subspce的PR值
cal <- function(z){
  if(z %in% f2)
  {
    return ((1-d)/634)
  }
  else{
    b<-which(FC1[,2]==z & FC1[,10]>FC1[,11])
    if(length(b)!=0){
      aa=0
      for(k in 1:length(b))
      {
        aa<-aa+d*(cal(FC1[[b[k],3]]))/(e[FC1[[b[k],3]]])
      }
    }
  }
  return((1-d)/634+aa)
}


prscore<-c()
for(i in 1:547)
{prscore<-c(prscore,cal(i))
cat(i,cal(i),'\n')}

#计算level2层级每个subspce来自于其从属level1层级subspace获得的PR分数
PR_part<-c()
for(i in 548:616){ #level1和level2两层subspace总数之和为616
  bb=0
  c<-which(FC12[,2]==i & FC12[,3]<548) #”FC12“为arcgis中导出的level1和level2的嵌套关系表
  for(j in c){
      bb<-bb+d*((prscore[FC12[[j,3]]])/(e[FC12[[j,3]]]))
  }
  PR_part[i]<-bb
}

#统计每个level2层级的subspce被同一层级subspace所指向的总数
e1<-c()
for (i in 548:616){
  m1<-length(which(FC12[,2]==i & FC12[,3]>547 & FC12[,10]<FC12[,11]))
  e1<-c(e1,m1)
}
e1<-e1+1  

#计算level2层级的subspce的PR值 
cal1 <- function(z){
  g<-which(FC12[,2]==z & FC12[,3]>547 & FC12[,10]>FC12[,11])
  if(length(g)!=0){
    cc=0
    for(k in 1:length(g))
    {
      cc<-cc+d*(cal1(FC12[[g[k],3]]))/(e1[FC12[[g[k],3]]-547]) 
    }
  }
  else{cc<-0}
  return((1-d)/634+cc+PR_part[z])
}


prscore1<-c()
for(i in 548:616)
{prscore1<-c(prscore1,cal1(i))
cat(i,cal1(i),'\n')}

#输出level1的PR值 
write.table (prscore, file ="C:/Users/dell/Desktop/prscore1.xls",row.names =FALSE, col.names =FALSE)
#输出level2的PR值
write.table (prscore1, file ="C:/Users/dell/Desktop/prscore2.xls",row.names =FALSE, col.names =FALSE)

#计算level3层级每个subspce来自于其从属level2层级subspace获得的PR分数
PR_part<-c()
for(i in 70:83){
  bb=0
  c<-which(FC23[,2]==i & FC23[,3]<70)  #”FC34“为arcgis中导出的level2和level3的嵌套关系表
  for(j in c){
    bb<-bb+d*((prscore1[FC23[[j,3]]])/(e1[FC23[[j,3]]]))
  }
  PR_part[i]<-bb
}

#统计每个level3层级的subspce被同一层级subspace所指向的总数
e2<-c()
for (i in 70:83){
  m1<-length(which(FC23[,2]==i & FC23[,3]>69 & FC23[,6]<FC23[,7]))
  e2<-c(e2,m1)
}
e2<-e2+1  

#计算level3层级的subspce的PR值
cal1 <- function(z){
  g<-which(FC23[,2]==z & FC23[,3]>69 & FC23[,6]>FC23[,7])
  if(length(g)!=0){
    cc=0
    for(k in 1:length(g))
    {
      cc<-cc+d*(cal1(FC23[[g[k],3]]))/(e2[FC23[[g[k],3]]-69]) 
    }
  }
  else{cc<-0}
  return((1-d)/634+cc+PR_part[z])
}

#输出level3的PR值
prscore2<-c()
for(i in 70:83)
{prscore2<-c(prscore2,cal1(i))
cat(i,cal1(i),'\n')}

write.table (prscore2, file ="C:/Users/dell/Desktop/prscore3.xls",row.names =FALSE, col.names =FALSE)

#计算level4层级每个subspce来自于其从属level3层级subspace获得的PR分数
PR_part<-c()
for(i in 15:17){
  bb=0
  c<-which(FC34[,2]==i & FC34[,3]<15) #”FC23“为arcgis中导出的level3和level4的嵌套关系表
  for(j in c){
    bb<-bb+d*((prscore2[FC34[[j,3]]])/(e2[FC34[[j,3]]]))
  }
  PR_part[i]<-bb
}

#计算level4层级的subspce的PR值
cal1 <- function(z){
  g<-which(FC23[,2]==z & FC34[,3]>14 & FC34[,6]>FC34[,7])
  if(length(g)!=0){
    cc=0
    for(k in 1:length(g))
    {
      cc<-cc+d*(cal1(FC23[[g[k],3]]))
    }
  }
  else{cc<-0}
  return((1-d)/634+cc+PR_part[z])
}

#输出level4的PR值
prscore3<-c()
for(i in 15:17)
{prscore3<-c(prscore3,cal1(i))
cat(i,cal1(i),'\n')}

write.table (prscore3, file ="C:/Users/dell/Desktop/prscore4.xls",row.names =FALSE, col.names =FALSE)
