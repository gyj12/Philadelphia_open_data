#本文件为计算幂率分布拟合优度p值的示例代码
library(tidyverse)
library(readxl)
#读入从arcgis中导出的犯罪热点面积表格数据
data<- read_excel("C:/Users/Administrator/Desktop/crime_hotspots_area.xls")
View(data)
ch_area<-data[,3]
w = as.numeric((unlist(ch_area)))
ch_area<- sort(ch_area,decreasing = T)
#先整体运行“plpva.r"文件中的程序，再执行下述代码
plpva(w, xmin) 
#应先利用本研究提供的幂率检验python代码计算出xmin值，再带入上述代码进行运算