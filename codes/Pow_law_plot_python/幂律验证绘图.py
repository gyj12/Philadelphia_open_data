import pandas as pd
import numpy as np
import powerlaw
import matplotlib.pyplot as plt
#读入从arcgis中导出的犯罪热点面积表格数据
data1 = pd.read_excel('C:/Users/dell/Desktop/crime_hotspots_area.xls')
data2=np.array(data1)
#从表格中提取犯罪热点面积数据并排序
ch_area=sorted(data2[:,2],reverse=True)
#利用幂率分布计算函数包进行幂率检验
fit=powerlaw.Fit(ch_area)
#计算拟合斜率
fit.power_law.alpha
#计算拟合偏差
fit.power_law.sigma
fit.power_law.xmin
#进行拟合效果图绘制
fig4 = fit.plot_ccdf(linewidth=3,label='natural city(level3)',color='b',linestyle=':',marker='o')
fit.power_law.plot_ccdf(ax=fig4, color='k', linestyle='--',label='Power law fit')
plt.ylim(10e-3,12e-1)
plt.xlim(10e-7,12e-6)
plt.text(2e-6,0.2,"α=2.831\np=0.371",color='k')
plt.legend()
plt.xlabel('X')
plt.ylabel('p (X>x)')
#检验，比较数据的分布是否更加偏向幂率分布
fit.distribution_compare('power_law', 'lognormal')
