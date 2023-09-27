
library("BiocManager")
library("ggplot2")
library("reshape2")
library("ggtree")
library("plyr")
library("ggpubr")
library("ggbreak")
library(tidyr)

####多组####
####数据准备####
data=read.delim("数据1 - 副本.txt",sep="\t",header=T,check.names=FALSE)
data[3:6]
data2=tidyr::pivot_longer(data, cols = 3:6, 
                          names_to ="Time", values_to = "value")
# 正态性检测
data3_p<-tapply(data2$value,data2$Time,t.test)
# 计算均数和标准差
data3_mean=tapply(data2$value,list(data2$class,data2$Time),mean) 
data3_sd=tapply(data2$value,list(data2$class,data2$Time),sd) 

data3 <- as.data.frame(data3_mean)
data3=tibble::rownames_to_column(data3,var="Group")
data4=pivot_longer(data3, cols = 2:5, 
                   names_to ="Time", values_to = "value")

data3_2 <- as.data.frame(data3_sd)
data3_2=tibble::rownames_to_column(data3_2,var="Group")
data4_2=pivot_longer(data3_2, cols = 2:5, 
                     names_to ="Time", values_to = "sd")
data4=merge(data4,data4_2,all = TRUE)

####画图####
#通过fill指定填充颜色，color指定描边颜色：
box=ggplot(data4,aes(x=Time,y=value,color=Group ))+
  geom_bar(width=0.6, position = position_dodge(0.9), stat="identity",fill="white",size=0.8)+ xlab("")+ylab("OCR (pMoles/min)")+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd,color=Group),position =position_dodge(width=0.9), width = 0.3,size=0.8) +
  scale_color_manual(values = unq)+
  scale_y_continuous(expand = c(0,0),breaks = seq(0,300,50)) +# 调整y轴属性，使柱子与X轴坐标接触
  scale_fill_manual(values = '#ffffff')+
  coord_cartesian(ylim = c(0,300))+
  theme(axis.text =  element_text(size=13, color="black"),
        legend.text =  element_text(size=13, color="black"),
        axis.title = element_text(size=14, color="black"),axis.text.x = element_text(angle = 45,hjust =0.5,vjust = .5)
  );box


box1=box+theme_classic()+ 
  geom_signif(data=data4,
              aes(xmin=0.75, xmax=1.25, annotations="***", y_position=270),
              textsize = 5, vjust = 0.05, tip_length = c(0.04, 0.2),
              manual=TRUE,color='black')+
  geom_signif(data=data4,
              aes(xmin=1.75, xmax=2.25, annotations="***", y_position=100),
              textsize = 5, vjust = 0.05, tip_length = c(0.04, 0.1),
              manual=TRUE,color='black') +
  geom_signif(data=data4,
              aes(xmin=2.75, xmax=3.25, annotations="***", y_position=140),
              textsize = 5, vjust = 0.05, tip_length = c(0.04, 0.1),
              manual=TRUE,color='black')+
  geom_signif(data=data4,
              aes(xmin=3.75, xmax=4.25, annotations="***", y_position=70),
              textsize = 5, vjust = 0.05, tip_length = c(0.04, 0.06),
              manual=TRUE,color='black')
ggsave(box1,file='3.png',width = 10,height = 6,dpi = 600)

library(agricolae)
library(Rmisc) 
library(ggplot2)

data("iris")  # data(package name)--加载内置数据集
model <- lm(Sepal.Width~Species,data = iris)  # 线性模型
(HSD.test(model, "Species")) # 计算显著性

data <- summarySE(data=iris, "Sepal.Width", groupvars="Species", conf.interval = 0.95) #汇总数据，计算标准差，置信度95%

g <- ggplot(data,aes(x = Species, y = Sepal.Width, ymax=4, ymin=0))  # 绘图，y轴坐标为0-4
g <- g+geom_bar(stat="identity", fill="gray50", colour = "black", width = 0.7)  #柱状图，stat代表含义默认，fill指柱状图填充颜色，colour指柱状边框颜色，width指柱的宽度
g <- g+geom_errorbar(aes(ymax=Sepal.Width+se,ymin=Sepal.Width-se), width=0.0,size=0.5,color="black") #geom_errorbar指对柱状图加误差限，ymax指平均值+误差限，ymin指平均值-误差限，width指误差上下限的宽度

g <- g+geom_text(aes(label=c("a","b","c"),hjust=0.5, vjust=-1))   #geom_text指在图中标注文本，label=c指以字母的形式进行标注，hjust指标注字母在柱状图的位置，0.5指正中，vjust指标注字母与柱状图的距离

g <- g+labs(x = "Sample location", y = "aam / height")   #labs指标签。
g <- g+ggtitle("Main title") +theme(plot.title = element_text(hjust = 0.5))


g <- g+theme_bw()  +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey80"),
        plot.title = element_text(size = rel(1.5), 
                                  face = "bold", hjust = 0.5),
        axis.title = element_text(face = "bold"),
        axis.title.y = element_text(vjust= 1.8),
        axis.title.x = element_text(vjust= -0.5),
        panel.border = element_rect(colour="black")
  )+ theme(panel.grid.major.y = element_blank())+theme(panel.grid.minor.y = element_blank()) #调整格式

g <- g+scale_y_continuous(expand = c(0,0)) #此命令使图中柱子与X轴的空隙不再出现

png('鸢尾花组间差异性柱状图.png',width=600*12,height=12*600,res=72*12) # 保存图片（空白）
print(g) # 往空白图片里保存“g”

dev.off() # 关闭绘图窗口（必须要加的一个命令）
