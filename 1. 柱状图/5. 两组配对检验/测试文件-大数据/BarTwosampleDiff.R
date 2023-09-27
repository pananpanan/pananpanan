#柱状图差异R脚本文件
#2023/06/27
#created by Pan

#setwd("E:\\panpan\\1. 柱状图")
#library("openxlsx")
#library(plyr)
#需要传入的参数
# datafile<-"E:/panpan/1. 柱状图/5. 两组配对检验/测试文件-大数据/data.xlsx"
# sampleinfosfile<-"E:/panpan/1. 柱状图/5. 两组配对检验/测试文件-大数据/ttest3.eg.xlsx"
# groupsfile<-"E:/panpan/1. 柱状图/5. 两组配对检验/测试文件-大数据/ttest2.eg.xlsx"
#####
#datafile<-"E:/panpan/1. 柱状图/5. 两组配对检验/data/ttest1.eg.txt"
#sampleinfosfile<-"E:/panpan/1. 柱状图/5. 两组配对检验/data/ttest3.eg.txt"
#groupsfile<-"E:/panpan/1. 柱状图/5. 两组配对检验/data/ttest2.eg.txt"
#####
#datafile<-"E:/panpan/1. 柱状图/5. 两组配对检验/data/ttest1.eg.csv"
#sampleinfosfile<-"E:/panpan/1. 柱状图/5. 两组配对检验/data/ttest3.eg.csv"
#groupsfile<-"E:/panpan/1. 柱状图/5. 两组配对检验/data/ttest2.eg.csv"
#####
#datafile<-"E:/panpan/1. 柱状图/5. 两组配对检验/data/ttest1.eg.xls"
#sampleinfosfile<-"E:/panpan/1. 柱状图/5. 两组配对检验/data/ttest3.eg.xls"
#groupsfile<-"E:/panpan/1. 柱状图/5. 两组配对检验/data/ttest2.eg.xls"


# ####修改成字符串#####
# method="wilcox_test"    ### t_test,wilcox_test,默认为t_test
# Method="pvalue"    #### pvalue,qvalue,默认为pvalue
# log2fc="1"
# pvalue="0.05"
# paired="T"      #####是否进行配对检验,T,F,默认为F
# ####命令行#####
# BarDiff(datafile,sampleinfosfile,groupsfile,method,Method,log2fc,pvalue,paired)

args <- commandArgs(trailingOnly = TRUE)
datafile <- args[1]
sampleinfosfile <- args[2]
groupsfile <- args[3]
method <- args[4]
Method <- args[5]
log2fc <- args[6]
pvalue <- args[7]
paired <- args[8]
#####命令行#####
#/opt/biosoft/anaconda3/envs/R_4.3.0/bin/Rscript /opt/onems/Rshell/PCA.R /root/panpan/barplot/ttest1.eg.xls  /root/panpan/barplot/ttest3.eg.xls /root/panpan/barplot/ttest2.eg.xls "t_test" "pvalue" 1 0.05 T
  
############# R包载入##############################
if(!require(openxlsx)){
  install.packages("openxlsx")
  library(openxlsx)
}
if(!require(readxl)){
  install.packages("readxl")
  library(readxl)
}
  

BarDiff <- function(datafile,sampleinfosfile,groupsfile,method,Method,log2fc,pvalue,paired){

  #############表格读取#############################
  readFile<- function(data_file){
    Read_file <- substr(data_file,nchar(data_file)-3,nchar(data_file))
    if (Read_file== ".csv"){
      matrix <- read.csv(data_file,header = F)
    }else if (Read_file == ".txt"){
      matrix <- read.table(data_file,sep = "\t",header = F)
    }else if (Read_file== "xlsx"){
      matrix <- read.xlsx(data_file,sheet = 1,sep="\t",rowNames=F,colNames = F)
     }else if(Read_file ==  ".xls" ) {
       matrix <- as.data.frame(read_excel(data_file,col_names = F) )
     }
  return(matrix)
  }
  
  readFile1<- function(data_file){
    Read_file <- substr(data_file,nchar(data_file)-3,nchar(data_file))
    if (Read_file== ".csv"){
      matrix <- read.csv(data_file,header = TRUE,row.names = 1,check.names=FALSE)
    }else if (Read_file == ".txt"){
      matrix <- read.table(data_file,sep = "\t",header = TRUE,row.names = 1,check.names=FALSE)
    }else if (Read_file== "xlsx"){
      matrix <- read.xlsx(data_file,sheet = 1,sep="\t",rowNames=T,colNames = T)
    }else if(Read_file ==  ".xls" ) {
      matrix <- as.data.frame(read_excel(data_file) )
      rownames(matrix) <- matrix[,1]
      matrix <- matrix[,-1] 
    }
    return(matrix)
  }
  
  ########数据准备########
  Data_Prepration <- function(data,sampleinfos,groups,method,Method,log2fc,pvalue,paire){
   
     Realign <- function(origin_matrix){
      res = "["
      df = c(paste('\"',colnames(origin_matrix)[1],'\"',sep = ""),paste('\"',colnames(origin_matrix)[2],'\"',sep = ""),paste('\"',colnames(origin_matrix)[3],'\"',sep = ""))
      df <- as.character(df)
      df <- paste(df,collapse = ",")
      res <- paste(res,"[",df,"],",collapse = "")
      for (i in 1:nrow(origin_matrix)) {
        indata <- c(paste('\"',origin_matrix[i,1],'\"',sep = ""),origin_matrix[i,2],origin_matrix[i,3])
        indata <- as.character(indata)
        indata <- paste(indata,collapse = ",")
        res <- paste(res,"[",indata,"],",collapse = "")}
      res <- paste(substr(res, 1, nchar(res)-1),"]",collapse = "")
      return(res)
  }
  #填充0值
  data[data==0]=min(data[data!=0])*0.5
  diffall=data.frame()
  
  for (j in 1:length(rownames(groups)) ){
    group=as.vector(groups[j,])
    sampleindex<-lapply(group,function(x) {which(sampleinfos[,2] %in% x)})
    data1=data[,sampleinfos[unlist(sampleindex),1]]
    n=length(sampleindex[[1]])
    m=length(sampleindex[[2]])
    if(method=="t_test"){
      if(paire==T){
        data1$sdvalue<-apply(data1,1,function(x) sd(x[1:n]-x[(n+1):(m+n)]))
        data2<-subset(data1,data1$sdvalue!=0)
        data2$pvalue<-apply(data2,1,function(x) t.test(x[1:n],x[(n+1):(m+n)],paired=T)$p.value)
        }else{
          data1$sdvalue<-apply(data1,1,function(x) sd(x[1:n])+sd(x[(n+1):(m+n)]))
          data2<-subset(data1,data1$sdvalue!=0)
          data2$pvalue<-apply(data2,1,function(x) t.test(x[1:n],x[(n+1):(m+n)],paired=F)$p.value)
          }
    }else{
      if (paire==T){
        data2=data1
        data2$pvalue<-apply(data2,1,function(x) wilcox.test(x[1:n],x[(n+1):(m+n)],paired=T)$p.value)
      }else{
        data2=data1
        data2$pvalue<-apply(data2,1,function(x) wilcox.test(x[1:n],x[(n+1):(m+n)],paired=F)$p.value)
      }
    }
    data2[,"qvalue"]=p.adjust(data2[,"pvalue"], "BH" )
    data2$log2FC= apply(data2,1,function(x) log2(mean(as.numeric(x[1:n]))/mean(as.numeric(x[(n+1):(m+n)]))))
    
    #若后期想用其他算法，可替换
    #data2[,"qvalue_BH"]=p.adjust(data2[,"pvalue"], "BH")
    #data2[,"qvalue_fdr"]=p.adjust(data2[,"pvalue"], "fdr")
    #data2[,"qvalue_BY"]=p.adjust(data2[,"pvalue"], "BY")
    if (Method=="pvalue"){
      data2$UPDOWN<-ifelse(data2$log2FC>=log2fc & data2$pvalue<=pvalue,"Up",
                           ifelse(data2$log2FC<=-log2fc & data2$pvalue<=pvalue,"Down","Not sig"))
    }else{
      data2$UPDOWN<-ifelse(data2$log2FC>=log2fc & data2$qvalue<=pvalue,"Up",
                           ifelse(data2$log2FC<=-log2fc & data2$qvalue<=pvalue,"Down","Not sig"))
    }
    
    # write.table(data2, file=paste(group[1],"-vs-",group[2],".",method,".fillter.txt",sep = ""),sep="\t",quote=FALSE,row.names = T,col.names=NA)
    # write.csv(data2,file=paste(group[1],"-vs-",group[2],".",method,".fillter.CSV",sep = ""),quote=F,row.names=T)
    # 
    diff=data.frame(paste(group[1],"-vs-",group[2],".",method,".fillter",sep = ""),sum(data2$UPDOWN=="Up"),sum(data2$UPDOWN=="Down"))
    diffall=rbind(diffall,diff)
    
  }
  
  colnames(diffall) <- c("group",'UP', 'DOWN')
  #write.table(diffall, file="Ttest.fillter.diff.txt",sep="\t",quote=FALSE,col.names=T,row.names=F)
  
  Realigns=Realign(diffall)
  #write.table(Realigns, file="Realigns.txt",sep="\t",quote=FALSE,col.names=F,row.names=F)
  return(Realigns)
  }
  
  #############表格读取############################# 
  Data<-readFile1(datafile)
  Sampleinfos<-readFile(sampleinfosfile)
  Groups<-readFile(groupsfile)
  
  ############数据分析#######################
  log2fc<-as.numeric(log2fc)
  pvalue<-as.numeric(pvalue)
  method1<-method
  Method2<-Method
  paired<-as.logical(paired)
  Data_Pr<-Data_Prepration(Data,Sampleinfos,Groups,method1,Method2,log2fc,pvalue,paired)
  return(Data_Pr)
}

# (Data,Sampleinfos,Groups,method1,Method2,log2fc,pvalue,paired)
# (data,sampleinfos,groups,method,Method,log2fc,pvalue,paire)
# 
# data=Data
# sampleinfos=Sampleinfos
# groups=Groups
# method=method1
# Method=Method2
# paire=paired

result <- BarDiff(datafile,sampleinfosfile,groupsfile,method,Method,log2fc,pvalue,paired)
cat(result, "\n")




