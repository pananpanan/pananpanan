#热图差异R脚本文件
#2023/06/28
#created by Pan

#setwd("E:\\panpan\\2. 聚类热图")
# #####数据读取#####
# datafile<-"E:/panpan/2. 聚类热图/data.csv"
# groups<-"E:/panpan/2. 聚类热图/group.csv"
# ##
# datafile<-"E:/panpan/2. 聚类热图/data.xlsx"
# groups<-"E:/panpan/2. 聚类热图/group.xlsx"
# ##
# datafile<-"E:/panpan/2. 聚类热图/data.txt"
# groups<-"E:/panpan/2. 聚类热图/group.txt"
# 
# ###
# datafile<-"E:/panpan/2. 聚类热图/data.xls"
# groups<-"E:/panpan/2. 聚类热图/group.xls"
# 
# #运行命令
# HeatmapDiff(datafile,groups)
############ R包载入##############################
if(!require(openxlsx)){
  install.packages("openxlsx")
  library(openxlsx)}
if(!require(readxl)){
  install.packages("readxl")
  library(readxl)}
  
########################################### 解析命令行参数#######################################
args <- commandArgs(trailingOnly = TRUE)
datafile <- args[1]
groups <- args[2]
#linux 命令
###/opt/biosoft/anaconda3/envs/R_4.3.0/bin/Rscript /opt/onems/Rshell/HeatmapDiff.R /home/panpan/HeatmapDiff/data.xlsx /home/panpan/HeatmapDiff/group.xlsx

HeatmapDiff<- function(datafile,groups){
 
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
  ####数据转换####
  
  Swtich <- function(rawdata,Sampleinfo){
    
    Realign1 <- function(df){
      df1="["
      for (i in df ){
        df1=paste(df1,'\"',i,'\",',collapse = "")
      }
      df1<-gsub(",$","]",df1)
      return(df1)
    }
    
    Realign <- function(origin_matrix){
      res = "["
      for (i in 1:nrow(origin_matrix)) {
        for (j in 1:6) {
          indata <- c(i-1,j-1,origin_matrix[i,j])
          indata <- as.character(indata)
          indata <- paste(indata,collapse = ",")
          res <- paste(res,"[",indata,"],",collapse = "")}}
      res <- paste(substr(res, 1, nchar(res)-1),"]",collapse = "")
      return(res)
    }
    
    Realign2<-function(Data){
      reps = "["
      for (i in 1:length(rownames(Data))){
        Data1 <- c(1,Data[i,"pvalue"],Data[i,"log2fc"])
        Data1 <- as.character(Data1)
        Data1 <- paste(Data1,collapse = ",")
        reps <- paste(reps,"[",Data1,"],",collapse = "") 
      }
      reps <- paste(substr(reps, 1, nchar(reps)-1),"]",collapse = "")
      return(reps)
    }
    ####空值替换####
    rawdata[is.na(rawdata)]=0
    rawdata[rawdata=="NA"]=0
    rawdata[is.null(rawdata)]=0
    ####标题####
    ###传入的x轴组别参数####
    n=length(colnames(rawdata))-2
    Xdat = colnames(rawdata)[1:n]
    xdata <- Realign1(Xdat)
    ###传入的组别参数####
    df2=unique(Sampleinfo[,2])
    xdata2<-Realign1(df2)
    ###传入的y轴组别参数####
    df3= rownames(rawdata)
    ydata<-Realign1(df3)
    ###传入的data####
    data11 <- Realign(rawdata)
    ###传入的data####
    raweqdata<-as.data.frame(t(rawdata[1:n]))
    raweqdata$group=Sampleinfo[rownames(raweqdata),2]
    count<-0
    resps="["
    for (a in raweqdata$group){
      if ( a == unique(Sampleinfo$group)[1]){
        resp=c(0,count,1)
        resp <- paste(resp,collapse = ",")
        resp <- paste("[",resp,"],",collapse = "")
      }else{
        resp=c(0,count,2)
        resp <- paste(resp,collapse = ",")
        resp <- paste("[",resp,"],",collapse = "")
      }
      count<-count+1
      resps <- paste(resps, resp,sep = "")
    }
    data22<-gsub(",$","]",resps)
    ######
    data33<-rawdata[,c("pvalue","log2fc")]
    Data3=Realign2(data33)
    
    return(c(xdata,xdata2,ydata,data11,data22,Data3))
  }

  ######数据读取#####
  data <- readFile1(datafile)
  sampleinfo <- readFile(groups)
  colnames(sampleinfo)<-c("name","group")
  rownames(sampleinfo)<-sampleinfo[,1]
  #####数据转换####
  Data<-Swtich(data,sampleinfo)
  
  result_data <- list(Xdata=Data[1],Xdata1=Data[2],Ydata=Data[3],data1=Data[4],
                      data2=Data[5],data3=Data[6])
  return(result_data)
}



result<-HeatmapDiff(datafile,groups)
cat(result,"/n")