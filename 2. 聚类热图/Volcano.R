#火山图异R脚本文件
#2023/07/05
#created by Pan


# #####代谢数据
# data <- "E:/panpan/4. 火山图/test_Volcano1.csv"
# data <- "E:/panpan/4. 火山图/test_Volcano1.txt"
# data <- "E:/panpan/4. 火山图/test_Volcano1.xls"
# data <- "E:/panpan/4. 火山图/工作簿.xlsx"
# 
# #####蛋白或基因数据
# data <- "E:/panpan/4. 火山图/test_Volcano2.csv"
# data <- "E:/panpan/4. 火山图/test_Volcano2.txt"
# data <- "E:/panpan/4. 火山图/test_Volcano2.xls"
# data <- "E:/panpan/4. 火山图/test_Volcano3.xlsx"
# # 
# log2FC <- "1"   ###数值范围正数，默认为1
# pvalue <- "0.01"   ###数值范围0-1，默认为0.05
# VIP1 <- F ####T,F,默认为F
# VIP2 <- "1"  ###数值范围正数，默认为1
# # 
# Volcano(data,log2FC,pvalue,VIP1,VIP2)
args <- commandArgs(trailingOnly = TRUE)
data <- args[1]
log2FC <- args[2]
pvalue <- args[3]
VIP1 <- args[4]
VIP2 <- args[5]
# 命令
#/opt/biosoft/anaconda3/envs/R_4.3.0/bin/Rscript /opt/onems/Rshell/Volcano.R /root/panpan/Volcano/test_Volcano1.xlsx 1 0.01 T 1

############# R包载入##############################
if(!require(openxlsx)){
  install.packages("openxlsx")
  library(openxlsx)}
if(!require(readxl)){
  install.packages("readxl")
  library(readxl) }
  

Volcano<-function(data,log2FC,pvalue,VIP1,VIP2){

  #############表格读取#############################
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
  
  ##################数据转换#############

  Swtich=function(df,Log2FC,Pvalue,VIP1,VIP2){
    
    Realign1 <- function(origin_matrix,VIP1){
      res = "["
      for (i in 1:nrow(origin_matrix)) {
        if (VIP1==T){
          indata <- c(origin_matrix[i,]["log2FC"],origin_matrix[i,]["lgp"],origin_matrix[i,]["VIP"],paste('\"',origin_matrix[i,]["name"],'\"',sep = ""),origin_matrix[i,]["five"])
        }else{
          indata <- c(origin_matrix[i,]["log2FC"],origin_matrix[i,]["lgp"],paste('\"',origin_matrix[i,]["name"],'\"',sep = ""),origin_matrix[i,]["five"])
        }
        indata <- as.character(indata)
        indata <- paste(indata,collapse = ",")
        res <- paste(res,"[",indata,"],",collapse = "")}
      res <- paste(substr(res, 1, nchar(res)-1),"]",collapse = "")
      return(res)
    }
    
    Realign2 <- function(origin_matrix,VIP1){
      res = "["
      for (i in 1:nrow(origin_matrix)) {
        if (VIP1==T){
          indata <- c(origin_matrix[i,]["log2FC"],origin_matrix[i,]["lgp"],origin_matrix[i,]["VIP"],paste('\"',origin_matrix[i,]["name"],'\"',sep = ""))
        }else{
          indata <- c(origin_matrix[i,]["log2FC"],origin_matrix[i,]["lgp"],paste('\"',origin_matrix[i,]["name"],'\"',sep = ""))
        }
        indata <- as.character(indata)
        indata <- paste(indata,collapse = ",")
        res <- paste(res,"[",indata,"],",collapse = "")}
      res <- paste(substr(res, 1, nchar(res)-1),"]",collapse = "")
      return(res)
    }
    
    if ( VIP1 == T ){
      df$UPDOWN<-ifelse(df$log2FC>= Log2FC & df$pvalue<=Pvalue & df$VIP >= VIP2,"Up",
                        ifelse(df$log2FC<=-Log2FC & df$pvalue<=Pvalue & df$VIP >= VIP2,"Down","Not sig"))
    }else{
      df$UPDOWN<-ifelse(df$log2FC>= Log2FC & df$pvalue<=Pvalue,"Up",
                        ifelse(df$log2FC<=-Log2FC & df$pvalue<=Pvalue,"Down","Not sig"))
    }
    
    df$name <- rownames(df)
    df$lgp = -log10(df$pvalue)
    
    redata1<-c('Down','Not sig','Up')
    res="["
    redata1 <- as.character(redata1)
    redata1 <- paste(paste('\"',redata1[1],'\"',sep = ""),paste('\"',redata1[2],'\"',sep = ""),paste('\"',redata1[3],'\"',sep = ""),sep = ",")
    redata <- paste("[",redata1,"]",collapse = "")
    
    #####
    downdata<-subset(df,df$UPDOWN=="Down")
    #排序
    if(nrow(downdata)>1){
      downdata<-downdata[order(downdata$lgp,decreasing=T),]
      downdata$five<-ifelse(nrow(downdata)>5,downdata$lgp[5],min(downdata$lgp))
      redowndata<- Realign1(downdata,VIP1)
    }else{
      redowndata<-"[]"
    }
    
    ####
    nondata<-subset(df,df$UPDOWN=="Not sig")
    if(nrow(nondata)>1){
      renondata<- Realign2(nondata,VIP1)
    }else{
      renondata<-"[]"
    }
    #####
    updata<-subset(df,df$UPDOWN=="Up")
    #排序
    if(nrow(updata)>0){
      updata<-updata[order(updata$lgp,decreasing=T),]
      updata$five<-ifelse(nrow(updata)>5,updata$lgp[5],min(updata$lgp))
      reupdata<-Realign1(updata,VIP1)
    }else{
      reupdata<-"[]"
    }
    fc1=Log2FC
    fc2=-Log2FC
    lgp=-log10(Pvalue)
    
    return(c(redata,redowndata,renondata,reupdata,fc1,fc2,lgp))
    #write.csv(Data,file="Volcano_test.csv",quote=F,row.names=T)
  }

  #############表格读取############################# 
  Data<-readFile1(data)
  
  ############数据分析#############################
  Log2FC=as.numeric(log2FC)
  Pvalue=as.numeric(pvalue)
  VIP1=as.logical(VIP1)
  VIP2=as.numeric(VIP2)
  Data<-Swtich(Data,Log2FC,Pvalue,VIP1,VIP2)
  
  result_data<-paste('{"name','":',Data[1],',"','data','":{','"Down','":',Data[2],',"Not sig','":',Data[3],',"Up','":',Data[4],'},"','XMax":',Data[5],',"XMin','":',Data[6],',"yMin','":',Data[7],"}",sep = "");result_data
  # result_data <- list(Redata=Data[1],Redowndata=Data[2],Renondata=Data[3],Reupdata=Data[4],
  #                     Fc1=Data[5],Fc2=Data[6],Lgp=Data[7])
  return(result_data)

} 

#df=Data
result <- Volcano(data,log2FC,pvalue,VIP1,VIP2)
cat(result, "\n")



