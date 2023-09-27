#柱状图差异R脚本文件
#2023/06/27
#created by Pan

#setwd("E:\\panpan\\1. 柱状图")
#library("openxlsx")
#library(plyr)
#需要传入的参数
#datafile<-"E:/panpan/1. 柱状图/ttest1-1.eg.xlsx"
#sampleinfosfile<-"E:/panpan/1. 柱状图/data/ttest3.eg.xlsx"
#groupsfile<-"E:/panpan/1. 柱状图/data/ttest2.eg.xlsx"
#####
#datafile<-"E:/panpan/1. 柱状图/data/ttest1.eg.txt"
#sampleinfosfile<-"E:/panpan/1. 柱状图/data/ttest3.eg.txt"
#groupsfile<-"E:/panpan/1. 柱状图/data/ttest2.eg.txt"
#####
#datafile<-"E:/panpan/1. 柱状图/data/ttest1.eg.csv"
#sampleinfosfile<-"E:/panpan/1. 柱状图/data/ttest3.eg.csv"
#groupsfile<-"E:/panpan/1. 柱状图/data/ttest2.eg.csv"


####修改成字符串#####
#method="t_test"    ### t_test,wilcox_test,默认为t_test
#Method="pvalue"    #### pvalue,qvalue,默认为pvalue
#log2fc="1"
#pvalue="0.05"
#####命令行#####
#BarDiff(datafile,sampleinfosfile,groupsfile,method,Method,log2fc,pvalue)



BarDiff <- function(datafile,sampleinfosfile,groupsfile,method,Method,log2fc,pvalue){
  
  ############# R包载入##############################
  if(!require(openxlsx)){
    install.packages("openxlsx")
    library(openxlsx)
  }
  
  #############表格读取#############################
  readFile<- function(data_file){
    Read_file <- substr(data_file,nchar(data_file)-3,nchar(data_file))
    if (Read_file== ".csv"){
      matrix <- read.csv(data_file,header = F)
    }else if (Read_file == ".txt"){
      matrix <- read.table(data_file,sep = "\t",header = F)
    }else if (Read_file== "xlsx"){
      matrix <- read.xlsx(data_file,sheet = 1,sep="\t",rowNames=F,colNames = F)
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
    }
    return(matrix)
  }
  
  ########数据准备########
  Data_Prepration <- function(data,sampleinfos,groups,method,Method,log2fc,pvalue){
   
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
    #数据筛选
    ############删去归一化后数值都一样和单组不同数值比例小于50%的物质########
    testN<-data1
    sdExclude<-NULL
    for(i in rownames(testN)){
      ratio_all<-NULL
      for(sdCls in 1:length(sampleindex)){
        colPick<-as.character(sampleinfos[sampleindex[[sdCls]],1])
        ratio<-length(testN[i,][colPick][testN[i,][colPick]!=min(testN)])/length(testN[i,][colPick])
        ratio_all<-c(ratio_all,ratio)
      }	
      if(all(ratio_all<0.5)){
        sdExclude=c(sdExclude,i)
      }
    }
    
    final=row.names(testN)[!row.names(testN) %in% sdExclude]
    data2=data1[final,]
    
    z<-data2
    z1 <- apply(z, 1, function(x) {
      length(unique(x))
    })
    
    sdExclude<-NULL
    for(i in rownames(z)){
      sd_all<-NULL
      for(sdCls in 1:length(sampleindex)){
        colPick<-as.character(sampleinfos[sampleindex[[sdCls]],1])
        sd1<-sd(z[i,colPick])
        sd_all<-c(sd1,sd_all)
      }
      if(all(sd_all<=0)){
        sdExclude=c(sdExclude,i)
      }
    }
    final <- row.names(z)[!row.names(z) %in% sdExclude]
    data2 <- z[z1 != 1,]
    data2=data2[final,]
    
    for (i in rownames(data2)){
      if(method=="t_test"){
        p= t.test(data2[i,sampleinfos[sampleindex[[1]],1]],data2[i,sampleinfos[sampleindex[[2]],1]]) 
      }else{
        p= wilcox.test(as.numeric(data2[i,sampleinfos[sampleindex[[1]],1]]),as.numeric(data2[i,sampleinfos[sampleindex[[2]],1]]))
      }
      data2[i,"pvalue"]=p$p.value
      data2[i,"log2FC"]= log2(mean(as.numeric(data2[i,colnames(data)[unlist(sampleindex[1])]]))/mean(as.numeric(data2[i,colnames(data)[unlist(sampleindex[2])]])))
      
    }
    
    data2[,"qvalue"]=p.adjust(data2[,"pvalue"], "BH")
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
    
    write.table(data2, file=paste(group[1],"-vs-",group[2],".",method,".fillter.txt",sep = ""),sep="\t",quote=FALSE,row.names = T,col.names=NA)
    write.csv(data2,file=paste(group[1],"-vs-",group[2],".",method,".fillter.CSV",sep = ""),quote=F,row.names=T)
    
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
  Method1<-Method
  Data_Pr<-Data_Prepration(Data,Sampleinfos,Groups,method,Method,log2fc,pvalue)
  return(Data_Pr)
}
