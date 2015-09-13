#####Handdle the char_data
##If we have run the first_10000.r, we don't run the first step
##1.Store the data with multiple/5 levels into a new data frame-Char_data
data = read.csv("train.csv",nrows=10000)
raw_data = data[,-1]
char_data = data.frame(rep(NA,nrow(raw_data)))
k=1
for (i in 1:ncol(raw_data)){
  if (is.factor(raw_data[,i])){
    if (length(levels(raw_data[,i])) > 5){
      char_data[,k]  = raw_data[,i]
      char_data=data.frame(char_data,rep(NA,nrow(raw_data)))
      names(char_data)[k]=colnames(raw_data)[i]
      k = k+1
      raw_data[,i] = NA
    }
    else{
      raw_data[,i] = as.numeric(raw_data[,i])
    }
  }
}

str(char_data)
################
##Time data
################
##2.Find out the time data and process them first
time_data = char_data[,c("VAR_0073","VAR_0075","VAR_0156","VAR_0157","VAR_0158",
                         "VAR_0159","VAR_0166","VAR_0167","VAR_0168","VAR_0169",
                         "VAR_0176","VAR_0177","VAR_0178","VAR_0179",
                         "VAR_0204","VAR_0217")]
new_time_data = data.frame(n = rep(NA,nrow(time_data)))
for (k in 1:ncol(time_data)){
  new_time_data = data.frame(new_time_data,n1=rep(NA,nrow(time_data)),
                             n2=rep(NA,nrow(time_data)), 
                             n3=rep(NA,nrow(time_data)),
                             n4=rep(NA,nrow(time_data)),
                             n5=rep(NA,nrow(time_data)),
                             n6=rep(NA,nrow(time_data))
                             )
  colnames(new_time_data)[(k-1)*6+1] = paste(colnames(time_data)[k],"_day",sep="")
  colnames(new_time_data)[(k-1)*6+2] = paste(colnames(time_data)[k],"_month",sep="")
  colnames(new_time_data)[(k-1)*6+3] = paste(colnames(time_data)[k],"_year",sep="")
  colnames(new_time_data)[(k-1)*6+4] = paste(colnames(time_data)[k],"_hour",sep="")
  colnames(new_time_data)[(k-1)*6+5] = paste(colnames(time_data)[k],"_minutes",sep="")
  colnames(new_time_data)[(k-1)*6+6] = paste(colnames(time_data)[k],"_seconds",sep="")
                              
  for (i in 1:nrow(time_data)){
    a = as.character(time_data[i,k])
    b= strsplit(a,":")[[1]][1]
    new_time_data[i,(k-1)*6+1]=paste(strsplit(b,"")[[1]][1],strsplit(b,"")[[1]][2],sep="")
    new_time_data[i,(k-1)*6+2]=paste(strsplit(b,"")[[1]][3],strsplit(b,"")[[1]][4],strsplit(b,"")[[1]][5],sep="")
    new_time_data[i,(k-1)*6+3]=paste(strsplit(b,"")[[1]][6],strsplit(b,"")[[1]][7],sep="")
    new_time_data[i,(k-1)*6+4]=strsplit(a,":")[[1]][2]
    new_time_data[i,(k-1)*6+5]=strsplit(a,":")[[1]][3]
    new_time_data[i,(k-1)*6+6]=strsplit(a,":")[[1]][4]
  }
}
new_time_data = new_time_data[,1:96]
#Delete NA colomn s
new_time_data_1 = new_time_data
####Each row and colunmn has NA value. 
###So, we should find a algorithm could work without the NA or imputate the NA.

################
##Other data
################
other_data = char_data[,c("VAR_0200","VAR_0237","VAR_0274","VAR_0283",
                          "VAR_0305","VAR_0325","VAR_0342","VAR_0404",
                          "VAR_0493")]

for (k in 1:ncol(other_data)){
    for (i in 1:nrow(other_data)){
      if(other_data[i,k]=="-1"){
          other_data[i,k] = NA
          }
    }}

#Delete NA colomn 
other_data = other_data[, colSums(is.na(other_data)) ==0]
other_data[,1] = as.factor(other_data[,1])
other_data[,2] = as.factor(other_data[,2])
other_data
################
##Conclusion
################
##For the character data, only 2 colunmns will be remained: 
#VAR_0200:    City
#VAR_0237:    State 

################
##Testing
################

