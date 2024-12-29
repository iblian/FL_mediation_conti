#' input mean_x, mean_y, Var_x, Var_y, correlation (x,y)
#' install.packages : readxl
#' no missing value is allowed
#' Please refer to the excel(.csv) file for the data format.
#' @excemples
#' \dontrun{
#'  #simulation data

#'  }
#'n=2000;e1=rnorm(n,0,sqrt(1));e2=rnorm(n,0,sqrt(1));e3=rnorm(n,0,sqrt(1))
#'alpha=0.2;beta=0.2;t_dir=0.5
#'X=e1
#'M=alpha*X+e2
#'Y=t_dir*X+beta*M+e3
#'data<-as.data.frame(cbind(Y,X,M))
#'N=4
#'data_n<-rep(n/N,N)
#'group <- sample(rep(1:N, length.out = n))
#'data$Group <- group
#'group1 <- data[data$Group == 1, ]
#'group2 <- data[data$Group == 2, ]
#'group3 <- data[data$Group == 3, ]
#'group4 <- data[data$Group == 4, ]
#'data_list <- list(group1, group2, group3,group4)
#'y_bar<-as.numeric(lapply(data_list,function(group) mean(group$Y))) #Y_mean
#'x_bar<-as.numeric(lapply(data_list,function(group) mean(group$X))) #X_mean
#'m_bar<-as.numeric(lapply(data_list,function(group) mean(group$M))) #M_mean
#'y_se<-as.numeric(lapply(data_list,function(group) sd(group$Y)))    #Y_sd
#'x_se<-as.numeric(lapply(data_list,function(group) sd(group$X)))    #X_sd
#'m_se<-as.numeric(lapply(data_list,function(group) sd(group$M)))    #M_sd
#'cor_xy<-as.numeric(lapply(data_list,function(group) cor(group$X,group$Y))) #correlation (X,Y)
#'cor_xm<-as.numeric(lapply(data_list,function(group) cor(group$X,group$M))) #correlation (X,M)
#'cor_my<-as.numeric(lapply(data_list,function(group) cor(group$M,group$Y))) #correlation (M,Y)
#'summary_data<-as.data.frame(cbind(y_bar,x_bar,m_bar,y_se,x_se,m_se,cor_xy,cor_xm,cor_my,data_n))
#'colnames(summary_data)<-c("y_bar","x_bar","m_bar","y_se","x_se","m_se","cor_xy","cor_xm","cor_my","n")
#'write.csv(summary_data,"path/dataname.csv")

# install.packages : readxl
# Please note that changing your data file location

data=read.csv("path/dataname.csv",header = T,sep = ",")
calculate_statistics<-function(summarydata){
  #global mean
  mean_fun<-function(bar,n){
    mean=sum(bar*n)/sum(n)
    return(mean)
  }
  x_mean=mean_fun(summarydata$x_bar,summarydata$n)
  y_mean=mean_fun(summarydata$y_bar,summarydata$n)
  m_mean=mean_fun(summarydata$m_bar,summarydata$n)
  
  
  #global sxx,syy,smm
  S_fun<-function(n,bar,se,mean){
    Si=(n-1)*(se^2)+n*(bar-mean)^2
    S=sum(Si)
    return(S)
  }
  sxx=S_fun(summarydata$n,summarydata$x_bar,summarydata$x_se,x_mean)
  syy=S_fun(summarydata$n,summarydata$y_bar,summarydata$y_se,y_mean)
  smm=S_fun(summarydata$n,summarydata$m_bar,summarydata$m_se,m_mean)
  
  #global sxy,sxm,smy
  Sxy_fun<-function(cor,n,se1,se2,bar1,bar2,mean1,mean2){
    sxyi=cor*(n-1)*se1*se2+n*(bar1-mean1)*(bar2-mean2)
    sxy=sum(sxyi)
    return(sxy)
  }
  sxy=Sxy_fun(summarydata$cor_xy,summarydata$n,summarydata$x_se,summarydata$y_se,summarydata$x_bar,summarydata$y_bar,mean1 = x_mean,mean2 = y_mean)
  sxm=Sxy_fun(summarydata$cor_xm,summarydata$n,summarydata$x_se,summarydata$m_se,summarydata$x_bar,summarydata$m_bar,mean1 = x_mean,mean2 = m_mean)
  smy=Sxy_fun(summarydata$cor_my,summarydata$n,summarydata$m_se,summarydata$y_se,summarydata$m_bar,summarydata$y_bar,mean1 = m_mean,mean2 = y_mean)
  
  #global cor
  rxy=sxy/(sqrt(sxx)*sqrt(syy))
  rxm= sxm/(sqrt(sxx)*sqrt(smm))
  rmy= smy/(sqrt(smm)*sqrt(syy))
  #calculate
  alpha_hat=sxm/sxx
  t_hat=sxy/sxx
  delta=sxx*smm-sxm^2
  beta_hat=(sxx*smy-sxm*sxy)/delta
  t_dir_hat=(smm*sxy-sxm*smy)/delta
  var_mx=smm*(1-rxm^2)/(sum(summarydata$n)-2)
  se_a_hat=sqrt(var_mx/sxx)
  var_yxm=(syy-beta_hat*smy-t_dir_hat*sxy)/(sum(summarydata$n)-3)
  se_b_hat=sqrt(var_yxm/(var_mx*(sum(summarydata$n)-2)))
  cat(paste0("alpha_hat: ",alpha_hat),paste0("beta_hat: ",beta_hat),paste0("t_dir_hat: ",t_dir_hat),
      paste0("t_hat: ",t_hat),paste0("se_a_hat: ",se_a_hat),paste0("se_b_hat: ",se_b_hat),sep ="\n")
  
}
calculate_statistics(data)