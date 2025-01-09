#' input mean_x, mean_y,mean_m, Var_x, Var_y,Var_m, correlation (x,y),correlation (x,m),correlation (m,y),datasize
#' install.packages : readxl
#' no missing value is allowed
#' Please refer to the excel(.csv) file for the data format.
#' @excemples
#' \dontrun{
#'  #simulation data
#' #case1_no confounder
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
#'y_var<-as.numeric(lapply(data_list,function(group) var(group$Y)) #Y_var
#'x_var<-as.numeric(lapply(data_list,function(group) var(group$X))) #X_var
#'m_var<-as.numeric(lapply(data_list,function(group) var(group$M)))  #M_var
#'cor_xy<-as.numeric(lapply(data_list,function(group) cor(group$X,group$Y))) #correlation (X,Y)
#'cor_xm<-as.numeric(lapply(data_list,function(group) cor(group$X,group$M))) #correlation (X,M)
#'cor_my<-as.numeric(lapply(data_list,function(group) cor(group$M,group$Y))) #correlation (M,Y)
#'summary<-as.data.frame(cbind(y_bar,x_bar,m_bar,y_var,x_var,m_var,cor_xy,cor_xm,cor_my,data_n))
#'colnames(summary)<-c("y_bar","x_bar","m_bar","y_var","x_var","m_var","cor_xy","cor_xm","cor_my","n")
#'write.csv(summary_data,"path/dataname.csv")
#'#case2_confounder
#'n=600
#'#設定expit函數
#'expit <- function(x){
#'output <- exp(x) / (1 + exp(x))
#'return(output)}
#'alpha=0.2;beta=0.2;t_dir=0.5
#'coef <-as.data.frame(matrix(rnorm(n*12,0,sqrt(1)), ncol = 12))
#'colnames(coef)<-c("e1","e2","e3","gamma1","gamma2","gamma3","theta1","theta2","theta3","phi1","phi2","phi3")
#'c1=rbinom(n,1,0.6)
#'p.c2=expit(1+0.5*c1);c2=rbinom(n,1,p.c2)
#'c3=rbinom(n,1,0.3)
#'X=coef$gamma1*c1+coef$gamma2*c2+coef$gamma3*c3+coef$e1
#'M=alpha*X+coef$theta1*c1+coef$theta2*c2+coef$theta3*c3+coef$e2
#'Y=t_dir*X+beta*M+coef$phi1*c1+coef$phi2*c2+coef$phi3*c3+coef$e3
#'data<-as.data.frame(cbind(Y,X,M,c1,c2,c3))
#'N=4
#'data_n<-rep(n/N,N)
#'group <- sample(rep(1:N, length.out = n))
#'data$Group <- group
#'group1 <- data[data$Group == 1, ]
#'group2 <- data[data$Group == 2, ]
#'group3 <- data[data$Group == 3, ]
#'group4 <- data[data$Group == 4, ]
#'data_list <- list(group1, group2, group3,group4)
#'m_re<-function(data){
#'residuals<-lm(M~c1+c2+c3,data = data)$residuals
#'return(residuals)}
#'x_re<-function(data){
#'  residuals=lm(X~c1+c2+c3,data = data)$residuals
#'  return(residuals)}
#'y_re<-function(data){
#'  residuals<-lm(Y~c1+c2+c3,data = data)$residuals
#'  return(residuals)}
#'x_bar=as.numeric(lapply(data_list,function(group) mean(x_re(group))))
#'m_bar=as.numeric(lapply(data_list,function(group) mean(m_re(group))))
#'y_bar=as.numeric(lapply(data_list,function(group) mean(y_re(group))))
#'x_se=as.numeric(lapply(data_list,function(group) sd(x_re(group))))
#'m_se=as.numeric(lapply(data_list,function(group) sd(m_re(group))))
#'y_se=as.numeric(lapply(data_list,function(group) sd(y_re(group))))
#'cor_xy<-as.numeric(lapply(data_list,function(group) cor(x_re(group),y_re(group))))
#'cor_xm<-as.numeric(lapply(data_list,function(group) cor(x_re(group),m_re(group))))
#'cor_my<-as.numeric(lapply(data_list,function(group) cor(m_re(group),y_re(group))))
#'summary<-as.data.frame(cbind(y_bar,x_bar,m_bar,y_se,x_se,m_se,cor_xy,cor_xm,cor_my,data_n))
#'colnames(summary)<-c("y_bar","x_bar","m_bar","y_se","x_se","m_se","cor_xy","cor_xm","cor_my","n")
#'write.csv(summary,"path/dataname.csv")
#'}

# install.packages : readxl
# Please note that changing your data file location

data=read.csv("path/dataname.csv",header = T,sep = ",")
calculate_statistics<-function(data){
  #global mean
  global_mean_fun<-function(bar,n){
    mean=sum(bar*n)/sum(n)
    return(mean)
  }
  global_mean_x=global_mean_fun(bar=data$x_bar,n=data$n)
  global_mean_y=global_mean_fun(bar=data$y_bar,n=data$n)
  global_mean_m=global_mean_fun(bar=data$m_bar,n=data$n)
  
  
  #global sxx,syy,smm
  S_fun<-function(n,bar,var,global_mean){
    Si=sum((n-1)*var)+sum(n*(bar-global_mean)^2)
    #S=sum(Si)
    return(Si)
  }
  sxx=S_fun(n=data$n,bar=data$x_bar,var=data$x_var,global_mean=global_mean_x)
  syy=S_fun(n=data$n,bar=data$y_bar,var=data$y_var,global_mean=global_mean_y)
  smm=S_fun(n=data$n,bar=data$m_bar,var=data$m_var,global_mean=global_mean_m)
  
  #global sxy,sxm,smy
  Sxy_fun<-function(cor,n,var1,var2,bar1,bar2,global_mean1,global_mean2){
    sxy=sum(cor*(n-1)*sqrt(var1)*sqrt(var2))+sum(n*(bar1-global_mean1)*(bar2-global_mean2))
    return(sxy)
  }
  sxy=Sxy_fun(cor=data$cor_xy,n=data$n,var1=data$x_var,var2=data$y_var,bar1=data$x_bar,bar2=data$y_bar,global_mean1 = global_mean_x,global_mean2 = global_mean_y)
  sxm=Sxy_fun(cor=data$cor_xm,n=data$n,var1=data$x_var,var2=data$m_var,bar1=data$x_bar,bar2=data$m_bar,global_mean1 = global_mean_x,global_mean2 = global_mean_m)
  smy=Sxy_fun(cor=data$cor_my,n=data$n,var1=data$m_var,var2=data$y_var,bar1=data$m_bar,bar2=data$y_bar,global_mean1 = global_mean_m,global_mean2 = global_mean_y)
  
  #global cor
  rxy=sxy/(sqrt(sxx)*sqrt(syy))
  rxm= sxm/(sqrt(sxx)*sqrt(smm))
  rmy= smy/(sqrt(smm)*sqrt(syy))
  #計算alpha_hat
  alpha_hat=sxm/sxx
  t_hat=sxy/sxx
  delta=sxx*smm-sxm^2
  beta_hat=(sxx*smy-sxm*sxy)/delta
  t_dir_hat=(smm*sxy-sxm*smy)/delta
  var_mx=smm*(1-rxm^2)/(sum(data$n)-2)
  se_a_hat=sqrt(var_mx/sxx)
  var_yxm=(syy-beta_hat*smy-t_dir_hat*sxy)/(sum(data$n)-3)
  se_b_hat=sqrt(var_yxm/(var_mx*(sum(data$n)-2)))
  ab_hat=alpha_hat*beta_hat
  ab_hat_se=sqrt(beta_hat^2*se_a_hat^2+alpha_hat^2*se_b_hat^2)
  ab_hat_se2=sqrt(beta_hat^2*se_a_hat^2+alpha_hat^2*se_b_hat^2+se_a_hat^2*se_b_hat^2)
  cat(paste0("alpha_hat: ",alpha_hat),paste0("beta_hat: ",beta_hat),paste0("t'_hat: ",t_dir_hat),
      paste0("t_hat: ",t_hat),paste0("se(a_hat): ",se_a_hat),paste0("se(b_hat): ",se_b_hat),paste0("ab_hat: ",ab_hat),
      paste0("se(ab_hat)(Sobel's formula): ",ab_hat_se),paste0("se(ab_hat)(Aroian's formula): ",ab_hat_se),sep ="\n")
  
}
calculate_statistics(data)
