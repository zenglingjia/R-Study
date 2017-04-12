a<-c(7,9)  #发病数
b<-c(347,347) #人口数
low<-rep(NA,length(a))
up<-rep(NA,length(a))
for (i in c(1:length(a))){
      end<-poisson.test(a[i],b[i])
      low[i]<-end[[4]][1]
      up[i]<-end[[4]][2] 
      }

result<-as.data.frame(cbind(a,b,low,up))
result
