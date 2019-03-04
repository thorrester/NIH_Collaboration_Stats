#BCEA by raw data cost
norm_2 <- read.csv("C:/Users/Test/Box Sync/Emory Box/R/New folder/norm_2.csv")
norm_2<-norm_2[-c(1)]
colnames(norm_2)[1:2]<-c("type","cost")
ec<-as.data.frame(select(norm_2, type, e, cost))
#single filter and sample
single_filter<-filter(ec,type=="single")
single_sample<-sample_n(single_filter,10000,replace = TRUE)
#multi filter and sample
multi_filter<-filter(ec,type=="multi")
multi_sample<-sample_n(multi_filter,10000,replace = TRUE)
e<-as.matrix(cbind(single_sample$e,multi_sample$e))
c<-as.matrix(cbind(single_sample$cost,multi_sample$cost))
colnames(c)<- c("single","multi")
colnames(e)<- c("single","multi")
m<-bcea(e,c,ref = 2, interventions = c("single","multi"), Kmax = 400000)
plot(m)


#BCEA by raw data cost with 25% weight to # of PIs
norm_2 <- read.csv("norm_2.csv")
norm_2<-norm_2[-c(1)]
colnames(norm_2)[1:2]<-c("type","cost")
epi<-((norm_2$Average.of.Pis*1.3) + norm_2$pubsperproj + norm_2$rcrperproj + norm_2$Average.of.authors + norm_2$ptperproj + norm_2$ctperproj)
norm_2<-cbind(norm_2,epi)
ec<-as.data.frame(select(norm_2, type, epi, cost))
#single filter and sample
single_filter<-filter(ec,type=="single")
single_sample<-sample_n(single_filter,10000,replace = TRUE)
#multi filter and sample
multi_filter<-filter(ec,type=="multi")
multi_sample<-sample_n(multi_filter,10000,replace = TRUE)
e<-as.matrix(cbind(single_sample$e,multi_sample$e))
c<-as.matrix(cbind(single_sample$cost,multi_sample$cost))
colnames(c)<- c("single","multi")
colnames(e)<- c("single","multi")
m<-bcea(e,c,ref = 2, interventions = c("single","multi"))
plot(m)




#Regular cost Sampling
set.seed(1)
norm_2 <- read.csv("C:/Users/Test/Box Sync/Emory Box/R/New folder/norm_2.csv")
norm_2<-norm_2[-c(1)]
colnames(norm_2)[1:2]<-c("type","cost")
ec<-as.data.frame(select(norm_2, type, e, cost))
#single filter and sample
single_filter<-filter(ec,type=="single")
singlee_sample<-replicate(10000,mean(sample(single_filter$e,1000,replace = TRUE)))
singlec_sample<-replicate(10000,mean(sample(single_filter$cost,1000,replace = TRUE)))
#multi filter and sample
multi_filter<-filter(ec,type=="multi")
multie_sample<-replicate(10000,mean(sample(multi_filter$e,1000,replace = TRUE)))
multic_sample<-replicate(10000,mean(sample(multi_filter$cost,1000,replace = TRUE)))
e<-as.matrix(cbind(singlee_sample,multie_sample))
c<-as.matrix(cbind(singlec_sample,multic_sample))
colnames(c)<- c("single","multi")
colnames(e)<- c("single","multi")
m<-bcea(e,c,ref = 2, interventions = c("single","multi"), Kmax = 400000)
plot(m)
plot1<-contour2(m,graph = c("ggplot2"), wtp = 400000)
plot2<-eib.plot(m,graph = c("ggplot2"))
plot3<-ceac.plot(m, graph=c("ggplot2"))
plot4<-evi.plot(m, graph = c("ggplot2"))
g <- arrangeGrob(plot1, plot2, plot3, plot4, ncol=2)
ggsave(file="C:/Users/Test/Box Sync/Emory Box/R/New folder/increase to pi/bcea.pdf", g)
rm(list = ls())


#sampling with 25% more wieght to num of pis funded
set.seed(1)
norm_2 <- read.csv("C:/Users/Test/Box Sync/Emory Box/R/New folder/norm_2.csv")
norm_2<-norm_2[-c(1)]
colnames(norm_2)[1:2]<-c("type","cost")
epi<-((norm_2$Average.of.Pis*1.7) + norm_2$pubsperproj + norm_2$rcrperproj + norm_2$Average.of.authors + norm_2$ptperproj + norm_2$ctperproj)
norm_2<-cbind(norm_2,epi)
ec<-as.data.frame(select(norm_2, type, epi, cost))
#single filter and sample
single_filter<-filter(ec,type=="single")
singlee_sample<-replicate(10000,mean(sample(single_filter$epi,1000,replace = TRUE)))
singlec_sample<-replicate(10000,mean(sample(single_filter$cost,1000,replace = TRUE)))
#multi filter and sample
multi_filter<-filter(ec,type=="multi")
multie_sample<-replicate(10000,mean(sample(multi_filter$epi,1000,replace = TRUE)))
multic_sample<-replicate(10000,mean(sample(multi_filter$cost,1000,replace = TRUE)))
e<-as.matrix(cbind(singlee_sample,multie_sample))
c<-as.matrix(cbind(singlec_sample,multic_sample))
colnames(c)<- c("single","multi")
colnames(e)<- c("single","multi")
m<-bcea(e,c,ref = 2, interventions = c("single","multi"), Kmax = 400000)
plot1<-contour2(m,graph = c("ggplot2"), wtp = 400000)
plot2<-eib.plot(m,graph = c("ggplot2"))
plot3<-ceac.plot(m, graph=c("ggplot2"))
plot4<-evi.plot(m, graph = c("ggplot2"))
g <- arrangeGrob(plot1, plot2, plot3, plot4, ncol=2)
ggsave(file="C:/Users/Test/Box Sync/Emory Box/R/New folder/increase to pi/bcea_pi_weight.pdf", g)
rm(list = ls())

#sampling 1 vs 2 pis
set.seed(1)
norm_2 <- read.csv("C:/Users/Test/Box Sync/Emory Box/R/New folder/norm_2.csv")
norm_2<-norm_2[-c(1)]
colnames(norm_2)[1:2]<-c("type","cost")
mastercsv<-read.csv("C:/Users/Test/Box Sync/Emory Box/R/New folder/mastercsv.csv")
mastercsv<-mastercsv[-c(1)]
norm_2<-cbind(norm_2,mastercsv$Average.of.Pis)
colnames(norm_2)[11]<-c("pi_num")
ec<-as.data.frame(select(norm_2, type, e, cost,pi_num))
#single filter and sample
single_filter<-filter(ec,pi_num==1)
singlee_sample<-replicate(10000,mean(sample(single_filter$e,1000,replace = TRUE)))
singlec_sample<-replicate(10000,mean(sample(single_filter$cost,1000,replace = TRUE)))
#multi filter and sample
multi_filter<-filter(ec,pi_num >= 2, pi_num <3)
multie_sample<-replicate(10000,mean(sample(multi_filter$e,1000,replace = TRUE)))
multic_sample<-replicate(10000,mean(sample(multi_filter$cost,1000,replace = TRUE)))
e<-as.matrix(cbind(singlee_sample,multie_sample))
c<-as.matrix(cbind(singlec_sample,multic_sample))
colnames(c)<- c("single","multi")
colnames(e)<- c("single","multi")
m<-bcea(e,c,ref = 2, interventions = c("single","multi"), Kmax = 400000)
plot1<-contour2(m,graph = c("ggplot2"), wtp = 400000)
plot2<-eib.plot(m,graph = c("ggplot2"))
plot3<-ceac.plot(m, graph=c("ggplot2"))
plot4<-evi.plot(m, graph = c("ggplot2"))
g <- arrangeGrob(plot1, plot2, plot3, plot4, ncol=2)
ggsave(file="C:/Users/Test/Box Sync/Emory Box/R/New folder/increase to pi/bcea_1vs2.pdf", g)
rm(list = ls())


#Regular cost Sampling without # of PIs
set.seed(1)
norm_2 <- read.csv("C:/Users/Test/Box Sync/Emory Box/R/New folder/norm_2.csv")
norm_2<-norm_2[-c(1)]
colnames(norm_2)[1:2]<-c("type","cost")
norm_2$e = norm_2$pubsperproj + norm_2$rcrperproj + norm_2$Average.of.authors + norm_2$ptperproj +norm_2$ctperproj
ec<-as.data.frame(select(norm_2, type, e, cost))
#single filter and sample
single_filter<-filter(ec,type=="single")
singlee_sample<-replicate(10000,mean(sample(single_filter$e,1000,replace = TRUE)))
singlec_sample<-replicate(10000,mean(sample(single_filter$cost,1000,replace = TRUE)))
#multi filter and sample
multi_filter<-filter(ec,type=="multi")
multie_sample<-replicate(10000,mean(sample(multi_filter$e,1000,replace = TRUE)))
multic_sample<-replicate(10000,mean(sample(multi_filter$cost,1000,replace = TRUE)))
e<-as.matrix(cbind(singlee_sample,multie_sample))
c<-as.matrix(cbind(singlec_sample,multic_sample))
colnames(c)<- c("single","multi")
colnames(e)<- c("single","multi")
m<-bcea(e,c,ref = 2, interventions = c("single","multi"), Kmax = 400000)
plot(m)
plot1<-contour2(m,graph = c("ggplot2"), wtp = 400000)
plot2<-eib.plot(m,graph = c("ggplot2"))
plot3<-ceac.plot(m, graph=c("ggplot2"))
plot4<-evi.plot(m, graph = c("ggplot2"))
g <- arrangeGrob(plot1, plot2, plot3, plot4, ncol=2)
ggsave(file="C:/Users/Test/Box Sync/Emory Box/R/New folder/increase to pi/bcea.pdf", g)
rm(list = ls())




