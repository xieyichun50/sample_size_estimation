genomiclist<-c(1:100)
k=1 ##sample size
a<-10000 ##sample times

numbertable<-as.data.frame(matrix(NA, nrow=a, ncol = 4))

for (i in 1:a) {
  numbertable[i,]<-sample(genomiclist,k,replace = T)
}

ptable<-as.data.frame(matrix(NA,nrow = a, ncol = 2))
names(ptable)<-c("mean","sd")

for (j in 550:a) {
  coveragetable<-as.data.frame(matrix(NA,nrow = 1000, ncol = 1))
  for (k in 1:1000) {
    numberlist<-sample(1:a, j, replace = T)
    coveragetable$V1[k]<-length(unique(c(numbertable$V1[numberlist],
                    numbertable$V2[numberlist],
                    numbertable$V3[numberlist],
                    numbertable$V4[numberlist])))/100
  }
  ptable$mean[j]<-mean(coveragetable$V1)
  ptable$sd[j]<-sd(coveragetable$V1)
 if (ptable$mean[j]>0.999) {
   j<-a
 }
}

library(ggplot2)
ptable$x<-as.numeric(row.names(ptable))
p<-ggplot(data = ptable, aes(x=x, y=mean))+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd, width = 0.2))+
  geom_line(size = 0.5)+
  geom_point(size = 1)+ 
  geom_hline(yintercept = 0.995, color = "red", linetype = "dashed")+
  scale_x_continuous(limit = c(0,800), breaks = seq(0, 800, 10))+
  theme(axis.line = element_line(linetype = "solid", size = 0.5), 
        axis.ticks = element_line(colour = "black", size = 0.5), 
        axis.title = element_text(size = 12), 
        axis.text = element_text(size = 12, colour = "black"), 
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12), 
        plot.title = element_text(size = 0, hjust = 0.5), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 0), 
        panel.background = element_rect(fill = NA))
p
