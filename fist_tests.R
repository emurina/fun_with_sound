library(tuneR)
Songs<-list.files("C:/Users/Elvis/Desktop/Radiohead - In Rainbows (2007) 320kbps/")
m1<-matrix(nrow = length(Songs),ncol=102)
for (i in 1:length(Songs))
{
  test=readMP3(paste("C:/Users/Elvis/Desktop/Radiohead - In Rainbows (2007) 320kbps/",Songs[i],sep=""))
  m1[i,]=c(quantile(test@left,probs = seq(0,1,0.02)),quantile(test@right,probs = seq(0,1,0.02)))
}
row.names(m1)<-Songs


Songs<-list.files("C:/Users/Elvis/Desktop/The Strokes Discography (2001-2013)/[2001] The Strokes - Is This It/")
m2<-matrix(nrow = length(Songs),ncol=102)
for (i in 1:length(Songs))
{
  test=readMP3(paste("C:/Users/Elvis/Desktop/The Strokes Discography (2001-2013)/[2001] The Strokes - Is This It/",Songs[i],sep=""))
  m2[i,]=c(quantile(test@left,probs = seq(0,1,0.02)),quantile(test@right,probs = seq(0,1,0.02)))
}
row.names(m2)<-Songs


data<-rbind(m1,m2)

library(dendextend)

plot(hclust(dist(data),method ="ward.D2"))
dend <- as.dendrogram(hclust(dist(data),method ="ward.D2"))

farbe<-NA
for (i in 1:((nrow(m1))+nrow(m2)))
{
if(sum(labels(dend)[i]==row.names(m1))!=0)
  {
  farbe[i]<-1
  }
if(sum(labels(dend)[i]==row.names(m2))!=0)
  {
  farbe[i]<-2
  }
}
palette(c("red","blue"))
palette()
png(filename = "C:/Users/Elvis/Documents/GitHub/fun_with_sound/dendo_first_try.png")
labels_colors(dend) <- farbe
par(mar=c(c(13, 4, 4, 2)))
plot(dend,main="Radiohead vs. Strokes")
dev.off()

#PCA test
png(filename = "C:/Users/Elvis/Documents/GitHub/fun_with_sound/pca_T_and_F.png")
par(mfrow=c(1,2))
farbe<-c(rep(1,nrow(m1)),rep(2,nrow(m2)))
pca<-prcomp(data,scale. = F)
summary(pca)
pca.p<-predict(pca)
plot(pca.p[,1],pca.p[,2],col=farbe,pch=16,main="scale=F")

pca<-prcomp(data,scale. = T)
summary(pca)
pca.p<-predict(pca)
plot(pca.p[,1],pca.p[,2],col=farbe,pch=16,main="scale=T")
dev.off()



