library(tuneR)
setwd("C:/Users/Elvis/Dropbox/CAS_Statistical_Modelling/Musik/")
Songs<-list.files("Radiohead - In Rainbows (2007) 320kbps/")
i<-1
test=readMP3(paste("Radiohead - In Rainbows (2007) 320kbps/",Songs[i],sep=""))
par(mfrow=c(1,3))
plot(test,main=Songs[i])
par(mfrow=c(1,2))
hist(test@left, main = paste(Songs[i],"left"))
hist(test@right, main = paste(Songs[i],"right"))


library(tuneR)
Songs<-list.files("Radiohead - In Rainbows (2007) 320kbps/")
m1<-matrix(nrow = length(Songs),ncol=102)
for (i in 1:length(Songs))
{
  test=readMP3(paste("Radiohead - In Rainbows (2007) 320kbps/",Songs[i],sep=""))
  m1[i,]=c(quantile(test@left,probs = seq(0,1,0.02)),quantile(test@right,probs = seq(0,1,0.02)))
  print(i)
}
row.names(m1)<-Songs



Songs<-list.files("The_Strokes_Discography/The_Strokes_Is_This_It/")
m2<-matrix(nrow = length(Songs),ncol=102)
for (i in 1:length(Songs))
{
  test=readMP3(paste("The_Strokes_Discography/The_Strokes_Is_This_It/",Songs[i],sep=""))
  m2[i,]=c(quantile(test@left,probs = seq(0,1,0.02)),quantile(test@right,probs = seq(0,1,0.02)))
  print(i)
}
row.names(m2)<-Songs




Songs<-list.files("Bob Marley & The Wailers - Exodus/")
m3<-matrix(nrow = length(Songs),ncol=102)
for (i in 1:length(Songs))
{
  test=readMP3(paste("Bob Marley & The Wailers - Exodus/",Songs[i],sep=""))
  m3[i,]=c(quantile(test@left,probs = seq(0,1,0.02)),quantile(test@right,probs = seq(0,1,0.02)))
  print(i)
}
row.names(m3)<-Songs

data<-rbind(m1,m2,m3)


library(dendextend)

plot(hclust(dist(data),method ="ward.D2"))
dend <- as.dendrogram(hclust(dist(data),method ="ward.D2"))

farbe<-NA
for (i in 1:(dim(data)[2]))
{
  if(sum(labels(dend)[i]==row.names(m1))!=0)
  {
    farbe[i]<-1
  }
  if(sum(labels(dend)[i]==row.names(m2))!=0)
  {
    farbe[i]<-2
  }
  if(sum(labels(dend)[i]==row.names(m3))!=0)
  {
    farbe[i]<-3
  }  
}

palette(c("red","blue","black"))
labels_colors(dend) <- farbe
par(mar=c(c(13, 4, 4, 2)))
plot(dend,main="Radiohead vs. Strokes vs. Bob Marley")


png(filename = "dendro_strokes_radiohead_bob.png")
labels_colors(dend) <- farbe
par(mar=c(c(13, 4, 4, 2)))
plot(dend,main="Radiohead vs. Strokes vs. Bob Marley")
dev.off()



#PCA test
png(filename = "pca_T_and_F.png")
par(mfrow=c(1,2))
farbe<-c(rep(1,nrow(m1)),rep(2,nrow(m2)),rep(3,nrow(m3)))
pca<-prcomp(data,scale. = F)
summary(pca)
pca.p<-predict(pca)
plot(pca.p[,1],pca.p[,2],col=farbe,pch=16,main="PCA no norm")

pca<-prcomp(data,scale. = T)
summary(pca)
pca.p<-predict(pca)
plot(pca.p[,1],pca.p[,2],col=farbe,pch=16,main="PCA norm")
dev.off()










