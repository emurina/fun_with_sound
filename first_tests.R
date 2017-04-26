

library(tuneR)
Songs<-list.files("C:/Users/Elvis/Desktop/Radiohead - In Rainbows (2007) 320kbps/")
i<-2

test=readMP3(paste("C:/Users/Elvis/Desktop/Radiohead - In Rainbows (2007) 320kbps/",Songs[i],sep=""))
test2<-mono(test, which = "left")
plot(test2@left)
fourier<-fft(test@left)
lala<-spectrum(test@left)

png("C:/Users/Elvis/Desktop/spec2.png")
plot(lala)
dev.off()



png("C:/Users/Elvis/Desktop/four_test.png")
plot(fourier)
dev.off()

c(quantile(fourier,probs = seq(0,1,0.02)))



library(tuneR)
Songs<-list.files("C:/Users/Elvis/Desktop/Radiohead - In Rainbows (2007) 320kbps/")
i<-2

test=readMP3(paste("C:/Users/Elvis/Desktop/Radiohead - In Rainbows (2007) 320kbps/",Songs[i],sep=""))
test2<-mono(test, which = "left")
plot(test2@left)
fourier<-fft(test@left)
plot(fourier)
test2<-mono(test, which = "right")
par(mfrow=c(1,2))
#plot(test)
#plot(test2)
mel1<-melfcc(test2)
mel2<-melfcc(test2)
dim(mel1)
#[1] 23122    12
#din sind nicht gleich....

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



Songs<-list.files("C:/Users/Elvis/Desktop/Bob Marley/")
m3<-matrix(nrow = length(Songs),ncol=102)
for (i in 1:length(Songs))
{
  test=readMP3(paste("C:/Users/Elvis/Desktop/Bob Marley/",Songs[i],sep=""))
  m3[i,]=c(quantile(test@left,probs = seq(0,1,0.02)),quantile(test@right,probs = seq(0,1,0.02)))
}
row.names(m3)<-Songs


if(sum(row.names(m1)=="Thumbs.db"))
{
  m1=m1[!row.names(m1)=="Thumbs.db",]
}

if(sum(row.names(m2)=="Thumbs.db"))
{
  m2=m2[!row.names(m2)=="Thumbs.db",]
}
if(sum(row.names(m3)=="Thumbs.db"))
{
  m3=m3[!row.names(m3)=="Thumbs.db",]
}



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
palette()
png(filename = "C:/Users/Elvis/Documents/GitHub/fun_with_sound/dendro_first_try.png")
labels_colors(dend) <- farbe
par(mar=c(c(13, 4, 4, 2)))
plot(dend,main="Radiohead vs. Strokes vs. Bob Marley")
dev.off()

#PCA test
png(filename = "C:/Users/Elvis/Documents/GitHub/fun_with_sound/pca_T_and_F.png")
par(mfrow=c(1,2))
farbe<-c(rep(1,nrow(m1)),rep(2,nrow(m2)),rep(3,nrow(m3)))
pca<-prcomp(data,scale. = F)
summary(pca)
pca.p<-predict(pca)
plot(pca.p[,1],pca.p[,2],col=farbe,pch=16,main="scale=F")

pca<-prcomp(data,scale. = T)
summary(pca)
pca.p<-predict(pca)
plot(pca.p[,1],pca.p[,2],col=farbe,pch=16,main="scale=T")
dev.off()



