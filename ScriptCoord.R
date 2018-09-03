

coord=data.frame(X=c(0,0,100,100),Y=c(0,50,50,0))
plot(coord,asp=1)

coordUTM=data.frame(Xutm=c(0,45,55,0),Yutm=c(95,105,0,0))
plot(coordUTM,asp=1)

data_GPS=cbind(coord,coordUTM)

library(vegan)
aa=procrustes(coord,coordUTM,scale=F)
#Translation
coordNew=sweep(coord,2,aa$translation,FUN="+")
#Rotation
coordNew=as.matrix(coordNew)%*%aa$rotation

plot(coordNew,asp=1)

dist(coordNew)



library(MCMCpack)
03
library(maps)
04
nov<-read.table("PCA.txt",header=TRUE,sep="\t")
05
X<-as.matrix(cbind(nov$longitude,nov$latitude))
06
Xstar<-as.matrix(cbind(nov$PC1,nov$PC2))
07
p<-procrustes(Xstar,X,translation=TRUE,dilation=TRUE)


# trans <- AffineTransformation(controlPoints = data_GPS)
# calculateParameters(trans)
trans <- SimilarityTransformation(controlPoints = data_GPS)
calculateParameters(trans)

DATA=data.frame(coord)
coordinates(DATA)=~X+Y
DATA_geog = applyTransformation(trans, DATA)
plot(DATA_geog)

dist(DATA_geog@coords)

