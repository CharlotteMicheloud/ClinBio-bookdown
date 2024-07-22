
############################################################################
##                                                                        ##
##       Schottlandlandkarte gruen-rot, 1000 Farbstufen, Log-Skala        ##
##                                                                        ##
############################################################################


scot <- matrix(scan("scotland/scotkoord.txt"),ncol=3,byrow=T)
scotland <- list(length=56)
for(i in 1:56) scotland[[i]] <- scot[scot[,1]==i,]

xm <- (min(scotland[[54]][,2],na.rm=T) + max(scotland[[54]][,2],na.rm=T))/2
xd <- xm - 407.5
ym <- (min(scotland[[54]][,3],na.rm=T) + max(scotland[[54]][,3],na.rm=T))/2
yd <- ym - 940
scotland[[54]][,2] <- scotland[[54]][,2]-xd
scotland[[54]][,3] <- scotland[[54]][,3]-yd

xm <- (min(scotland[[55]][,2],na.rm=T) + max(scotland[[55]][,2],na.rm=T))/2
xd <- xm - 500
ym <- (min(scotland[[55]][,3],na.rm=T) + max(scotland[[55]][,3],na.rm=T))/2
yd <- ym - 902.5
scotland[[55]][,2] <- scotland[[55]][,2]-xd
scotland[[55]][,3] <- scotland[[55]][,3]-yd


data <- read.table("scotland/scotland.txt")
med <- data[,1]/data[,2]


unten <- 0.25
oben <- 4


# Definieren der Farbmatrix

buntcol <- matrix(nrow=1001, ncol=3)
vari <- 80000
mit <- 650
buntcol[,3] <- 0.0
buntcol[1,1] <- 0.0
buntcol[1,2] <- 0.0
for(i in 1:1000){
buntcol[i+1,1] <- exp(-(i-mit)*(i-mit)/(2*vari))
buntcol[1001-i+1,2] <- exp(-(i-mit)*(i-mit)/(2*vari))
}

#mycol <- rgb(buntcol[,1],buntcol[,2],buntcol[,3])
mycol <- gray(seq(1,0,length=1001))

farb <- 1:1001

eps <- 1e-10
grenz <- 1:1002
grenz[1] <- -eps
grenz[2] <- unten
grenz[1001] <- oben
grenz[1002] <- 1000000
diff <- log(grenz[1001])-log(grenz[2])
schritt <- diff/999
gleich <- 1:1000
for(i in 1:1000){
gleich[i] <- log(grenz[2]) + (i-1)*schritt
}
for(i in 1:998){
grenz[i+2] <- exp(gleich[i+1])
}

farben <- cut(med,grenz)
for(i in 1:56){
if(farben[i]==1) farben[i] <- 2}

#xmin <- 1:56
#xmax <- 1:56
#ymin <- 1:56
#ymax <- 1:56

#for(i in 1:56){
#xmin[i] <- min(scotland[[i]][,2],na.rm=T)
#xmax[i] <- max(scotland[[i]][,2],na.rm=T)
#ymin[i] <- min(scotland[[i]][,3],na.rm=T)
#ymax[i] <- max(scotland[[i]][,3],na.rm=T)}
#breite <- c(min(xmin),max(xmax))
#hoehe <- c(min(ymin),max(ymax))

breite <- c(50,550)
hoehe <- c(500,1000)
 
#pdf("test.pdf")
par(pty="s")
plot(breite,hoehe,type="n",col=0,xlab="",ylab="",axes=F)
for(k in 1:length(scotland)){
polygon(scotland[[k]][,2],scotland[[k]][,3],col=mycol[farben[k]],border=F)
polygon(scotland[[k]][,2],scotland[[k]][,3],density=0,lwd=.3,col=1)}
polygon(c(365,365,450,450),c(890,990,990,890),density=0,lwd=.3,col=1)
polygon(c(450,450,550,550),c(815,990,990,815),density=0,lwd=.3,col=1)

for(i in 1:1001){
polygon(c(440,440,455,455),c(550+.2*(i-1),550+.2*(i),550+.2*(i),550+.2*(i-1)),col=mycol[i],border=F)
if(grenz[i] < .25 && grenz[i+2] >= .25){
text(475,550+.2*(i+1),"0.250",cex=.7,col=1)
lines(c(455,458),c(550+.2*(i+1),550+.2*(i+1)),lwd=.3,col=1001)}
if(grenz[i] < .4 && grenz[i+2] >= .4){
text(475,550+.2*(i+1),"0.400",cex=.7,col=1)
lines(c(455,458),c(550+.2*(i+1),550+.2*(i+1)),lwd=.3,col=1001)}
if(grenz[i] < .625 && grenz[i+2] >= .625){
text(475,550+.2*(i+1),"0.625",cex=.7,col=1)
lines(c(455,458),c(550+.2*(i+1),550+.2*(i+1)),lwd=.3,col=1001)}
#if(grenz[i] < .8 && grenz[i+2] >= .8){
#text(475,550+.2*(i+1),"0.800",cex=.7,col=1)
#lines(c(455,458),c(550+.2*(i+1),550+.2*(i+1)),lwd=.3,col=1001)}
if(grenz[i] < 1 && grenz[i+2] >= 1){
text(475,550+.2*(i+1),"1.000",cex=.7,col=1)
lines(c(455,458),c(550+.2*(i+1),550+.2*(i+1)),lwd=.3,col=1001)}
#if(grenz[i] < 1.25 && grenz[i+2] >= 1.25){
#text(475,550+.2*(i+1),"1.250",cex=.7,col=1)
#lines(c(455,458),c(550+.2*(i+1),550+.2*(i+1)),lwd=.3,col=1001)}
if(grenz[i] < 1.6 && grenz[i+2] >= 1.6){
text(475,550+.2*(i+1),"1.600",cex=.7,col=1)
lines(c(455,458),c(550+.2*(i+1),550+.2*(i+1)),lwd=.3,col=1001)}
if(grenz[i] < 2.5 && grenz[i+2] >= 2.5){
text(475,550+.2*(i+1),"2.500",cex=.7,col=1)
lines(c(455,458),c(550+.2*(i+1),550+.2*(i+1)),lwd=.3,col=1001)}
if(grenz[i] < 4 && grenz[i+2] >= 4){
text(475,550+.2*(i+1),"4.000",cex=.7,col=1)
lines(c(455,458),c(550+.2*(i+1),550+.2*(i+1)),lwd=.3,col=1001)}
}
polygon(c(440,440,455,455),c(550,750,750,550),density=0,col=1,lwd=.3)
#title(main="MEDIAN",col=1)
#dev.off()

