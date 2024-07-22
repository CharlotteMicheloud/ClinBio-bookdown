
cases <- c(10,11,13,9,7,16,50,16,4,18,60,27,5,21,125,91)
controls <- c(38,26,36,8,27,35,60,19,12,16,49,14,8,20,52,27)

av.alc <- c(0.0,0.2,1.0,2.0)%x%rep(1, 4)
av.tob <- rep(c(0,10,30,50), 4)
alc <- c(rep("0.0",4),rep("0.1-0.3",4),rep("0.4-1.5",4),rep("1.6+",4))
tob <- rep(c("0","1-19","20-39","40+"), 4)

mydata <- data.frame(cases, controls, alc, tob, av.alc, av.tob)
names(mydata) <- c("Cases", "Controls", "Alcohol", "Tobacco", "av.Alcohol", "av.Tobacco")
write.table(mydata, file="Oral.csv", row.names=FALSE)
