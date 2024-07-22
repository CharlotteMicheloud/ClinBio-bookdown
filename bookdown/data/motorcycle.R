
helmet.d <- rep(c("Yes","No"),4)
helmet.p <- c(rep("No", 4),rep("Yes", 4))
sex.p <- rep(c("Male","Male","Female","Female"),2)


both.dead <- c(37,226,10,171,152,7,159,6)
driver.dead <- c(70,546,27,342,360,34,279,39)
passenger.dead <- c(84,378,36,413,259,8,270,33)

rr <- (both.dead+driver.dead)/(both.dead+passenger.dead)


mydata <- data.frame(helmet.d, helmet.p, sex.p, both.dead, driver.dead, passenger.dead, rr)
names(mydata) <- c("Helmet.D", "Helmet.P", "Sex.P", "Dead.DP", "Dead.D", "Dead.P", "RR")
write.table(mydata, file="Motorcycle.csv", row.names=FALSE)
