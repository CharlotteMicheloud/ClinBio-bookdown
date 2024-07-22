
x <- c(2,12,14,4,5,8)
T <- c(311.9, 878.1, 667.5,607.9, 1272.1, 888.9)
exposure <- as.factor(c(rep("yes",3),rep("no",3)))
agegroup <- as.factor(rep(c("40-49","50-59","60-69"),2))
mydata <- data.frame(x, T, exposure, agegroup)
names(mydata) <- c("No.Cases", "Person.Years", "Exposure", "Age.Group")
write.table(mydata, file="IHD.csv", row.names=FALSE)
