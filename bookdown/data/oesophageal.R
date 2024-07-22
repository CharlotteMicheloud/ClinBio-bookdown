
cases <- c(1,0,4,5,25,21,42,34,19,36,5,8)
controls <- c(9,106,26,164,29,138,27,139,18,88,0,31)
exposure <- factor(rep(c("yes","no"), 6))
agegroup <- c(rep("25-34",2),rep("35-44",2),rep("45-54",2),rep("55-64",2),rep("65-74",2),rep("75+",2))
mydata <- data.frame(cases, controls, exposure, agegroup)
names(mydata) <- c("No.Cases", "No.Controls", "Exposure", "Age.Group")
write.table(mydata, file="Oesophageal.csv", row.names=FALSE)
