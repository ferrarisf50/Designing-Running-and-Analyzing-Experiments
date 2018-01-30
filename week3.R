timeonsite = read.csv("timeonsite.csv")
View(timeonsite)
timeonsite$Subject = factor(timeonsite$Subject)
summary(timeonsite)