library(readr)
library(ggplot2)

HospitalCost <- read.csv(file.choose(), header=T)

attach(HospitalCost)

head(HospitalCost)
nrow(HospitalCost)
ncol(HospitalCost)
table(HospitalCost$AGE)
hist(HospitalCost$AGE)

expd <- tapply(HospitalCost$TOTCHG, HospitalCost$AGE, FUN = sum)
expd

sort(expd, FALSE )

Diagnosis <- tapply(HospitalCost$TOTCHG, HospitalCost$APRDRG, FUN = sum)
Diagnosis
sort(Diagnosis,FALSE)

mean(HospitalCost$RACE,na.rm = T)

HospitalCost$Age [HospitalCost$APRDRG == "RACE" & is.na(HospitalCost$AGE) == TRUE]
summary(HospitalCost$RACE)

model <- lm(HospitalCost$TOTCHG ~ HospitalCost$AGE + HospitalCost$FEMALE, data = HospitalCost)
summary(model)

modellenth <- lm(HospitalCost$LOS ~ HospitalCost$AGE + HospitalCost$FEMALE + HospitalCost$RACE, data = HospitalCost)
summary(modellenth)

aov(HospitalCost$LOS ~ HospitalCost$TOTCHG,  data =HospitalCost)
formal
formals()
arg




