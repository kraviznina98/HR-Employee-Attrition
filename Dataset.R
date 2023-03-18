library(ggplot2)

data <- read.csv("HR-Employee-Attrition.csv", stringsAsFactors = F)

str(data)

apply(data, MARGIN = 2, FUN = function(x) sum(is.na(x)))
apply(data, MARGIN = 2, FUN = function(x) sum(x == "-", na.rm = T))
apply(data, MARGIN = 2, FUN = function(x) sum(x == "", na.rm = T))
apply(data, MARGIN = 2, FUN = function(x) sum(x == " ", na.rm = T))

#svi zaposleni imaju preko 18 godina
table(data$Over18)
#uklanjamo varijablu
data$Over18 <- NULL

#svako je jedinstveni zaposleni
table(data$EmployeeCount)
data$EmployeeCount <- NULL

#izlaznu varijablu pretvaramo u faktor
#kod stabla odlucivanja izlazna varijabla mora biti faktorska
data$Attrition <- as.factor(data$Attrition)

table(data$StandardHours)
#Svim zaposlenima je standardan broj sati 80
#izbacicemo tu varijablu
data$StandardHours <- NULL

str(data)
#char varijable takodje pretvaramo u faktorske odnosno numericke kako bismo mogli da radimo nad njima
#proveravamo broj razlicitih vrednosti za svaku varijablu
table(data$BusinessTravel)
data$BusinessTravel <- as.factor(data$BusinessTravel)

table(data$Department)
data$Department <- as.factor(data$Department)

table(data$EducationField)
data$EducationField <- as.factor(data$EducationField)

table(data$MaritalStatus)
data$MaritalStatus <- as.factor(data$MaritalStatus)

table(data$Gender)
data$Gender <- as.factor(data$Gender)

table(data$OverTime)
data$OverTime <- as.factor(data$OverTime)

table(data$JobRole)
data$JobRole <- as.factor(data$JobRole)

table(data$PerformanceRating)
data$PerformanceRating <- as.factor(data$PerformanceRating)

str(data)

########################
################Age################################

ggplot(data, aes(x = Age, fill=Attrition)) +
  geom_density(alpha=0.4) +
  theme_minimal()

KruskalAge <- kruskal.test(data$Age ~ data$Attrition)
KruskalAge
#Na osnovu p-value = 5.301e-11 zakljucujemo 
#da je varijabla Age znacajna za predikciju

data$Age <- as.numeric(data$Age)

################BusinessTravel#####################

ggplot(data, aes(x = BusinessTravel, fill=Attrition)) +
  geom_bar() +
  theme_minimal()

ggplot(data, aes(x = BusinessTravel, fill=Attrition)) +
  geom_bar(position = "fill") +
  ylab("Proportion") +
  theme_minimal()

#Varijabla BusinessTravel je znacajna za predikciju

################DailyRate##########################

ggplot(data, aes(x = DailyRate, fill=Attrition)) +
  #geom_histogram(bins = 20) +
  geom_density(alpha=0.4) +
  theme_minimal()

KruskalDailyRate <- kruskal.test(data$DailyRate ~ data$Attrition)
KruskalDailyRate

#Na osnovu p-value = 0.029 zakljucujemo da je
#varijabla DailyRate znacajna

data$DailyRate <- as.numeric(data$DailyRate)

################Department#########################

ggplot(data, aes(x = Department, fill=Attrition)) +
  geom_bar() +
  theme_minimal()

ggplot(data, aes(x = Department, fill=Attrition)) +
  geom_bar(position = "fill") +
  ylab("Proportion") +
  theme_minimal()

ChiSqDepartment <- chisq.test(x = data$Department, y = data$Attrition)
ChiSqDepartment

#Na osnovu p-value = 0.004526 iz Chi-square testa zakljucujemo da
#je varijabla Department znacajna za predikciju

################DistanceFromHome###################

ggplot(data, aes(x = DistanceFromHome, fill=Attrition)) +
  geom_density(alpha=0.4) +
  theme_minimal()

KruskalDistance <- kruskal.test(data$DistanceFromHome ~ data$Attrition)
KruskalDistance

#Na osnovu p-value = 0.002386 iz Kruskal-Valis testa zakljucujemo da
#je varijabla DistanceFromHome znacajna za predikciju

data$DistanceFromHome <- as.numeric(data$DistanceFromHome)

################Education##########################

ggplot(data, aes(x = Education, fill=Attrition)) +
  geom_density(alpha=0.4) +
  theme_minimal()

KruskalEducation <- kruskal.test(data$Education ~ data$Attrition)
KruskalEducation

#Na osnovu p-value = 0.2448 iz Kruskal-Valis testa zakljucujemo da
#varijabla Education nije znacajna za predikciju
data$Education <- NULL

################EducationField#####################

ggplot(data, aes(x = EducationField, fill=Attrition)) +
  geom_bar() +
  theme_minimal()

ggplot(data, aes(x = EducationField, fill=Attrition)) +
  geom_bar(position = "fill") +
  ylab("Proportion") +
  theme_minimal()

ChiSqEducField <- chisq.test(x = data$EducationField, y = data$Attrition, simulate.p.value = TRUE)
ChiSqEducField

#simulate.p.value = TRUE zbog 
#Warning message:
#In chisq.test(a) : Chi-squared approximation may be incorrect

#Na osnovu p-value = 0.008496 iz Chi-square testa zakljucujemo da
#je varijabla EducationField znacajna za predikciju


################EmployeeNumber#####################

ggplot(data, aes(x = EmployeeNumber, fill=Attrition)) +
  geom_density(alpha=0.4) +
  theme_minimal()

KruskalEmplNum <- kruskal.test(data$EmployeeNumber ~ data$Attrition)
KruskalEmplNum

#Na osnovu p-value = 0.6911 iz Kruskal-Valis testa zakljucujemo da
#varijabla EmployeeNumber nije znacajna i da je mozemo odbaciti
data$EmployeeNumber <- NULL

################EnvironmentSatisfaction############

ggplot(data, aes(x = EnvironmentSatisfaction, fill=Attrition)) +
  geom_density(alpha=0.4) +
  theme_minimal()

KruskalEnvironmentSatisfaction<- kruskal.test(data$EnvironmentSatisfaction ~ data$Attrition)
KruskalEnvironmentSatisfaction

#Na osnovu p-value = 0.0002172 iz Kruskal-Valis testa zakljucujemo da
#je varijabla EnvironmentSatisfaction znacajna i da je ne mozemo odbaciti

data$EnvironmentSatisfaction <- as.numeric(data$EnvironmentSatisfaction)

################Gender#############################

ggplot(data, aes(x = Gender, fill=Attrition)) +
  geom_bar() +
  theme_minimal()

ggplot(data, aes(x = Gender, fill=Attrition)) +
  geom_bar(position = "fill") +
  ylab("Proportion") +
  theme_minimal()

ChiSqGender <- chisq.test(x = data$Gender, y = data$Attrition)
ChiSqGender

#Na osnovu p-value = 0.2906 iz Chi-square testa zakljucujemo da
#varijabla Gender nije znacajna za predikciju i da je mozemo odbaciti

data$Gender <- NULL

################HourlyRate#########################

ggplot(data, aes(x = HourlyRate, fill=Attrition)) +
  geom_density(alpha=0.4) +
  theme_minimal()

KruskalHourlyRate <- kruskal.test(data$HourlyRate ~ data$Attrition)
KruskalHourlyRate

#Na osnovu p-value = 0.7976 iz Kruskal-Valis testa zakljucujemo da
#varijabla HourlyRate nije znacajna i da je mozemo odbaciti
data$HourlyRate<-NULL

################JobInvolvement#####################

ggplot(data, aes(x = JobInvolvement, fill=Attrition)) +
  geom_density(alpha=0.4) +
  theme_minimal()

KruskalJobInvolvement <- kruskal.test(data$JobInvolvement ~ data$Attrition)
KruskalJobInvolvement

#Varijabla JobInvolvement je znacajna za predikciju

data$JobInvolvement <- as.numeric(data$JobInvolvement)

################JobLevel###########################

ggplot(data, aes(x = JobLevel, fill=Attrition)) +
  geom_density(alpha=0.4) +
  theme_minimal()

KruskalJobLevel <- kruskal.test(data$JobLevel ~ data$Attrition)
KruskalJobLevel

#Varijabla JobLevel je znacajna za predikciju

data$JobLevel <- as.numeric(data$JobLevel)

################JobRole############################

ggplot(data, aes(x = JobRole, fill=Attrition)) +
  geom_bar() +
  theme_minimal()

ggplot(data, aes(x = JobRole, fill=Attrition)) +
  geom_bar(position = "fill") +
  ylab("Proportion") +
  theme_minimal()

#Varijabla JobRole je znacajna za predikciju

################JobSatisfaction####################

ggplot(data, aes(x = JobSatisfaction, fill=Attrition)) +
  geom_density(alpha=0.4) +
  theme_minimal()

KruskalJobSatisfaction <- kruskal.test(data$JobSatisfaction ~ data$Attrition)
KruskalJobSatisfaction

#Varijabla JobSatisfaction je znacajna za predikciju na osnovu
#p-value = 7.955e-05 iz Kruskal-Valis testa

data$JobSatisfaction<- as.numeric(data$JobSatisfaction)

################MaritalStatus######################

ggplot(data, aes(x = MaritalStatus, fill=Attrition)) +
  geom_bar() +
  theme_minimal()

ggplot(data, aes(x = MaritalStatus, fill=Attrition)) +
  geom_bar(position = "fill") +
  ylab("Proportion") +
  theme_minimal()

#Varijabla MaritalStatus je znacajna za predikciju

################MonthlyIncome######################

ggplot(data, aes(x = MonthlyIncome, fill=Attrition)) +
  geom_density(alpha=0.4) +
  theme_minimal()

KruskalMonthlyIncome <- kruskal.test(data$MonthlyIncome ~ data$Attrition)
KruskalMonthlyIncome
#Varijabla MonthlyIncome je znacajna za predikciju
#(na osnovu p-value = 2.949e-14 iz Kruskal-Valis testa)

data$MonthlyIncome <- as.numeric(data$MonthlyIncome)

################MonthlyRate########################

ggplot(data, aes(x = MonthlyRate, fill=Attrition)) +
  geom_density(alpha=0.4) +
  theme_minimal()

KruskalMonthlyRate <- kruskal.test(data$MonthlyRate ~ data$Attrition)
KruskalMonthlyRate

#Varijabla MonthlyRate nije znacajna za predikciju
#(na osnovu p-value = 0.5587 iz Kruskal-Valis testa)

data$MonthlyRate <- NULL

################NumCompaniesWorked#################

ggplot(data, aes(x = NumCompaniesWorked, fill=Attrition)) +
  geom_density(alpha=0.4) +
  theme_minimal()

KruskalNumCompaniesWorked <- kruskal.test(data$NumCompaniesWorked ~ data$Attrition)
KruskalNumCompaniesWorked
#Varijabla NumCompaniesWorked je znacajna za predikciju
#(na osnovu p-value = 0.2423 iz Kruskal-Valis testa)

data$NumCompaniesWorked <- as.numeric(data$NumCompaniesWorked)

################OverTime###########################

ggplot(data, aes(x = OverTime, fill=Attrition)) +
  geom_bar() +
  theme_minimal()

ggplot(data, aes(x = OverTime, fill=Attrition)) +
  geom_bar(position = "fill") +
  ylab("Proportion") +
  theme_minimal()

ChiSqOverTime <- chisq.test(x = data$OverTime, y = data$Attrition)
ChiSqOverTime

#Varijabla OverTime je znacajna za predikciju
#(na osnovu p-value < 2.2e-16 iz ChiSquare testa)

################PercentSalaryHike##################

ggplot(data, aes(x = PercentSalaryHike, fill=Attrition)) +
  geom_density(alpha=0.4) +
  theme_minimal()

KruskalPercSalHike <- kruskal.test(data$PercentSalaryHike ~ data$Attrition)
KruskalPercSalHike

#Varijabla PercentSalaryHike nije znacajna za predikciju
#(na osnovu p-value = 0.3655 iz Kruskal-Valis testa)
data$PercentSalaryHike <- NULL

################PerformanceRating##################

ggplot(data, aes(x = PerformanceRating, fill=Attrition)) +
  geom_bar() +
  theme_minimal()

ggplot(data, aes(x = PerformanceRating, fill=Attrition)) +
  geom_bar(position = "fill") +
  ylab("Proportion") +
  theme_minimal()

ChiSqPerfRating <- chisq.test(x = data$PerformanceRating, y = data$Attrition)
ChiSqPerfRating

#Varijabla PerformanceRating nije znacajna za predikciju
#(na osnovu p-value = 0.9901 iz ChiSquare testa)
data$PerformanceRating <- NULL

################RelationshipSatisfaction##########

ggplot(data, aes(x = RelationshipSatisfaction, fill=Attrition)) +
  geom_density(alpha=0.4) +
  theme_minimal()

KruskalRelationshipSatisfaction <- kruskal.test(data$RelationshipSatisfaction ~ data$Attrition)
KruskalRelationshipSatisfaction

#Varijabla RelationshipSatisfaction nije znacajna za predikciju
#(na osnovu p-value = 0.102 iz Kruskal-Valis testa)
data$RelationshipSatisfaction<-NULL

################StockOptionLevel##################

ggplot(data, aes(x = StockOptionLevel, fill=Attrition)) +
  geom_density(alpha=0.4) +
  theme_minimal()

KruskalStockOptionLevel <- kruskal.test(data$StockOptionLevel ~ data$Attrition)
KruskalStockOptionLevel

#Varijabla StockOptionLevel je znacajna za predikciju
#(na osnovu p-value = 4.011e-11 iz Kruskal-Valis testa)

data$StockOptionLevel <- as.numeric(data$StockOptionLevel)

################TotalWorkingYears#################

ggplot(data, aes(x = TotalWorkingYears, fill=Attrition)) +
  geom_density(alpha=0.4) +
  theme_minimal()

KruskalTotalWorkingYears <- kruskal.test(data$TotalWorkingYears ~ data$Attrition)
KruskalTotalWorkingYears

#Varijabla TotalWorkingYears je znacajna za predikciju
#(na osnovu p-value = 2.398e-14 iz Kruskal-Valis testa)

data$TotalWorkingYears <- as.numeric(data$TotalWorkingYears)

################TrainingTimesLastYear##############

ggplot(data, aes(x = TrainingTimesLastYear, fill=Attrition)) +
  geom_density(alpha=0.4) +
  theme_minimal()

KruskalTrainingTimesLastYear <- kruskal.test(data$TrainingTimesLastYear ~ data$Attrition)
KruskalTrainingTimesLastYear

#Varijabla TrainingTimesLastYear je znacajna za predikciju
#(na osnovu p-value = 0.04729 iz Kruskal-Valis testa)

data$TrainingTimesLastYear <- as.numeric(data$TrainingTimesLastYear)

################WorkLifeBalance####################

ggplot(data, aes(x = WorkLifeBalance, fill=Attrition)) +
  geom_density(alpha=0.4) +
  theme_minimal()

KruskalWorkLifeBalance <- kruskal.test(data$WorkLifeBalance ~ data$Attrition)
KruskalWorkLifeBalance

#Varijabla WorkLifeBalance je znacajna za predikciju
#(na osnovu p-value = 0.04646 iz Kruskal-Valis testa)

data$WorkLifeBalance <- as.numeric(data$WorkLifeBalance)

################YearsAtCompany#####################

ggplot(data, aes(x = YearsAtCompany, fill=Attrition)) +
  geom_density(alpha=0.4) +
  theme_minimal()

KruskalYearsAtCompany <- kruskal.test(data$YearsAtCompany ~ data$Attrition)
KruskalYearsAtCompany

#Varijabla YearsAtCompany je znacajna za predikciju
#(na osnovu p-value = 2.914e-13 iz Kruskal-Valis testa)

data$YearsAtCompany <- as.numeric(data$YearsAtCompany)

################YearsInCurrentRole ################

ggplot(data, aes(x = YearsInCurrentRole, fill=Attrition)) +
  geom_density(alpha=0.4) +
  theme_minimal()

KruskalYearsInCurrentRole <- kruskal.test(data$YearsInCurrentRole  ~ data$Attrition)
KruskalYearsInCurrentRole

#Varijabla YearsInCurrentRole je znacajna za predikciju
#(na osnovu p-value = 4.427e-12 iz Kruskal-Valis testa)

data$YearsInCurrentRole <- as.numeric(data$YearsInCurrentRole)

################YearsSinceLastPromotion##################

ggplot(data, aes(x = YearsSinceLastPromotion, fill=Attrition)) +
  geom_density(alpha=0.4) +
  theme_minimal()

KruskalYrsSinceLastPromo <- kruskal.test(data$YearsSinceLastPromotion ~ data$Attrition)
KruskalYrsSinceLastPromo

#Varijabla YearsSinceLastPromotion je znacajna za predikciju
#(na osnovu p-value = 0.04117 iz Kruskal-Valis testa)

data$YearsSinceLastPromotion <- as.numeric(data$YearsSinceLastPromotion)

################YearsWithCurrManager##################

ggplot(data, aes(x = YearsWithCurrManager, fill=Attrition)) +
  geom_density(alpha=0.4) +
  theme_minimal()

KruskalYearsWithCurrManager<- kruskal.test(data$YearsWithCurrManager ~ data$Attrition)
KruskalYearsWithCurrManager

#Varijabla YearsWithCurrManager je znacajna za predikciju
#(na osnovu p-value = 1.806e-11 iz Kruskal-Valis testa)

data$YearsWithCurrManager <- as.numeric(data$YearsWithCurrManager)

#####################################

#promena kolona dataseta kako bi izlazna varijabla Attrition bila poslednja
data <- data[,c(1,3:24,2)]

#prilagodjavanje izlazne varijable
levels(data$Attrition)<- c("Yes", "No")
str(data)

saveRDS(data, "HR-Employee-Attrition.rds")
