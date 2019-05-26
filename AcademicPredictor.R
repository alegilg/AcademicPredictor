#Librarys used
library(plyr)
library(dplyr)

#Insert data
library(readxl)
getwd()
##admissions18_19 <- read_excel("/Users/Katy/Desktop/MuyNecesario/Predictor/Ingreso18-19.xlsx")
##entry19 <- read_excel("/Users/Katy/Desktop/MuyNecesario/Predictor/Ingreso19.xlsx")
##entry18 <- read_excel("/Users/Katy/Desktop/MuyNecesario/Predictor/Ingreso18.xlsx")
admissions18_19 <- read_excel("Admissions18-19.xlsx")
entry19 <- read_excel("Ingreso19.xlsx")
entry18 <- read_excel("Ingreso18.xlsx")


#Defining colnames of the three data frames
names(entry18) <- c("ID", "Status", "Name", "Sex", "Career", "Cohort", "Entry", "Cal.Math", "Rec.Math", "Cal.Physics", "Rec.Physics", "IC.Average", "BenefitType", "RequestedBenefit", "GetScholarship", "SchoolAverage", "PercentageGiven", "School")
names(entry19) <- c("ID", "Status", "Name", "Sex", "Career", "Cohort", "Entry", "Cal.Math", "Rec.Math", "Cal.Physics", "Rec.Physics", "IC.Average", "BenefitType", "RequestedBenefit", "GetScholarship", "SchoolAverage", "PercentageGiven", "ApplicantStatus", "EngineeringStatus", "School")
names(admissions18_19) <- c("ID", "Entry", "Name", "Sex", "School", "Nationality", "Province", "Area", "GraduationYear", "Career", "EntryCourse", "Cal.Physics", "Cal.Math", "Scholarship", "SchoolAverage", "ResultOfEC")

#MODIFYING ENTRY19
#Merging engineering status with applicant status
entry19$ApplicantStatus[which(!is.na(entry19$EngineeringStatus))] <- entry19$EngineeringStatus[which(!is.na(entry19$EngineeringStatus))]
#Replacing engineering status in status
entry19$Status <- entry19$ApplicantStatus
entry19$EngineeringStatus <- NULL  #Delete engineering status because its data is in applicant status
entry19$ApplicantStatus <- NULL  #Delete applicant status because its data is in status

#MERGING ENTRY18 WITH ENTRY19
entry18_19 <- rbind(entry19,entry18)

#FIXING BECA DATA
#All negative phrases in get scholarship to NO
unique(entry18_19$GetScholarship)
obtieneBecaPhrases <- c("MO","BAJA","NO OBTIENE","N/A","No","?",NA)
entry18_19$GetScholarship <- mapvalues(entry18_19$GetScholarship, obtieneBecaPhrases, rep("NO", length(obtieneBecaPhrases)))
#Deleting all requested benefit that were not obtained
entry18_19$RequestedBenefit[which(entry18_19$GetScholarship=="NO")] <- NA


names(entry18_19)
entry18_19$GetScholarship <- NULL #Delete get scholarship because the negatives were clean up in requested benefit
entry18_19$BenefitType <- NULL #Delete benefit type becuase it is not relevant for our study
names(entry18_19)[13] <- "Scholarship" #Changing requested benefit to scholarship
#Replacing percentage given values in scholarship
entry18_19$Scholarship[which(!is.na(entry18_19$PercentageGiven))] <- entry18_19$PercentageGiven[which(!is.na(entry18_19$PercentageGiven))]
entry18_19$PercentageGiven <- NULL #Delete percentage given because the info was replace in scholarship

unique(entry18_19$Scholarship)
scholarshipPhrases <- c("40/ 20","40/20","20/20")
scholarshipMeanings <- c("60","60","40")
entry18_19$Scholarship <- mapvalues(entry18_19$Scholarship, scholarshipPhrases, scholarshipMeanings)
#Correcting the data of Beca to show only the number
entry18_19$Scholarship[grepl("100",entry18_19$Scholarship)] <- "100" 
entry18_19$Scholarship[!grepl("100",entry18_19$Scholarship)] <- substr(entry18_19$Scholarship[!grepl("100",entry18_19$Scholarship)], 1, 2)


#Correcting status
unique(entry18_19$Status)
matriculadoPhrases <- c(entry18_19$Status[grep("matriculado",entry18_19$Status,TRUE)],"A - ASISTIÓ AL CURSO","HACE CUATRIMESTRAL")
entry18_19$Status <- mapvalues(entry18_19$Status, matriculadoPhrases, rep("Matriculado", length(matriculadoPhrases)))
bajaPhrases <- c(entry18_19$Status[grep("baja",entry18_19$Status,TRUE)],"FEBRERO",NA)
entry18_19$Status <- mapvalues(entry18_19$Status, bajaPhrases, rep("Baja", length(bajaPhrases)))

admissions18_19$Province[which(admissions18_19$ID=="272")] <- NA

delete <- c("AUS", "ausente", "-", "A", "APROBADO", "Desaprobado", "Es pase", "x", "Es pase interno")

entry18_19$Cal.Math <- mapvalues(entry18_19$Cal.Math, delete, rep(NA, length(delete)))
entry18_19$Rec.Math <- mapvalues(entry18_19$Rec.Math, delete, rep(NA, length(delete)))
entry18_19$Cal.Physics <- mapvalues(entry18_19$Cal.Physics, delete, rep(NA, length(delete)))
entry18_19$Rec.Physics <- mapvalues(entry18_19$Rec.Physics, delete, rep(NA, length(delete)))

entry18_19$Cal.Math = as.numeric(entry18_19$Cal.Math)
entry18_19$Rec.Math = as.numeric(entry18_19$Rec.Math)
entry18_19$Cal.Physics = as.numeric(entry18_19$Cal.Physics)
entry18_19$Rec.Physics = as.numeric(entry18_19$Rec.Physics)

entry18_19$IC.Average <- round(rowMeans(entry18_19[,8:11], na.rm = TRUE), digits = 2)
entry18_19[entry18_19 == "NaN"] <- NA

notInEntry <- admissions18_19$Name[is.na(match(admissions18_19$Name, entry18_19$Name))]

df <- filter(admissions18_19, Name %in% notInEntry)

df <- df[, c("ID", "Province", "Name", "Sex", "Career", "Entry", "EntryCourse", "Cal.Math", "Area", "Cal.Physics", "GraduationYear", "ResultOfEC", "Scholarship", "SchoolAverage", "School")]
df
names(df) <- c("ID", "Status", "Name", "Sex", "Career", "Cohort", "Entry", "Cal.Math", "Rec.Math", "Cal.Physics", "Rec.Physics", "IC.Average", "Scholarship", "SchoolAverage", "School")

change <- c("Ingeniería en Informática", "Ingeniería Industrial", "Ingeniería Biomédica")
df$Career <- mapvalues(df$Career, change, c("INF", "IND", "BIO"))

df$Status <- NA
df$Rec.Math <- NA
df$Rec.Physics <- NA

df$Cal.Math = as.numeric(df$Cal.Math)
df$Rec.Math = as.numeric(df$Rec.Math)
df$Cal.Physics = as.numeric(df$Cal.Physics)
df$Rec.Physics = as.numeric(df$Rec.Physics)
df$IC.Average = as.numeric(df$IC.Average)

df$IC.Average <- round(rowMeans(df[,8:11], na.rm = TRUE), digits = 2)
df$IC.Average[df$IC.Average == "NaN"] <- NA

finalEntry <- rbind(entry18_19, df)
finalEntry
unique(finalEntry$School)

#Correcting schools
finalEntry$School <- gsub(" \\(","|",finalEntry$School) #Deleting (x)
finalEntry$School <- sub("\\|.*", "", finalEntry$School)
finalEntry$School <- sapply(finalEntry$School, tolower) #All to lower case

#Correcting entry
unique(finalEntry$Entry)
febrero <- finalEntry$Entry[grep(("febrero"),finalEntry$Entry,TRUE)]
correctFebrero <- rep("Febrero",length(febrero))

directo <- finalEntry$Entry[grep(("directo"),finalEntry$Entry,TRUE)]
correctDirecto <- rep("Directo",length(directo))

cuatrimestral <- finalEntry$Entry[grep("cuatrimestral",finalEntry$Entry,TRUE)]
correctCuatrimestral <- rep("Cuatrimestral",length(cuatrimestral))

septiembre <- finalEntry$Entry[grep("septiembre",finalEntry$Entry,TRUE)]
correctSeptiembre <- rep("Septiembre",length(septiembre))

octubre <- finalEntry$Entry[grep("octubre",finalEntry$Entry,TRUE)]
correctOctubre <- rep("Octubre",length(octubre))

agosto <- finalEntry$Entry[grep("agosto",finalEntry$Entry,TRUE)]
correctAgosto <- rep("Agosto",length(agosto))

libre <- finalEntry$Entry[grep("libre",finalEntry$Entry,TRUE)]
correctLibre <- rep("Libre",length(libre))

entryPhrases <- c(febrero,directo,cuatrimestral,septiembre,octubre,agosto,libre)
correctEntryPhrases <- c(correctFebrero,correctDirecto,correctCuatrimestral,correctSeptiembre,correctOctubre,correctAgosto,correctLibre)
finalEntry$Entry <- mapvalues(finalEntry$Entry, entryPhrases, correctEntryPhrases)

#Editing the cals from NA to 1 except those who enter as directo or pase universitario
naCalsIndex <- which(!finalEntry$Entry=="Directo"&!finalEntry$Entry=="Pase Universitario"&is.na(finalEntry$IC.Average))
finalEntry[naCalsIndex,8:12] <- 1


#PLOTS
library(ggplot2)
par(mfrow=c(1,1))

#Status
statusTable <- table(finalEntry$Status) 
statusLabels <- names(statusTable)
statusPercentage <- round(statusTable/sum(statusTable)*100)
statusLabels <- paste(statusLabels, statusPercentage) # add percents to labels 
statusLabels <- paste(statusLabels,"%",sep="") # ad % to labels 
pie(statusTable,labels = statusLabels, col=rainbow(length(statusLabels)),main="Status")

#Sex
sexTable <- table(finalEntry$Sex) 
sexLabels <- names(sexTable)
sexPercentage <- round(sexTable/sum(sexTable)*100)
sexLabels <- paste(sexLabels, sexPercentage) # add percents to labels 
sexLabels <- paste(sexLabels,"%",sep="") # ad % to labels 
pie(sexTable,labels = sexLabels, col=rainbow(length(sexLabels)),main="Sex")

#Career
careerTable <- table(finalEntry$Career) 
careerLabels <- names(careerTable)
careerPercentage <- round(careerTable/sum(careerTable)*100)
careerLabels <- paste(careerLabels, careerPercentage) # add percents to labels 
careerLabels <- paste(careerLabels,"%",sep="") # ad % to labels 
pie(careerTable,labels = careerLabels, col=rainbow(length(careerLabels)),main="Career")

#Entry
entryTable <- table(finalEntry$Entry) 
entryLabels <- names(entryTable)
entryPercentage <- round(entryTable/sum(entryTable)*100)
entryLabels <- paste(entryLabels, entryPercentage) # add percents to labels 
entryLabels <- paste(entryLabels,"%",sep="") # ad % to labels 
pie(entryTable,labels = entryLabels, col=rainbow(length(entryLabels)),main="Entry")

#Cals
hist(finalEntry$IC.Average)
hist(finalEntry$Cal.Math)
hist(finalEntry$Rec.Math)
hist(finalEntry$Cal.Physics)
hist(finalEntry$Rec.Physics)

#Sex & Status
counts <- table(finalEntry$Sex, finalEntry$Status)
barplot(counts, col=c("red","darkblue"),legend = rownames(counts), beside=TRUE)

#Sex & Career
counts <- table(finalEntry$Sex, finalEntry$Career)
barplot(counts, col=c("red","darkblue"),legend = rownames(counts), beside=TRUE)

#Scholarship
scholarshipTable <- table(is.na(finalEntry$Scholarship)) 
scholarshipLabels <- c("Yes","No")
scholarshipPercentage <- round(scholarshipTable/sum(scholarshipTable)*100)
scholarshipLabels <- paste(scholarshipLabels, scholarshipPercentage) # add percents to labels 
scholarshipLabels <- paste(scholarshipLabels,"%",sep="") # ad % to labels 
pie(scholarshipTable,labels = scholarshipLabels, col=rainbow(length(scholarshipLabels)),main="Scholarship")
