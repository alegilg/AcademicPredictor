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
names(admissions18_19) <- c("ID", "Entry", "Name", "Sex", "School", "Nationality", "Province", "Area", "GraduationYear", "Career", "EntryCourse", "Cal.Physics", "Cal.Math", "Scholarship", "SchoolAverage", "RecultOfEC")

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
entry18_19 <- rename(entry18_19, replace = c("RequestedBenefit" = "Scholarship")) #Changing requested benefit to scholarship
#Replacing percentage given values in scholarship
entry18_19$Scholarship[which(!is.na(entry18_19$PercentageGiven))] <- entry18_19$PercentageGiven[which(!is.na(entry18_19$PercentageGiven))]
entry18_19$PercentageGiven <- NULL #Delete percentage given because the info was replace in scholarship

unique(entry18_19$Beca)
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

admissions18_19[admissions18_19 == "Michael Ham (Nordelta)"] <- "Michael Ham"
admissions18_19[admissions18_19 == "Michael Ham (Vicente López)"] <- "Michael Ham"
admissions18_19[admissions18_19 == "Lincoln (La Lucila)"] <- "Lincoln"
admissions18_19[admissions18_19 == "Lincoln (Del Viso)"] <- "Lincoln"
admissions18_19[admissions18_19 == "Northlands (Nordelta)"] <- "Northlands"
admissions18_19[admissions18_19 == "Northlands (Olivos)"] <- "Northlands"
admissions18_19[admissions18_19 == "Pilgrims´ (Gral. Pacheco)"] <- "Pilgrims´"
admissions18_19[admissions18_19 == "Pilgrims´ (San Isidro)"] <- "Pilgrims´"
admissions18_19[admissions18_19 == "St. Mary of the Hills (San Fernando)"] <- "St. Mary of the Hills"
admissions18_19[admissions18_19 == "St. Mary of the Hills (Pilar)"] <- "St. Mary of the Hills"


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

df <- df[, c("ID", "Province", "Name", "Sex", "Career", "Entry", "EntryCourse", "Cal.Math", "Area", "Cal.Physics", "GraduationYear", "ResultOfEC", "Scholarship", "SchoolAverage", "SecondarySchool")]
df

names(df) <- c("ID", "Status", "Name", "Sex", "Career", "Cohort", "Entry", "Cal.Math", "Rec.Math", "Cal.Physics", "Rec.Physics", "IC.Average", "Scholarship", "SchoolAverage", "SecondarySchool")

change <- c("Ingeniería en Informática", "Ingeniería Industrial", "Ingeniería Biomédica")
df$Career <- mapvalues(df$Career, change, c("INF", "IND", "BIO"))

df$Status <- NA
df$Rec.Math <- NA
df$Rec.Physics <- NA

df$Cal.Math = as.numeric(df$Cal.Math)
df$Rec.Math = as.numeric(df$Rec.Math)
df$Cal.Physics = as.numeric(df$Cal.Physics)
df$Rec.Physics = as.numeric(df$Rec.Physics)
df$ResultOfEC = as.numeric(df$ResultOfEC)

df$IC.Average <- round(rowMeans(df[,8:11], na.rm = TRUE), digits = 2)
df$IC.Average[df$IC.Average == "NaN"] <- NA

finalEntry <- rbind(entry18_19, df)
finalEntry

