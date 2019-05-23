#Insert data
library(readxl)
library(dplyr)
getwd()
admissions18_19 <- read_excel("/Users/Katy/Desktop/MuyNecesario/Predictor/Ingreso18-19.xlsx")
entry19 <- read_excel("/Users/Katy/Desktop/MuyNecesario/Predictor/Ingreso19.xlsx")
entry18 <- read_excel("/Users/Katy/Desktop/MuyNecesario/Predictor/Ingreso18.xlsx")

#Merging status ingenieria with estado del aspirante
entry19$Estado.del..Aspirante[which(!is.na(entry19$Status..Ingeniería))] <- entry19$Status..Ingeniería[which(!is.na(entry19$Status..Ingeniería))]
#Replacing status ingenieria in status
entry19$Status <- entry19$Estado.del..Aspirante
names(entry19)
entry19 <- entry19[,-19]  #Delete status ingenieria because the its data is in estado del aspirante
entry19 <- entry19[,-18]  #Delete estado del aspirante because the its data is in status

#MERGING ENTRY18 WITH ENTRY19
names(entry18)
names(entry19)
names(entry18)[7] <- "Ingreso" #Matching columns names
entry18_19 <- rbind(entry19,entry18)



#FIXING BECA DATA
if (!is.element("plyr", installed.packages()[,1])){
  install.packages("plyr")
}
library("plyr")
unique(entry18_19$Obtiene..BECA)
obtieneBecaPhrases <- c("MO","BAJA","NO OBTIENE","N/A","No","?",NA)
entry18_19$Obtiene..BECA <- mapvalues(entry18_19$Obtiene..BECA, obtieneBecaPhrases, rep("NO", length(obtieneBecaPhrases)))
entry18_19$Beneficio..Solicitado....[which(entry18_19$Obtiene..BECA=="NO")] <- NA

names(entry18_19)
entry18_19 <- entry18_19[,-15] #Delete Obtiene Beca because the negatives were clean up in beneficio solicitado
entry18_19 <- entry18_19[,-13] #Delete Tipo de beneficio becuase it is not relevant for our study
names(entry18_19)[13] <- "Beca" #Changing name to beneficio solicitado
#Replacing porcentaje otorgado values in Beca
entry18_19$Beca[which(!is.na(entry18_19$Porcentaje..Otorgado))] <- entry18_19$Porcentaje..Otorgado[which(!is.na(entry18_19$Porcentaje..Otorgado))]
entry18_19 <- entry18_19[,-15] #Delete Porcentaje Otorgado because the info was replace in Beca
#Suming the ones with two types of beca
unique(entry18_19$Beca)
becaPhrases <- c("40/ 20","40/20","20/20")
becaMeanings <- c("60","60","40")
entry18_19$Beca <- mapvalues(entry18_19$Beca, becaPhrases, becaMeanings)
#Correcting the data of Beca to show only the number
entry18_19$Beca[grepl("100",entry18_19$Beca)] <- "100" 
entry18_19$Beca[!grepl("100",entry18_19$Beca)] <- substr(entry18_19$Beca[!grepl("100",entry18_19$Beca)], 1, 2)


#Correcting status
unique(entry18_19$Status)
matriculadoPhrases <- c(entry18_19$Status[grep("matriculado",entry18_19$Status,TRUE)],"A - ASISTIÓ AL CURSO","HACE CUATRIMESTRAL")
entry18_19$Status <- mapvalues(entry18_19$Status, matriculadoPhrases, rep("Matriculado", length(matriculadoPhrases)))

bajaPhrases <- c(entry18_19$Status[grep("baja",entry18_19$Status,TRUE)],"FEBRERO",NA)
entry18_19$Status <- mapvalues(entry18_19$Status, bajaPhrases, rep("Baja", length(bajaPhrases)))

names(entry18_19) <- c("ID", "Status", "Name", "Sex", "Career", "Cohort", "Entry", "Cal.Math", "Rec.Math", "Cal.Physics", "Rec.Physics", "IC.Average", "Scholarship", "SchoolAverage", "SecondarySchool")
names(admissions18_19) <- c("ID", "Entry", "Name", "Sex", "SecondarySchool", "Nationality", "Province", "Area", "GraduationYear", "Career", "EntryCourse", "Cal.Physics", "Cal.Math", "Scholarship", "SchoolAverage", "ResultOfEC")
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
