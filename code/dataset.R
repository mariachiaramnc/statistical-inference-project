library(haven)
data_complete <- read_sas("cy08msp_stu_qqq.sas7bdat", NULL)
data_school <- read_sas("cy08msp_sch_qqq.sas7bdat", NULL)


#### DATASET STUDENTI ####
ITA = data_complete[which(data_complete$CNTRYID==380),]

## school ID
head(as.character(ITA$CNTSCHID))
## student id
head(as.character(ITA$CNTSTUID))

## grade
table(ITA$ST001D01T)
# 0: prima superiore, 1: seconda, 2: terza
grade = ITA$ST001D01T - 9
table(grade)
# gender: male = 0, female = 1
table(ITA$ST004D01T)
gender =rep(0, dim(ITA)[1])
for(i in 1: dim(ITA)[1]){
  if(ITA$ST004D01T[i]==1) gender[i]=1
    else if(ITA$ST004D01T[i]==2) gender[i]=0
}
table(gender)
## immigrant status: 0 is native, 1 and 2 are first ad second generation respectively
table(ITA$IMMIG)
immig = rep(NA, dim(ITA)[1])
for(i in 1: dim(ITA)[1]){
  if(is.na(ITA$IMMIG[i])==F){
    if(ITA$IMMIG[i]==1) immig[i]= 0
    else if(ITA$IMMIG[i]==2) immig[i]= 1
    else if(ITA$IMMIG[i]==3) immig[i]= 1
  }
}
table(immig)
## school climate
summary(ITA$BULLIED)
summary(ITA$SCHRISK)
head(ITA$FEELSAFE)
## sense of belonging to school
summary(ITA$BELONG)
## support of teacher
summary(ITA$TEACHSUP)
## support of family
# Students’ ratings of how often their parents or someone else in their family engaged in a range of
# behaviours indicative of family support (e.g., “Discuss how well you are doing at school”, “Spend time just
#                                          talking with you”)
summary(ITA$FAMSUP)
# Students’ frequency ratings of how often someone in their family provided specific kinds of learning support
# (e.g., “Help me create a learning schedule”; “Help me access learning materials online”) 
summary(ITA$FAMSUPSL)
## Students’ frequency ratings of how often they engaged in behaviours 
## indicative of effort and persistence in mathematics
summary(ITA$MATHPERS)
## Mathematics anxiety
summary(ITA$ANXMAT)
## ESCS
summary(ITA$ESCS)
## STUDY
## mmins (learning time in minutes per maths per week)
table(ITA$ST296Q01JA)
## tmins (learning time in minutes per all subjects per week)
table(ITA$ST296Q04JA)
# 1  = "Up to 30 minutes a day"
# 2  = "More than 30 minutes and up to 1 hour a day"
# 3  = "More than 1 hour and up to 2 hours a day"
# 4  = "More than 2 hours and up to 3 hours a day"
# 5  = "More than 3 hours and up to 4 hours a day"
# 6  = "More than 4 hours a day"
# Exercising or practising a sport before or after school
# Values on this index range from 
# 0 (noexercise or sports) to 10 (10 or more times exercise or sport a per week). 
table(ITA$EXERPRAC)
# Studying for school or homework before or after school
# Values on this index range from 0 (no
# studying) to 10 (10 or more times of studying per week). 
table(ITA$STUDYHMW)

## EDUCATION AT HOME
## highest level of schooling of mum
table(ITA$MISCED,useNA = "always")
misced=ITA$MISCED
table(misced,useNA = "always")
## dad
table(ITA$FISCED, useNA = "always")
fisced=ITA$FISCED
## both
table(ITA$HISCED,useNA = "always")
hisced=ITA$HISCED
table(hisced)
## both in years
summary(ITA$PAREDINT)
# mate = media dei vari PV MATH
mate=rowMeans(ITA[,1167:1176], na.rm = T)

# NON UTILI AL NOSTRO SCOPO
# ## video_games
# table(ITA$IC171Q06JA,useNA = "always")
# video_games = ifelse(ITA$IC171Q06JA==6,0,ifelse(is.na(ITA$IC171Q06JA), NA, 1))
# table(video_games, useNA = "always")
# ## internet 
# internet1=ITA$ICTAVHOM
# internet2=ITA$ICTQUAL
# ## cooperation and teamwork disposition
# summary(ITA$COOPAGR)
# ## SELF PERCEPTION
# summary(ITA$PERSEVAGR)
# summary(ITA$ASSERAGR)
# summary(ITA$EMPATAGR)
# summary(ITA$CURIOAGR)
# summary(ITA$STRESAGR)
# summary(ITA$EMOCOAGR)


##### DAL QUESTIONARIO/DATASET DELLE SCUOLE #####
# Altre informazioni relative alla scuola del ragazzo
ITAS = data_school[which(data_school$CNTRYID==380),]
## NOTA BENE: un dato per ogni scuola
library(dplyr)
n_distinct(ITAS$CNTSCHID)
length(ITAS$CNTSCHID)
# Mathematics extension courses offered at school (three different categories)
# Schools that
# responded “Yes” to offering additional mathematics courses without differentiation based on prior
# achievement (SC181Q03JA) and “No” to offering enrichment (SC181Q01JA) and remedial (SC181Q02JA)
# mathematics classes were assigned a ‘1’. Schools that responded “Yes” to offering either enrichment
# mathematics lessons or remedial mathematics lessons were assigned a ‘2’. Schools that responded “Yes”
# to offering both enrichment and remedial mathematics classes were assigned a ‘3’.
table(ITAS$MATHEXC)
#Mathematics-related extra-curricular activities at school
table(ITAS$MACTIV)
# Computers connected to the Internet (RATCMP2)
# School principals were asked in SC004 to report the number of desktop or laptop computers at their school
# that are connected to the Internet. The index of computers connected to the Internet (RATCMP2) is the
# ratio of the number of desktop or laptop computers available for 15-year-olds for educational purposes
# (SC004Q02TA) to the number of these computers that are connected to the Internet (SC004Q03TA).
table(ITAS$RATCMP2)
# School size
summary(ITAS$SCHSIZE)
hist(ITAS$SCHSIZE)
#Student-mathematics teacher ratio 
summary(ITAS$SMRATIO)
hist(ITAS$SMRATIO)
# School selectivity (tre classi, la selettività va salire con l'indice)
# (interessante la spiegazione sul technical report per capirne meglio il significato)
table(ITAS$SCHSEL)

MACTIV = rep(NA, dim(ITA)[1])
SCHSIZE = rep(NA, dim(ITA)[1])
SMRATIO = rep(NA, dim(ITA)[1])
SCHSEL = rep(NA, dim(ITA)[1])
for(i in 1: dim(ITA)[1]){
  for(j in 1: dim(ITAS)[1])
  if (ITAS$CNTSCHID[j] == ITA$CNTSCHID[i]){
    MACTIV[i] = ITAS$MACTIV[j]
    SCHSIZE[i] = ITAS$SCHSIZE[j]
    SMRATIO[i] = ITAS$SMRATIO[j]
    SCHSEL[i] = ITAS$SCHSEL[j]
  }
}






##### CREAZIONE DATASET #####
student_ita = data.frame(as.character(ITA$CNTSCHID), as.character(ITA$CNTSTUID), grade, 
                         gender,immig,
                         
                         ITA$SCHRISK, ITA$BULLIED, ITA$FEELSAFE, ITA$BELONG,  
                         ITA$TEACHSUP, ITA$ANXMAT, 
                         
                         ITA$MATHPERS, ITA$ST296Q01JA, ITA$ST296Q04JA, ITA$EXERPRAC, ITA$STUDYHMW,
                         
                         ITA$PAREDINT, ITA$ESCS, ITA$FAMSUP, ITA$FAMSUPSL,
                         
                         ITA$PV1MATH, ITA$PV2MATH,ITA$PV3MATH,ITA$PV4MATH,ITA$PV5MATH,
                         ITA$PV6MATH,ITA$PV7MATH,ITA$PV8MATH,ITA$PV9MATH,ITA$PV10MATH, mate,
                         
                         MACTIV, SCHSIZE, SMRATIO, SCHSEL)

colnames(student_ita)=c(# caratteristiche personali
                        "school_ID", "stud_ID", "grade", 
                        "gender","immig",
                        
                        # ambiente scolastico
                        "SCHRISK", "BULLIED", "FEELSAFE", "BELONG", 
                        "TEACHSUP", "ANXMAT",
                        
                        # impegno individuale nello studio e vita fuori da scuola
                        "MATHPERS", "math_time", "study_time", "EXERPRAC", "STUDYHMW",
                        
                        # background familiare
                        "PAREDINT", "ESCS", "FAMSUP", "FAMSUPSL",
                        
                        # voti in matematica
                        "PV1MATH", "PV2MATH","PV3MATH","PV4MATH","PV5MATH",
                        "PV6MATH","PV7MATH","PV8MATH","PV9MATH","PV10MATH", "mate",
                        
                        # caratteristiche della scuola
                        "MACTIV", "SCHSIZE", "SMRATIO", "SCHSEL")

# solo 1^, 2^ e 3^ superiore
student_ita = student_ita[which(grade==0 | grade==1 | grade==2),]
write.table(student_ita, file='stuita.txt')



