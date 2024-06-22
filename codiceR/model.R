getwd()
setwd("C:/Users/user/Desktop/Inferenza")

rm(list=ls())
graphics.off()
cat("\014")

library(car)
library(ellipse)
library(leaps)
library(MASS)
library(GGally)
library(BAS)
library(rgl)
library(dplyr)
library(data.table)
library(ggplot2)
library(corrplot)
library(RColorBrewer)
library(AID)
library(onewaytests)
library(nortest)
library(boot)
library(tidyverse)
library(dplyr)

#### NOTE ####

# QUESTIONI APERTE

# scelta: punti di leva, residui stand/stud, cook (sulla base di R^2_adj e della normalità dei residui?)

# estrapolare delle caratteristiche degli outliers per giustificarne la rimozione?

# normalità dopo la rimozione dei punti influenti (leva belli e cook meh)

# presentazione

# SNIPPETS UTILI
# ggpairs(data=..., lower=list(continuos=wrap("points", alpha=0.5, size=0.1)))




#### IMPORTAZIONE DEI DATI ####
d = read.table("stuita.txt",header=TRUE)
d=d[,-c(1:2, 21:30)] # rimuovo le prime due colonne (ID inutili) e risposte non utilizzate
dim(d)
head(d)
summary(d)
str(d) # controllo tipo di ogni covariata

# Riempimento degli NA (con media e moda)
# attach(d)
# d <- d %>% replace_na(list(immig = 0,
#                            SCHRISK = -0.4624,
#                            BULLIED = mean(d$BULLIED, na.rm = TRUE),
#                            FEELSAFE = mean(d$FEELSAFE, na.rm = TRUE),
#                            BELONG = mean(d$BELONG, na.rm = TRUE),
#                            TEACHSUP = mean(d$TEACHSUP, na.rm = TRUE),
#                            ANXMAT = mean(d$ANXMAT, na.rm = TRUE),
#                            MATHPERS = mean(d$MATHPERS, na.rm = TRUE),
#                            math_time = 1,
#                            study_time = 3,
#                            EXERPRAC = 0,
#                            STUDYHMW  = 5,
#                            PAREDINT  = 16,
#                            ESCS = mean(d$ESCS, na.rm = TRUE),
#                            FAMSUP = mean(d$FAMSUP, na.rm = TRUE),
#                            FAMSUPSL = mean(d$FAMSUPSL, na.rm = TRUE),
#                            MACTIV = 3,
#                            SCHSIZE = ceiling(mean(d$SCHSIZE, na.rm = TRUE)),
#                            SMRATIO = mean(d$SMRATIO, na.rm = TRUE),
#                            SCHSEL =  3))
# Omissione degli na
d <- na.omit(d)
n = dim(d)[1]




#### VARIABILI CATEGORICHE ####

# 1. One-way ANOVA 
# sfruttiamo l'ANOVA per capire come raggruppare le variabili categoriche


# math_time
table(d$math_time)
boxplot(d$mate ~ d$math_time, xlab = 'math_time', ylab = 'mate',
        main = 'mate according to math_time', 
        col = brewer.pal(n_distinct(d$math_time), 'Set2'))
abline(h = mean(d$mate))
# Ipotesi
# Normalità
tapply(d$mate, d$math_time, function(x) (shapiro.test(x)$p))
# Omoschedasticità
tapply(d$mate, d$math_time, var)
bartlett.test(d$mate, d$math_time)
leveneTest(d$mate, factor(d$math_time)) # meno sensibilità alla normalità
# Stessa media?
summary(lm(d$mate ~ as.factor(d$math_time)))
anova(lm(d$mate ~ as.factor(d$math_time)))
summary(aov(d$mate ~ as.factor(d$math_time)))
kw.test(mate ~ as.factor(math_time), d, alpha = 0.001, na.rm = TRUE, verbose = TRUE)
# coerentemente con quanto appare nel boxplot, è rifiutata l'ipotesi che le 
# medie di tutte le classi coincidano: facciamo l'ANOVA solo su classi 2,3 e 4,5:
dm=d[which(d$math_time == 2 | d$math_time ==3),]
anova(lm(dm$mate ~ as.factor(dm$math_time)))
summary(aov(dm$mate ~ as.factor(dm$math_time)))
kw.test(mate ~ as.factor(math_time), dm, alpha = 0.1, na.rm = TRUE, verbose = TRUE)
# => 2-3 hanno la stessa media
dm=d[which(d$math_time == 4 | d$math_time == 5),]
anova(lm(dm$mate ~ as.factor(dm$math_time)))
summary(aov(dm$mate ~ as.factor(dm$math_time)))
kw.test(mate ~ as.factor(math_time), dm, alpha = 0.1, na.rm = TRUE, verbose = TRUE)
# => 4-5 hanno la stessa media

# study_time
table(d$study_time)
boxplot(d$mate ~ d$study_time, xlab = 'study_time', ylab = 'mate',
        main = 'mate according to study_time', 
        col = brewer.pal(n_distinct(d$math_time), 'Set2'))
abline(h = mean(d$mate))
# Ipotesi
# Normalità
tapply(d$mate, d$study_time, function(x) (shapiro.test(x)$p))
# Omoschedasticità
tapply(d$mate, d$study_time, var)
bartlett.test(d$mate, d$study_time)
leveneTest(d$mate, factor(d$study_time)) # meno sensibilità alla normalità
# Stessa media?
summary(lm(d$mate ~ as.factor(d$study_time)))
anova(lm(d$mate ~ as.factor(d$study_time)))
summary(aov(d$mate ~ as.factor(d$study_time)))
kw.test(mate ~ as.factor(study_time), d, alpha = 0.001, na.rm = TRUE, verbose = TRUE)
# ANOVA solo su classi 2,3,4,5:
dm=d[which(d$study_time == 2 | d$study_time == 3 | d$study_time == 4 | d$study_time == 5),]
anova(lm(dm$mate ~ as.factor(dm$study_time)))
summary(aov(dm$mate ~ as.factor(dm$study_time)))
kw.test(mate ~ as.factor(study_time), dm, alpha = 0.1, na.rm = TRUE, verbose = TRUE)
# => 2-5 hanno la stessa media


# EXERPRAC
table(d$EXERPRAC)
boxplot(d$mate ~ d$EXERPRAC, xlab = 'EXERPRAC', ylab = 'mate',
        main = 'mate according to EXERPRAC', 
        col = brewer.pal(n_distinct(d$EXERPRAC), 'Set2'))
abline(h = mean(d$mate))
# Normalità
tapply(d$mate, d$EXERPRAC, function(x) (shapiro.test( x[sample(length(x),100)] )$p))
# Omoschedasticità
tapply(d$mate, d$EXERPRAC, var)
bartlett.test(d$mate, d$EXERPRAC)
leveneTest(d$mate, factor(d$EXERPRAC)) # meno sensibilità alla normalità
# Stessa media?
summary(lm(d$mate ~ as.factor(d$EXERPRAC)))
anova(lm(d$mate ~ as.factor(d$EXERPRAC)))
summary(aov(d$mate ~ as.factor(d$EXERPRAC)))
kw.test(mate ~ as.factor(EXERPRAC), d, alpha = 0.001, na.rm = TRUE, verbose = TRUE)

# altri rimossi successivamente
boxplot(d$mate ~ d$MACTIV)
boxplot(d$mate ~ d$SCHSEL)


# 2. Raggruppamenti ulteriori per variabili categoriche
# EXERPRAC (3 categorie)
# 0: nessun allenamento
d[which(d$EXERPRAC >=1 & d$EXERPRAC <=5),]$EXERPRAC = 1 # 1: da 1 a 5 volte
d[which(d$EXERPRAC >=6 & d$EXERPRAC <=10),]$EXERPRAC = 2 # 2: da 6 a 10 volte
# math_time (3 categorie)
# le performance tendono ad abbassarsi quando math_time sale
d[which(d$math_time <= 3),]$math_time = 0 # 0: fino a 2h di studio al giorno
d[which(d$math_time == 4 | d$math_time == 5),]$math_time = 1 # 1: tra 2h e 4h 
d[which(d$math_time == 6),]$math_time = 2 # 2: più di 4h
# study_time (3 categorie)
# d[which(d$study_time == 1),]$study_time = 0 
# d[which(d$study_time >= 2),]$study_time = 1 
d[which(d$study_time == 1),]$study_time = 0 # 0: meno di 30' di studio al giorno
d[which(d$study_time >= 2 & d$study_time <= 5 ),]$study_time = 1 # 1: tra 30' e 4h 
d[which(d$study_time == 6),]$study_time = 2 # 2: più di 4h

# 3. Conversione in factor delle variabili
d$grade = factor(d$grade, labels = c("1^", "2^", "3^"))
d$gender = factor(d$gender, labels = c("M", "F"))
d$immig = factor(d$immig, labels = c("N", "Y"))
d$EXERPRAC = factor(d$EXERPRAC, labels = c("no", "poco", "tanto"))
d$SCHSEL = factor(d$SCHSEL, labels = c("no", "1factor", "2factors"))
d$study_time = factor(d$study_time)
d$math_time = factor(d$math_time)




#### PRIMO MODELLO LINEARE ####

# DOMANDA DI RICERCA
# spiegare i risultati del test di matematica (mate) in termini di:
# - anno delle superiori frequentato dallo studente (1^, 2^, 3^)
# - impegno individuale rivolto allo studio + ansia
# - ambiente scolastico + ansia
# - contesto familiare
# (escludendo gli aspetti di self-perception e di ICT Familiarity)
# Rimozione delle variabili che non riguardano questi fattori

dev.new()
annotations <- data.frame(
  x = c(round(as.numeric(quantile(d$mate,0.25)), 2), 
        round(mean(d$mate), 2), 
        round(as.numeric(quantile(d$mate,0.75)), 2)),
  y = c(600, 650, 600),
  label = c("1st Qu.:", "Mean:", "3rd Qu.:")
) 
ggplot(d, aes(mate)) +
  geom_histogram(color = "#000000", fill="#4444bc") +
  geom_vline(aes(xintercept = mean(mate)), color = "#000000", size = 1.25) +
  geom_vline(aes(xintercept = mean(mate) + sd(mate)), color = "#000000", size = 1, linetype = "dashed") +
  geom_vline(aes(xintercept = mean(mate) - sd(mate)), color = "#000000", size = 1, linetype = "dashed") +
  geom_text(data = annotations, aes(x = x+c(-100,3,100), y = y, label = paste(label, x)), size = 7) +
  theme_classic()

# 1. Selezione prelimirare

m1 = lm(mate ~ ., data = d)
summary(m1)
# Rimozione SCHSEL, SCHSIZE (poco commentabili), BELONG (non attinenti 
# alla domanda di ricerca), MATHPERS (scontata relazione con mate)
# Rimozione PAREDINT (perchè problematico con ESCS, preferisco quest'ultimo)
# Rimozione di FEELSAFE perché simile a SCHRISK
# Rimozione di STUDYHMW perché simile a study_time
d = d[,-c(6,7,10,14,15,21,23)]
# Visualizzazione correlazione tra variabili
temp = d[,c("mate", "ESCS", "ANXMAT", "FAMSUP", "FAMSUPSL", "SMRATIO", 
            "SCHRISK", "BULLIED", "TEACHSUP")];
x = temp
cor(x)
dev.new()
corrplot(cor(x), method = 'color', addCoef.col = 'black')
corrplot(cor(temp), method = 'number')
heatmap(cor(x), symm=TRUE, keep.dendro=F)

m1 = lm(mate ~ ., data = d)
summary(m1)
# Validazione
m = m1
# omoschedasticità
plot(m$fitted.values, m$residuals, xlab = "Fitted values", ylab = "Residuals", 
     main = "Residuals vs Fitted Values", pch = 16) 
abline(h=0, col='red')
plot(m, which=1)
# normalità
dev.new()
par(mfrow=c(2,1))
qqnorm(m$residuals, ylab = "Residuals", pch = 16)
qqline(m$residuals, col='red')
hist(m$residuals/summary(m)$sigma, breaks = 20, probability = TRUE, col = 'lavender',
     main = 'Residuals', xlab = "", ylab = "density")
x=0
for (i in 1:1000) {
  x=x+shapiro.test(m$residuals[sample(length(m$residuals),5000)])$p
}
x/1000
ks.test(m$residuals, y="pnorm", mean=0, sd=sd(m$residuals))
ad.test(m$residuals)
# Nessun problema con la collinearità
vif(m1)



#### PUNTI INFLUENTI (outliers e leverages) ####

# 1. Leverages (matrice di proiezione)
g = m1
lev = hatvalues(g)
sum(lev)
p = g$rank
n = dim(d)[1]
soglia = 2*p/n
# impatto di leva
sum(lev[lev>soglia])
# modello senza i punti di leva
gl = lm(mate ~ ., data = d, subset=(lev<=soglia))
summary(gl)
summary(g)

# 2. Residui standardizzati
gs = summary(g)
res_std = g$residuals/gs$sigma
# modello senza gli outliers
g_std = lm(mate ~ ., 
           data = d, subset=(abs(res_std)<=2))
summary(g_std)
# variazione relativa dei coefficienti
abs((g_std$coef - g$coef)/g$coef)

# 3. Residui studentizzati
gs = summary(g)
res_stu = g$residuals/(gs$sigma * sqrt(1-lev))
# alternativamente res_stu = rstandard(g)
g_stu = lm(mate ~ ., 
           data = d, subset=(abs(res_stu)<=2))
summary(g_stu)
abs((g_stu$coef - g$coef)/g$coef)

# 4. Distanza di Cook
Cdist = cooks.distance(g)
gc = lm(mate ~ ., 
        data = d, subset=Cdist<=4/(n-p))
summary(gc)
abs((gc$coef - g$coef)/g$coef)

# 5. Validazione
m = gc
# omoschedasticità
plot(m$fitted.values, m$residuals, xlab = "Fitted values", ylab = "Residuals", 
     main = "Residuals vs Fitted Values", pch = 16) 
abline(h=0, col='red')
plot(m, which=1)
# normalità
dev.new()
par(mfrow=c(2,1))
qqnorm(m$residuals, ylab = "Residuals", pch = 16)
qqline(m$residuals, col='red')
hist(m$residuals/summary(m)$sigma, breaks = 20, probability = TRUE, col = 'lavender',
     main = 'Residuals', xlab = "", ylab = "density")
x=0
for (i in 1:1000) {
  x=x+shapiro.test(m$residuals[sample(length(m5$residuals),5000)])$p
}
x/1000
ks.test(m$residuals, y="pnorm", mean=0, sd=sd(m$residuals))
ad.test(m$residuals)

# 6. plot
# punti di leva
watchout_ids_lev = which(lev>soglia)
plot(g$fitted.values, lev, xlab = "Fitted values", ylab = "Leverages", 
     main = "Plot of Leverages", pch = 16, col = 'black')
abline(h = soglia, lty = 2, col = 'red')
# confronto: outliers con residui standardizzati e punti leva
watchout_ids_rstd = which(abs(res_std)>2)
watchout_rstd = res_std[ watchout_ids_rstd ]
plot(g$fitted.values, res_std, xlab = "Fitted values", 
     ylab = "Standardized Residuals", main = "Standardized Residuals")
abline(h = c(-2,2), lty = 2, col = 'orange')
points(g$fitted.values[watchout_ids_rstd], 
       res_std[watchout_ids_rstd], col = 'red', pch = 16)
points(g$fitted.values[watchout_ids_lev], 
       res_std[watchout_ids_lev], col = 'orange', pch = 16)
legend('topright', col = c('red','orange'), 
       c('Standardized Residuals', 'Leverages'), pch = rep(16, 2), bty = 'n')
# confronto: outliers con residui studentizzati e punti leva
watchout_ids_stud = which(abs(res_stu)>2)
watchout_stud = res_stu[watchout_ids_stud]
plot( g$fitted.values, res_stu, ylab = "Studentized Residuals", main = "Studentized Residuals", pch = 16 )
points( g$fitted.values[watchout_ids_stud], 
        res_stu[watchout_ids_stud], col = 'pink', pch = 16 )
points( g$fitted.values[watchout_ids_lev], 
        res_stu[watchout_ids_lev], col = 'orange', pch = 16 )
abline( h = c(-2,2), lty = 2, col = 'orange' )
legend('topright', col = c('pink','orange'), 
       c('Studentized Residual', 'Leverages'), pch = rep( 16, 3 ), bty = 'n' )
# tutti insieme
watchout_ids_Cdist = which(Cdist>4/(n-p)) 
watchout_Cdist = Cdist[watchout_ids_Cdist]
dev.new()
par(mfrow = c(2,2))
plot(g$fitted.values, res_std, pch = 16, xlab = "Fitted values", 
     ylab = "Standardized Residuals", main = "Standardized Residuals")
points(g$fitted.values[watchout_ids_rstd], 
       res_std[watchout_ids_rstd], col = 'red', pch = 16)
plot(g$fitted.values, Cdist, pch = 16, xlab = 'Fitted values', 
     ylab = "Cook's Distance", main = 'Cooks Distance')
points(g$fitted.values[watchout_ids_Cdist], Cdist[watchout_ids_Cdist], 
       col = 'green', pch = 16)
plot(g$fitted.values, res_stu, pch = 16, xlab = 'Fitted values', 
     ylab = 'Studentized Residuals', main = 'Studentized Residuals')
points(g$fitted.values[watchout_ids_stud], res_stu[ watchout_ids_stud], 
       col = 'pink', pch = 16)
plot(g$fitted.values, lev, pch = 16, xlab = 'Fitted values', 
     ylab = 'Leverages', main = 'Leverages')
points(g$fitted.values[watchout_ids_lev], lev[watchout_ids_lev],
       col = 'orange', pch = 16)


# 7. Caratteristiche punti influenti
colors = rep('black', nrow(d))
colors[Cdist > 4/(n-p)] = 'red'
dev.new()
pairs( d[,c("mate", "ESCS", "ANXMAT", "FAMSUP", "FAMSUPSL", "SMRATIO", 
            "SCHRISK", "BULLIED", "TEACHSUP")],
       pch = 16, col = colors, cex = 1 + 0.5 * as.numeric(colors != 'black'))

# estraggo il dataset dei punti rimossi in base alla distanza di Cook:
d1 = filter(d,Cdist > 4/(n-p))
summary(d)
summary(d1)
summary(lm(mate ~ ., data = d))
summary(lm(mate ~ ., data = d1))
summary(lm(mate ~ grade, data = d1))
# per d1 il coefficiente di grade2 ? NEGATIVO, mentre per d ? positivo
par(mfrow = c(1,2))
boxplot(d$mate ~ d$grade)
boxplot(d1$mate ~ d1$grade)
par(mfrow = c(1,2))
hist(d$mate)
summary(d$mate)
hist(d1$mate)
summary(d1$mate)

# il coefficiente subisce una variazione significativa dopo la rimozione dei 
# punti influenti secondo la distanza di Cook
par(mfrow = c(1,2))
boxplot(d$mate ~ d$math_time)
boxplot(d1$mate ~ d1$math_time)
par(mfrow = c(1,2))
pie(table(d$math_time))
pie(table(d1$math_time))


#### MODELLO LINEARE FINALE ####

# 1. Selezione senza interazioni
summary(gc)
# elimino TEACHSUP perché poco significativa
d=d %>% select(-TEACHSUP)
# elimino immig perché poco significativa
d=d %>% select(-immig)
# elimino FAMSUP perchè poco significativa
d=d %>% select(-FAMSUP)
m1 = lm(mate ~ ., data = d, subset=(Cdist<=4/(n-p)))
# Nessun problema con la collinearità
vif(m1)

m0 = lm(mate ~ 1, data = d, subset=(Cdist<=4/(n-p)))
summary(m0)

# Backward selection
# a) Versione stepwise AIC
step(m1, direction = "backward", trace = F, k = 2)
# b) Versione stepwise BIC
step(m1, direction = "backward" , trace = F, k = log(n))
# c) Versione brute force R^2_adj
x = model.matrix(m1)[,-1]
y = d$mate
adjr = leaps(x, y, method = "adjr2") # R^2_adj come funzione obiettivo
# Seleziono il mdello tra quelli con il migliore R^2_adj
maxadjr(adjr,6)

# Forward selection 
# a) Versione stepwise AIC
step(m0, direction = "forward", scope=formula(m1), trace = F, k = 2)
# b) Versione stepwise BIC
step(m0, direction = "forward", scope=formula(m1), trace = F, k = log(n))

# Forward/Backward selection 
# a) Versione stepwise AIC
step(m0, direction = "both", scope=formula(m1), trace = F, k = 2)
# b) Versione stepwise BIC
step(m0, direction = "both", scope=formula(m1), trace = F, k = log(n))


# 2. Modello finale

# Proposta finale (both BIC senza interazioni non rimuove alcuna variabile)
# rimuoviamo anche BULLIED
d=d %>% select(-BULLIED)
m5 = lm(mate ~ ., data = d, subset=(Cdist<=4/(n-p)))
summary(m5)

# Validazione
# omoschedasticità
dev.new()
plot(m5$fitted.values, m5$residuals, xlab = "Fitted values", ylab = "Residuals", 
     main = "Residuals vs Fitted Values", pch = 16) 
abline(h=0, col='red')
plot(m5, which=1)
# normalità
dev.new()
par(mfrow=c(1,2))
qqnorm(m5$residuals, ylab = "Residuals", pch = 16)
qqline(m5$residuals, col='red')
hist( m5$residuals/summary(m5)$sigma, breaks = 20, probability = TRUE, col = 'lavender',
      main = 'Residuals', xlab = "", ylab = "density")
x=0
for (i in 1:1000) {
  x=x+shapiro.test(m5$residuals[sample(length(m5$residuals),5000)])$p
}
x/1000
ks.test(m5$residuals, y="pnorm", mean=0, sd=sd(m5$residuals))
ad.test(m5$residuals)
# Non ci sono problemi di collinearità
vif(m5)



#### BOX-COX ####
b = boxcox(gc)
best_lambda = b$x[which.max(b$y)]
mb = lm((mate ^ best_lambda - 1)/best_lambda ~ ., data = d[which(Cdist<=4/(n-p)),])
summary(mb)
# Validazione
# omoschedasticità
plot(mb$fitted.values, mb$residuals, xlab = "Fitted values", ylab = "Residuals", 
     main = "Residuals vs Fitted Values", pch = 16) 
abline(h=0, col='red')
plot(mb, which=1)
# normbalità
dev.new()
par(mfrow=c(2,1))
qqnorm(mb$residuals, ylab = "Residuals", pch = 16)
qqline(mb$residuals, col='red')
hist(mb$residuals/summary(mb)$sigma, breaks = 20, probability = TRUE, col = 'lavender',
     main = 'Residuals', xlab = "", ylab = "density")
shapiro.test(mb$residuals[sample(length(mb$residuals),500)])
ks.test(mb$residuals, y="pnorm", mean=0, sd=sd(mb$residuals))




#### CROSS VALIDATION e PREVISONE #####
# glm deve essere uguale a g_stu
dc = d[which(Cdist<=4/(n-p)),]
g = m5
glm = glm(mate ~ ., data = dc)
summary(glm)
# Intervalli di previsione per mate|X=x
# naturalmente per la complessità del fenomeno questo modello 
# non è efficace per questo scopo, al massimo può essere utile 
# ad escludere risultati improbabili
sample = dc[sample(dim(dc)[1],5),]
predict(g, sample, interval = "prediction")
# Intervalli di confidenza per E[mate|X=x] (più efficace a questo scopo)
predict(g, sample, interval = "confidence", se = T)
# Cross validation
# SD di mate
sd(d$mate)
# (sqrt di) MSE sul training set
mean(glm$residuals**2)
# K-fold
cv.err = cv.glm(dc, glm, K = 10)
cv.err$delta[1]
# leave-one-out
n = dim(dc)[1]
cv.err = cv.glm(dc, glm, K = n)
cv.err$delta[1]
# Confronto miglioramento di precisione
var(d$mate)/cv.err$delta[1]
# il risultato dal laboratorio di riferimento è 2.725978, 
# ma comunque circa 1.4 non è male per il tipo di problema scelto
