# Modèle de régression linéaire multiple
# Tests d'hypothèses
# Algorithmes de sélection de modèles (de variables)
# Estimation de l'erreur théorique (de prévision) d'un modèle par validation croisée

#Vider la mémoire
rm(list = ls())

library(ISLR)
library(lmtest)
library(leaps)
library(glmulti)
library(lattice)
library(boot)
library(glmnet)
library(gglasso)


Childhood.Respiratory.Disease <- read.table("/Users/lutherleckomba/Desktop/MASTER1\ UNIV\ REIMS/S2/machine\ learning/PROJET/fev.txt", header = TRUE, sep = "\t")
str(Childhood.Respiratory.Disease)
View(Childhood.Respiratory.Disease)

# Vérifier s'il y a des données manquantes dans le dataset
any(is.na(Childhood.Respiratory.Disease))

# Construction Modèle de RLM de la variable FEV en fonction de toutes les variables de la base de données Childhood.Respiratory.Disease
modele.RLM <- lm(formula = FEV ~ ., data = Childhood.Respiratory.Disease)
attributes(modele.RLM)
summary(modele.RLM)
summary(modele.RLM)$coefficients


summary(modele.RLM)
#Ordonner les variables explicatives numériques selon les valeurs 
#des p_values croissantes (du test de Student)
vect.pvalues.Student <- summary(modele.RLM)$coefficients[,"Pr(>|t|)"]
vect.pvalues.Student
#On supprime la p_value de l'intercept 
vect.pvalues.Student <-vect.pvalues.Student[2:length(vect.pvalues.Student)] 
#Variables explicatives ordonnées, de la plus significative à la moins significative
sort(vect.pvalues.Student) 


#Ordonner les variables explicatives numériques/qualitatives 
#selon les valeurs des P_values croissantes du test de Fisher
tests.Fisher <- anova(modele.RLM)
tests.Fisher
str(tests.Fisher)
m <- nrow(tests.Fisher)
vect.pvalues.Fisher <- tests.Fisher[1:m-1,"Pr(>F)"] #Extrait le vecteur des p_values
names(vect.pvalues.Fisher) <- rownames(tests.Fisher[1:m-1,])
sort(vect.pvalues.Fisher) #Attention pour les variables explicatives qualitatives ayant
#plus de deux modalités (on compare à dimension égale)
#dans ce cas il faudrait faire comme suit
XX <- model.matrix(FEV ~., data = Childhood.Respiratory.Disease)[,-c(1)] #Cette fonction construit la matrice de design en remplaçant 
#chacune des variables qualitatives par les indicatrices 
#de ses modalités (la première modalité est supprimée)
#on supprime la première colonne correspondant à l'intercept

View(model.matrix(FEV ~., data = Childhood.Respiratory.Disease))

View(XX)
Childhood.Respiratory.Disease.num.data <- cbind(FEV = Childhood.Respiratory.Disease[,"FEV"],XX)
Childhood.Respiratory.Disease.num.data <- as.data.frame(Childhood.Respiratory.Disease.num.data) #Bd constituée que de variables numériques
View(Childhood.Respiratory.Disease.num.data)
tests.Fisher2 <- anova(lm(FEV~., data = Childhood.Respiratory.Disease.num.data))
tests.Fisher2
m <- nrow(tests.Fisher2)
vect.pvalues.Fisher2 <- tests.Fisher2[1:m-1,"Pr(>F)"] #Extrait le vecteur des p_values
names(vect.pvalues.Fisher2) <- rownames(tests.Fisher2[1:m-1,])
sort(vect.pvalues.Fisher2)

#Comparaison avec le classement selon le test de Student
sort(vect.pvalues.Fisher2)
sort(vect.pvalues.Student) #On n'obtient pas le même classement; 
#il est recommandé de retenir celui de Fisher

#########################################################################################
#########################################################################################
#### Sélection de modèles (variables) ####

## Algorithmes de recherche exhaustive ##

#La fonction glmulti() : sélection de variables explicatives numériques/qualitatives
require(glmulti)
select.modele.aic <- glmulti(FEV ~., data = Childhood.Respiratory.Disease, level = 1, 
                             fitfunction = lm, crit = "aic", plotty = FALSE, method = "h")
modele.opt.aic <- summary(select.modele.aic)$bestmodel
modele.opt.aic
anova(lm(modele.opt.aic, data = Childhood.Respiratory.Disease))
select.modele.bic <- glmulti(FEV ~., data = Childhood.Respiratory.Disease, level = 1,
                             fitfunction = lm, crit = "bic", plotty = FALSE, method = "h")
modele.opt.bic <- summary(select.modele.bic)$bestmodel
modele.opt.bic
anova(lm(modele.opt.bic, data = Childhood.Respiratory.Disease))

#La fonction glmulti() appliquée aux variables explicatives numériques 
#après transformation des variables explicatives qualitatives en quantitatives
select.modele.aic2 <- glmulti(FEV ~., data = Childhood.Respiratory.Disease.num.data, level = 1, 
                             fitfunction = lm, crit = "aic", plotty = FALSE, method = "h")
modele.opt.aic2 <- summary(select.modele.aic2)$bestmodel
modele.opt.aic2
anova(lm(modele.opt.aic2, data = Childhood.Respiratory.Disease.num.data))
select.modele.bic2 <- glmulti(FEV ~., data = Childhood.Respiratory.Disease.num.data, level = 1,
                             fitfunction = lm, crit = "bic", plotty = FALSE, method = "h")
modele.opt.bic2 <- summary(select.modele.bic2)$bestmodel
modele.opt.bic2
anova(lm(modele.opt.bic2, data = Childhood.Respiratory.Disease.num.data))

################################################################################
################################################################################



################################################################################
################################################################################
#### Evaluation de l'erreur théorique du modele sélectionné par critère BIC ####
# comparaison avec le modèle complet

## Méthode1 : L'approche de l'ensemble de validation 
#Vecteur d'indices, de taille `partie entière de (2/3)*n'; 
#les indices sont tirés de manière aléatoire sans remise dans l'ensemble {1,2, ...,n} 
n <- nrow(Childhood.Respiratory.Disease)
erreur.modele1 = NULL
erreur.modele2 = NULL
Err1 = NULL
Err2 = NULL
M <- 1000 #nombre de réplications de la méthode 1
for (i in 1:M)
{
  indices <- sample(x = n, size = trunc((2/3)*n), replace = FALSE)
  ensemble.apprentissage <- Childhood.Respiratory.Disease[indices, ]
  ensemble.validation <- Childhood.Respiratory.Disease[-indices, ]
  modele1 <- lm(formula = modele.opt.bic, data = ensemble.apprentissage) #modèle optimal selon BIC
  modele2 <- lm(formula = FEV ~ ., data = ensemble.apprentissage) #modèle complet
  valeurs.predites1 <- predict(object = modele1, newdata = ensemble.validation)
  valeurs.predites2 <- predict(object = modele2, newdata = ensemble.validation)
  erreur.modele1[i] <- mean((ensemble.validation$FEV - valeurs.predites1)^2)
  erreur.modele2[i] <- mean((ensemble.validation$FEV - valeurs.predites2)^2)
  Err1[i] <- mean(erreur.modele1[1:i])
  Err2[i] <- mean(erreur.modele2[1:i])
}
a <- min(c(min(Err1), min(Err2)))
b <- max(c(max(Err1), max(Err2)))
par(mfrow = c(1,1))
require(lattice)
xyplot(Err1+Err2 ~ 1:M, type = "l", col = c("blue", "red"), xlab = "m", 
       ylab = "Erreurs du modèle1 (bleu) et du modèle2 (rouge)")
estimation.erreur.modele1 <- Err1[M] #meilleur estimation de l'erreur théorique du modele1
estimation.erreur.modele2 <- Err2[M] #meilleur estimation de l'erreur théorique du modele2

estimation.erreur.relative.modele1 <- Err1[M]/mean((ensemble.validation$FEV)^2)*100 
estimation.erreur.relative.modele2 <- Err2[M]/mean((ensemble.validation$FEV)^2)*100 

estimation.erreur.relative.modele1
estimation.erreur.relative.modele2

print(c("Résultats des estimations par la méthode de l'ensemble de validation : ",
        paste("Estimation de l'erreur du modele1 = ", as.character(estimation.erreur.modele1)),
        paste("Estimation de l'erreur du modele2 = ", as.character(estimation.erreur.modele2)))) 

################################################################################

#### Méthode 2 K-fold CV : estimation de l'erreur des deux modèles précédents par K-fold CV, avec K = 10 ####
#Estimation de l'erreur du modele1 par la méthode  K-fold CV

modele1 <- glm(formula = modele.opt.bic,  data = Childhood.Respiratory.Disease)

modele2 <- glm(formula = FEV ~ ., data = Childhood.Respiratory.Disease)

estimation.erreur.modele1 <- cv.glm(data = Childhood.Respiratory.Disease, glmfit = modele1, 
                                    K = 10)$delta[1] 
#Estimation de l'erreur du modele2 par la méthode K-fold CV
estimation.erreur.modele2 <- cv.glm(data = Childhood.Respiratory.Disease, glmfit =  modele2, 
                                    K = 10)$delta[1] 

print(c("Résultats des estimations par 10-fold CV : ",
        paste("Estimation de l'erreur du modele1 = ", as.character(estimation.erreur.modele1)),
        paste("Estimation de l'erreur du modele2 = ", as.character(estimation.erreur.modele2)))) 

#####################################################################
#####################################################################

#### Régression ridge ####
XX <- model.matrix(FEV ~., data = Childhood.Respiratory.Disease)[,-1] #matrice de design sans l'intercept
View(XX)
require(glmnet)
reg.ridge <- glmnet(x = scale(XX), y = Childhood.Respiratory.Disease[,"FEV"], alpha = 0) #alpha=0 pour construire les modèles ridge
par(mfrow = c(1,1))
plot(reg.ridge, label = TRUE)
plot(reg.ridge, xvar = "lambda", label = TRUE, lwd = 2)
reg.cvridge <- cv.glmnet(x = scale(XX), y = Childhood.Respiratory.Disease[,"FEV"], alpha = 0)
bestlam <- reg.cvridge$lambda.min
bestlam
plot(reg.cvridge)
min(reg.cvridge$cvm) #erreur de prevision du modele ridge optimal 
coef(reg.cvridge)

#Comparaison avec le modele de RLM complet
reg.cvridge <- cv.glmnet(x = scale(XX), y = Childhood.Respiratory.Disease[,"FEV"], alpha = 0)
erreur.modele.ridge.opt <- min(reg.cvridge$cvm) #erreur de prevision du modele ridge optimal 
erreur.modele.RLM.complet <- cv.glm(data = Childhood.Respiratory.Disease, glmfit =  glm(formula = FEV ~., 
                                                              data = Childhood.Respiratory.Disease), K = 10)$delta[1]

erreur.modele.ridge.opt
erreur.modele.RLM.complet

#####################################################################
#####################################################################

#### le Lasso ####(IL FAUT NORMALISER)
reg.lasso <- glmnet(x = scale(XX), y = Childhood.Respiratory.Disease[,"FEV"], alpha = 1) # alhpa=1 pour construire le modèle lasso
par(mfrow = c(1,2))
plot(reg.lasso, label = TRUE)
plot(reg.lasso, xvar = "lambda", label = TRUE, lwd = 2)

reg.cvlasso <- cv.glmnet(x = scale(XX), y = Childhood.Respiratory.Disease[,"FEV"], alpha = 1)
bestlam <- reg.cvlasso$lambda.min
bestlam
par(mfrow = c(1,1))
plot(reg.cvlasso)
min(reg.cvlasso$cvm) #erreur de prevision du modele lasso optimal 
coef(reg.cvlasso)

#Comparaison avec le modele de RLM complet
reg.cvlasso <- cv.glmnet(x = scale(XX), y = Childhood.Respiratory.Disease[,"FEV"], alpha = 1)
erreur.modele.lasso.opt <- min(reg.cvlasso$cvm) #erreur de prevision du modele lasso optimal 
erreur.modele.RLM.complet <- cv.glm(data = Childhood.Respiratory.Disease, glmfit =  glm(formula = FEV ~., 
                                                              data = Childhood.Respiratory.Disease), K = 10)$delta[1]

erreur.modele.lasso.opt
erreur.modele.RLM.complet
