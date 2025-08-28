# Cargement des bibliothèques
library(ggplot2)
library(gridExtra)
library(corrplot)
library(dplyr)
library(GGally)
library(caret)
library(themis)
library(ade4)
library(tidymodels)
library(FactoMineR)
library(lmtest)
library(pROC)
library(randomForest)
library(reshape2)
#=======================                         ====================
                          #Chargement des données
#=======================                         ====================
# Chargement du jeu de données
df <- read.csv("~/Data_Mining Project/wine+quality/winequality-white.csv", sep=";")

# Aperçu du jeu de données
head(df)

# Structure des variables
str(df) 

# Dimension du jeu de données
dim(df)

# noms des Variables
colnames(df)

# Aperçu complet
View(df)
#==============                                       ================
#                    Nettoyage du jeu de données 
#==============                                       ================

# Données manquantes
colSums(is.na.data.frame(df)) # Pas de données manquante

# Données dupliquées
nb_data_duplicated <- sum(duplicated(df))
nb_data_duplicated

# Suppression des observations identiques
df <- df[!duplicated(df),]

# Données extrêmes
  # Calcul des quartiles et de l'IQR
nb_point_extreme <- function(x){
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  IQR <- Q3 - Q1
  # Définir les bornes des outliers
  borne_inf <- Q1 - 1.5 * IQR
  borne_sup <- Q3 + 1.5 * IQR
  # Détection des outliers
  outliers <- x[x < borne_inf | x > borne_sup]
  # Nombre d'outliers
  length(outliers)
}
points_extreme <- data.frame(nb_point_extreme(df$fixed.acidity),
                             nb_point_extreme(df$volatile.acidity),
                             nb_point_extreme(df$citric.acid),
                             nb_point_extreme(df$residual.sugar),
                             nb_point_extreme(df$chlorides),
                             nb_point_extreme(df$free.sulfur.dioxide),
                             nb_point_extreme(df$total.sulfur.dioxide),
                             nb_point_extreme(df$density),
                             nb_point_extreme(df$pH),
                             nb_point_extreme(df$sulphates),
                             nb_point_extreme(df$alcohol),
                             nb_point_extreme(df$quality))

# Nombre de données extrêmes par variable
View(points_extreme)
# Nombre de données extrêmes
somme <- sum(points_extreme)
# Nombre total de données extrêmes 
somme

# ================                      ================
#                    Analyse univariée 
#================                       ================
  # Résumé statistique
summary(df)

# Histogrammes et boxplot des variables


  # fixed.acidity
#%%%%%%%%%%%%%%%%% Histogramme %%%%%%%%%%%%%%%%%%
hist(df$fixed.acidity,main ="Histogramme de fixed.acidity",
     xlab = "fixed.acidity",ylab = "Nombre",col="pink",freq=F)
lines(density(df$fixed.acidity),col="red",lwd=2)

# Ajout de la mediane et la moyenne
abline(v=median(df$fixed.acidity,col="darkgreen",lwd=2,lty=2))
abline(v=mean(df$fixed.acidity),col="orange",lwd=2,lty=2)

# Légende
legend("topright", legend = c("Densité", "Médiane", "Moyenne"),
       col = c("red", "darkgreen", "orange"), lwd = 2, lty = c(1, 2, 2))

#%%%%%%%%%%%%%%%% Boxplot #%%%%%%%%%%%%%%%%%
boxplot(df$fixed.acidity,main = "Boxplot de fixed.acidity",col = 'purple',xlab="fixed.acidity",ylab="valeurs")



  # volatile.acidity
#%%%%%%%%%%%%%%%% Histogramme #%%%%%%%%%%%%%%%%%
hist(df$volatile.acidity,main ="Histogramme de volatile.acidity",
     xlab = "volatile.acidity",ylab = "Nombre",col="lightgreen",freq=F)
lines(density(df$volatile.acidity),col="red",lwd=2)

# Ajout de la mediane et la moyenne
abline(v=median(df$volatile.acidity,col="darkgreen",lwd=2,lty=2))
abline(v=mean(df$volatile.acidity),col="orange",lwd=2,lty=2)

# Légende
legend("topright", legend = c("Densité", "Médiane", "Moyenne"),
       col = c("red", "darkgreen", "orange"), lwd = 2, lty = c(1, 2, 2))

#%%%%%%%%%%%%%%% Boxplot #%%%%%%%%%%%%%%%%%
boxplot(df$volatile.acidity,main = "Boxplot de volatile.acidity",col = 'purple', xlab="volatile.acidity",ylab="valeurs")



  # citric.acid
#%%%%%%%%%%%%%% Histogramme #%%%%%%%%%%%%%%%%%
hist(df$citric.acid,main ="Histogramme de l'acide citrique",
     xlab = "citric.acid",ylab = "Nombre",col="lightgreen",freq=F)
lines(density(df$citric.acid),col="red",lwd=2)

# Ajout de la mediane et la moyenne
abline(v=median(df$citric.acid,col="darkgreen",lwd=2,lty=2))
abline(v=mean(df$citric.acid),col="orange",lwd=2,lty=2)

# Légende
legend("topright", legend = c("Densité", "Médiane", "Moyenne"),
       col = c("red", "darkgreen", "orange"), lwd = 2, lty = c(1, 2, 2))


#%%%%%%%%%%%%%% Boxplot #%%%%%%%%%%%%%%%%%
boxplot(df$citric.acid,main = "Boxplot de citric.acid",col = 'purple',xlab="citric.acid",ylab="valeurs")


  # residual.sugar
#%%%%%%%%%%%%%%% Histogramme #%%%%%%%%%%%%%%%%%
hist(df$residual.sugar,main ="Histogramme de residual.sugar",
     xlab = "residual.sugar",ylab = "Nombre",col="lightgreen",freq=F)
lines(density(df$residual.sugar),col="red",lwd=2)

# Ajout de la mediane et la moyenne
abline(v=median(df$residual.sugar,col="darkgreen",lwd=2,lty=2))
abline(v=mean(df$residual.sugar),col="orange",lwd=2,lty=2)

# Légende
legend("topright", legend = c("Densité", "Médiane", "Moyenne"),
       col = c("red", "darkgreen", "orange"), lwd = 2, lty = c(1, 2, 2))

#%%%%%%%%%%%%% Boxplot #%%%%%%%%%%%%%%%%%
boxplot(df$residual.sugar,main = "Boxplot de residual.sugar",col = 'purple',xlab="residual.sugar", ylab="Valeurs")



  # chlorides
#%%%%%%%%%%%%%% Histogramme #%%%%%%%%%%%%%%%%
hist(df$chlorides,main ="Histogramme de chlorides",
     xlab = "chlorides",ylab = "Nombre",col="lightgreen",freq=F)
lines(density(df$chlorides),col="red",lwd=2)

# Ajout de la mediane et la moyenne
abline(v=median(df$chlorides,col="darkgreen",lwd=2,lty=2))
abline(v=mean(df$chlorides),col="orange",lwd=2,lty=2)

# Légende
legend("topright", legend = c("Densité", "Médiane", "Moyenne"),
       col = c("red", "darkgreen", "orange"), lwd = 2, lty = c(1, 2, 2))

#%%%%%%%%%%%%%% Boxplot #%%%%%%%%%%%%%%%%
boxplot(df$chlorides,main = "Boxplot de chlorides",col = 'purple',xlab="chlorides", ylab="Valeurs")



  # free.sulfur.dioxide
#%%%%%%%%%%%%% Histogramme #%%%%%%%%%%%%%%%%%
hist(df$free.sulfur.dioxide,main ="Histogramme de free.sulfur.dioxide",
     xlab = "free.sulfur.dioxide",ylab = "Nombre",col="lightgreen",freq=F)
lines(density(df$free.sulfur.dioxide),col="red",lwd=2)

# Ajout de la mediane et la moyenne
abline(v=median(df$free.sulfur.dioxide,col="darkgreen",lwd=2,lty=2))
abline(v=mean(df$free.sulfur.dioxide),col="orange",lwd=2,lty=2)

# Légende
legend("topright", legend = c("Densité", "Médiane", "Moyenne"),
       col = c("red", "darkgreen", "orange"), lwd = 2, lty = c(1, 2, 2))

#%%%%%%%%%%%% Boxplot #%%%%%%%%%%%%%%%%%
boxplot(df$free.sulfur.dioxide,main = "Boxplot de free.sulfur.dioxide",col = 'purple',
        xlab="free.sulfur.dioxide", ylab="Valeurs")



  # total.sulfur.dioxide
#%%%%%%%%%%% Histogramme #%%%%%%%%%%%%%%%%%
hist(df$total.sulfur.dioxide,main ="Histogramme de total.sulfur.dioxide",
     xlab = "total.sulfur.dioxide",ylab = "Nombre",col="lightgreen",freq=F)
lines(density(df$total.sulfur.dioxide),col="red",lwd=2)

# Ajout de la mediane et la moyenne
abline(v=median(df$total.sulfur.dioxide,col="darkgreen",lwd=2,lty=2))
abline(v=mean(df$total.sulfur.dioxide),col="orange",lwd=2,lty=2)

# Légende
legend("topright", legend = c("Densité", "Médiane", "Moyenne"),
       col = c("red", "darkgreen", "orange"), lwd = 2, lty = c(1, 2, 2))

#%%%%%%%%%%% Boxplot #%%%%%%%%%%%%%%%%%
boxplot(df$total.sulfur.dioxide,main = "Boxplot de total.sulfur.dioxide",col = 'purple'
        ,xlab="total.sulfur.dioxide", ylab="Valeurs")



  # density
#%%%%%%%%%% Histogramme #%%%%%%%%%%%%%%%%%
hist(df$density,main ="Histogramme de density",
     xlab = "density",ylab = "Nombre",col="lightgreen",freq=F)
lines(density(df$density),col="red",lwd=2)

# Ajout de la mediane et la moyenne
abline(v=median(df$density,col="darkgreen",lwd=2,lty=2))
abline(v=mean(df$density),col="orange",lwd=2,lty=2)

# Légende
legend("topright", legend = c("Densité", "Médiane", "Moyenne"),
       col = c("red", "darkgreen", "orange"), lwd = 2, lty = c(1, 2, 2))


#%%%%%%%%%% Boxplot #%%%%%%%%%%%%%%%%
boxplot(df$density,main = "Boxplot de density",col = 'purple',
        xlab="density", ylab="valeurs")



  # pH
#%%%%%%%%%% Histogramme #%%%%%%%%%%%%%%%%%
hist(df$pH,main ="Histogramme de pH",
     xlab = "pH",ylab = "Nombre",col="lightgreen",freq=F)
lines(density(df$pH),col="red",lwd=2)

# Ajout de la mediane et la moyenne
abline(v=median(df$pH,col="darkgreen",lwd=2,lty=2))
abline(v=mean(df$pH),col="orange",lwd=2,lty=2)

# Légende
legend("topright", legend = c("Densité", "Médiane", "Moyenne"),
       col = c("red", "darkgreen", "orange"), lwd = 2, lty = c(1, 2, 2))


#%%%%%%%%%% Boxplot #%%%%%%%%%%%%%%%%%
boxplot(df$pH,main = "Boxplot de pH",col = 'purple',
        xlab="pH", ylab="Valeurs")



  # sulphates
#%%%%%%%%%% Histogramme #%%%%%%%%%%%%%%%%
hist(df$sulphates,main ="Histogramme de sulphates",
     xlab = "sulphates",ylab = "Nombre",col="lightgreen",freq=F)
lines(density(df$sulphates),col="red",lwd=2)

# Ajout de la mediane et la moyenne
abline(v=median(df$sulphates,col="darkgreen",lwd=2,lty=2))
abline(v=mean(df$sulphates),col="orange",lwd=2,lty=2)

# Légende
legend("topright", legend = c("Densité", "Médiane", "Moyenne"),
       col = c("red", "darkgreen", "orange"), lwd = 2, lty = c(1, 2, 2))

#%%%%%%%%%% Boxplot #%%%%%%%%%%%%%%%%%
boxplot(df$sulphates,main = "Boxplot de sulphates",col = 'purple',
        xlab="sulphates", ylab="Valeurs")



  # alcohol
#%%%%%%%%%% Histogramme #%%%%%%%%%%%%%%%%%
hist(df$alcohol,main ="Histogramme de alcohol",
     xlab = "alcohol",ylab = "Nombre",col="lightgreen",freq=F)
lines(density(df$alcohol),col="red",lwd=2)

# Ajout de la mediane et la moyenne
abline(v=median(df$alcohol,col="darkgreen",lwd=2,lty=2))
abline(v=mean(df$alcohol),col="orange",lwd=2,lty=2)

# Légende
legend("topright", legend = c("Densité", "Médiane", "Moyenne"),
       col = c("red", "darkgreen", "orange"), lwd = 2, lty = c(1, 2, 2))


#%%%%%%%%%% Boxplot #%%%%%%%%%%%%%%%%%
boxplot(df$alcohol,main = "Boxplot de alcohol",col = 'purple',
        xlab="alcohol", ylab="Valeurs")



  # quality
#%%%%%%%%%% Histogramme #%%%%%%%%%%%%%%%%%
hist(df$quality,main = "Histogramme de quality",xlab = "quality",ylab = "Nombre",col='lightblue')

#%%%%%%%%%% Boxplot #%%%%%%%%%%%%%%%%%
boxplot(df$quality,main = "Boxplot de quality",col = 'cadetblue')



# Variable d'interêt
wine_quality <- ifelse(df$quality>=5,0,1)
df_new <- df
df_new$wine_quality <- wine_quality

# Conversion en facteur
df_new$wine_quality <- factor(df_new$wine_quality)
str(df_new)

# table de contingence
contingence_tableau <- table(df_new$wine_quality)
contingence_tableau

# Normalisation des données
df_norm <- df_new %>%
  mutate(across(where(is.numeric) & !all_of("wine_quality"), scale))

# Suppression de la variable "quality" au profit de "wine_quality"
df_norm$quality=NULL

# ================                      ================
#                   Analyse Multivariée
# ================                      ================

# ============ Rélation entre les variables quantitative et wine_quality ==========

# boxplot bivarié pour chaque variable quantitative par rapport à wine_quality
          # fixed.acidity
boxplot(fixed.acidity~wine_quality,data=df,main="Boxplot de fixed.acidity par rapport à la qualité du vin",
        xlab = "Qualité",ylab = "fixed.acidity",xaxt="n",col=rainbow(2))
axis(1,1:2,labels = c("Mauvais","Bon"))
          # volatile.acidity
boxplot(volatile.acidity~wine_quality,data=df,main="Boxplot de volatile.acidity par rapport à la qualité du vin",
        xlab = "Qualité",ylab = "volatile.acidity",xaxt="n",col=rainbow(2))
axis(1,1:2,labels = c("Mauvais","Bon"))
          # citric.acid
boxplot(citric.acid~wine_quality,data=df,main="Boxplot de citric.acid par rapport à la qualité du vin",
        xlab = "Qualité",ylab = "citric.acid",xaxt="n",col=rainbow(2))
axis(1,1:2,labels = c("Mauvais","Bon"))
        # residual.sugar
boxplot(residual.sugar~wine_quality,data=df,main="Boxplot de residual.sugar par rapport à la qualité du vin",
        xlab = "Qualité",ylab = "residual.sugar",xaxt="n",col=rainbow(2))
axis(1,1:2,labels = c("Mauvais","Bon"))
        # chlorides
boxplot(chlorides~wine_quality,data=df,main="Boxplot de chlorides par rapport à la qualité du vin",
        xlab = "Qualité",ylab = "chlorides",xaxt="n",col=rainbow(2))
axis(1,1:2,labels = c("Mauvais","Bon"))
        # free.sulfur.dioxide
boxplot(free.sulfur.dioxide~wine_quality,data=df,main="Boxplot de free.sulfur.dioxide par rapport à la qualité du vin",
        xlab = "Qualité",ylab = "free.sulfur.dioxide",xaxt="n",col=rainbow(2))
axis(1,1:2,labels = c("Mauvais","Bon"))
        # total.sulfur.dioxide
boxplot(total.sulfur.dioxide~wine_quality,data=df,main="Boxplot de total.sulfur.dioxide par rapport à la qualité du vin",
        xlab = "Qualité",ylab = "total.sulfur.dioxide",xaxt="n",col=rainbow(2))
axis(1,1:2,labels = c("Mauvais","Bon"))
        # density
boxplot(density~wine_quality,data=df,main="Boxplot de density par rapport à la qualité du vin",
        xlab = "Qualité",ylab = "density",xaxt="n",col=rainbow(2))
axis(1,1:2,labels = c("Mauvais","Bon"))
        # pH
boxplot(pH~wine_quality,data=df,main="Boxplot de pH par rapport à la qualité du vin",
        xlab = "Qualité",ylab = "pH",xaxt="n",col=rainbow(2))
axis(1,1:2,labels = c("Mauvais","Bon"))
        # sulphates
boxplot(sulphates~wine_quality,data=df,main="Boxplot de sulphates par rapport à la qualité du vin",
        xlab = "Qualité",ylab = "sulphates",xaxt="n",col=rainbow(2))
axis(1,1:2,labels = c("Mauvais","Bon"))
        # alcohol
boxplot(alcohol~wine_quality,data=df,main="Boxplot de alcohol par rapport à la qualité du vin",
        xlab = "Qualité",ylab = "alcohol",xaxt="n",col=rainbow(2))
axis(1,1:2,labels = c("Mauvais","Bon"))

#==============                                           ================
#               Selection des caractéristiques pertinentes 
#==============                                           ================

################## Test statitique de Kruskal-Wallis ###############

# Récuperation des variables quantitatives
variables_quantitatives <-  c("fixed.acidity","volatile.acidity", "citric.acid",
                              "residual.sugar","chlorides","free.sulfur.dioxide",
                              "total.sulfur.dioxide","density","pH","sulphates",
                              "alcohol")

# Création d'un data frame pour stocker les résultats des test
results <- data.frame(Variable = character(), Kruskal_Wallis = numeric(),
                      P_value = numeric())

for (var in variables_quantitatives) {
  # Test de Kruskal-Wallis
  kruskal_test <- kruskal.test(df_norm[[var]] ~ df_norm$wine_quality)
  
  # Ajout des résultats au tableau
  results <- rbind(results, data.frame(Variable = var, Kruskal_Wallis = kruskal_test$statistic, P_value = kruskal_test$p.value))
}
# Trie des résultats par la statistique de test décroissant
results <- results[order(results$Kruskal_Wallis, decreasing = TRUE), ]
# Affichage des résultats
print(results)  # sulphates et pH sont non significatifs

#==============                                ================
#               Analyse de la multicolinéarité 
#==============                                ================

############### Matrice de corrélation #################

# Sélection les variables numériques
variables_numeriques <- df[, c("fixed.acidity", "volatile.acidity", "citric.acid", "residual.sugar", "chlorides",
                                 "free.sulfur.dioxide","total.sulfur.dioxide","density",
                                 "pH","sulphates","alcohol","quality")]

# Calcule de la matrice de corrélation
correlation_matrix <- cor(variables_numeriques, use = "complete.obs")

# Création du heatmap de corrélation
corrplot(cor(correlation_matrix),addCoef.col = "black", method = "color", type = "upper",order = "hclust", tl.cex = 0.8)

# Suppression de la variable quality au profit de wine_quality
df$quality=NULL

#==============                               ================
#                Partitionnement des données
#==============                               ================

# Définition de la proportion de données à garder dans l'ensemble d'entraînement (par exemple, 70%)
proportion_entrainement <- 0.7

# Création d'indices pour un partitionnement stratifié
set.seed(123) # Pour la reproductibilité
indices_entrainement <- createDataPartition(df_norm$wine_quality, p = proportion_entrainement, list = FALSE)

# Création des ensembles d'entraînement et de test
data_entrainement <- df_norm[indices_entrainement, ]
data_test <- df_norm[-indices_entrainement, ]

# Dimension
dim(data_entrainement)
dim(data_test)

# ===============                                                       ================

#                  Réalisation d'une Analyse en Composantes Principales 

# ===============                                                       ================

var.keep <- c("fixed.acidity", "volatile.acidity", "citric.acid", "residual.sugar", "chlorides",
              "free.sulfur.dioxide","total.sulfur.dioxide","density",
              "pH","sulphates","alcohol")

pca=PCA(data_entrainement[,var.keep], scale.unit=TRUE, graph=F);
pca

vp=pca$eig;
vp

seuil=80;
nb=max(which(vp[,3]< seuil));
nb

pca=PCA(data_entrainement[,var.keep],ncp=nb, scale.unit=TRUE, graph=FALSE);
pca_df=pca$ind$coord;
dim(pca_df)

# ===================                            ===================
#                     Ré-échantillonnage (SMOTE) 
# ===================                            ===================

# Création de la recette avec SMOTE appliqué au données d'entrainement
recette_smote <- recipe(wine_quality~.,data = df_norm)%>%
  step_pca(all_predictors(),num_comp = 5) %>%
  step_smote(wine_quality,over_ratio = 0.8)

# Entrainement de la recette
recette_prep <- prep(recette_smote)

# Application de la recette
data_entrainement_smote <- bake(recette_prep,new_data=NULL)

# Application au testset (sans SMOTE, mais avec le même PCA)
data_test_prepare <- bake(recette_prep,new_data = data_test)

# Aperçu des nouveaux données
head(data_entrainement_smote)
head(data_test_prepare)

# Vérification des dimention de la distribution
table(df_norm$wine_quality)  # Avant smote
table(data_entrainement_smote$wine_quality)  # Après smote

# Dimension du nouveau jeu de données d'entrainement
dim(data_entrainement_smote)

#================                             ================
#                   Entrainement du Modèle 
#================                             ================

# Entraînement du modèle de régression logistique avec glm
modele_logistique <- glm(wine_quality ~ ., data = data_entrainement_smote, family = binomial)

# Affichage du résumé du modèle
summary(modele_logistique)

# ===============                                                           ==============
#                  Significativité global du modèle (Likelihood Ratio Test) 
# ===============                                                           ==============

    # Test du rapport de vraisemblance (Likelihood Ratio Test)
test_lr <- lrtest(modele_logistique)

    # Affichage des résultats du test
print(test_lr) 

# ================                                                         ================
#                   Mesure du Pouvoir explicaif du modèle (R² de McFdden) 
# ================                                                         ================

# Calcule des déviations nulles et proposées
ll.null <- modele_logistique$null.deviance / -2
ll.proposed <- modele_logistique$deviance / -2

# Calcule du pseudo R-carré de McFadden
pseudo_r_squared_mcfadden <- 1 - (ll.proposed / ll.null)

# Affichage du pseudo R-carré de McFadden
print(pseudo_r_squared_mcfadden)

# ================                                                  ===============
#                   Interprétation du modèle (coéficient OddsRatio) 
# ================                                                  ===============

# Obtenir les coefficients estimés du modèle
coefficients <- coef(modele_logistique)

# Calcule des rapports de cotes en exponentiant les coefficients
odds_ratios <- exp(coefficients)

# Création d'un tableau avec les noms des variables et leurs rapports de cotes
variables <- names(coefficients)
tableau_odds_ratios <- data.frame(Variable = variables, OddsRatio = odds_ratios)

# Affichage du tableau des rapports de cotes
tableau_odds_ratios

# ===================                                            ==================
#                       Evaluation des performances du modèle 
#====================                                            ==================

probas_train <- predict(modele_logistique, data_entrainement_smote, type = "response")
probas_test <- predict(modele_logistique, data_test_prepare, type = "response")

roc_train <- roc(response = data_entrainement_smote$wine_quality, predictor = probas_train)
roc_test <- roc(response = data_test_prepare$wine_quality, predictor = probas_test)

# Affichage des courbes ROC avec AUC
par(mfrow=c(1,2))  # Afficher deux graphiques côte à côte
plot.roc(roc_train, main = "Courbe ROC - Base d'Entraînement", col = "blue", print.auc = TRUE)
plot.roc(roc_test, main = "Courbe ROC - Base Test", col = "red", print.auc = TRUE)

auc_train <- auc(roc_train)
auc_test <- auc(roc_test)

auc_table <- data.frame(Base = c("Entraînement", "Test"), AUC = c(auc_train, auc_test))

print(auc_table)

# ====================                                  ======================
#                        Prédiction et interprétation 
# ====================                                  ======================

# Prédiction de la probabilités sur la base d'entraînement
probas_train <- predict(modele_logistique, data_entrainement_smote, type = "response")

# Création d'un data frame avec les probabilités prédites et les étiquettes de la qualité du vin
predicted_data <- data.frame(Probabilite = probas_train, wine_quality = data_entrainement_smote$wine_quality)

# Remplacer les valeurs de wine_quality (0 par "mauvais" et 1 par "bon")
predicted_data$wine_quality <- factor(predicted_data$wine_quality, levels = c(0, 1), labels = c("mauvais", "bon"))

# Créer un graphique de densité pour les vins de mauvaise qualité et les vins de bonne qualité
ggplot(predicted_data, aes(x = Probabilite, fill = wine_quality)) +
  geom_density(alpha = 0.5) +
  labs(title = "Densité de Probabilité Prédite - mauvaise qualité vs. bonne qualité", x = "Probabilité Prédite") +
  scale_fill_manual(values = c("mauvais" = "red", "bon" = "blue")) +
  theme_minimal() +
  theme(legend.title = element_blank()) +  # Supprimer le titre de la légende
  labs(fill = "Qualité du vin")  # Renommer la légende


# Prédire les classes en utilisant un seuil de probabilité de 0.4 pour la base d'entraînement
seuil <- 0.4
predictions_train <- ifelse(probas_train >= seuil, 1, 0)
predictions_train <- factor(predictions_train, levels = c(0, 1))

# Créer la matrice de confusion pour la base d'entraînement
confusion_matrix_train <- confusionMatrix(predictions_train, data_entrainement_smote$wine_quality)

# Prédire les classes en utilisant un seuil de probabilité de 0.5 pour la base de test
predictions_test <- ifelse(probas_test >= seuil, 1, 0)
predictions_test <- factor(predictions_test, levels = c(0, 1))
# Créer la matrice de confusion pour la base de test
confusion_matrix_test <- confusionMatrix(predictions_test, data_test_prepare$wine_quality)

# Afficher les matrices de confusion

confusion_matrix_train  # Matrice de confusion de la base d'entrainement
# Transformation en data frame pour ggplot
conf_df <- as.data.frame(confusion_matrix_train$table)
colnames(conf_df) <- c("Predicted", "Actual", "Freq")

# Heatmap avec ggplot2
ggplot(conf_df, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), color = "black", size = 6) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "Heatmap de la matrice de confusion",
       x = "Classe réelle",
       y = "Classe prédite") +
  theme_minimal()



confusion_matrix_test #  Matrice de confusion de la base test
  # Transformation en data frame pour ggplot
conf_df <- as.data.frame(confusion_matrix_test$table)
colnames(conf_df) <- c("Predicted", "Actual", "Freq")

  # Heatmap avec ggplot2
ggplot(conf_df, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), color = "black", size = 6) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "Heatmap de la matrice de confusion",
       x = "Classe réelle",
       y = "Classe prédite") +
  theme_minimal()


# ==============                         ================
#                 Modèle random forest 
# ==============                         ================

# ==============  Entrainement du modèle =================
set.seed(123)
model_rf <- randomForest(wine_quality ~ ., data = data_entrainement_smote, ntree = 10, importance = TRUE)

# ============= Résumé du modèle ======================
summary(model_rf)
  # Prédictions
pred_rf_train <- predict(model_rf, data_entrainement_smote)
pred_rf_test <- predict(model_rf, data_test_prepare)

  # Évaluation (Matrice de confusion)
rf_cofusion_matrix_train <- confusionMatrix(pred_rf_train, data_entrainement_smote$wine_quality)
rf_confusion_matrix_test <- confusionMatrix(pred_rf_test, data_test_prepare$wine_quality)

  # Importance des variables
varImpPlot(model_rf)

ggplot(data_test_prepare, aes(x = pred_rf, fill = target)) +
  geom_density(alpha = 0.5) +
  labs(title = "Densité des prédictions (Random Forest)", x = "Probabilité prédite", y = "Densité") +
  theme_minimal()

# ==============                          ================
#                 Comparaison des modèles 
# ==============                          ================

#=============== Tableau des métriques des deux modèles ===================

# Fonction pour calculer toutes les métriques importantes
calculer_metriques <- function(matrice_confusion, nom_modele) {
  
  # Extraction des valeurs de la matrice de confusion
  TP <- matrice_confusion$table[2,2]  # Vrais Positifs
  TN <- matrice_confusion$table[1,1]  # Vrais Négatifs
  FP <- matrice_confusion$table[1,2]  # Faux Positifs
  FN <- matrice_confusion$table[2,1]  # Faux Négatifs
  
  # Calcul des métriques
  accuracy <- (TP + TN) / (TP + TN + FP + FN)
  precision <- TP / (TP + FP)
  recall <- TP / (TP + FN)
  specificity <- TN / (TN + FP)
  f1_score <- 2 * (precision * recall) / (precision + recall)
  
  # Création du dataframe avec les métriques
  metriques <- data.frame(
    Modele = nom_modele,
    Accuracy = round(accuracy, 4),
    Precision = round(precision, 4),
    Recall_Sensitivity = round(recall, 4),
    Specificity = round(specificity, 4),
    F1_Score = round(f1_score, 4),
    stringsAsFactors = FALSE
  )
  
  return(metriques)
}


# Calcul des métriques pour chaque modèle
metriques_rf <- calculer_metriques(rf_confusion_matrix_test, "Random Forest")
metriques_glm <- calculer_metriques(confusion_matrix_test, "Regression Logistique")

# Combinaison des résultats
comparaison_modeles <- rbind(metriques_rf, metriques_glm)

# Affichage du tableau de comparaison
print("=== COMPARAISON DES MODELES ===")
print(comparaison_modeles)


#=================== Visualisation graphique des différences =================

# Calcul des différences
diff_metriques <- data.frame(
  Metrique = c("Accuracy", "Precision", "Recall_Sensitivity", "Specificity", "F1_Score"),
  RF_minus_GLM = c(
    comparaison_modeles$Accuracy[1] - comparaison_modeles$Accuracy[2],
    comparaison_modeles$Precision[1] - comparaison_modeles$Precision[2],
    comparaison_modeles$Recall_Sensitivity[1] - comparaison_modeles$Recall_Sensitivity[2],
    comparaison_modeles$Specificity[1] - comparaison_modeles$Specificity[2],
    comparaison_modeles$F1_Score[1] - comparaison_modeles$F1_Score[2]
  )
)


# Visualisation graphique
# Transformation des données pour ggplot
comparaison_long <- melt(comparaison_modeles, id.vars = "Modele")

# Graphique en barres
p1 <- ggplot(comparaison_long, aes(x = variable, y = value, fill = Modele)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparaison des Métriques de Performance",
       x = "Métriques", y = "Valeur") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("Random Forest" = "#2E8B57", 
                               "Regression Logistique" = "#4169E1"))

print(p1)



#====================== Identification meilleur modèle ===================

# Fonction pour identifier le meilleur modèle
identifier_meilleur_modele <- function(comparaison_df) {
  meilleur <- data.frame(
    Metrique = c("Accuracy", "Precision", "Recall", "Specificity", "F1_Score"),
    Meilleur_Modele = c(
      ifelse(comparaison_df$Accuracy[1] > comparaison_df$Accuracy[2], "Random Forest", "Regression Logistique"),
      ifelse(comparaison_df$Precision[1] > comparaison_df$Precision[2], "Random Forest", "Regression Logistique"),
      ifelse(comparaison_df$Recall_Sensitivity[1] > comparaison_df$Recall_Sensitivity[2], "Random Forest", "Regression Logistique"),
      ifelse(comparaison_df$Specificity[1] > comparaison_df$Specificity[2], "Random Forest", "Regression Logistique"),
      ifelse(comparaison_df$F1_Score[1] > comparaison_df$F1_Score[2], "Random Forest", "Regression Logistique")
    )
  )
  return(meilleur)
}

print("\n=== MEILLEUR MODELE PAR METRIQUE ===")
print(identifier_meilleur_modele(comparaison_modele

                                 