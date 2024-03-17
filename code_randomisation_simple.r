install.packages("dplyr")
install.packages("random")



library(dplyr)
library(random)
library(Minirand)

# Randomisation simple 20 patient 
datasimp20 = data.frame()




for (i in 1:1000){
  # Création de la bdd
  data20 = data.frame(etude = "",idpatient=c(1:20))
  data20$pays = sample(1:3,size = nrow(data20),replace = T,prob = c(0.5,0.3,0.2))
  moyenne = 65
  ecart_type = 10
  data20$etude =  rep(i,20)
  for (j in 1:20){
    data20$age[j] = round(rnorm(1, mean = moyenne, sd = ecart_type))
    
  }
  
  data20 = data20 %>% mutate(Traitement = "")
  
  
  for (i in data20$idpatient) {
    
    data20$Traitement = sample(c("A", "B"),prob = c(0.5,0.5),size = 20,replace = TRUE)
  }
  
  
  if (is.null(datasimp20)) {
    datasimp20 <- data20
  } else {
    datasimp20 <- rbind(datasimp20, data20)
  }
  
  
}

# Probabilite de succes pour chaque combinaison de pays et traitement
prob_succes <- ifelse(datasimp20$pays == 1 & datasimp20$Traitement == "A", 0.5,
                      ifelse(datasimp20$pays == 2 & datasimp20$Traitement == "A", 0.3,
                             ifelse(datasimp20$pays == 3 & datasimp20$Traitement == "A", 0.1,
                                    ifelse(datasimp20$pays == 1 & datasimp20$Traitement == "B", 0.3,
                                           ifelse(datasimp20$pays == 2 & datasimp20$Traitement == "B", 0.15,
                                                  ifelse(datasimp20$pays == 3 & datasimp20$Traitement == "B", 0.1, NA))))))



# Generation aleatoire des succes ou echecs selon la probabilite
datasimp20$succes <- ifelse(runif(length(datasimp20$idpatient)) < prob_succes, "Succes", "echec")



####################################################################################


# Randomisation simple 50 patient 
datasimp50 = data.frame()




for (i in 1:1000){
  # Création de la bdd
  data50 = data.frame(etude = "",idpatient=c(1:50))
  data50$pays = sample(1:3,size = nrow(data50),replace = T,prob = c(0.5,0.3,0.2))
  moyenne = 65
  ecart_type = 10
  data50$etude =  rep(i,50)
  for (j in 1:50){
    data50$age[j] = round(rnorm(1, mean = moyenne, sd = ecart_type))
    
  }
  
  data50 = data50 %>% mutate(Traitement = "")
  
  
  for (i in data50$idpatient) {
    
    data50$Traitement = sample(c("A", "B"),prob = c(0.5,0.5),size = 50,replace = TRUE)
  }
  
  
  if (is.null(datasimp50)) {
    datasimp50 <- data50
  } else {
    datasimp50 <- rbind(datasimp50, data50)
  }
  
  
}

# Probabilité de succès pour chaque combinaison de pays et traitement
prob_succes <- ifelse(datasimp50$pays == 1 & datasimp50$Traitement == "A", 0.5,
                      ifelse(datasimp50$pays == 2 & datasimp50$Traitement == "A", 0.3,
                             ifelse(datasimp50$pays == 3 & datasimp50$Traitement == "A", 0.1,
                                    ifelse(datasimp50$pays == 1 & datasimp50$Traitement == "B", 0.3,
                                           ifelse(datasimp50$pays == 2 & datasimp50$Traitement == "B", 0.15,
                                                  ifelse(datasimp50$pays == 3 & datasimp50$Traitement == "B", 0.1, NA))))))



# Génération aléatoire des succès ou échecs selon la probabilité
datasimp50$succes <- ifelse(runif(length(datasimp50$idpatient)) < prob_succes, "Succes", "echec")


prop.table(table(datasimp20$Traitement, datasimp20$succes),margin = 1)

# write.csv2(datasimp20,"rando simple 20.csv")
# write.csv2(datasimp50,"rando simple 50.csv")


############################# Graphique##############

# ============================================================================ #
# Taille 20 
setwd("D:/université/BUT2/travail/Randomisation/simple")
datasimp20 <- read.csv("rando simple 20.csv",
                       sep = ";")
count_A <- datasimp20 %>%
  filter(Traitement == "A") %>%
  count(etude)

count_B <- datasimp20 %>%
  filter(Traitement == "B") %>%
  count(etude)
r <- merge(count_A, count_B, by = "etude", suffixes = c("_A", "_B"))
r$ecart <- abs(r$n_A - r$n_B)

ecart_freq <- table(r$ecart)
barplot(ecart_freq, 
        col = "skyblue", 
        main = "Fr?quences des ?carts pour une \n?tude de taille 20", 
        xlab = "?cart", 
        ylab = "Fr?quence",
        border = "white", 
        ylim = c(0,350),
        las = 1)
text(barplot(ecart_freq, plot = FALSE), 
     ecart_freq, labels = ecart_freq, 
     pos = 3, col = "blue", cex = 0.8)


# ============================================================================ #
# Taille 50 

datasimp50 <- read.csv("rando simple 50.csv",
                       sep = ";")
count_A <- datasimp50 %>%
  filter(Traitement == "A") %>%
  count(etude)

count_B <- datasimp50 %>%
  filter(Traitement == "B") %>%
  count(etude)
r2 <- merge(count_A, count_B, by = "etude", suffixes = c("_A", "_B"))
r2$ecart <- abs(r2$n_A - r2$n_B)

ecart_freq2 <- table(r2$ecart)
barplot(ecart_freq2, 
        col = "skyblue", 
        main = "Fr?quences des ?carts pour une \n?tude de taille 50", 
        xlab = "?cart", 
        ylab = "Fr?quence",
        border = "white", 
        ylim = c(0,250),
        las = 1)
text(barplot(ecart_freq2, plot = FALSE), 
     ecart_freq, labels = ecart_freq2, 
     pos = 3, col = "blue", cex = 0.8)
# ============================================================================ #

par(mfrow=c(1,2))
ecart_freq <- table(r$ecart)
barplot(ecart_freq, 
        col = "skyblue", 
        main = "Fr?quences des ?carts pour une ?tude de taille 20", 
        xlab = "?cart", 
        ylab = "Fr?quence",
        border = "white", 
        ylim = c(0,350),
        las = 1)
text(barplot(ecart_freq, plot = FALSE), 
     ecart_freq, labels = ecart_freq, 
     pos = 3, col = "blue", cex = 0.8)


ecart_freq2 <- table(r2$ecart)
barplot(ecart_freq2, 
        col = "skyblue", 
        main = "Fr?quences des ?carts pour une ?tude de taille 50", 
        xlab = "?cart", 
        ylab = "Fr?quence",
        border = "white", 
        ylim = c(0,250),
        las = 1)
text(barplot(ecart_freq2, plot = FALSE), 
     ecart_freq2, labels = ecart_freq2, 
     pos = 3, col = "blue", cex = 0.8)

par(mfrow=c(1,1))

# ============================================================================ #



# Initialiser une colonne pour stocker les r?sultats
datasimp20$significatif <- NA
datasimp20$test <- NA
datasimp20$pvaleur <- NA

datasimp50$significatif <- NA
datasimp50$test <- NA
datasimp50$pvaleur <- NA



############## Pvaleur 20################
# Boucle ? travers les niveaux de la variable "etude"
for (i in unique(datasimp20$etude)) {
  # Sous-ensemble des donn?es pour le niveau actuel de "etude"
  subset_data <- datasimp20[datasimp20$etude == i, ]
  
  # Effectuer le test du chi-deux
  test_chi2 <- chisq.test(table(subset_data$Traitement, subset_data$succes), correct = FALSE)
  
  test_fisher <- fisher.test(table(subset_data$Traitement, subset_data$succes),simulate.p.value = T,B=2000)
  
  if (all(test_chi2$expected > 5)) {
    # Si oui, marquer le niveau comme significatif
    datasimp20$significatif[datasimp20$etude == i] <- 1
    datasimp20$test[datasimp20$etude == i] = "chi2"
    datasimp20$pvaleur[datasimp20$etude == i] = test_chi2$p.value
    
  } else {
    # Sinon, marquer comme non significatif
    datasimp20$significatif[datasimp20$etude == i] <- 0
    datasimp20$test[datasimp20$etude == i] = "fisher"
    datasimp20$pvaleur[datasimp20$etude == i] = test_fisher$p.value
    
  }
}

############## Pvaleur 50################

for (i in unique(datasimp50$etude)) {
  # Sous-ensemble des donn?es pour le niveau actuel de "etude"
  subset_data <- datasimp50[datasimp50$etude == i, ]
  
  # Effectuer le test du chi-deux
  test_chi2 <- chisq.test(table(subset_data$Traitement, subset_data$succes), correct = FALSE)
  
  test_fisher <- fisher.test(table(subset_data$Traitement, subset_data$succes),simulate.p.value = T,B=2000)
  
  # V?rifier si les fr?quences attendues sont toutes sup?rieures ? 5
  if (all(test_chi2$expected > 5)) {
    # Si oui, marquer le niveau comme significatif
    datasimp50$significatif[datasimp50$etude == i] <- 1
    datasimp50$test[datasimp50$etude == i] = "chi2"
    datasimp50$pvaleur[datasimp50$etude == i] = test_chi2$p.value
  } else {
    # Sinon, marquer comme non significatif
    datasimp50$significatif[datasimp50$etude == i] <- 0
    datasimp50$test[datasimp50$etude == i] = "fisher"
    datasimp50$pvaleur[datasimp50$etude == i] = test_fisher$p.value
  }
}
