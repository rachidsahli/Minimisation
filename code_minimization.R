library(dplyr)
library(random)

############################################ Facteur à 100% ##############################
datasimp50 = data.frame()
for (j in 1:1000) {
  data = data.frame(Idpatient = c(1:50))
  
  data$Pays <- sample(1:3, size = nrow(data), replace = TRUE, prob = c(0.5, 0.3, 0.2))
  data$Traitement <- ""
  
  moyenne <- 65
  sd <- 10
  nombre_patients <- 50
  
  for (i in 1:nombre_patients) {
    data$Age[i] <- round(rnorm(1, mean = moyenne, sd = sd))
  }
  
  data$Traitement[1] <- sample(c("A", "B"), 1, replace = TRUE, prob = c(0.5, 0.5))
  
  brasa <- rep(NA, nrow(data))
  brasb <- rep(NA, nrow(data))
  
  data$Etude <- rep(j, 50)
  
  for (i in 1:nrow(data)) {
    Pays1A <- sum(data$Traitement == "A" & data$Pays == 1)
    Pays2A <- sum(data$Traitement == "A" & data$Pays == 2)
    Pays3A <- sum(data$Traitement == "A" & data$Pays == 3)
    Pays1B <- sum(data$Traitement == "B" & data$Pays == 1)
    Pays2B <- sum(data$Traitement == "B" & data$Pays == 2)
    Pays3B <- sum(data$Traitement == "B" & data$Pays == 3)
    
    if (data$Pays[i] == 1) {
      brasa[i] <- abs((Pays1A + 1) - Pays1B)
      brasb[i] <- abs(Pays1A - (1 + Pays1B))
    } else if (data$Pays[i] == 2) {
      brasa[i] <- abs((Pays2A + 1) - Pays2B)
      brasb[i] <- abs(Pays2A - (1 + Pays2B))
    } else {
      brasa[i] <- abs((Pays3A + 1) - Pays3B)
      brasb[i] <- abs(Pays3A - (1 + Pays3B))
    }
    
    data$Traitement[i] <- ifelse(
      brasa[i] < brasb[i], 
      sample(c("A", "B"), 1, replace = TRUE, prob = c(0.1,0)),  # facteur aléatoire 100%
      ifelse(
        brasa[i] == brasb[i], 
        sample(c("A", "B"), 1, replace = TRUE, prob = c(0.5, 0.5)),
        ifelse(
          brasa[i] > brasb[i], 
          sample(c("B", "A"), 1, replace = TRUE, prob = c(1, 0)),
          0
        )
      )
    )
    
    data$prob_succes[i] <- case_when (
      data$Pays[i] == 1 & data$Traitement[i] == "A" ~ 0.5,
      data$Pays[i] == 2 & data$Traitement[i] == "A" ~ 0.3,
      data$Pays[i] == 3 & data$Traitement[i] == "A" ~ 0.1,
      data$Pays[i] == 1 & data$Traitement[i] == "B" ~ 0.3,
      data$Pays[i] == 2 & data$Traitement[i] == "B" ~ 0.15,
      data$Pays[i] == 3 & data$Traitement[i] == "B" ~ 0.1,
      TRUE ~ NA_real_
    )
    
    data$Statut[i] <- if_else(runif(1) < data$prob_succes[i], "Succes", "Echec")
  }
  
  # Test de Fisher pour chaque étude
  subset_data <- data
  fisher_result <- fisher.test(subset_data$Traitement,subset_data$Statut)
  p_value <- fisher_result$p.value
  
  data$p_value_fisher <- p_value
  
  if (j == 1) {
    datasimp50 <- data
  } else {
    datasimp50 <- rbind(datasimp50, data)
  }
}# Fin de la boucle

datasimp50$CMH <- NA
for (i in unique(datasimp50$Etude)) {
  subset_data <- datasimp50[datasimp50$Etude == i, ]
  if (!any(table(subset_data$Pays, subset_data$Traitement) == 0)) {
    test_result <- mantelhaen.test(subset_data$Pays, subset_data$Statut, subset_data$Traitement, exact = TRUE)
    datasimp50$CMH[datasimp50$Etude == i] <- test_result$p.value
  } else {
    datasimp50$CMH[datasimp50$Etude == i] <- NA
  }
}

count_A <- datasimp50 %>%
  filter(Traitement == "A") %>%
  count(Etude)

count_B <- datasimp50 %>%
  filter(Traitement == "B") %>%
  count(Etude)

r <- merge(count_A, count_B, by = "Etude", suffixes = c("_A", "_B"))

r$ecart <- abs(r$n_A - r$n_B)

mean(r$ecart)

write.csv(datasimp50,"/Users/rs777/Documents/SAE_PLAN_EXPERIENCE/50000_etudes/Facteur_aleatoire100/fichier50000_FA100.csv")

############################################ Facteur à 60% ##############################
datasimp50 = data.frame()
for (j in 1:1000) {
  data = data.frame(Idpatient = c(1:50))
  
  data$Pays <- sample(1:3, size = nrow(data), replace = TRUE, prob = c(0.5, 0.3, 0.2))
  data$Traitement <- ""
  
  moyenne <- 65
  sd <- 10
  nombre_patients <- 50
  
  for (i in 1:nombre_patients) {
    data$Age[i] <- round(rnorm(1, mean = moyenne, sd = sd))
  }
  
  data$Traitement[1] <- sample(c("A", "B"), 1, replace = TRUE, prob = c(0.5, 0.5))
  
  brasa <- rep(NA, nrow(data))
  brasb <- rep(NA, nrow(data))
  
  data$Etude <- rep(j, 50)
  
  for (i in 1:nrow(data)) {
    Pays1A <- sum(data$Traitement == "A" & data$Pays == 1)
    Pays2A <- sum(data$Traitement == "A" & data$Pays == 2)
    Pays3A <- sum(data$Traitement == "A" & data$Pays == 3)
    Pays1B <- sum(data$Traitement == "B" & data$Pays == 1)
    Pays2B <- sum(data$Traitement == "B" & data$Pays == 2)
    Pays3B <- sum(data$Traitement == "B" & data$Pays == 3)
    
    if (data$Pays[i] == 1) {
      brasa[i] <- abs((Pays1A + 1) - Pays1B)
      brasb[i] <- abs(Pays1A - (1 + Pays1B))
    } else if (data$Pays[i] == 2) {
      brasa[i] <- abs((Pays2A + 1) - Pays2B)
      brasb[i] <- abs(Pays2A - (1 + Pays2B))
    } else {
      brasa[i] <- abs((Pays3A + 1) - Pays3B)
      brasb[i] <- abs(Pays3A - (1 + Pays3B))
    }
    
    data$Traitement[i] <- ifelse(
      brasa[i] < brasb[i], 
      sample(c("A", "B"), 1, replace = TRUE, prob = c(0.6,0.4)),  # facteur aléatoire 60%
      ifelse(
        brasa[i] == brasb[i], 
        sample(c("A", "B"), 1, replace = TRUE, prob = c(0.5, 0.5)),
        ifelse(
          brasa[i] > brasb[i], 
          sample(c("B", "A"), 1, replace = TRUE, prob = c(0.6,0.4)),
          0
        )
      )
    )
    
    data$prob_succes[i] <- case_when (
      data$Pays[i] == 1 & data$Traitement[i] == "A" ~ 0.5,
      data$Pays[i] == 2 & data$Traitement[i] == "A" ~ 0.3,
      data$Pays[i] == 3 & data$Traitement[i] == "A" ~ 0.1,
      data$Pays[i] == 1 & data$Traitement[i] == "B" ~ 0.3,
      data$Pays[i] == 2 & data$Traitement[i] == "B" ~ 0.15,
      data$Pays[i] == 3 & data$Traitement[i] == "B" ~ 0.1,
      TRUE ~ NA_real_
    )
    
    data$Statut[i] <- if_else(runif(1) < data$prob_succes[i], "Succes", "Echec")
  }
  
  # Test de Fisher pour chaque étude
  subset_data <- data
  fisher_result <- fisher.test(subset_data$Traitement,subset_data$Statut)
  p_value <- fisher_result$p.value
  
  data$p_value_fisher <- p_value
  
  if (j == 1) {
    datasimp50 <- data
  } else {
    datasimp50 <- rbind(datasimp50, data)
  }
}# Fin de la boucle

datasimp50$CMH <- NA
for (i in unique(datasimp50$Etude)) {
  subset_data <- datasimp50[datasimp50$Etude == i, ]
  if (!any(table(subset_data$Pays, subset_data$Traitement) == 0)) {
    test_result <- mantelhaen.test(subset_data$Pays, subset_data$Statut, subset_data$Traitement, exact = TRUE)
    datasimp50$CMH[datasimp50$Etude == i] <- test_result$p.value
  } else {
    datasimp50$CMH[datasimp50$Etude == i] <- NA
  }
}

count_A <- datasimp50 %>%
  filter(Traitement == "A") %>%
  count(Etude)

count_B <- datasimp50 %>%
  filter(Traitement == "B") %>%
  count(Etude)

r <- merge(count_A, count_B, by = "Etude", suffixes = c("_A", "_B"))

r$ecart <- abs(r$n_A - r$n_B)

mean(r$ecart)

write.csv(datasimp50,"/Users/rs777/Documents/SAE_PLAN_EXPERIENCE/50000_etudes/Facteur_aleatoire60/fichier50000_FA60.csv")

############################################ Facteur à 70% ##############################
datasimp50 = data.frame()
for (j in 1:1000) {
  data = data.frame(Idpatient = c(1:50))
  
  data$Pays <- sample(1:3, size = nrow(data), replace = TRUE, prob = c(0.5, 0.3, 0.2))
  data$Traitement <- ""
  
  moyenne <- 65
  sd <- 10
  nombre_patients <- 50
  
  for (i in 1:nombre_patients) {
    data$Age[i] <- round(rnorm(1, mean = moyenne, sd = sd))
  }
  
  data$Traitement[1] <- sample(c("A", "B"), 1, replace = TRUE, prob = c(0.5, 0.5))
  
  brasa <- rep(NA, nrow(data))
  brasb <- rep(NA, nrow(data))
  
  data$Etude <- rep(j, 50)
  
  for (i in 1:nrow(data)) {
    Pays1A <- sum(data$Traitement == "A" & data$Pays == 1)
    Pays2A <- sum(data$Traitement == "A" & data$Pays == 2)
    Pays3A <- sum(data$Traitement == "A" & data$Pays == 3)
    Pays1B <- sum(data$Traitement == "B" & data$Pays == 1)
    Pays2B <- sum(data$Traitement == "B" & data$Pays == 2)
    Pays3B <- sum(data$Traitement == "B" & data$Pays == 3)
    
    if (data$Pays[i] == 1) {
      brasa[i] <- abs((Pays1A + 1) - Pays1B)
      brasb[i] <- abs(Pays1A - (1 + Pays1B))
    } else if (data$Pays[i] == 2) {
      brasa[i] <- abs((Pays2A + 1) - Pays2B)
      brasb[i] <- abs(Pays2A - (1 + Pays2B))
    } else {
      brasa[i] <- abs((Pays3A + 1) - Pays3B)
      brasb[i] <- abs(Pays3A - (1 + Pays3B))
    }
    
    data$Traitement[i] <- ifelse(
      brasa[i] < brasb[i], 
      sample(c("A", "B"), 1, replace = TRUE, prob = c(0.7,0.3)),  # facteur aléatoire 70%
      ifelse(
        brasa[i] == brasb[i], 
        sample(c("A", "B"), 1, replace = TRUE, prob = c(0.5, 0.5)),
        ifelse(
          brasa[i] > brasb[i], 
          sample(c("B", "A"), 1, replace = TRUE, prob = c(0.7,0.3)),
          0
        )
      )
    )
    
    data$prob_succes[i] <- case_when (
      data$Pays[i] == 1 & data$Traitement[i] == "A" ~ 0.5,
      data$Pays[i] == 2 & data$Traitement[i] == "A" ~ 0.3,
      data$Pays[i] == 3 & data$Traitement[i] == "A" ~ 0.1,
      data$Pays[i] == 1 & data$Traitement[i] == "B" ~ 0.3,
      data$Pays[i] == 2 & data$Traitement[i] == "B" ~ 0.15,
      data$Pays[i] == 3 & data$Traitement[i] == "B" ~ 0.1,
      TRUE ~ NA_real_
    )
    
    data$Statut[i] <- if_else(runif(1) < data$prob_succes[i], "Succes", "Echec")
  }
  
  # Test de Fisher pour chaque étude
  subset_data <- data
  fisher_result <- fisher.test(subset_data$Traitement,subset_data$Statut)
  p_value <- fisher_result$p.value
  
  data$p_value_fisher <- p_value
  
  if (j == 1) {
    datasimp50 <- data
  } else {
    datasimp50 <- rbind(datasimp50, data)
  }
}# Fin de la boucle

datasimp50$CMH <- NA
for (i in unique(datasimp50$Etude)) {
  subset_data <- datasimp50[datasimp50$Etude == i, ]
  if (!any(table(subset_data$Pays, subset_data$Traitement) == 0)) {
    test_result <- mantelhaen.test(subset_data$Pays, subset_data$Statut, subset_data$Traitement, exact = TRUE)
    datasimp50$CMH[datasimp50$Etude == i] <- test_result$p.value
  } else {
    datasimp50$CMH[datasimp50$Etude == i] <- NA
  }
}

count_A <- datasimp50 %>%
  filter(Traitement == "A") %>%
  count(Etude)

count_B <- datasimp50 %>%
  filter(Traitement == "B") %>%
  count(Etude)

r <- merge(count_A, count_B, by = "Etude", suffixes = c("_A", "_B"))

r$ecart <- abs(r$n_A - r$n_B)

mean(r$ecart)

write.csv(datasimp50,"/Users/rs777/Documents/SAE_PLAN_EXPERIENCE/50000_etudes/Facteur_aleatoire70/fichier50000_FA70.csv")
############################################ Facteur à 80% ##############################
datasimp50 = data.frame()
for (j in 1:1000) {
  data = data.frame(Idpatient = c(1:50))
  
  data$Pays <- sample(1:3, size = nrow(data), replace = TRUE, prob = c(0.5, 0.3, 0.2))
  data$Traitement <- ""
  
  moyenne <- 65
  sd <- 10
  nombre_patients <- 50
  
  for (i in 1:nombre_patients) {
    data$Age[i] <- round(rnorm(1, mean = moyenne, sd = sd))
  }
  
  data$Traitement[1] <- sample(c("A", "B"), 1, replace = TRUE, prob = c(0.5, 0.5))
  
  brasa <- rep(NA, nrow(data))
  brasb <- rep(NA, nrow(data))
  
  data$Etude <- rep(j, 50)
  
  for (i in 1:nrow(data)) {
    Pays1A <- sum(data$Traitement == "A" & data$Pays == 1)
    Pays2A <- sum(data$Traitement == "A" & data$Pays == 2)
    Pays3A <- sum(data$Traitement == "A" & data$Pays == 3)
    Pays1B <- sum(data$Traitement == "B" & data$Pays == 1)
    Pays2B <- sum(data$Traitement == "B" & data$Pays == 2)
    Pays3B <- sum(data$Traitement == "B" & data$Pays == 3)
    
    if (data$Pays[i] == 1) {
      brasa[i] <- abs((Pays1A + 1) - Pays1B)
      brasb[i] <- abs(Pays1A - (1 + Pays1B))
    } else if (data$Pays[i] == 2) {
      brasa[i] <- abs((Pays2A + 1) - Pays2B)
      brasb[i] <- abs(Pays2A - (1 + Pays2B))
    } else {
      brasa[i] <- abs((Pays3A + 1) - Pays3B)
      brasb[i] <- abs(Pays3A - (1 + Pays3B))
    }
    
    data$Traitement[i] <- ifelse(
      brasa[i] < brasb[i], 
      sample(c("A", "B"), 1, replace = TRUE, prob = c(0.8,0.2)),  # facteur aléatoire 80%
      ifelse(
        brasa[i] == brasb[i], 
        sample(c("A", "B"), 1, replace = TRUE, prob = c(0.5, 0.5)),
        ifelse(
          brasa[i] > brasb[i], 
          sample(c("B", "A"), 1, replace = TRUE, prob = c(0.8,0.2)),
          0
        )
      )
    )
    
    data$prob_succes[i] <- case_when (
      data$Pays[i] == 1 & data$Traitement[i] == "A" ~ 0.5,
      data$Pays[i] == 2 & data$Traitement[i] == "A" ~ 0.3,
      data$Pays[i] == 3 & data$Traitement[i] == "A" ~ 0.1,
      data$Pays[i] == 1 & data$Traitement[i] == "B" ~ 0.3,
      data$Pays[i] == 2 & data$Traitement[i] == "B" ~ 0.15,
      data$Pays[i] == 3 & data$Traitement[i] == "B" ~ 0.1,
      TRUE ~ NA_real_
    )
    
    data$Statut[i] <- if_else(runif(1) < data$prob_succes[i], "Succes", "Echec")
  }
  
  # Test de Fisher pour chaque étude
  subset_data <- data
  fisher_result <- fisher.test(subset_data$Traitement,subset_data$Statut)
  p_value <- fisher_result$p.value
  
  data$p_value_fisher <- p_value
  
  if (j == 1) {
    datasimp50 <- data
  } else {
    datasimp50 <- rbind(datasimp50, data)
  }
}# Fin de la boucle

datasimp50$CMH <- NA
for (i in unique(datasimp50$Etude)) {
  subset_data <- datasimp50[datasimp50$Etude == i, ]
  if (!any(table(subset_data$Pays, subset_data$Traitement) == 0)) {
    test_result <- mantelhaen.test(subset_data$Pays, subset_data$Statut, subset_data$Traitement, exact = TRUE)
    datasimp50$CMH[datasimp50$Etude == i] <- test_result$p.value
  } else {
    datasimp50$CMH[datasimp50$Etude == i] <- NA
  }
}

count_A <- datasimp50 %>%
  filter(Traitement == "A") %>%
  count(Etude)

count_B <- datasimp50 %>%
  filter(Traitement == "B") %>%
  count(Etude)

r <- merge(count_A, count_B, by = "Etude", suffixes = c("_A", "_B"))

r$ecart <- abs(r$n_A - r$n_B)

mean(r$ecart)

write.csv(datasimp50,"/Users/rs777/Documents/SAE_PLAN_EXPERIENCE/50000_etudes/Facteur_aleatoire80/fichier50000_FA80.csv")

#### Variable succès ####
check_effectifs <- function(x) {
  if(all(x > 5)) {
    return("Khi-2")
  } else {
    return("Fisher")
  }
}
#### Test Fisher ####
table(datasimp50$Test)

dataetude1 <- datasimp50 %>% 
  filter(etude == 3)

fisher.test(dataetude1$Succes,dataetude1$Traitement)
table(dataetude1$Traitement)
table(dataetude1$Succes)


p_values <- numeric(length(unique(datasimp50$etude)))
for (i in unique(datasimp50$etude)) {
  subset_data <- datasimp50[datasimp50$etude == i, ]
  fisher_result <- fisher.test(subset_data$Traitement, subset_data$Succes)
  p_values[i] <- fisher_result$p.value
}
datasimp50$p_value_fisher <- p_values
print(p_values)

# Répéter les valeurs de p pour chaque étude
p_values_rep <- rep(p_values, table(datasimp50$etude))

# Assigner les p-valeurs répétées à une nouvelle colonne dans le dataframe
datasimp50$p_value_fisher <- p_values_rep

ggplot(p_values_df, aes(x = p_value)) +
  geom_histogram(bins = 50, fill = "orange", color = "black", alpha = 0.7) +
  labs(x = "P-valeur", y = "Fréquence") +
  theme_minimal()
mean(p_values_df$p_value)









#####Test CMHetude#####
datasimp50$CMH <- NA
for (i in unique(datasimp50$etude)) {
  subset_data <- datasimp50[datasimp50$etude == i, ]
  test_result <- mantelhaen.test(subset_data$Succes, subset_data$Pays, subset_data$Traitement, exact = TRUE)
  datasimp50$CMH[datasimp50$etude == i] <- test_result$p.value
}

CMHetude <- datasimp50 %>% 
  distinct(etude,CMH)

dataetude1 <- datasimp50 %>% 
  filter(etude == 41)
mantelhaen.test(table(dataetude1$Succes,dataetude1$Pays,dataetude1$Traitement),exact = T)
### Graphique test Fisher 
library(ggplot2)
library(dplyr)
library(tidyr)

# Créer un dataframe pour stocker les p-valeurs et les études
p_values_df <- data.frame(etude = unique(datasimp50$etude), p_value = p_values)

###########################Graphique supplémentaire mais a revoir #################

# Créer un histogramme pour chaque étude
histogram <- ggplot(p_values_df, aes(x = p_value)) +
  geom_histogram(aes(y = ..density..), bins = 50, fill = "orange", color = "black", alpha = 0.7) +
  labs(x = "P-valeur", y = "Densité") +
  theme_minimal()

# Ajouter une courbe représentant la densité d'une distribution normale
normal_curve <- function(mean, sd) {
  x <- seq(0, 1, length.out = 1000)
  y <- dnorm(x, mean = mean, sd = sd)
  data.frame(x = x, y = y)
}

# Calculer la moyenne et l'écart-type des p-valeurs
mean_p <- mean(p_values_df$p_value, na.rm = TRUE)
sd_p <- sd(p_values_df$p_value, na.rm = TRUE)

# Créer la courbe de densité normale
normal_data <- normal_curve(mean_p, sd_p)

# Tracer la courbe de densité normale
density_curve <- geom_line(data = normal_data, aes(x, y), color = "red", size = 1)

# Afficher le graphique combiné
print(histogram + density_curve)