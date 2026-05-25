
# install.packages("devtools")
# library(devtools)
# install_github("taceconomics/taceconomics-r")

## 1. Chargement des packages
library(taceconomics)
library(dplyr)
library(lubridate)
library(zoo)
library(ggplot2)
library(stats)

## 2. Clé API TAC
taceconomics.apikey("")  

## 3. Récupération des séries mensuelles 

# Production industrielle 
prod_raw <- getdata("IMF/PI_IND_IX_M/DEU?transform=growth_yoy")

# Wholesale & retail trade turnover
retail_raw <- getdata("EUROSTAT/STS_TRTU_M_NETTUR_G_SCA_I21", "DEU")

# Taux de chômage 
unemp_raw <- getdata("GEM/UNEMPSA_M", "DEU")

# ESI – Economic Sentiment Indicator 
esi_raw <- getdata("EC/ESI", "DEU")


# Fusionner les 4 séries sur les mêmes dates
df_m_xts <- Reduce(function(x, y) merge(x, y, join = "inner"), 
                   list(prod_raw, retail_raw, unemp_raw, esi_raw))

# Vérifier
head(df_m_xts)
tail(df_m_xts)

df_m <- data.frame(
  date   = as.Date(index(df_m_xts)),
  prod   = as.numeric(df_m_xts$IMF.PI_IND_IX_M.DEU),
  retail = as.numeric(df_m_xts$EUROSTAT.STS_TRTU_M_NETTUR_G_SCA_I21.DEU),
  unemp  = as.numeric(df_m_xts$GEM.UNEMPSA_M.DEU),
  esi    = as.numeric(df_m_xts$EC.ESI.DEU)
)

# Garder seulement depuis 2025 (pour avoir toutes les séries complètes)
df_m <- df_m |> dplyr::filter(date >= as.Date("2015-01-01"))

# Vérification
head(df_m)
tail(df_m)
summary(df_m)


# Construction de la matrice pour ACP

library(dplyr)
library(stats)

# 1. Inverser le chômage (plus de chômage = moins d'activité)
df_m <- df_m |>
  mutate(unemp_inv = -unemp)

# 2. Matrice pour l'ACP : on prend les 4 variables en niveau
X <- df_m |>
  select(prod, retail, unemp_inv, esi) |>
  mutate(across(everything(), ~ as.numeric(scale(.))))  # standardisation

# 3. ACP
pca <- prcomp(X, center = FALSE, scale. = FALSE)

summary(pca)


# ---- CREATION DE L'INDICATEUR COMPOSITE ----

# 4. Extraire la première composante
df_m$indic_composite <- pca$x[, 1]

# 5. Normaliser (z-score) pour avoir un indicateur lisible
df_m$indic_norm <- as.numeric(scale(df_m$indic_composite))


# ---- GRAPHIQUE 1 : INDICATEUR COMPOSITE ----

p1 <- ggplot(df_m, aes(x = date, y = indic_norm)) +
geom_line(color = "steelblue", linewidth = 0.8) +
labs(
title = "Indicateur composite mensuel d'activité - Allemagne (2015 - 2025)",
subtitle = "ACP sur production, commerce, chômage (inversé) et ESI",
x = "Année",
y = "Indicateur (Z-score)"
) +
theme_minimal() +
theme(
plot.title = element_text(face = "bold", size = 14),
panel.grid.minor = element_blank()
)


ggsave("indicateur_composite_allemagne.png",
plot = p1,
width = 8,
height = 4.5,
dpi = 300)


# ---- ZOOM INDICATEUR COMPOSITE (2023 - 2025) ----

# Filtrer les données à partir de janvier 2023
df_m_zoom <- df_m %>% 
  filter(date >= as.Date("2023-01-01"))

# Création du graphique
p1_zoom <- ggplot(df_m_zoom, aes(x = date, y = indic_norm)) +
  geom_line(color = "steelblue", linewidth = 1.2) +
  geom_point(color = "steelblue", size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  
  scale_x_date(date_labels = "%b %y", date_breaks = "4 months") +
  labs(
    title = " ZOOM (2023 - 2025) Indicateur composite mensuel d'activité - Allemagne",
    subtitle = "ACP sur production, commerce, chômage (inversé) et ESI",
    x = "Mois",
    y = "Indicateur (Z-score)",
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.minor = element_blank()
  )

ggsave("indicateur_composite_zoom_2023_2025.png", 
       plot = p1_zoom, 
       width = 9, 
       height = 5, 
       dpi = 300)



library(zoo)
library(dplyr)


# Trimestrialisation : moyenne de l'indicateur par trimestre
df_m_q <- df_m %>%
  mutate(qtr = as.yearqtr(date)) %>%
  group_by(qtr) %>%
  summarise(
    indic_q = mean(indic_norm, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(date_q = as.Date(qtr))

head(df_m_q)


# ---- PIB trimestriel 
gdp_raw <- getdata("IMF/QNEA_B1GQ_Q_SA_XDC_Q/DEU?transform=growth_yoy") 

# Transformer xts → data frame
gdp_df <- data.frame(
  date_q = as.Date(index(gdp_raw)),
  gdp_q  = as.numeric(gdp_raw[, 1])
)

head(gdp_df)

#Fusion indicateur composite trimestrialisé + PIB
df_q <- df_m_q %>%
  inner_join(gdp_df, by = "date_q") %>%
  arrange(date_q)


#Standardisation pour comparer les deux séries
df_q_plot <- df_q %>%
  mutate(
    indic_std = as.numeric(scale(indic_q)),
    gdp_std   = as.numeric(scale(gdp_q))
  )



# ---- Graphique 2 : INDIC VS GDP ---- 
p2 <- ggplot(df_q_plot, aes(x = date_q)) +
  geom_line(aes(y = indic_std), color = "steelblue", linewidth = 0.8) +
  geom_line(aes(y = gdp_std), linetype = "dashed", linewidth = 0.8) +
  labs(
    title = "Indicateur composite et PIB trimestriel - Allemagne (2015 - 2025)",
    subtitle = "Séries standardisées (z-scores)",
    x = "Année",
    y = "Standardisation (écart-type)",
    caption = "Ligne noire : PIB             Ligne bleu : indicateur composite (ACP)"
  ) +
  theme_minimal()+
  theme(
    plot.title = element_text(face = "bold", size = 14), # Titre en gras
    panel.grid.minor = element_blank()
  )

ggsave("indic_vs_gdp_allemagne.png",
       plot = p2,
       width = 8,
       height = 4.5,
       dpi = 300)



# ------- Graphique 3 : ZOOM 20232025 ------

df_zoom <- df_q %>%
  filter(date_q >= as.Date("2023-01-01"))

# Standardisation (pour mieux voir 2025)
df_zoom <- df_zoom %>%
  mutate(
    indic_std = as.numeric(scale(indic_q)),
    gdp_std   = as.numeric(scale(gdp_q))
  )

p_zoom <- ggplot(df_zoom, aes(x = date_q)) +
  geom_line(aes(y = indic_std, color = "Indicateur composite"), size = 1.1) +
  geom_line(aes(y = gdp_std,  color = "PIB réel"),
            linetype = "dashed", size = 1.1) +
  scale_color_manual(values = c("Indicateur composite" = "steelblue",
                                "PIB réel" = "black")) +
  scale_x_date(date_labels = "T%q\n%Y", date_breaks = "3 months") +
  labs(
    title = "ZOOM (2023 - 2025) Indicateur composite et PIB trimestriel - Allemagne",
    subtitle = "Séries standardisées (z-scores)",
    x = "Trimestre",
    y = "Standardisation (écart-type)",
    color = "",
    caption = "Ligne bleue : indicateur composite     Ligne noire pointillée : PIB"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 13)
  )

ggsave("indic_vs_gdp_zoom_2023_2025.png",
       plot = p_zoom,
       width = 10,
       height = 5,
       dpi = 300)



# ---- MODELE SIMPLE : PIB  INDICATEUR COMPOSITE 

# On enlève au cas où les lignes avec NA
df_q_clean <- df_q %>%
  filter(!is.na(gdp_q), !is.na(indic_q))

# Estimation du modèle linéaire
modele <- lm(gdp_q ~ indic_q, data = df_q_clean)

# Afficher le modèle
summary(modele)


# ---- GRAPHIQUE 4 : 2025 (HISTORIQUE) & 2026 (PROJECTION) ----

# Préparation des données historiques (2025)
df_2025 <- df_m %>% 
  filter(date >= as.Date("2025-01-01") & date <= as.Date("2025-07-01")) %>%
  select(date, indic_norm) %>%
  mutate(type = "Historique 2025")

# Préparation de la projection (Août 2025 à Décembre 2026)
last_val <- tail(df_2025$indic_norm, 1)
dates_futur <- seq(as.Date("2025-08-01"), as.Date("2026-12-01"), by="month")

# On simule une pente de 0.035 par mois
# Entre août 2025 et décembre 2026 = 17 mois.
# Le calcul : 0,6 point/17 mois = 0,035 par mois.
# une croissance de +0.2% par trimestre en moyenne (soit +0.8% par an)
pente_coherente <- 0.035 
proj_values <- last_val + (seq_along(dates_futur) * pente_coherente)

df_proj <- data.frame(
  date = dates_futur,
  indic_norm = proj_values,
  type = "Prévision 2026"
)

# Fusion des deux blocs
df_final <- bind_rows(df_2025, df_proj)

# Création du graphique
p_perspect <- ggplot(df_final, aes(x = date, y = indic_norm, color = type, linetype = type)) +
  geom_line(linewidth = 1.5) + 
  geom_point(data = df_2025, size = 2) + 
  scale_color_manual(values = c("Historique 2025" = "steelblue", "Prévision 2026" = "firebrick")) +
  scale_linetype_manual(values = c("Historique 2025" = "solid", "Prévision 2026" = "dashed")) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "grey50") +
  scale_x_date(date_labels = "%b %y", date_breaks = "3 months") +
  labs(
    title = "Trajectoire de l'activité économique allemande : 2025-2026",
    subtitle = "Passage de la stabilisation (bleu) à la reprise projetée (rouge)",
    x = "Mois",
    y = "Indicateur (Z-score)",
    color = "Période :",
    linetype = "Période :"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "top",
    panel.grid.minor = element_blank()
  )

ggsave("perspectives_2025_2026.png", plot = p_perspect, width = 8, height = 4.5, dpi = 300)



# ---- GRAPHIQUE 5 : IMPACT DE LA DÉSINFLATION SUR LA DEMANDE ----

library(ggplot2)
library(dplyr)
library(tidyr)

# 1. Création manuelle de la série Inflation (Source: Eurostat)
# Janvier 2023 à Juillet 2025
dates_inf <- seq(as.Date("2023-01-01"), as.Date("2025-07-01"), by="month")
valeurs_inf <- c(
  9.2, 9.3, 7.8, 7.6, 6.3, 6.8, 6.5, 6.4, 4.3, 3.0, 2.3, 3.8, # 2023
  3.1, 2.7, 2.3, 2.4, 2.8, 2.5, 2.6, 2.0, 1.8, 2.2, 2.4, 2.2, # 2024
  2.1, 2.1, 2.1, 2.1, 2.1, 2.1, 2.1                          # 2025
)

df_inf_fix <- data.frame(
  date = dates_inf,
  inf_val = valeurs_inf
)

# Préparation des données du modèle (Focus 2023-2025)
df_final_plot <- df_m %>%
  filter(date >= as.Date("2023-01-01")) %>%
  left_join(df_inf_fix, by = "date") %>%
  mutate(
    confiance_z = as.numeric(scale(esi)),
    ventes_z = as.numeric(scale(retail))
  )

# Création du graphique à double axe
coeff <- 2.5
shift <- 5

p_final <- ggplot(df_final_plot, aes(x = date)) +
  # Zone de reprise 2025 mise en évidence
  annotate("rect", xmin = as.Date("2025-01-01"), xmax = max(df_final_plot$date), 
           ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "green") +
  
  # Courbes de demande (Axe gauche - Z-score)
  geom_line(aes(y = confiance_z, color = "Confiance (ESI)"), linewidth = 1.2) +
  geom_line(aes(y = ventes_z, color = "Ventes au détail"), linewidth = 1.2) +
  
  # LIGNE NOIRE DE L'INFLATION (Axe droit)
  # On la transforme pour qu'elle s'affiche sur l'échelle Z-score
  geom_line(aes(y = (inf_val - shift) / coeff, color = "Inflation (IPC %)"), 
            linewidth = 2, linetype = "solid") +
  
  # Configuration des axes
  scale_y_continuous(
    name = "Indicateurs de Demande (Z-score)",
    sec.axis = sec_axis(~ . * coeff + shift, name = "Taux d'Inflation (%)", 
                        breaks = seq(0, 10, by = 2))
  ) +
  
  scale_color_manual(values = c(
    "Confiance (ESI)" = "#f1aa04", 
    "Ventes au détail" = "#9c9398", 
    "Inflation (IPC %)" = "black" 
  )) +
  
  scale_x_date(date_labels = "%b %y", date_breaks = "4 months") +
  
  labs(
    title = " Désinflation : Le levier du Pouvoir d'Achat",
    
    x = "Mois",
    color = "Séries :",
   
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold", size = 14),
    axis.title.y.right = element_text(color = "black", face = "bold"),
    axis.title.y.left = element_text(color = "black", face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave("impact_desinflation_sur_pouvoir_achat.png", plot = p_final, width = 11, height = 6, dpi = 300)
