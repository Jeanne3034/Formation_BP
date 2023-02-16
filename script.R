


# I- CHARGEMENTS ----
## 1. Packages ----
library(tidyverse)
library(dplyr)
library(forcats)



api_token <- yaml::read_yaml("secrets.yaml")[["jeton_api"]]

## 2. Chargements fonctions ----
source("functions.R", encoding = "UTF8")


# II- IMPORTS DONNEES----
# 1. Imports ----
df <- arrow::read_parquet(
  "individu_reg.parquet",
  col_select = c(
    "region", "aemm", "aged", "anai", "catl", "cs1", "cs2", "cs3",
    "couple", "na38", "naf08", "pnai12", "sexe", "surf", "tp",
    "trans", "ur"
  )
)

# 2. Mise en forme----

df$sexe <- df$sexe %>%
  as.character() %>%
  fct_recode(Homme = "1", Femme = "2")




# III - STATS----
summarise(group_by(df, aged), n())

# part d'homme dans chaque cohort ----
df %>%
  group_by(aged, sexe) %>%
  summarise(SH_sexe = n()) %>%
  group_by(aged) %>%
  mutate(SH_sexe = SH_sexe / sum(SH_sexe)) %>%
  filter(sexe == 1) %>%
  ggplot() +
  geom_bar(aes(x = as.numeric(aged), y = SH_sexe), stat = "identity") +
  geom_point(aes(x = as.numeric(aged), y = SH_sexe), stat = "identity",
             color = "red") +
  coord_cartesian(c(0, 100))

# stats trans par statut----
df2 <- df %>%
  group_by(couple, trans) %>%
  summarise(x = n()) %>%
  group_by(couple) %>%
  mutate(y = 100 * x / sum(x))

stats_age <- df %>%
  mutate(age = as.numeric(aged))
group_by(decennie = decennie_a_partir_annee(age)) %>%
  summarise(n())

table_age <- gt::gt(stats_age) %>%
  gt::tab_header(
    title = "Distribution des âges dans notre population"
  ) %>%
  gt::fmt_number(
    columns = `n()`,
    sep_mark = " ",
    decimals = 0
  ) %>%
  gt::cols_label(
    decennie = "Tranche d'âge",
    `n()` = "Population"
  )

# IV - GRAPHIQUES ----
ggplot(df) +
  geom_histogram(aes(x = 5 * floor(as.numeric(aged) / 5)), stat = "count")


p <- ggplot(df2) +
  geom_bar(aes(x = trans, y = y, color = couple), stat = "identity",
           position = "dodge")

ggsave("p.png", p)






df %>%
  filter(sexe == "Homme") %>%
  mutate(aged = as.numeric(aged)) %>%
  pull(aged) %>%
  calcul_stat_desc()

df %>%
  filter(sexe == "Femme") %>%
  mutate(aged = as.numeric(aged)) %>%
  pull(aged) %>%
  calcul_stat_desc()



# modelisation
df %>%
  select(surf, cs1, ur, couple, aged) %>%
  filter(surf != "Z") %>%
  MASS::polr(factor(surf) ~ cs1 + factor(ur), .)

