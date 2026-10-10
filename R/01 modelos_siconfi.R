# ==============================================================================
# SCRIPT REGRESSÕES - DISSERTAÇÃO DE MESTRADO
# ==============================================================================

getwd()
setwd("../")
setwd("./00. Dissertação/03. RScript")
dir()

# Instalação dos pacotes necessários (descomente a linha abaixo se for a primeira vez)
# install.packages(c("readxl", "tidyverse", "car", "lmtest", "sandwich", "plm"))

library(readxl)
library(tidyverse)
library(car)
library(lmtest)
library(sandwich)
library(plm)

# ------------------------------------------------------------------------------
# CARGA E PREPARAÇÃO DOS DADOS (RESHAPE E MERGE)
# ------------------------------------------------------------------------------

path_base <- "nova_base.xlsx"

# Importação das bases brutas
saeb_raw     <- read_excel(path_base, sheet = "SAEB")
gasto_raw    <- read_excel(path_base, sheet = "GASTO")
inse_raw     <- read_excel(path_base, sheet = "INSE")
matr_raw     <- read_excel(path_base, sheet = "MATRÍCULAS")
tdi_raw      <- read_excel(path_base, sheet = "TDI")

# 1.1 Reshape SAEB (A única base que vai manter o nome do município)
df_saeb <- saeb_raw %>%
  pivot_longer(cols = starts_with("SAEB"), names_to = c("ano", "materia"), names_pattern = "SAEB(\\d+)_(\\w+)_5") %>%
  mutate(ano = as.numeric(paste0("20", ano))) %>%
  pivot_wider(names_from = materia, values_from = value, names_prefix = "SAEB_")

# 1.2 Reshape INSE (Excluindo o nome do município para evitar duplicidade)
df_inse <- inse_raw %>%
  rename(COD_IBGE = CO_MUNICIPIO) %>%
  select(-NO_MUNICIPIO) %>% # Remove nome
  pivot_longer(cols = starts_with("MD_INSE_"), names_to = "ano", names_prefix = "MD_INSE_", values_to = "INSE") %>%
  mutate(ano = as.numeric(ano))

# 1.3 Outras variáveis (Excluindo nomes de municípios)
df_matr <- matr_raw %>% 
  rename(COD_IBGE = codigo_ibge) %>% 
  select(COD_IBGE, ano, p_mat_rur_AI) # Mantém apenas chaves e a variável

df_tdi <- tdi_raw %>% 
  rename(ano = Ano, COD_IBGE = `Código do Município`) %>% 
  select(COD_IBGE, ano, TDI) # Mantém apenas chaves e a variável

# 1.4 Construção dos Lags Temporais e Médias Móveis no Gasto
df_gasto <- gasto_raw %>%
  rename(ano = NUM_ANO) %>%
  # Limpeza prévia: removemos COD_ENTE, nome do município e valores não corrigidos
  select(COD_IBGE, ano, SICONFI_CORR) %>% 
  arrange(COD_IBGE, ano) %>%
  group_by(COD_IBGE) %>%
  mutate(
    # USO OBRIGATÓRIO do dplyr::lag para evitar conflitos com o pacote plm/stats
    SICONFI_t_1 = dplyr::lag(SICONFI_CORR, n = 1),
    SICONFI_t_2 = dplyr::lag(SICONFI_CORR, n = 2),
    SICONFI_t_3 = dplyr::lag(SICONFI_CORR, n = 3),
    SICONFI_t_4 = dplyr::lag(SICONFI_CORR, n = 4),
    
    # Médias Móveis Sustentadas
    SICONFI_M_t2_t4 = (SICONFI_t_2 + SICONFI_t_3 + SICONFI_t_4) / 3,
    SICONFI_MA5 = (SICONFI_CORR + SICONFI_t_1 + SICONFI_t_2 + SICONFI_t_3 + SICONFI_t_4) / 5
  ) %>%
  ungroup() %>%
  mutate(
    # Aplicação do logaritmo (agora com os dados corretos)
    ln_gasto           = log(SICONFI_CORR),
    ln_gasto_t_1       = log(SICONFI_t_1),
    ln_gasto_t_2       = log(SICONFI_t_2),
    ln_gasto_t_3       = log(SICONFI_t_3),
    ln_gasto_t_4       = log(SICONFI_t_4),
    ln_gasto_MA5       = log(SICONFI_MA5),
    ln_mm_t2_t4        = log(SICONFI_M_t2_t4)
  )

# 1.5 Merge Final do Painel e Filtro 2019-2023
base_painel <- df_saeb %>%
  left_join(df_gasto, by = c("COD_IBGE", "ano")) %>%
  left_join(df_inse, by = c("COD_IBGE", "ano")) %>%
  left_join(df_matr, by = c("COD_IBGE", "ano")) %>%
  left_join(df_tdi, by = c("COD_IBGE", "ano")) %>%
  filter(ano >= 2019)

# Visualizando a base limpa para confirmar
head(base_painel)

rm(df_gasto, df_inse, df_matr, df_saeb, df_tdi)
rm(gasto_raw, inse_raw, matr_raw, saeb_raw, tdi_raw)


# ------------------------------------------------------------------------------
# REGRESSÕES EM PAINEL - LÍNGUA PORTUGUESA (SAEB_LP)
# Modelos: Efeitos Fixos (Within), Aleatórios (Random), Hausman e TWFE
# ------------------------------------------------------------------------------
base_pdf <- pdata.frame(base_painel, index = c("COD_IBGE", "ano"))

#Desvio-padrão within da média do gasto
plm::Within(base_pdf$ln_gasto) |> sd()
plm::Within(base_pdf$ln_gasto_t_1) |> sd()
plm::Within(base_pdf$ln_mm_t2_t4) |> sd()
plm::Within(base_pdf$ln_gasto_MA5) |> sd()


cat("\n======================================================================\n")
cat("                              REGRESSÕES                                \n")
cat("========================================================================\n")

# ----- MODELO A-LP: LAGS DE 2+m3 ANOS (t, t-1, m3) - PORTUGUÊS -----
mod_LP_lags_EF <- plm(SAEB_LP ~ ln_gasto + ln_gasto_t_1 + ln_mm_t2_t4 + INSE + p_mat_rur_AI + TDI, data = base_pdf, model = "within", effect = "individual")
mod_LP_lags_EA <- plm(SAEB_LP ~ ln_gasto + ln_gasto_t_1 + ln_mm_t2_t4 + INSE + p_mat_rur_AI + TDI, data = base_pdf, model = "random", effect = "individual")
mod_LP_lags_TWFE <- plm(SAEB_LP ~ ln_gasto + ln_gasto_t_1 + ln_mm_t2_t4 + INSE + p_mat_rur_AI + TDI, data = base_pdf, model = "within", effect = "twoways")

# ----- MODELO B-LP: MÉDIA MÓVEL 5 ANOS (MA5) - PORTUGUÊS -----
mod_LP_MA5_EF <- plm(SAEB_LP ~ ln_gasto_MA5 + INSE + p_mat_rur_AI + TDI, data = base_pdf, model = "within", effect = "individual")
mod_LP_MA5_EA <- plm(SAEB_LP ~ ln_gasto_MA5 + INSE + p_mat_rur_AI + TDI, data = base_pdf, model = "random", effect = "individual")
mod_LP_MA5_TWFE <- plm(SAEB_LP ~ ln_gasto_MA5 + INSE + p_mat_rur_AI + TDI, data = base_pdf, model = "within", effect = "twoways")

# ----- MODELO A-MT: LAGS DE 2+m3 ANOS (t, t-1, m3) - MATEMÁTICA -----
mod_MT_lags_EF <- plm(SAEB_MT ~ ln_gasto + ln_gasto_t_1 + ln_mm_t2_t4 + INSE + p_mat_rur_AI + TDI, data = base_pdf, model = "within", effect = "individual")
mod_MT_lags_EA <- plm(SAEB_MT ~ ln_gasto + ln_gasto_t_1 + ln_mm_t2_t4 + INSE + p_mat_rur_AI + TDI, data = base_pdf, model = "random", effect = "individual")
mod_MT_lags_TWFE <- plm(SAEB_MT ~ ln_gasto + ln_gasto_t_1 + ln_mm_t2_t4 + INSE + p_mat_rur_AI + TDI, data = base_pdf, model = "within", effect = "twoways")

# ----- MODELO B-MT: MÉDIA MÓVEL 5 ANOS (MA5) - MATEMÁTICA -----
mod_MT_MA5_EF <- plm(SAEB_MT ~ ln_gasto_MA5 + INSE + p_mat_rur_AI + TDI, data = base_pdf, model = "within", effect = "individual")
mod_MT_MA5_EA <- plm(SAEB_MT ~ ln_gasto_MA5 + INSE + p_mat_rur_AI + TDI, data = base_pdf, model = "random", effect = "individual")
mod_MT_MA5_TWFE <- plm(SAEB_MT ~ ln_gasto_MA5 + INSE + p_mat_rur_AI + TDI, data = base_pdf, model = "within", effect = "twoways")

# TESTES DE HAUSMAN, TESTE F, WOOLDRIDGE - PORTUGUÊS
cat("\n--- Testes de Hausman (LP) ---\n")
print(phtest(mod_LP_lags_EF, mod_LP_lags_EA))
print(phtest(mod_LP_MA5_EF, mod_LP_MA5_EA))
pFtest(mod_LP_lags_TWFE, mod_LP_lags_EF)
pFtest(mod_LP_MA5_TWFE, mod_LP_MA5_EF)
pwartest(mod_LP_lags_TWFE)

# TESTES DE HAUSMAN E TEST F, WOOLDRIDGE
cat("\n--- Testes de Hausman (MT) ---\n")
print(phtest(mod_MT_lags_EF, mod_MT_lags_EA))
print(phtest(mod_MT_MA5_EF, mod_MT_MA5_EA))
pFtest(mod_MT_lags_TWFE, mod_LP_lags_EF)
pFtest(mod_MT_MA5_TWFE, mod_LP_MA5_EF)
pwartest(mod_MT_lags_TWFE)

# OUTPUTS FINAIS COM ERROS ROBUSTOS AGRUPADOS

cat("\n--- TWFE: Lags Simultâneos - PORTUGUÊS ---\n")
print(coeftest(mod_LP_lags_TWFE, vcovHC(mod_LP_lags_TWFE, type = "HC1", cluster = "group")))
resumo_lags_LP <- summary(mod_LP_lags_TWFE)
print(c("R2" = resumo_lags_LP$r.squared[1], "R2_Adj" = resumo_lags_LP$r.squared[2], "F_stat" = resumo_lags_LP$fstatistic$statistic))
lags_LP_global <- lm(SAEB_LP ~ ln_gasto + ln_gasto_t_1 + ln_mm_t2_t4 + INSE + p_mat_rur_AI + TDI + factor(COD_IBGE) + factor(ano), data = base_pdf)
resumo_lags_LP_global <- summary(lags_LP_global)
print(paste("R2 Global:", round(resumo_lags_LP_global$r.squared, 4)))
print(paste("R2 Ajustado Global:", round(resumo_lags_LP_global$adj.r.squared, 4)))

cat("\n--- TWFE: Média Móvel 5 Anos - PORTUGUÊS ---\n")
print(coeftest(mod_LP_MA5_TWFE, vcovHC(mod_LP_MA5_TWFE, type = "HC1", cluster = "group")))
resumo_TWFE_LP <- summary(mod_LP_MA5_TWFE)
print(c("R2" = resumo_TWFE_LP$r.squared[1], "R2_Adj" = resumo_TWFE_LP$r.squared[2], "F_stat" = resumo_TWFE_LP$fstatistic$statistic))
twfe_LP_global <- lm(SAEB_LP ~ ln_gasto_MA5 + INSE + p_mat_rur_AI + TDI + factor(COD_IBGE) + factor(ano), data = base_pdf)
resumo_TWFE_LP_global <- summary(twfe_LP_global)
print(paste("R2 Global:", round(resumo_TWFE_LP_global$r.squared, 4)))
print(paste("R2 Ajustado Global:", round(resumo_TWFE_LP_global$adj.r.squared, 4)))

cat("\n--- TWFE: Lags Simultâneos - MATEMÁTICA ---\n")
print(coeftest(mod_MT_lags_TWFE, vcovHC(mod_MT_lags_TWFE, type = "HC1", cluster = "group")))
resumo_lags_MT <- summary(mod_MT_lags_TWFE)
print(c("R2" = resumo_lags_MT$r.squared[1], "R2_Adj" = resumo_lags_MT$r.squared[2], "F_stat" = resumo_lags_MT$fstatistic$statistic))
lags_MT_global <- lm(SAEB_MT ~ ln_gasto + ln_gasto_t_1 + ln_mm_t2_t4 + INSE + p_mat_rur_AI + TDI + factor(COD_IBGE) + factor(ano), data = base_pdf)
resumo_lags_MT_global <- summary(lags_MT_global)
print(paste("R2 Global:", round(resumo_lags_MT_global$r.squared, 4)))
print(paste("R2 Ajustado Global:", round(resumo_lags_MT_global$adj.r.squared, 4)))

cat("\n--- TWFE: Média Móvel 5 Anos - MATEMÁTICA ---\n")
print(coeftest(mod_MT_MA5_TWFE, vcovHC(mod_MT_MA5_TWFE, type = "HC1", cluster = "group")))
resumo_TWFE_MT <- summary(mod_MT_MA5_TWFE)
print(c("R2" = resumo_TWFE_MT$r.squared[1], "R2_Adj" = resumo_TWFE_MT$r.squared[2], "F_stat" = resumo_TWFE_MT$fstatistic$statistic))
twfe_MT_global <- lm(SAEB_MT ~ ln_gasto_MA5 + INSE + p_mat_rur_AI + TDI + factor(COD_IBGE) + factor(ano), data = base_pdf)
resumo_TWFE_MT_global <- summary(twfe_MT_global)
print(paste("R2 Global:", round(resumo_TWFE_MT_global$r.squared, 4)))
print(paste("R2 Ajustado Global:", round(resumo_TWFE_MT_global$adj.r.squared, 4)))

# FIM DO SCRIPT