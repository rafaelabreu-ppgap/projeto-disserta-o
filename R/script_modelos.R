getwd()
dir()
setwd("./00. script_dissertacao")

# ----- INSTALA E CARREGA PACOTES -----
#readxl --------------- para ler arquivos excel
#dplyr ---------------- para manipulação de dados
#plm ------------------ para análise de dados em painel
#lmtest --------------- testes para modelos de regressão
#zoo ------------------ para séries temporais e janelas móveis
#PerformanceAnalytics - análise exploratória
#tseries -------------- para séries temporasis: análise de tendência, etc.
#tidyverse ------------ para manipulação de dados

install.packages(c("readxl", "dplyr", "plm", "deaR", "lmtest", "zoo", "broom"))
install.packages(c("PerformanceAnalytics", "tseries", "tidyverse", "gtsummary", "litedown"))
install.packages("ggrepel")

library(readxl)
library(plm)
library(zoo)
library(tidyverse)
library(gtsummary)
library(deaR)
library(ggrepel)
library(broom)
library(broom.helpers)
library(litedown)
## ----- FIM PACOTES -----


# ----- LEITURA DE DADOS - painel -----
dados_qualidade <- read_excel("dados_mucuri.xlsx", sheet = "qualidade")
dados_gastos <- read_excel("dados_mucuri.xlsx", sheet = "gasto")
dados_completos <- left_join(dados_gastos, dados_qualidade, by = c("ano", "id_ente", "nom_munic"))
glimpse(dados_completos)
# ----- FIM LEITURA DADOS -----

# ----- CRIA ÍNDICE QUALIDADE - PCA -----
matriz_pca <- dados_qualidade %>%
  dplyr::select(lp_adequado, mt_adequado) %>%
  scale()

pca_resultado <- prcomp(matriz_pca)
print(summary(pca_resultado))
dados_qualidade$indice_qualidade <- pca_resultado$x[, 1]

# adiciona índice sintético ao painel com todos os dados
dados_completos <- dados_completos %>%
  left_join(dados_qualidade %>% dplyr::select(ano, id_ente, indice_qualidade), by = c("ano", "id_ente"))

rm(matriz_pca)
dados_completos$lp_adequado <- NULL
dados_completos$mt_adequado <- NULL
# ----- FIM ÍNDICE QUALIDADE -----

# ----- PAINEL -----
#padroniza os dados de qualidade e aplica log em gastos
painel <- dados_completos %>%
  dplyr::arrange(id_ente, ano) %>%
  dplyr::group_by(id_ente) %>%
  mutate(
    #padroniza o índice de qualidade
    indice_qualidade_std = as.vector(scale(indice_qualidade)),
    #aplica o logaritmo no investimento
    log_gasto_siope = log(gasto_aluno_SIOPE),
    log_gasto_siconfi = log(gasto_aluno_SICONFI),
    
    #--- Variáveis para a Abordagem 1 (Defasagens Distribuídas) ---
    #primeira diferença do investimento, usando o método seguro
    d_log_gasto_siope = log_gasto_siope - dplyr::lag(log_gasto_siope),
    d_log_gasto_siconfi = log_gasto_siconfi - dplyr::lag(log_gasto_siconfi),
    
    #--- Variáveis para a Abordagem 2 (Gasto Acumulado) ---
    #média móvel de 3 anos do investimento EM NÍVEL
    media_movel_3a_siope = zoo::rollmean(log_gasto_siope, k = 3, fill = NA, align = "right"),
    media_movel_3a_siconfi = zoo::rollmean(log_gasto_siconfi, k = 3, fill = NA, align = "right"),
  ) %>%
  
  dplyr::ungroup()

# ----- FIM PAINEL -----

# ----- ABORDAGEM 1 - DEFASG. DISTRIBUÍDA COM LAG - SIOPE -----
#prepara dados para o modelo 1
modelo1_dados_siope <- painel %>%
  dplyr::group_by(id_ente) %>%
  dplyr::mutate(
    #cria os lags da variável em diferença
    lag1_d_log_gasto_siope = dplyr::lag(d_log_gasto_siope, 1),
    lag2_d_log_gasto_siope = dplyr::lag(d_log_gasto_siope, 2)
  ) %>%
  dplyr::ungroup() %>%
 
   #filtra para os anos com dados de qualidade e sem NAs dos lags
  dplyr::filter(!is.na(indice_qualidade_std)) %>%
  na.omit()

#estima os modelos FE e RE
fe_modelo1_siope <- plm(indice_qualidade_std ~ d_log_gasto_siope + lag1_d_log_gasto_siope + lag2_d_log_gasto_siope,
                  data = modelo1_dados_siope, model = "within")

re_modelo1_siope <- plm(indice_qualidade_std ~ d_log_gasto_siope + lag1_d_log_gasto_siope + lag2_d_log_gasto_siope,
                  data = modelo1_dados_siope, model = "random")

#escolhe o modelo com o Teste de Hausman
phtest(fe_modelo1_siope, re_modelo1_siope)

#resultado do modelo escolhido (geralmente Efeitos Fixos - FE)
print(fe_modelo1_siope)
summary(fe_modelo1_siope)

# ----- FIM ABORDAGEM 1 - SIOPE -----

# ----- ABORDAGEM 1 - DEFASG. DISTRIBUÍDA COM LAG - SICONFI -----
#prepara dados para o modelo 1
modelo1_dados_siconfi <- painel %>%
  dplyr::group_by(id_ente) %>%
  dplyr::mutate(
    #cria os lags da variável em diferença
    lag1_d_log_gasto_siconfi = dplyr::lag(d_log_gasto_siconfi, 1),
    lag2_d_log_gasto_siconfi = dplyr::lag(d_log_gasto_siconfi, 2)
  ) %>%
  dplyr::ungroup() %>%
  
  #filtra para os anos com dados de qualidade e sem NAs dos lags
  dplyr::filter(!is.na(indice_qualidade_std)) %>%
  na.omit()

#estima os modelos FE e RE
fe_modelo1_siconfi <- plm(indice_qualidade_std ~ d_log_gasto_siconfi + lag1_d_log_gasto_siconfi + lag2_d_log_gasto_siconfi,
                        data = modelo1_dados_siconfi, model = "within")

re_modelo1_siconfi <- plm(indice_qualidade_std ~ d_log_gasto_siconfi + lag1_d_log_gasto_siconfi + lag2_d_log_gasto_siconfi,
                        data = modelo1_dados_siconfi, model = "random")

#escolhe o modelo com o Teste de Hausman
phtest(fe_modelo1_siconfi, re_modelo1_siconfi)

#resultado do modelo escolhido (geralmente Efeitos Fixos - FE)
print(fe_modelo1_siconfi)
summary(fe_modelo1_siconfi)

tbl_regression(fe_modelo1_siope)
tbl_regression(fe_modelo1_siconfi)

# ----- FIM ABORDAGEM 1 - SICONFI -----

# ----- ABORDAGEM 2 - GASTO ACUMULADO - SIOPE -----
modelo2_dados_siope <- painel %>%
  #filtra para os anos com dados de qualidade e sem NAs da média móvel
  dplyr::filter(!is.na(indice_qualidade_std)) %>%
  na.omit()

#estima os modelos FE e RE
fe_modelo2_siope <- plm(indice_qualidade_std ~ media_movel_3a_siope,
                  data = modelo2_dados_siope, model = "within")

re_modelo2_siope <- plm(indice_qualidade_std ~ media_movel_3a_siope,
                  data = modelo2_dados_siope, model = "random")

#escolhe o modelo com o Teste de Hausman
phtest(fe_modelo2_siope, re_modelo2_siope)

#resultado do modelo escolhido
summary(re_modelo2_siope)

# ----- FIM ABORDAGEM 2 - SIOPE -----

# ----- ABORDAGEM 2 - GASTO ACUMULADO - SICONFI -----
modelo2_dados_siconfi <- painel %>%
  #filtra para os anos com dados de qualidade e sem NAs da média móvel
  dplyr::filter(!is.na(indice_qualidade_std)) %>%
  na.omit()

#estima os modelos FE e RE
fe_modelo2_siconfi <- plm(indice_qualidade_std ~ media_movel_3a_siconfi,
                        data = modelo2_dados_siconfi, model = "within")

re_modelo2_siconfi <- plm(indice_qualidade_std ~ media_movel_3a_siconfi,
                        data = modelo2_dados_siconfi, model = "random")

#escolhe o modelo com o Teste de Hausman
phtest(fe_modelo2_siconfi, re_modelo2_siconfi)

#resultado do modelo escolhido
summary(re_modelo2_siconfi)

# ----- FIM ABORDAGEM 2 - SICONFI -----

# ----- DEA/Malmquist - SIOPE e SICONFI -----

dados_dea <- read_excel("dados_mucuri.xlsx", sheet = "malmquist_data")
glimpse(dados_dea)

# SIOPE
dea_malmquist_siope <- make_malmquist(dados_dea,
                                percol = 2,
                                nper = 3,
                                arrangement = "vertical",
                                input = 3, #coluna 3 -> SIOPE
                                output = 5:6) 

modelo_malmquist_siope <- malmquist_index( dea_malmquist_siope,
                                     orientation = "oo",
                                     rts = "vrs")

summary(modelo_malmquist_siope, exportExcel = TRUE, filename = "malmquist_siope.xlsx")

#SICONFI
dea_malmquist_siconfi <- make_malmquist(dados_dea,
                                      percol = 2,
                                      nper = 3,
                                      arrangement = "vertical",
                                      input = 4, #coluna 4 -> SICONFI
                                      output = 5:6) 

modelo_malmquist_siconfi <- malmquist_index( dea_malmquist_siconfi,
                                           orientation = "oo",
                                           rts = "vrs")

summary(modelo_malmquist_siconfi, exportExcel = TRUE, filename = "malmquist_siconfi.xlsx")

# ----- FIM DEA - SIOPE e SICONFI -----

# ----- DEA por ano - SIOPE -----
dados_2017_siope <- dados_dea %>% 
  filter(ano == 2017)

dados_2019_siope <- dados_dea %>% 
  filter(ano == 2019)

dados_2023_siope <- dados_dea %>% 
  filter(ano == 2023)

#ANO DE 2017
dea_2017_siope <- make_deadata(dados_2017_siope,
                         ni = 1,
                         no = 2,
                         dmus = 1,
                         inputs = 3, # coluna 3 -> SIOPE
                         outputs = 5:6)

modelo_dea_2017_siope <- model_basic(dea_2017_siope,
                               orientation = "oo",
                               rts = "vrs")

summary(modelo_dea_2017_siope, exportExcel = TRUE, filename = "dea_2017_siope.xlsx")

#ANO DE 2019
dea_2019_siope <- make_deadata(dados_2019_siope,
                         ni = 1,
                         no = 2,
                         dmus = 1,
                         inputs = 3, # coluna 3 -> SIOPE
                         outputs = 5:6)

modelo_dea_2019_siope <- model_basic(dea_2019_siope,
                               orientation = "oo",
                               rts = "vrs")

summary(modelo_dea_2019_siope, exportExcel = TRUE, filename = "dea_2019_siope.xlsx")

# ANO DE 2023
dea_2023_siope <- make_deadata(dados_2023_siope,
                         ni = 1,
                         no = 2,
                         dmus = 1,
                         inputs = 3, # coluna 3 -> SIOPE
                         outputs = 5:6)

modelo_dea_2023_siope <- model_basic(dea_2023_siope,
                               orientation = "oo",
                               rts = "vrs")

summary(modelo_dea_2023_siope, exportExcel = TRUE, filename = "dea_2023_siope.xlsx")

# ----- FIM DEA POR ANO - SIOPE -----

# ----- DEA por ano - SICONFI -----
dados_2017_siconfi <- dados_dea %>% 
  filter(ano == 2017)

dados_2019_siconfi <- dados_dea %>% 
  filter(ano == 2019)

dados_2023_siconfi <- dados_dea %>% 
  filter(ano == 2023)

#ANO DE 2017
dea_2017_siconfi <- make_deadata(dados_2017_siconfi,
                               ni = 1,
                               no = 2,
                               dmus = 1,
                               inputs = 4, # coluna 3 -> siconfi
                               outputs = 5:6)

modelo_dea_2017_siconfi <- model_basic(dea_2017_siconfi,
                                     orientation = "oo",
                                     rts = "vrs")

summary(modelo_dea_2017_siconfi, exportExcel = TRUE, filename = "dea_2017_siconfi.xlsx")

#ANO DE 2019
dea_2019_siconfi <- make_deadata(dados_2019_siconfi,
                               ni = 1,
                               no = 2,
                               dmus = 1,
                               inputs = 4, # coluna 3 -> siconfi
                               outputs = 5:6)

modelo_dea_2019_siconfi <- model_basic(dea_2019_siconfi,
                                     orientation = "oo",
                                     rts = "vrs")

summary(modelo_dea_2019_siconfi, exportExcel = TRUE, filename = "dea_2019_siconfi.xlsx")

#ANO DE 2023
dea_2023_siconfi <- make_deadata(dados_2023_siconfi,
                               ni = 1,
                               no = 2,
                               dmus = 1,
                               inputs = 4, # coluna 3 -> siconfi
                               outputs = 5:6)

modelo_dea_2023_siconfi <- model_basic(dea_2023_siconfi,
                                     orientation = "oo",
                                     rts = "vrs")

summary(modelo_dea_2023_siconfi, exportExcel = TRUE, filename = "dea_2023_siconfi.xlsx")

# ----- FIM DEA POR ANO - SICONFI -----
