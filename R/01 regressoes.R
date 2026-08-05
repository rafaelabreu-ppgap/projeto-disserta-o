# ==============================================================================
# DISSERTAÇÃO PPGAP/UFVJM — GASTO POR MATRÍCULA E DESEMPENHO ESCOLAR (IDENE-MG)
# 
# ESTATÍSTICA DESCRITIVA, DECOMPOSIÇÃO DE VARIÂNCIA E CORRELAÇÕES
#
# Saídas: tabelas .csv, planilha .xlsx consolidada e figuras .png
# ==============================================================================

# ------------------------------------------------------------------------------
# 0. PACOTES
# ------------------------------------------------------------------------------
# install.packages(c("readxl","tidyverse","plm","lmtest","sandwich","car",
#                    "writexl","corrplot","knitr"))

library(readxl)
library(tidyverse)
library(plm)
library(lmtest)
library(sandwich)
library(writexl)
library(car)
library(dplyr)

options(scipen = 999)
dir.create("saidas", showWarnings = FALSE)

path_base <- "nova_base.xlsx"

# ------------------------------------------------------------------------------
# 1. CARGA E CONSTRUÇÃO DO PAINEL (idêntica a modelos_siope.R / modelos_siconfi.R)
# ------------------------------------------------------------------------------
saeb_raw <- read_excel(path_base, sheet = "SAEB")
gasto_raw <- read_excel(path_base, sheet = "GASTO")
inse_raw <- read_excel(path_base, sheet = "INSE")
matr_raw <- read_excel(path_base, sheet = "MATRÍCULAS")
tdi_raw <- read_excel(path_base, sheet = "TDI")

df_saeb <- saeb_raw %>%
  pivot_longer(cols = starts_with("SAEB"),
               names_to = c("ano", "materia"),
               names_pattern = "SAEB(\\d+)_(\\w+)_5") %>%
  mutate(ano = as.numeric(paste0("20", ano))) %>%
  pivot_wider(names_from = materia, values_from = value, names_prefix = "SAEB_")

df_inse <- inse_raw %>%
  rename(COD_IBGE = CO_MUNICIPIO) %>%
  select(-NO_MUNICIPIO) %>%
  pivot_longer(cols = starts_with("MD_INSE_"), names_to = "ano",
               names_prefix = "MD_INSE_", values_to = "INSE") %>%
  mutate(ano = as.numeric(ano))

df_matr <- matr_raw %>%
  rename(COD_IBGE = codigo_ibge) %>%
  select(COD_IBGE, ano, p_mat_rur_AI)

df_tdi <- tdi_raw %>%
  rename(ano = Ano, COD_IBGE = `Código do Município`) %>%
  select(COD_IBGE, ano, TDI)

# Função que constrói lags e médias móveis para a fonte escolhida ---------------
constroi_gasto <- function(fonte) {
  col <- paste0(fonte, "_CORR")
  gasto_raw %>%
    rename(ano = NUM_ANO) %>%
    select(COD_IBGE, ano, gasto = all_of(col)) %>%
    arrange(COD_IBGE, ano) %>%
    group_by(COD_IBGE) %>%
    mutate(
      g_t1 = dplyr::lag(gasto, 1),
      g_t2 = dplyr::lag(gasto, 2),
      g_t3 = dplyr::lag(gasto, 3),
      g_t4 = dplyr::lag(gasto, 4),
      M_t2_t4 = (g_t2 + g_t3 + g_t4) / 3,
      MedGasto5a = (gasto + g_t1 + g_t2 + g_t3 + g_t4) / 5
    ) %>%
    ungroup() %>%
    mutate(
      ln_gasto = log(gasto),
      ln_gasto_t_1 = log(g_t1),
      ln_mm_t2_t4 = log(M_t2_t4),
      ln_gasto_MA5 = log(MedGasto5a)
    ) %>%
    select(COD_IBGE, ano, gasto, MedGasto5a,
           ln_gasto, ln_gasto_t_1, ln_mm_t2_t4, ln_gasto_MA5)
}

constroi_painel <- function(fonte) {
  df_saeb %>%
    left_join(constroi_gasto(fonte), by = c("COD_IBGE", "ano")) %>%
    left_join(df_inse, by = c("COD_IBGE", "ano")) %>%
    left_join(df_matr, by = c("COD_IBGE", "ano")) %>%
    left_join(df_tdi, by = c("COD_IBGE", "ano")) %>%
    filter(ano >= 2019)
}

painel_siope <- constroi_painel("SIOPE")
painel_siconfi <- constroi_painel("SICONFI")

VARS <- c("SAEB_LP", "SAEB_MT", "gasto", "MedGasto5a",
          "ln_gasto", "ln_gasto_MA5", "INSE", "p_mat_rur_AI", "TDI")

# ------------------------------------------------------------------------------
# VERIFICAÇÃO DE INTEGRIDADE DO PAINEL
# ------------------------------------------------------------------------------
cat("\n===== INTEGRIDADE DO PAINEL =====\n")
cat("Municípios (N):", n_distinct(painel_siope$COD_IBGE), "\n")
cat("Períodos (T): ", n_distinct(painel_siope$ano), "->",
    paste(sort(unique(painel_siope$ano)), collapse = ", "), "\n")
cat("Observações (NT):", nrow(painel_siope), "\n")
cat("Painel balanceado:", is.pbalanced(pdata.frame(painel_siope,
                                                  index = c("COD_IBGE", "ano"))), "\n")
cat("Valores ausentes por variável:\n")
print(colSums(is.na(painel_siope[VARS])))

# ------------------------------------------------------------------------------
#    AUDITORIA DO DEFLATOR (fator implícito GASTO_CORR / GASTO_NOM)
#    Um índice de preços acumulado com base fixa é MONOTÔNICO por construção.
# ------------------------------------------------------------------------------
tab_deflator <- gasto_raw %>%
  group_by(NUM_ANO) %>%
  summarise(
    fator_SIOPE = mean(SIOPE_CORR / SIOPE_NOM, na.rm = TRUE),
    fator_SICONFI = mean(SICONFI_CORR / SICONFI_NOM, na.rm = TRUE),
    dp_fator_SIOPE = sd(SIOPE_CORR / SIOPE_NOM, na.rm = TRUE),
    gasto_nom_medio_SIOPE = mean(SIOPE_NOM, na.rm = TRUE),
    gasto_corr_medio_SIOPE = mean(SIOPE_CORR, na.rm = TRUE),
    gasto_nom_medio_SICONFI = mean(SICONFI_NOM, na.rm = TRUE),
    gasto_corr_medio_SICONFI = mean(SICONFI_CORR, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(monotonico = c(NA, diff(fator_SIOPE) < 0))

cat("\n===== AUDITORIA DO FATOR DE CONVERSÃO =====\n")
print(as.data.frame(tab_deflator), digits = 4)

# ------------------------------------------------------------------------------
#  ESTATÍSTICA DESCRITIVA GLOBAL COM DECOMPOSIÇÃO WITHIN / BETWEEN
# ------------------------------------------------------------------------------
descritivas <- function(base, fonte) {
  map_dfr(VARS, function(v) {
    x <- base[[v]]
    med_i <- ave(x, base$COD_IBGE, FUN = function(z) mean(z, na.rm = TRUE))
    within <- x - med_i
    between <- tapply(x, base$COD_IBGE, mean, na.rm = TRUE)
    tibble(
      Fonte = fonte, Variavel = v, N = sum(!is.na(x)),
      Media = mean(x, na.rm = TRUE), Mediana = median(x, na.rm = TRUE),
      DP_total = sd(x, na.rm = TRUE), DP_between = sd(between, na.rm = TRUE),
      DP_within = sd(within, na.rm = TRUE),
      Minimo = min(x, na.rm = TRUE), Maximo = max(x, na.rm = TRUE),
      Perc_var_within = 100 * var(within, na.rm = TRUE) / var(x, na.rm = TRUE)
    )
  })
}

tab_desc <- bind_rows(descritivas(painel_siope, "SIOPE"),
                      descritivas(painel_siconfi, "SICONFI"))
cat("\n===== DESCRITIVAS GLOBAIS =====\n"); print(as.data.frame(tab_desc), digits = 4)

# Alternativa nativa do plm para conferência do desvio-padrão within ------------
pdf_siope <- pdata.frame(painel_siope, index = c("COD_IBGE", "ano"))
pdf_siconfi <- pdata.frame(painel_siconfi, index = c("COD_IBGE", "ano"))
cat("\nConferência (plm::Within) — SIOPE ln_gasto_MA5:",
    sd(plm::Within(pdf_siope$ln_gasto_MA5)), "\n")
cat("Conferência (plm::Within) — SICONFI ln_gasto_MA5:",
    sd(plm::Within(pdf_siconfi$ln_gasto_MA5)), "\n")

# ------------------------------------------------------------------------------
#  ESTATÍSTICA DESCRITIVA POR ANO
# ------------------------------------------------------------------------------
descritivas_ano <- function(base, fonte) {
  base %>%
    select(ano, all_of(VARS)) %>%
    pivot_longer(-ano, names_to = "Variavel", values_to = "valor") %>%
    group_by(Fonte = fonte, Variavel, ano) %>%
    summarise(N = sum(!is.na(valor)), Media = mean(valor, na.rm = TRUE),
              Mediana = median(valor, na.rm = TRUE), DP = sd(valor, na.rm = TRUE),
              Minimo = min(valor, na.rm = TRUE), Maximo = max(valor, na.rm = TRUE),
              .groups = "drop")
}
tab_ano <- bind_rows(descritivas_ano(painel_siope, "SIOPE"),
                     descritivas_ano(painel_siconfi, "SICONFI"))
cat("\n===== DESCRITIVAS POR ANO =====\n"); print(as.data.frame(tab_ano), digits = 4)

# ------------------------------------------------------------------------------
# MATRIZES DE CORRELAÇÃO — POOLED E WITHIN
# ------------------------------------------------------------------------------
CORR_VARS <- c("SAEB_LP", "SAEB_MT", "ln_gasto", "ln_gasto_MA5",
               "INSE", "p_mat_rur_AI", "TDI")

corr_pooled <- function(base) round(cor(base[CORR_VARS], use = "complete.obs"), 3)

corr_within <- function(base) {
  w <- base %>% group_by(COD_IBGE) %>%
    mutate(across(all_of(CORR_VARS), ~ .x - mean(.x, na.rm = TRUE))) %>%
    ungroup()
  round(cor(w[CORR_VARS], use = "complete.obs"), 3)
}

cat("\n===== CORRELAÇÃO POOLED — SIOPE =====\n");   print(corr_pooled(painel_siope))
cat("\n===== CORRELAÇÃO WITHIN — SIOPE =====\n");   print(corr_within(painel_siope))
cat("\n===== CORRELAÇÃO POOLED — SICONFI =====\n"); print(corr_pooled(painel_siconfi))
cat("\n===== CORRELAÇÃO WITHIN — SICONFI =====\n"); print(corr_within(painel_siconfi))

# ------------------------------------------------------------------------------
#    INVARIÂNCIA DO DEFLATOR SOB DUPLA CENTRALIZAÇÃO
#    ln(G_it * f_t) = ln(G_it) + ln(f_t): o fator anual é absorvido pelas dummies
#    de tempo no regressor contemporâneo, mas NÃO na média móvel (log da soma).
# ------------------------------------------------------------------------------
two_way <- function(base, v) {
  x <- base[[v]]
  x - ave(x, base$COD_IBGE, FUN = mean) - ave(x, base$ano, FUN = mean) + mean(x)
}

teste_invariancia <- function(fonte) {
  nom <- gasto_raw %>% rename(ano = NUM_ANO) %>%
    select(COD_IBGE, ano, gasto = all_of(paste0(fonte, "_NOM"))) %>%
    arrange(COD_IBGE, ano) %>% group_by(COD_IBGE) %>%
    mutate(MA5 = (gasto + dplyr::lag(gasto, 1) + dplyr::lag(gasto, 2) +
                    dplyr::lag(gasto, 3) + dplyr::lag(gasto, 4)) / 5) %>%
    ungroup() %>%
    mutate(ln_gasto_nom = log(gasto), ln_MA5_nom = log(MA5)) %>%
    filter(ano %in% c(2019, 2021, 2023)) %>%
    select(COD_IBGE, ano, ln_gasto_nom, ln_MA5_nom)

  base <- constroi_painel(fonte) %>% left_join(nom, by = c("COD_IBGE", "ano"))
  cat("\n--- Invariância do deflator —", fonte, "---\n")
  cat("cor(ln_gasto duplamente centrado: nominal x corrigido) =",
      round(cor(two_way(base, "ln_gasto"), two_way(base, "ln_gasto_nom")), 8), "\n")
  cat("cor(ln_MA5   duplamente centrado: nominal x corrigido) =",
      round(cor(two_way(base, "ln_gasto_MA5"), two_way(base, "ln_MA5_nom")), 8), "\n")
}
teste_invariancia("SIOPE"); teste_invariancia("SICONFI")

# ------------------------------------------------------------------------------
# 8. FIGURAS
# ------------------------------------------------------------------------------
# Figura A — auditoria do deflator
p_defl <- tab_deflator %>%
  ggplot(aes(NUM_ANO, fator_SIOPE)) +
  geom_hline(yintercept = 1, linetype = 2, colour = "grey40") +
  geom_line(colour = "#B03A2E", linewidth = .8) +
  geom_point(colour = "#B03A2E", size = 2) +
  geom_text(aes(label = sprintf("%.3f", fator_SIOPE)), vjust = -.9, size = 2.6) +
  scale_x_continuous(breaks = 2015:2024) +
  labs(x = "Ano", y = "Fator", title = "Fator de conversão implícito (CORR/NOM)") +
  theme_minimal(base_size = 9)
ggsave("saidas/fig_A_auditoria_deflator.png", p_defl, width = 6, height = 3.2, dpi = 300)

# Figura B — distribuições por ano
p_dist <- painel_siope %>%
  select(ano, SAEB_LP, SAEB_MT, MedGasto5a, INSE) %>%
  pivot_longer(-ano) %>%
  ggplot(aes(factor(ano), value)) +
  geom_boxplot(fill = "#AED6F1", colour = "#2874A6", outlier.size = .5) +
  facet_wrap(~name, scales = "free_y") +
  labs(x = "Ano", y = NULL) + theme_minimal(base_size = 9)
ggsave("saidas/fig_B_distribuicoes.png", p_dist, width = 7, height = 5, dpi = 300)

# Figura C — decomposição da variância
p_var <- tab_desc %>%
  filter(Variavel %in% CORR_VARS) %>%
  ggplot(aes(reorder(Variavel, -Perc_var_within), Perc_var_within, fill = Fonte)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c(SIOPE = "#2874A6", SICONFI = "#B9770E")) +
  labs(x = NULL, y = "% da variância total", fill = NULL,
       title = "Participação da variância intramunicipal (within)") +
  theme_minimal(base_size = 9) + theme(axis.text.x = element_text(angle = 20, hjust = 1))
ggsave("saidas/fig_C_decomposicao_variancia.png", p_var, width = 6.5, height = 3.4, dpi = 300)

# Figura D — dispersão within: SAEB x gasto
w_siope <- painel_siope %>% group_by(COD_IBGE) %>%
  mutate(across(all_of(CORR_VARS), ~ .x - mean(.x))) %>% ungroup()
p_disp <- w_siope %>%
  select(SAEB_LP, ln_gasto, ln_gasto_MA5) %>%
  pivot_longer(-SAEB_LP) %>%
  ggplot(aes(value, SAEB_LP)) +
  geom_point(alpha = .35, size = .8, colour = "#2874A6") +
  geom_smooth(method = "lm", se = FALSE, colour = "#B03A2E", linewidth = .8) +
  facet_wrap(~name, scales = "free_x") +
  labs(x = "Desvio da média municipal", y = "SAEB LP (desvio da média municipal)") +
  theme_minimal(base_size = 9)
ggsave("saidas/fig_D_within_saeb_gasto.png", p_disp, width = 7, height = 3.4, dpi = 300)

# ------------------------------------------------------------------------------
# 9. EXPORTAÇÃO
# ------------------------------------------------------------------------------
write_xlsx(list(
  descritivas_globais = tab_desc,
  descritivas_por_ano = tab_ano,
  auditoria_deflator = tab_deflator,
  corr_pooled_siope = as.data.frame(corr_pooled(painel_siope)) %>% rownames_to_column("var"),
  corr_within_siope = as.data.frame(corr_within(painel_siope)) %>% rownames_to_column("var"),
  corr_pooled_siconfi = as.data.frame(corr_pooled(painel_siconfi)) %>% rownames_to_column("var"),
  corr_within_siconfi = as.data.frame(corr_within(painel_siconfi)) %>% rownames_to_column("var")
), "saidas/estatisticas_descritivas.xlsx")

# ==============================================================================
#     TESTES DE DIAGNÓSTICO ADEQUADOS A PAINEL CURTO
#     Substituem Durbin-Watson, Shapiro-Wilk e Breusch-Pagan de MQO clássico.
# ==============================================================================

# ------------------------------------------------------------------------------
# Utilitário: remove classe pseries e atributo index de todas as colunas
# ------------------------------------------------------------------------------
limpa_pseries <- function(x) {
  x <- as.data.frame(x)
  x[] <- lapply(x, function(z) {
    if (inherits(z, "pseries")) {
      attr(z, "index") <- NULL
      class(z) <- setdiff(class(z), "pseries")
    }
    z
  })
  x
}

# ------------------------------------------------------------------------------
# RESET de Ramsey sobre a especificação TWFE
#
# Duas versões equivalentes:
#   (a) LSDV — inclui explicitamente as dummies de município e de ano;
#   (b) within duplamente centrada — mesma estatística, sem estimar 190 dummies.
# Reporta-se (a) por transparência; (b) serve de conferência.
# ------------------------------------------------------------------------------
reset_twfe <- function(base, formula, id = "COD_IBGE", tempo = "ano") {
  d <- limpa_pseries(base)
  
  # (a) LSDV
  f_lsdv <- update(formula, paste0(". ~ . + factor(", id, ") + factor(", tempo, ")"))
  m_lsdv <- lm(f_lsdv, data = d)
  r_lsdv <- resettest(m_lsdv, power = 2:3, type = "fitted")
  
  # (b) dupla centralização manual
  vars <- all.vars(formula)
  w <- d %>%
    group_by(.data[[id]])    %>% mutate(across(all_of(vars), ~ .x - mean(.x, na.rm = TRUE))) %>% ungroup() %>%
    group_by(.data[[tempo]]) %>% mutate(across(all_of(vars), ~ .x - mean(.x, na.rm = TRUE))) %>% ungroup()
  r_within <- resettest(formula, power = 2:3, type = "fitted", data = as.data.frame(w))
  
  list(lsdv = r_lsdv, within = r_within)
}

# ------------------------------------------------------------------------------
# Bloco de diagnósticos
# ------------------------------------------------------------------------------
diagnosticos <- function(base, dep) {
  
  pdf_base <- pdata.frame(base, index = c("COD_IBGE", "ano"))
  d        <- limpa_pseries(base)
  
  f_ma5 <- as.formula(paste(dep, "~ ln_gasto_MA5 + INSE + p_mat_rur_AI + TDI"))
  f_lag <- as.formula(paste(dep, "~ ln_gasto + ln_gasto_t_1 + ln_mm_t2_t4 + INSE + p_mat_rur_AI + TDI"))
  
  ef_ma5 <- plm(f_ma5, pdf_base, model = "within",  effect = "individual")
  ea_ma5 <- plm(f_ma5, pdf_base, model = "random",  effect = "individual")
  tw_ma5 <- plm(f_ma5, pdf_base, model = "within",  effect = "twoways")
  ef_lag <- plm(f_lag, pdf_base, model = "within",  effect = "individual")
  tw_lag <- plm(f_lag, pdf_base, model = "within",  effect = "twoways")
  
  cat("\n########## DIAGNÓSTICOS —", dep, "##########\n")
  
  cat("\n[1] Hausman (FE x RE, one-way)\n");                print(phtest(ef_ma5, ea_ma5))
  cat("\n[2] Teste F de efeitos temporais — MA5\n");        print(pFtest(tw_ma5, ef_ma5))
  cat("\n[3] Teste F de efeitos temporais — defasagens\n"); print(pFtest(tw_lag, ef_lag))
  
  # Wooldridge aplicado à especificação FINAL (MA5) e à de defasagens
  cat("\n[4] Wooldridge (autocorrelação) — MA5\n");         print(pwartest(tw_ma5))
  cat("\n[5] Wooldridge (autocorrelação) — defasagens\n");  print(pwartest(tw_lag))
  
  cat("\n[6] Pesaran CD (dependência transversal) — MA5\n")
  print(pcdtest(tw_ma5, test = "cd"))
  
  cat("\n[7] RESET de Ramsey sobre a especificação TWFE\n")
  rr <- reset_twfe(d, f_ma5)
  cat("  (a) LSDV com dummies de município e ano:\n");  print(rr$lsdv)
  cat("  (b) Conferência por dupla centralização:\n");  print(rr$within)
  
  cat("\n[8] VIF — especificação com defasagens\n")
  print(vif(lm(f_lag, data = d)))
  
  cat("\n[9] Erros-padrão HC1 agrupados por município (inferência principal)\n")
  print(coeftest(tw_ma5, vcovHC(tw_ma5, type = "HC1", cluster = "group")))
  
  cat("\n[10] Erros-padrão de Driscoll-Kraay (robustez)\n")
  print(coeftest(tw_ma5, vcovSCC(tw_ma5, type = "HC1", maxlag = 1)))
  
  cat("\n[11] Medidas de ajuste\n")
  s <- summary(tw_ma5)
  cat("  R2 within:", s$r.squared[["rsq"]], "\n")
  g <- summary(lm(update(f_ma5, . ~ . + factor(COD_IBGE) + factor(ano)), data = d))
  cat("  R2 global:", g$r.squared, " | R2 global ajustado:", g$adj.r.squared, "\n")
  
  invisible(list(tw_ma5 = tw_ma5, tw_lag = tw_lag))
}

# ------------------------------------------------------------------------------
# Execução — atenção: passar os PAINÉIS (data.frame), não os pdata.frame
# ------------------------------------------------------------------------------
cat("\n\n############ FONTE: SIOPE ############\n")
diag_siope_lp <- diagnosticos(painel_siope, "SAEB_LP")
diag_siope_mt <- diagnosticos(painel_siope, "SAEB_MT")

cat("\n\n############ FONTE: SICONFI ############\n")
diag_siconfi_lp <- diagnosticos(painel_siconfi, "SAEB_LP")
diag_siconfi_mt <- diagnosticos(painel_siconfi, "SAEB_MT")

# FIM DO SCRIPT
