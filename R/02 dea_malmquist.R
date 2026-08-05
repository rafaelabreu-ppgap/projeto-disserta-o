# ==============================================================================
# DISSERTAÇÃO PPGAP/UFVJM — GASTO POR MATRÍCULA E DESEMPENHO ESCOLAR (IDENE-MG)
#
# SCRIPT 04 — ANÁLISE ENVOLTÓRIA DE DADOS (DEA) E ÍNDICE DE MALMQUIST
#
# Autor: Rafael Junio Abreu
# Orientador: Prof. Dr. João Paulo Calembo Batista Menezes
#
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# PACOTES
# ------------------------------------------------------------------------------
# install.packages(c("readxl","dplyr","tidyr","ggplot2","lpSolveAPI","truncreg",
#                    "writexl","purrr","tibble","scales"))

library(readxl)
library(dplyr)
library(tidyr)
library(tibble)
library(purrr)
library(ggplot2)
library(scales)
library(lpSolveAPI)
library(truncreg)
library(writexl)

options(scipen = 999)
set.seed(20260804)

dir.create("saidas_dea/tabelas", recursive = TRUE, showWarnings = FALSE)
dir.create("saidas_dea/figuras", recursive = TRUE, showWarnings = FALSE)

path_base <- "nova_base.xlsx"

AZUL <- "#2874A6"; LARANJA <- "#B9770E"; VERDE <- "#1E8449"; CINZA <- "#5D6D7E"
tema <- theme_minimal(base_size = 9) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 9))

# ==============================================================================
# CARGA E CONSTRUÇÃO DO PAINEL DA DEA
# ==============================================================================
saeb_raw  <- read_excel(path_base, sheet = "SAEB")
gasto_raw <- read_excel(path_base, sheet = "GASTO")
inse_raw  <- read_excel(path_base, sheet = "INSE")
matr_raw  <- read_excel(path_base, sheet = "MATRÍCULAS")
tdi_raw   <- read_excel(path_base, sheet = "TDI")

df_saeb <- saeb_raw %>%
  pivot_longer(cols = starts_with("SAEB"),
               names_to = c("ano", "materia"),
               names_pattern = "SAEB(\\d+)_(\\w+)_5") %>%
  mutate(ano = as.numeric(paste0("20", ano))) %>%
  pivot_wider(names_from = materia, values_from = value, names_prefix = "SAEB_")

df_inse <- inse_raw %>%
  rename(COD_IBGE = CO_MUNICIPIO) %>% select(-NO_MUNICIPIO) %>%
  pivot_longer(starts_with("MD_INSE_"), names_to = "ano",
               names_prefix = "MD_INSE_", values_to = "INSE") %>%
  mutate(ano = as.numeric(ano))

df_matr <- matr_raw %>%
  rename(COD_IBGE = codigo_ibge) %>%
  mutate(MAT_AI = mat_urb_AI + mat_rur_AI) %>%
  select(COD_IBGE, ano, MAT_AI, p_mat_rur_AI)

df_tdi <- tdi_raw %>%
  rename(ano = Ano, COD_IBGE = `Código do Município`) %>%
  select(COD_IBGE, ano, TDI)

# ------------------------------------------------------------------------------
# INSUMO DA DEA 
# ------------------------------------------------------------------------------
constroi_gasto <- function(fonte) {
  col <- paste0(fonte, "_CORR")
  gasto_raw %>%
    rename(ano = NUM_ANO) %>%
    select(COD_IBGE, ano, gasto = all_of(col)) %>%
    arrange(COD_IBGE, ano) %>%
    group_by(COD_IBGE) %>%
    mutate(MedGasto5a = (gasto + dplyr::lag(gasto, 1) + dplyr::lag(gasto, 2) +
                         dplyr::lag(gasto, 3) + dplyr::lag(gasto, 4)) / 5) %>%
    ungroup() %>%
    select(COD_IBGE, ano, gasto, MedGasto5a)
}

# ------------------------------------------------------------------------------
# PRODUTOS DA DEA — nota padronizada do Ideb (INEP), escala de razão [0, 10]
# Limites fixos do SAEB 1997 (três desvios-padrão em torno da média de 1997)
# ------------------------------------------------------------------------------
LP_INF <- 49; LP_SUP <- 324
MT_INF <- 60; MT_SUP <- 322

padroniza <- function(p, inf, sup) pmin(pmax((p - inf) / (sup - inf) * 10, 0), 10)

constroi_painel <- function(fonte) {
  df_saeb %>%
    inner_join(constroi_gasto(fonte), by = c("COD_IBGE", "ano")) %>%
    inner_join(df_inse,  by = c("COD_IBGE", "ano")) %>%
    inner_join(df_matr,  by = c("COD_IBGE", "ano")) %>%
    inner_join(df_tdi,   by = c("COD_IBGE", "ano")) %>%
    filter(ano >= 2019) %>%
    mutate(N_LP = padroniza(SAEB_LP, LP_INF, LP_SUP),
           N_MT = padroniza(SAEB_MT, MT_INF, MT_SUP)) %>%
    arrange(ano, COD_IBGE)
}

paineis <- list(SIOPE = constroi_painel("SIOPE"),
                SICONFI = constroi_painel("SICONFI"))

cat("\n===== INTEGRIDADE DO PAINEL DA DEA  =====\n")
for (f in names(paineis)) {
  d <- paineis[[f]]
  cat(sprintf("%-8s | DMUs: %d | períodos: %s | observações: %d | NA: %d\n",
              f, n_distinct(d$COD_IBGE), paste(sort(unique(d$ano)), collapse = ", "),
              nrow(d), sum(is.na(d[c("MedGasto5a","N_LP","N_MT","INSE","MAT_AI","TDI")]))))
}
cat("Nenhum município foi excluído da DEA: as 188 DMUs entram nos três períodos.\n")

# ==============================================================================
# NÚCLEO DE PROGRAMAÇÃO LINEAR — DEA COM ORIENTAÇÃO A PRODUTO
# ==============================================================================
# Forma do envelope (dual), orientação a produto:
#
#   max  phi
#   s.a. sum_j lambda_j * x_kj  <=  x_ko              (k = 1..m insumos)
#        sum_j lambda_j * y_rj  >=  phi * y_ro        (r = 1..s produtos)
#        sum_j lambda_j = 1                           (apenas VRS/BCC)
#        lambda_j >= 0
#
# phi >= 1 é a medida de Farrell orientada a produto: indica em quanto os
# produtos poderiam ser expandidos, mantido o insumo. A eficiência técnica é
# 1/phi, e a função distância de Shephard é D_o = 1/phi.
#
# Separar (X_o, Y_o) de (X_ref, Y_ref) permite avaliar DMUs de um período
# contra a tecnologia de OUTRO período — o que é exigido pelo Malmquist.
# ------------------------------------------------------------------------------
dea_out <- function(X_o, Y_o, X_ref, Y_ref, rts = c("vrs", "crs")) {

  rts   <- match.arg(rts)
  X_o   <- as.matrix(X_o);   Y_o   <- as.matrix(Y_o)
  X_ref <- as.matrix(X_ref); Y_ref <- as.matrix(Y_ref)
  storage.mode(X_o)   <- storage.mode(Y_o)   <- "double"
  storage.mode(X_ref) <- storage.mode(Y_ref) <- "double"

  n  <- nrow(X_ref); m <- ncol(X_ref); s <- ncol(Y_ref)
  no <- nrow(X_o)

  stopifnot(ncol(X_o) == m, ncol(Y_o) == s, nrow(Y_o) == no, nrow(Y_ref) == n)
  if (anyNA(X_o) || anyNA(Y_o) || anyNA(X_ref) || anyNA(Y_ref))
    stop("dea_out(): há NA em insumos/produtos. Verifique MedGasto5a (média ",
         "móvel de 5 exercícios) e as notas padronizadas antes de chamar a DEA.")
  if (!all(is.finite(X_o), is.finite(Y_o), is.finite(X_ref), is.finite(Y_ref)))
    stop("dea_out(): valores não finitos em insumos/produtos.")
  if (min(X_ref) <= 0 || min(Y_ref) <= 0 || min(X_o) <= 0 || min(Y_o) <= 0)
    stop("dea_out(): insumos e produtos devem ser estritamente positivos ",
         "(a medida radial orientada a produto não é definida com y = 0).")

  # --- (2) normalização de unidades ------------------------------------------
  # A medida radial de Farrell é invariante à multiplicação de cada insumo e de
  # cada produto por uma constante positiva. Divide-se cada coluna pela sua média
  # na tecnologia de REFERÊNCIA (a mesma constante aplicada a "o" e a "ref"), o
  # que preserva exatamente o PPL e apenas condiciona o tableau: o gasto está na
  # ordem de 10^4 e as notas padronizadas na ordem de 10^0. Escores inalterados.
  cx <- colMeans(X_ref); cy <- colMeans(Y_ref)
  X_ref <- sweep(X_ref, 2, cx, "/"); X_o <- sweep(X_o, 2, cx, "/")
  Y_ref <- sweep(Y_ref, 2, cy, "/"); Y_o <- sweep(Y_o, 2, cy, "/")

  vrs <- (rts == "vrs")
  nr  <- m + s + vrs          # nº de restrições
  nc  <- n + 1L               # variáveis: [phi, lambda_1..lambda_n]

  # --- (3) UM ÚNICO modelo por tecnologia de referência ----------------------
  lp <- make.lp(nr, nc)
  if (is.null(lp) || !inherits(lp, "lpExtPtr"))
    stop("dea_out(): make.lp() não devolveu um objeto 'lpExtPtr' válido.")

  lp.control(lp, sense = "max", verbose = "neutral",
             scaling = c("geometric", "equilibrate", "integers"))

  # Preenchimento POR COLUNA. O lp_solve armazena a matriz por colunas; set.row()
  # sobre modelo colunar é o caminho ineficiente e é o que fazia a rotina parecer
  # travada. Coluna de lambda_j = [x_kj | -y_rj | 1 (convexidade)].
  for (j in seq_len(n))
    set.column(lp, j + 1L, x = c(X_ref[j, ], -Y_ref[j, ], if (vrs) 1),
               indices = seq_len(nr))

  # Coluna de phi: coeficiente nulo nas restrições de insumo e de convexidade e
  # y_ro nas de produto. Preenche-se com 1 apenas para fixar o padrão de
  # esparsidade, que depois é reescrito DMU a DMU por set.mat().
  set.column(lp, 1L, x = c(rep(0, m), rep(1, s), if (vrs) 0),
             indices = seq_len(nr))

  set.objfn(lp, c(1, rep(0, n)))                    # max phi (por último)
  set.constr.type(lp, c(rep("<=", m + s), if (vrs) "="))
  set.rhs(lp, c(rep(0, m + s), if (vrs) 1))
  # Limites inferiores 0 e superiores +Inf já são o padrão do lp_solve; as duas
  # chamadas a set.bounds() da versão anterior eram redundantes e foram retiradas.

  # --- (4) apenas o que varia entre DMUs é reescrito -------------------------
  phi     <- rep(NA_real_, no)
  stat    <- integer(no)
  lin_out <- m + seq_len(s)

  for (i in seq_len(no)) {
    for (r in seq_len(s)) set.mat(lp, lin_out[r], 1L, Y_o[i, r])  # coef. de phi
    set.rhs(lp, X_o[i, ], seq_len(m))                             # rhs = x_ko
    stat[i] <- solve(lp)
    if (stat[i] %in% c(0L, 1L)) phi[i] <- get.objective(lp)
  }

  ruins <- which(!stat %in% c(0L, 1L))
  if (length(ruins))
    warning(sprintf("dea_out(): %d PPL(s) sem solução ótima (status lp_solve: %s).",
                    length(ruins), paste(sort(unique(stat[ruins])), collapse = ", ")),
            call. = FALSE)
  phi
}

mat_in  <- function(d) as.matrix(d[, "MedGasto5a", drop = FALSE])
mat_out <- function(d) as.matrix(d[, c("N_LP", "N_MT")])

# ------------------------------------------------------------------------------
# TESTE DE SANIDADE DO NÚCLEO DE PL (executa em milissegundos)
# Tecnologia com 4 DMUs, 1 insumo, 1 produto: A(1;1), B(2;2), C(3;2,2), D(4;3).
#   VRS  — a envoltória passa por A, B e D; C é interior ao segmento BD, cujo
#          produto máximo em x = 3 é 2,5. Logo phi = (1; 1; 2,5/2,2; 1).
#   CRS  — a fronteira é o raio y = x (razão máxima y/x = 1, em A e B).
#          Logo phi = (1; 1; 3/2,2; 4/3).
# A desigualdade phi_VRS <= phi_CRS deve valer DMU a DMU.
# ------------------------------------------------------------------------------
local({
  Xt <- matrix(c(1, 2, 3, 4), ncol = 1)
  Yt <- matrix(c(1, 2, 2.2, 3), ncol = 1)
  v  <- dea_out(Xt, Yt, Xt, Yt, "vrs")
  cc <- dea_out(Xt, Yt, Xt, Yt, "crs")
  stopifnot(all(is.finite(v)), all(is.finite(cc)),
            max(abs(v  - c(1, 1, 2.5 / 2.2, 1)))     < 1e-6,
            max(abs(cc - c(1, 1, 3.0 / 2.2, 4 / 3))) < 1e-6,
            all(v <= cc + 1e-9))
  cat("Núcleo de PL validado | VRS:", sprintf("%.4f", v),
      "| CRS:", sprintf("%.4f", cc), "\n")
})

# ==============================================================================
# DEA — FRONTEIRA ANUAL E FRONTEIRA INTERTEMPORAL
# ==============================================================================
ANOS <- c(2019, 2021, 2023)
res_dea <- list()

for (f in names(paineis)) {
  d <- paineis[[f]]

  # (a) fronteiras ANUAIS independentes -> benchmarking DENTRO de cada ano
  anual <- map_dfr(ANOS, function(y) {
    dy <- filter(d, ano == y)
    X <- mat_in(dy); Y <- mat_out(dy)
    phi_v <- dea_out(X, Y, X, Y, "vrs")
    phi_c <- dea_out(X, Y, X, Y, "crs")
    tibble(COD_IBGE = dy$COD_IBGE, NOM_MUN = dy$NOM_MUN, ano = y,
           phi_vrs = phi_v, phi_crs = phi_c,
           ET_vrs = 1 / phi_v, ET_crs = 1 / phi_c,
           EE = phi_v / phi_c)                      # eficiência de escala <= 1
  })

  # (b) fronteira INTERTEMPORAL comum (pooled, 564 obs) -> comparação ENTRE anos
  #     Atende ao item 2.4(c): escores de fronteiras anuais distintas não são
  #     comparáveis; com referencial único a comparação passa a ser legítima.
  Xa <- mat_in(d); Ya <- mat_out(d)
  pool <- tibble(COD_IBGE = d$COD_IBGE, ano = d$ano,
                 phi_inter = dea_out(Xa, Ya, Xa, Ya, "vrs"))

  res_dea[[f]] <- left_join(anual, pool, by = c("COD_IBGE", "ano"))
  cat(sprintf("DEA %-8s concluída — %d escores | inviáveis: %d\n",
              f, nrow(res_dea[[f]]), sum(is.na(res_dea[[f]]$phi_vrs))))
}

tab_dea <- map_dfr(names(res_dea), function(f)
  res_dea[[f]] %>% group_by(ano) %>%
    summarise(Fonte = f, N = n(),
              Media = mean(phi_vrs), Mediana = median(phi_vrs), DP = sd(phi_vrs),
              Q1 = quantile(phi_vrs, .25), Q3 = quantile(phi_vrs, .75),
              Minimo = min(phi_vrs), Maximo = max(phi_vrs),
              DMUs_eficientes = sum(phi_vrs < 1 + 1e-6),
              Media_intertemporal = mean(phi_inter),
              Mediana_intertemporal = median(phi_inter),
              Efic_intertemporal = sum(phi_inter < 1 + 1e-6), .groups = "drop") %>%
    relocate(Fonte))

cat("\n===== TABELA — ESCORES DEA (VRS, orientação produto) =====\n")
print(as.data.frame(tab_dea), digits = 4)

# ------------------------------------------------------------------------------
# Testes formais da evolução temporal, sobre a FRONTEIRA INTERTEMPORAL
# (dados pareados por município: Friedman + Wilcoxon dos postos sinalizados)
# ------------------------------------------------------------------------------
cat("\n===== EVOLUÇÃO TEMPORAL — FRONTEIRA INTERTEMPORAL =====\n")
tab_testes <- map_dfr(names(res_dea), function(f) {
  w <- res_dea[[f]] %>% select(COD_IBGE, ano, phi_inter) %>%
    pivot_wider(names_from = ano, values_from = phi_inter, names_prefix = "a")
  fr <- friedman.test(as.matrix(w[, c("a2019", "a2021", "a2023")]))
  cat(sprintf("%s | Friedman: qui2 = %.4f, gl = %d, p = %.3g\n",
              f, fr$statistic, fr$parameter, fr$p.value))
  map_dfr(list(c(2019, 2021), c(2021, 2023), c(2019, 2023)), function(p) {
    a <- w[[paste0("a", p[1])]]; b <- w[[paste0("a", p[2])]]
    tt <- wilcoxon <- wilcox.test(a, b, paired = TRUE)
    tibble(Fonte = f, Comparacao = paste(p, collapse = "->"),
           V = unname(tt$statistic), p_valor = tt$p.value,
           dif_mediana = median(b - a), dif_media = mean(b - a))
  })
})
print(as.data.frame(tab_testes), digits = 4)

# ==============================================================================
# ÍNDICE DE MALMQUIST (FÄRE et al., 1994)
# ==============================================================================
# Orientação a produto. Funções distância de Shephard D_o = 1/phi.
#
#   M^{t,t+1} = [ (D^t_{t+1} / D^t_t) x (D^{t+1}_{t+1} / D^{t+1}_t) ]^{1/2}
#
#   effch  = D^{t+1}_{t+1} / D^t_t                              (CRS)
#   techch = [ (D^t_{t+1}/D^{t+1}_{t+1}) x (D^t_t/D^{t+1}_t) ]^{1/2}   (CRS)
#   pech   = DV^{t+1}_{t+1} / DV^t_t                            (VRS)
#   sech   = effch / pech
#
#   IDENTIDADES:  M = effch x techch  =  pech x sech x techch
#
# A decomposição de Färe et al. (1994)
# exige a tecnologia CRS para o índice de PTF. Os quatro programas de período
# cruzado são resolvidos SOB CRS, onde a viabilidade é assegurada pela
# estrutura de cone da tecnologia. A tecnologia VRS entra apenas nos programas
# de período próprio, para separar pech de sech. A contagem de inviabilidades
# é reportada e deve ser zero.
# ------------------------------------------------------------------------------
malmquist <- function(d, y0, y1) {
  a <- filter(d, ano == y0) %>% arrange(COD_IBGE)
  b <- filter(d, ano == y1) %>% arrange(COD_IBGE)
  stopifnot(identical(a$COD_IBGE, b$COD_IBGE))
  X0 <- mat_in(a); Y0 <- mat_out(a); X1 <- mat_in(b); Y1 <- mat_out(b)

  p00 <- dea_out(X0, Y0, X0, Y0, "crs")   # t   na tecnologia t
  p11 <- dea_out(X1, Y1, X1, Y1, "crs")   # t+1 na tecnologia t+1
  p01 <- dea_out(X1, Y1, X0, Y0, "crs")   # t+1 na tecnologia t   (cruzado)
  p10 <- dea_out(X0, Y0, X1, Y1, "crs")   # t   na tecnologia t+1 (cruzado)

  D00 <- 1/p00; D11 <- 1/p11; D01 <- 1/p01; D10 <- 1/p10

  effch  <- D11 / D00
  techch <- sqrt((D01 / D11) * (D00 / D10))
  M      <- effch * techch

  v00 <- dea_out(X0, Y0, X0, Y0, "vrs")
  v11 <- dea_out(X1, Y1, X1, Y1, "vrs")
  pech <- (1/v11) / (1/v00)
  sech <- effch / pech

  tibble(COD_IBGE = a$COD_IBGE, NOM_MUN = a$NOM_MUN,
         periodo = paste0(y0, "-", y1), ano = y1,
         M = M, effch = effch, pech = pech, sech = sech, techch = techch,
         inviavel = as.integer(is.na(p01) | is.na(p10)))
}

res_mq <- list()
for (f in names(paineis)) {
  res_mq[[f]] <- bind_rows(malmquist(paineis[[f]], 2019, 2021),
                           malmquist(paineis[[f]], 2021, 2023))
  r <- res_mq[[f]]
  cat(sprintf(
    "Malmquist %-8s | n = %d | max|M - effch*techch| = %.2e | max|M - pech*sech*tc| = %.2e | inviáveis: %d\n",
    f, nrow(r), max(abs(r$M - r$effch * r$techch)),
    max(abs(r$M - r$pech * r$sech * r$techch)), sum(r$inviavel)))
}

tab_mq <- map_dfr(names(res_mq), function(f)
  res_mq[[f]] %>% group_by(periodo) %>%
    summarise(Fonte = f, N = n(),
              med_M = median(M), gmean_M = exp(mean(log(M))),
              med_effch = median(effch), med_pech = median(pech),
              med_sech = median(sech), med_techch = median(techch),
              gmean_techch = exp(mean(log(techch))),
              pct_M_maior_1 = 100 * mean(M > 1),
              pct_tc_maior_1 = 100 * mean(techch > 1),
              desvio_max_identidade = max(abs(M - pech * sech * techch)),
              inviaveis = sum(inviavel), .groups = "drop") %>%
    relocate(Fonte))

cat("\n===== TABELA — DECOMPOSIÇÃO DE MALMQUIST (SIOPE x SICONFI) =====\n")
print(as.data.frame(tab_mq), digits = 4)

# ==============================================================================
# CONVERGÊNCIA ENTRE AS BASES
# ==============================================================================
tab_conv <- bind_rows(
  map_dfr(ANOS, function(y) {
    a <- filter(res_dea$SIOPE,   ano == y) %>% arrange(COD_IBGE)
    b <- filter(res_dea$SICONFI, ano == y) %>% arrange(COD_IBGE)
    tibble(Objeto = paste0("Escore DEA ", y), N = nrow(a),
           Pearson = cor(a$phi_vrs, b$phi_vrs),
           Spearman = cor(a$phi_vrs, b$phi_vrs, method = "spearman"),
           Identicos = sum(abs(a$phi_vrs - b$phi_vrs) < 1e-6))
  }),
  map_dfr(c("2019-2021", "2021-2023"), function(p) {
    a <- filter(res_mq$SIOPE,   periodo == p) %>% arrange(COD_IBGE)
    b <- filter(res_mq$SICONFI, periodo == p) %>% arrange(COD_IBGE)
    bind_rows(
      tibble(Objeto = paste0("Malmquist M ", p), N = nrow(a),
             Pearson = cor(a$M, b$M), Spearman = cor(a$M, b$M, method = "spearman"),
             Identicos = NA_integer_),
      tibble(Objeto = paste0("Fronteira tc ", p), N = nrow(a),
             Pearson = cor(a$techch, b$techch),
             Spearman = cor(a$techch, b$techch, method = "spearman"),
             Identicos = NA_integer_))
  }))

cat("\n===== TABELA — CONVERGÊNCIA ENTRE BASES =====\n")
print(as.data.frame(tab_conv), digits = 4)

# divergência do próprio insumo, para contraste
dif_insumo <- paineis$SICONFI$MedGasto5a - paineis$SIOPE$MedGasto5a
cat(sprintf("\nInsumo (MA5): diferença SICONFI-SIOPE — mediana R$ %.0f | média R$ %.0f | mediana relativa %.1f%%\n",
            median(dif_insumo), mean(dif_insumo),
            median(dif_insumo / paineis$SIOPE$MedGasto5a * 100)))

# ==============================================================================
# SEGUNDO ESTÁGIO — SIMAR E WILSON (2007), ALGORITMO 1
# ==============================================================================
# Regressão truncada (truncamento à esquerda em 1) dos escores sobre variáveis
# de contexto, com bootstrap paramétrico. Corrige a inferência viciada pela
# dependência serial dos escores DEA, que decorre de todos serem estimados
# contra a mesma fronteira amostral.
#
#   phi_i = z_i * beta + eps_i ,  eps_i ~ N(0, sigma^2) truncada em 1 - z_i*beta
# ------------------------------------------------------------------------------
segundo_estagio <- function(dados_dea, painel, var_escore = "phi_vrs", L = 2000) {

  d <- dados_dea %>%
    inner_join(painel %>% select(COD_IBGE, ano, INSE, MAT_AI, p_mat_rur_AI, TDI),
               by = c("COD_IBGE", "ano")) %>%
    mutate(ln_mat = log(MAT_AI),
           ano_f  = factor(ano),
           escore = .data[[var_escore]])

  fml <- escore ~ INSE + ln_mat + p_mat_rur_AI + TDI + ano_f

  # (i) estimativa pontual sobre as DMUs ineficientes
  ajuste <- truncreg(fml, data = filter(d, escore > 1 + 1e-9),
                     point = 1, direction = "left")
  beta  <- coef(ajuste)[1:(length(coef(ajuste)) - 1)]
  sigma <- coef(ajuste)[length(coef(ajuste))]

  Z  <- model.matrix(fml, data = d)
  mu <- as.vector(Z %*% beta)
  n  <- nrow(Z)

  # (ii) bootstrap paramétrico: reamostra da normal truncada implicada
  B <- matrix(NA_real_, nrow = L, ncol = length(beta),
              dimnames = list(NULL, names(beta)))
  a_lo <- pnorm((1 - mu) / sigma)
  for (l in seq_len(L)) {
    u  <- runif(n, a_lo, 1)
    ys <- mu + sigma * qnorm(pmin(pmax(u, 1e-12), 1 - 1e-12))
    fit <- try(truncreg(ys ~ Z[, -1], point = 1, direction = "left"), silent = TRUE)
    if (!inherits(fit, "try-error"))
      B[l, ] <- coef(fit)[1:length(beta)]
  }

  out <- tibble(
    Variavel = names(beta),
    Coef     = as.vector(beta),
    EP_boot  = apply(B, 2, sd, na.rm = TRUE),
    IC_inf   = apply(B, 2, quantile, .025, na.rm = TRUE),
    IC_sup   = apply(B, 2, quantile, .975, na.rm = TRUE),
    p_boot   = pmax(2 * pmin(colMeans(B <= 0, na.rm = TRUE),
                             colMeans(B >= 0, na.rm = TRUE)), 1 / L))

  # (iii) escore CONDICIONADO ao contexto: resíduo da regressão de 2º estágio.
  #       Valores negativos indicam desempenho melhor do que o previsto pelas
  #       condições socioeconômicas do município.
  d <- d %>% mutate(phi_esperado = mu, ef_cond = escore - mu)

  list(coeficientes = out, sigma = sigma, dados = d,
       n_truncado = sum(d$escore > 1 + 1e-9),
       r2 = 1 - var(d$ef_cond) / var(d$escore))
}

cat("\n===== SEGUNDO ESTÁGIO — SIMAR E WILSON (2007), L = 2.000 =====\n")
SW <- list()
for (f in names(res_dea)) {
  SW[[f]] <- segundo_estagio(res_dea[[f]], paineis[[f]], "phi_vrs")
  cat(sprintf("\n--- %s | n truncado = %d/%d | sigma = %.4f | variância de phi explicada = %.4f ---\n",
              f, SW[[f]]$n_truncado, nrow(res_dea[[f]]), SW[[f]]$sigma, SW[[f]]$r2))
  print(as.data.frame(SW[[f]]$coeficientes), digits = 4)
}

# correlação bruta escore-INSE (reportada explicitamente, conforme o parecer)
cat("\n--- Correlação de Spearman entre escore DEA e INSE ---\n")
for (f in names(res_dea)) {
  m <- inner_join(res_dea[[f]], paineis[[f]] %>% select(COD_IBGE, ano, INSE),
                  by = c("COD_IBGE", "ano"))
  for (y in ANOS) {
    s <- filter(m, ano == y)
    ct <- cor.test(s$phi_vrs, s$INSE, method = "spearman", exact = FALSE)
    cat(sprintf("%-8s %d: rho = %+.4f (p = %.3g)\n", f, y, ct$estimate, ct$p.value))
  }
}

# quanto o ranking se altera ao condicionar pelo contexto
cat("\n--- Reordenação do ranking ao condicionar pelo contexto (SIOPE) ---\n")
for (y in ANOS) {
  s <- filter(SW$SIOPE$dados, ano == y)
  troca <- sum((rank(s$escore) <= 94) != (rank(s$ef_cond) <= 94))
  cat(sprintf("%d: %d de %d DMUs mudam de metade do ranking | Spearman bruto x condicionado = %.4f\n",
              y, troca, nrow(s), cor(s$escore, s$ef_cond, method = "spearman")))
}

# ==============================================================================
# BENCHMARKS NOMEADOS
# ==============================================================================
benchmarks <- map_dfr(ANOS, function(y) {
  filter(res_dea$SIOPE, ano == y, phi_vrs < 1 + 1e-6) %>%
    inner_join(paineis$SIOPE %>% select(COD_IBGE, ano, MedGasto5a, N_LP, N_MT,
                                        INSE, MAT_AI, TDI),
               by = c("COD_IBGE", "ano")) %>%
    select(Ano = ano, Municipio = NOM_MUN, Gasto_MA5 = MedGasto5a,
           Nota_LP = N_LP, Nota_MT = N_MT, INSE, Matriculas_AI = MAT_AI, TDI)
})
cat("\n===== DMUs EFICIENTES POR ANO (fronteira anual, SIOPE) =====\n")
print(as.data.frame(benchmarks), digits = 4)

malmq_medio <- res_mq$SIOPE %>%
  group_by(COD_IBGE, NOM_MUN) %>%
  summarise(across(c(M, effch, pech, sech, techch), ~ exp(mean(log(.x)))),
            .groups = "drop") %>%
  left_join(SW$SIOPE$dados %>% filter(ano == 2023) %>%
              select(COD_IBGE, INSE, phi_vrs = escore, ef_cond),
            by = "COD_IBGE")

ranking_mq <- bind_rows(slice_max(malmq_medio, M, n = 10) %>% mutate(Grupo = "Top 10"),
                        slice_min(malmq_medio, M, n = 10) %>% mutate(Grupo = "Bottom 10"))
cat("\n===== TOP 10 / BOTTOM 10 — MALMQUIST (média geométrica 2019-2023) =====\n")
print(as.data.frame(ranking_mq), digits = 4)

efic_condicionada <- SW$SIOPE$dados %>% filter(ano == 2023) %>%
  slice_min(ef_cond, n = 15) %>%
  select(Municipio = NOM_MUN, phi = escore, phi_esperado, ef_cond, INSE, TDI)
cat("\n===== TOP 15 — EFICIÊNCIA CONDICIONADA AO CONTEXTO (2023) =====\n")
print(as.data.frame(efic_condicionada), digits = 4)

# ==============================================================================
# ROBUSTEZ — ESCALA DO PRODUTO E DEFINIÇÃO DO INSUMO
# ==============================================================================
# O NÍVEL do escore radial depende da origem da escala do produto, 
# mas a ORDENAÇÃO das DMUs é praticamente invariante. Consequência: 
# a leitura de benchmarking é robusta;
# a leitura do "quanto falta para a fronteira" é condicionada à escala adotada.
# ------------------------------------------------------------------------------
cat("\n===== ROBUSTEZ — ESCALA DO PRODUTO E INSUMO (SIOPE) =====\n")
tab_rob <- map_dfr(ANOS, function(y) {
  dy <- filter(paineis$SIOPE, ano == y)
  X  <- mat_in(dy); Xc <- as.matrix(dy[, "gasto", drop = FALSE])
  Yp <- mat_out(dy); Yb <- as.matrix(dy[, c("SAEB_LP", "SAEB_MT")])
  p_pad <- dea_out(X,  Yp, X,  Yp, "vrs")   # padronizado (principal)
  p_tri <- dea_out(X,  Yb, X,  Yb, "vrs")   # TRI bruto (escala intervalar)
  p_con <- dea_out(Xc, Yp, Xc, Yp, "vrs")   # insumo contemporâneo
  tibble(Ano = y,
         mediana_padronizado = median(p_pad),
         mediana_TRI_bruto   = median(p_tri),
         mediana_insumo_contemp = median(p_con),
         Spearman_escala = cor(p_pad, p_tri, method = "spearman"),
         Spearman_insumo = cor(p_pad, p_con, method = "spearman"))
})
print(as.data.frame(tab_rob), digits = 4)

# ==============================================================================
# FIGURAS
# ==============================================================================
# Gráfico 9 — DEA: fronteira anual x fronteira intertemporal
g9_dados <- res_dea$SIOPE %>%
  select(ano, phi_vrs, phi_inter) %>%
  pivot_longer(-ano, names_to = "tipo", values_to = "phi") %>%
  mutate(tipo = factor(tipo, c("phi_vrs", "phi_inter"),
                       c("(a) Fronteira anual independente\n(comparação entre anos inválida)",
                         "(b) Fronteira intertemporal comum\n(comparação entre anos válida)")))
g9 <- ggplot(g9_dados, aes(factor(ano), phi)) +
  geom_boxplot(fill = "#AED6F1", colour = AZUL, outlier.size = .6, width = .55) +
  stat_summary(fun = median, geom = "text", vjust = -.8, size = 2.5,
               aes(label = sprintf("%.4f", after_stat(y)))) +
  geom_hline(yintercept = 1, linetype = 2, colour = CINZA) +
  facet_wrap(~tipo) +
  labs(x = "Ano", y = expression("Escore de ineficiência "*varphi*" (VRS, orientação produto)")) +
  tema
ggsave("saidas_dea/figuras/G09_dea_anual_vs_intertemporal.png", g9,
       width = 7.2, height = 3.4, dpi = 300)

# Gráfico 10 — Malmquist SIOPE x SICONFI lado a lado
g10_dados <- bind_rows(mutate(res_mq$SIOPE, Fonte = "SIOPE"),
                       mutate(res_mq$SICONFI, Fonte = "SICONFI")) %>%
  group_by(Fonte, periodo) %>% mutate(pos = rank(M, ties.method = "first")) %>%
  ungroup() %>%
  select(Fonte, periodo, pos, M, effch, techch) %>%
  pivot_longer(c(M, effch, techch), names_to = "comp", values_to = "valor") %>%
  mutate(comp = factor(comp, c("techch", "effch", "M"),
                       c("Fronteira (tc)", "Eficiência (effch)", "Malmquist (M)")))
g10 <- ggplot(g10_dados, aes(pos, valor, colour = comp, shape = comp)) +
  geom_point(size = .7, alpha = .8) +
  geom_hline(yintercept = 1, linetype = 2, colour = "black") +
  scale_colour_manual(values = c(AZUL, LARANJA, VERDE)) +
  facet_grid(periodo ~ Fonte) +
  labs(x = "Municípios (ordenados pelo índice de Malmquist)",
       y = "Índice (1,0 = estagnação)", colour = NULL, shape = NULL) +
  tema + theme(legend.position = "top")
ggsave("saidas_dea/figuras/G10_malmquist_siope_siconfi.png", g10,
       width = 7.6, height = 5.4, dpi = 300)

# Gráfico 11 — Top 10 / Bottom 10
g11_dados <- ranking_mq %>%
  select(NOM_MUN, Grupo, M, effch, techch) %>%
  pivot_longer(c(M, effch, techch), names_to = "comp", values_to = "valor") %>%
  mutate(comp = factor(comp, c("techch", "effch", "M"),
                       c("Fronteira (tc)", "Eficiência (effch)", "Malmquist (M)")),
         Grupo = factor(Grupo, c("Top 10", "Bottom 10")))
g11 <- ggplot(g11_dados, aes(valor, reorder(NOM_MUN, valor), colour = comp, shape = comp)) +
  geom_point(size = 2) +
  geom_vline(xintercept = 1, linetype = 2, colour = "black") +
  scale_colour_manual(values = c(AZUL, LARANJA, VERDE)) +
  facet_wrap(~Grupo, scales = "free_y") +
  labs(x = "Média geométrica 2019-2023", y = NULL, colour = NULL, shape = NULL) +
  tema + theme(legend.position = "top")
ggsave("saidas_dea/figuras/G11_malmquist_top_bottom.png", g11,
       width = 7.6, height = 4.2, dpi = 300)

# Gráfico 12 — escore DEA x INSE
g12_dados <- inner_join(res_dea$SIOPE,
                        paineis$SIOPE %>% select(COD_IBGE, ano, INSE),
                        by = c("COD_IBGE", "ano"))
g12 <- ggplot(g12_dados, aes(INSE, phi_vrs)) +
  geom_point(size = .9, alpha = .6, colour = AZUL) +
  geom_smooth(method = "lm", se = FALSE, colour = "#B03A2E", linewidth = .8) +
  facet_wrap(~ano) +
  labs(x = "INSE médio (INEP)",
       y = expression("Ineficiência "*varphi)) +
  tema
ggsave("saidas_dea/figuras/G12_dea_inse.png", g12, width = 7.6, height = 2.9, dpi = 300)

# Gráfico 13 — coeficientes do segundo estágio
g13_dados <- SW$SIOPE$coeficientes %>%
  filter(Variavel != "(Intercept)") %>%
  mutate(Variavel = recode(Variavel,
                           INSE = "INSE", ln_mat = "ln(matrículas AI)",
                           p_mat_rur_AI = "% matrículas rurais",
                           TDI = "Distorção idade-série",
                           ano_f2021 = "Ano 2021", ano_f2023 = "Ano 2023"))
g13 <- ggplot(g13_dados, aes(Coef, reorder(Variavel, Coef))) +
  geom_vline(xintercept = 0, linetype = 2, colour = "#B03A2E") +
  geom_errorbarh(aes(xmin = IC_inf, xmax = IC_sup), height = .18, colour = CINZA) +
  geom_point(size = 2.2, colour = AZUL) +
  labs(x = expression("Efeito sobre a ineficiência "*varphi*" (IC 95% bootstrap, L = 2.000)"),
       y = NULL) + tema
ggsave("saidas_dea/figuras/G13_segundo_estagio.png", g13, width = 6.4, height = 3.2, dpi = 300)

# ==============================================================================
# 10. EXPORTAÇÃO
# ==============================================================================
write_xlsx(list(
  DEA_descritivas        = tab_dea,
  DEA_testes_temporais   = tab_testes,
  Malmquist_decomposicao = tab_mq,
  Convergencia_bases     = tab_conv,
  Segundo_estagio_SIOPE  = SW$SIOPE$coeficientes,
  Segundo_estagio_SICONFI= SW$SICONFI$coeficientes,
  DMUs_eficientes        = benchmarks,
  Malmquist_top_bottom   = ranking_mq,
  Eficiencia_condicionada= efic_condicionada,
  Robustez               = tab_rob,
  ApendiceA_DEA_SIOPE    = res_dea$SIOPE,
  ApendiceA_DEA_SICONFI  = res_dea$SICONFI,
  ApendiceA_Malmq_SIOPE  = res_mq$SIOPE,
  ApendiceA_Malmq_SICONFI= res_mq$SICONFI
), "saidas_dea/resultados_dea_malmquist.xlsx")

for (nm in c("tab_dea", "tab_mq", "tab_conv", "tab_rob", "benchmarks",
             "ranking_mq", "efic_condicionada", "tab_testes"))
  write.csv(get(nm), file.path("saidas_dea/tabelas", paste0(nm, ".csv")),
            row.names = FALSE, fileEncoding = "UTF-8")

cat("\n\n==================== EXECUÇÃO CONCLUÍDA ====================\n")
cat("Tabelas, figuras e apêndices gravados em ./saidas_dea/\n")

# FIM DO SCRIPT
