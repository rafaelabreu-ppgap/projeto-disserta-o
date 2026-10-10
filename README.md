# Gasto público e desempenho escolar: evidências das redes públicas municipais de ensino nos municípios na área de abrangência do IDENE-MG

Bases de dados, memória de cálculo e scripts em R e Python que reproduzem os resultados da dissertação de mestrado e do Produto Técnico-Tecnológico (PTT) dela derivado:

- **Dissertação:** *Gasto público e desempenho escolar: evidências das redes públicas municipais de ensino nos municípios na área de abrangência do IDENE-MG* — Mestrado Profissional em Administração Pública (PPGAP), Universidade Federal dos Vales do Jequitinhonha e Mucuri (UFVJM), 2026.
- **PTT:** *Relatório técnico conclusivo — Diagnóstico da infraestrutura informacional do gasto e do desempenho educacional nos municípios da área de abrangência do IDENE* (GT/CAPES, produto n.º 10).

**Autor:** Rafael Junio Abreu  
**Orientador:** Prof. Dr. João Paulo Calembo Batista Menezes  
**Programa:** PPGAP/UFVJM — linha de pesquisa Planejamento e Governança no Campo da Administração Pública

---

## Sobre a pesquisa

A pesquisa confronta duas bases oficiais de gasto municipal por matrícula no ensino fundamental — o indicador 4.2 do **SIOPE** (FNDE) e o indicador reconstruído a partir das declarações do **SICONFI** (DCA – Anexo I-E, Tesouro Nacional), com rateio proporcional dos custos indiretos da subfunção 12.122 conforme a NBC TSP 34 — e estima sua relação com o desempenho no **SAEB** (5º ano, Língua Portuguesa e Matemática).

- **Território:** 265 municípios da área de abrangência do IDENE (MG); painel analítico de 188 municípios.
- **Período:** exercícios de 2015 a 2024 (gasto); edições do SAEB de 2019, 2021 e 2023 (desempenho).
- **Métodos:** confrontação SIOPE × SICONFI; regressões em painel de efeitos fixos de duas vias (TWFE); Análise Envoltória de Dados (modelo BCC, orientação a produto), fronteira intertemporal, Índice de Malmquist e segundo estágio de Simar e Wilson (2007).
- **Valores monetários:** a preços de 2023, deflacionados pelo IPCA.

---

## Estrutura do repositório

```
├── DataBases/
│   ├── nova_base.xlsx              # base analítica usada por todos os scripts em R
│   ├── nova_base_completa.xlsx     # memória de cálculo da reconstrução do indicador SICONFI
│   ├── tabelas.xlsx                # tabelas de apoio da dissertação
│   ├── desenho_dissert.xlsx        # desenho da pesquisa (figuras da dissertação)
│   └── NíveisSAEB.xlsx             # níveis de proficiência do SAEB
├── Python/
│   ├── tesouro_api_consumer.py     # coleta das declarações DCA – Anexo I-E na API do Tesouro
│   ├── cod_reg_2015-2024.csv       # lista completa de entes e exercícios (265 municípios × 10 anos)
│   ├── Dados/cod_reg.csv           # exemplo de arquivo de entrada para o coletor
│   ├── Resultados/anexos_despesas.csv  # exemplo de saída do coletor
│   ├── leia-me.txt
│   └── prompt_claudeIA.txt         # registro do uso de IA na elaboração do coletor
├── R/
│   ├── 00. analise_idene.rmd       # confrontação SIOPE × SICONFI (relatório em R Markdown)
│   ├── 00.-analise_idene.html      # relatório compilado
│   ├── 01 modelos_siconfi.R        # modelos em painel — base SICONFI
│   ├── 02 modelos_siope.R          # modelos em painel — base SIOPE
│   ├── 03 estatisticas_descritivas.R  # descritivas, decomposição da variância, correlações e testes de diagnóstico
│   └── 04 dea_malmquist.R          # DEA, Malmquist e segundo estágio de Simar e Wilson
├── CITATION.cff
├── LICENSE                         # licença do código (MIT)
└── DADOS-LICENCA.md                # licença das bases de dados (CC BY 4.0)
```

### Bases de dados

**`nova_base.xlsx`** — base analítica dos 188 municípios, com as planilhas:

| Planilha | Conteúdo |
|---|---|
| `SAEB` | Proficiência média no SAEB, 5º ano, LP e MT (2017–2023) |
| `GASTO` | Gasto por matrícula no SIOPE e no SICONFI, nominal (`_NOM`) e a preços de 2023 (`_CORR`), 2015–2024 |
| `INSE` | Indicador de Nível Socioeconômico médio (2019, 2021 e 2023) |
| `MATRÍCULAS` | Matrículas urbanas e rurais, anos iniciais e finais (2015–2024) |
| `TDI` | Taxa de distorção idade-série (2019, 2021 e 2023) |

**`nova_base_completa.xlsx`** — memória de cálculo da reconstrução do gasto por matrícula a partir do SICONFI: planilha `SICONFI_calc` (custo direto total, peso do ensino fundamental, custo indireto rateado e custo total — CDT, Pef, CIadm e CTef), `SICONFI` (gasto por matrícula), `CENSO` (matrículas), `IPCA` (deflacionamento) e as bases originais antes da seleção do painel.

### Fontes

SIOPE/FNDE; SICONFI/Secretaria do Tesouro Nacional (DCA – Anexo I-E); SAEB, INSE, Taxa de Distorção Idade-Série e Censo Escolar/INEP; IPCA/IBGE. Todas as fontes são oficiais e de acesso público.

---

## Como reproduzir

### Requisitos

- **R** 4.5.1 ou superior (os scripts foram executados no RStudio 2025.09.1).
- Pacotes R (a linha `install.packages(...)` no cabeçalho de cada script lista os necessários):
  `readxl`, `tidyverse`, `plm`, `lmtest`, `sandwich`, `car`, `writexl`, `lpSolveAPI`, `truncreg`, `knitr`, `kableExtra`, `patchwork`, `scales`.
- **Python** 3, apenas para a coleta: `requests` e `pandas` (`tkinter` já acompanha o Python).

### Passo a passo

1. Baixe o repositório (**Code → Download ZIP**) ou clone-o.
2. Copie `DataBases/nova_base.xlsx` para a pasta `R/` — ou defina como diretório de trabalho a pasta que contém o arquivo. Os scripts leem `nova_base.xlsx` por caminho relativo (`path_base`).
3. No R, execute os scripts na ordem da numeração:
   - `00. analise_idene.rmd` — compile com **Knit** para gerar o relatório HTML;
   - `01 modelos_siconfi.R` e `02 modelos_siope.R` — modelos TWFE para cada base;
   - `03 estatisticas_descritivas.R` — descritivas e testes (saídas em `saidas/`);
   - `04 dea_malmquist.R` — DEA, Malmquist e segundo estágio, com 2.000 replicações bootstrap e semente fixa `set.seed(20260804)` (saídas em `saidas_dea/`).

### Coleta dos dados do SICONFI (opcional)

`tesouro_api_consumer.py` abre uma janela que lê um CSV com as colunas `ano;codigo` (código IBGE do município) e consulta a API de dados abertos do Tesouro Nacional (`apidatalake.tesouro.gov.br`), salvando os anexos DCA – Anexo I-E em CSV ou JSON. Use `cod_reg_2015-2024.csv` para repetir a coleta completa:

```
python tesouro_api_consumer.py
```

Para atualizar o diagnóstico para exercícios posteriores, colete as novas declarações, acrescente-as às planilhas de entrada e execute novamente os scripts.

---

## Como citar

Use o botão **Cite this repository**, na coluna à direita, ou:

> ABREU, Rafael Junio. **Projeto-disserta-o**: bases de dados e scripts da dissertação de mestrado profissional em Administração Pública (PPGAP/UFVJM). Versão 1.0.0. [S. l.]: GitHub, 2026. Disponível em: https://github.com/rafaelabreu-ppgap/projeto-disserta-o.

<!-- Após a release no Zenodo, substitua a linha acima pela referência com o DOI. -->

## Licenças

- **Código** (scripts em R e Python): [MIT](LICENSE).
- **Bases de dados**: [CC BY 4.0](DADOS-LICENCA.md) — uso livre, com atribuição de crédito.

## Uso de inteligência artificial

Ferramentas de IA generativa (Claude, da Anthropic) foram usadas de forma auxiliar na elaboração de trechos de código e na revisão textual, conforme a declaração constante da dissertação. Todo o código foi executado, testado e validado pelo autor.

