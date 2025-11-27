# GASTO PÚBLICO E PROFICIÊNCIA ESCOLAR: evidências da rede pública municipal de ensino no Vale do Mucuri

Este repositório contém os arquivos (bases de dados, scripts e resultados) utilizados na pesquisa "GASTO PÚBLICO E PROFICIÊNCIA ESCOLAR: evidências da rede pública municipal de ensino no Vale do Mucuri".

* **Autor:** Rafael Junio Abreu
* **Afiliação:** Discente do PPGAP - Mestrado Profissional em Administração Pública - UFVJM

---

## 📂 Estrutura do Repositório

O projeto está organizado em três pastas principais, detalhadas abaixo:

### 1. /DataBases

Contém as bases de dados originais e brutas utilizadas no estudo.

### 2. /Python

Contém os arquivos relacionados à coleta de dados de despesas do SICONFI (Sistema de Informações Contábeis e Fiscais do Setor Público Brasileiro).

* **Raiz da pasta:**
    * `script.py`: Script principal em Python utilizado para automatizar o download dos relatórios de despesas.

* **Subpasta `./Dados/`:**
    * Contém as listas de códigos de municípios e os anos de referência que o script de download utiliza.

* **Subpasta `./Resultados/`:**
    * Armazena os relatórios brutos de despesas baixados do SICONFI.

### 3. /R

Contém todos os arquivos para o processamento, análise estatística (DEA/Malmquist) e visualização dos dados.

* **Raiz da pasta:**
    * Scripts `.R` e arquivos `.Rmd` (R Markdown) contendo todo o código para análise e geração dos relatórios.

* **Subpasta `./Dados/`:**
    * Contém as planilhas já processadas e consolidadas (dados de gastos, Prova SAEB, Censo Escolar e indicadores do SIOPE) que são lidas pelos scripts R para a análise.

* **Subpasta `./Markdown/`:**
    * Contém os relatórios finais em formato HTML, gerados a partir da compilação dos arquivos R Markdown.

* **Subpasta `./Resultados/`:**
    * Armazena todos os resultados e saídas geradas pelos modelos de Análise Envoltória de Dados (DEA) e Índice de Malmquist.