# reviewAgroCData

Pacote R para coleta, processamento e integração de indicadores socioeconômicos e de saúde a partir de diversas fontes públicas, com foco em dados municipais.

## 📦 Visão Geral

O `reviewAgroCData` automatiza a coleta e organização de indicadores estratégicos utilizados em estudos sobre políticas públicas, agroalimentação e desenvolvimento local. A função principal do pacote, `pega_dados_indicadores()`, atua como orquestradora da coleta e integração de dados vindos de fontes como IBGE, DATASUS, RAIS, CAPAG, PNAE, entre outras.

## 📥 Instalação

Você pode instalar a versão de desenvolvimento diretamente do GitHub:

```r
# Instale o pacote devtools se ainda não tiver
install.packages("devtools")

# Instale o reviewAgroCData
devtools::install_github("SamuelBRodrigues/reviewAgroCData")
```

## 📚 Dependências

Este pacote depende de diversos pacotes do ecossistema `tidyverse` e da família `googlesheets4`:

- `tidyverse`: Manipulação e visualização de dados.
- `googlesheets4`: Leitura de planilhas do Google Sheets.
- `googledrive`: Acesso e gerenciamento de arquivos no Google Drive.
- `httr`, `readxl`, `jsonlite`, `janitor`: Suporte à leitura de arquivos e requisições web.

> ⚠️ **Importante sobre `googlesheets4`**:
> 
> - O acesso a planilhas privadas requer autenticação. Por padrão, ao chamar qualquer função que utiliza `googlesheets4`, será aberta uma janela para login na sua conta Google.
> - Se quiser acessar planilhas públicas sem autenticação, use `gs4_deauth()`:
>
> ```r
> library(googlesheets4)
> gs4_deauth()
> ```
>
> - Para reutilizar um token de autenticação do `googledrive`, você pode usar:
>
> ```r
> library(googledrive)
> drive_auth()
> gs4_auth(token = drive_token())
> ```

---

## 🚀 Função Principal: `pega_dados_indicadores()`

Essa é a função central do pacote. Ela orquestra a execução de várias funções de coleta e transformação de dados, retornando um `data.frame` com os indicadores selecionados para os municípios desejados.

### 🧾 Exemplo de uso

```r
library(reviewAgroCData)

dados <- pega_dados_indicadores(
cod_ibge = target_cities$municipio_codigo, 
ano = "last", 
mes = "03", 
download = TRUE,
dir = "data_raw")

head(dados)
```

### 🔧 Parâmetros

| Parâmetro     | Descrição                                                                 |
|---------------|---------------------------------------------------------------------------|
| `cod_ibge`    | Vetor com códigos IBGE dos municípios. Valor padrão: todos os disponíveis.|
| `ano`         | Ano de referência. Valor padrão: ano atual.                               |
| `mes`         | Mes de referência. Valor padrão: ano atual.                               |
| `download`    | Lógico. Se TRUE, baixa os dados. Padrão: TRUE                             |
| `dir`         | Diretório dos dados baixados. Padrão: "data_raw".                         |

### 🧠 Retorno

Um `data.frame` com os dados integrados de diversos indicadores por município e ano.

---

## 📑 Outras Funções Disponíveis

O pacote contém funções auxiliares, agrupadas por temática. Algumas delas:

### 📚 Educação
- `get_IDEB_fundamental()`

### 💼 Emprego e Economia
- `get_empregos_formais_total()`
- `get_PIB_per_capta()`
- `get_endividamento()`
- `get_empregos_formais_por_100khab()`

### 🩺 Saúde
- `get_cobertura_ab()`
- `get_cobertura_aps()`
- `get_cobertura_vacinal()`
- `get_equip_estab()`
- `constroi_obesid_mun()`
- `constroi_subnutricao_sisvan()`

### 💧 Infraestrutura
- `get_abastecimento_esgoto()`

### 🏠 Assistência Social
- `get_alimentos_pnae()`
- `get_cad_bf_data()`
- `download_cras_data()`
- `extract_cras_data()`

### 🌿 Sustentabilidade e Desenvolvimento
- `get_emissao()`
- `get_estab_dirigido_por_mulheres()`
- `download_CAPAG()`
- `download_IVS()`
- `extract_IVS_2010()`

---

## 🙋 Contribuições

Sinta-se à vontade para contribuir com sugestões, correções ou novas funcionalidades via pull request ou issues.

---

## 📄 Licença

Este projeto está licenciado sob a Licença MIT. Consulte o arquivo `LICENSE` para mais detalhes.

---

## ✉️ Contato

Criado por [SamuelBRodrigues](https://github.com/SamuelBRodrigues). Dúvidas, sugestões ou colaborações são bem-vindas!
