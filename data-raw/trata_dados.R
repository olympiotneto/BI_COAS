library(tidyverse)
library(readxl)

options(OutDec = ",")


#Leitura do conjunto de dados
#Exclusão idades com NA e menores que 0
dados <- read_excel("data-raw/Dados_saude.xlsx", col_types = c("text",
                                                      "numeric", "numeric", "text", "date",
                                                      "numeric", "date", "text", "text", "text",
                                                      "text", "text")) |>
  filter(`Idade - Início licença`>=0 & !is.na(`Data início licença`)) |>
  mutate(CID_Grupo = str_sub(CID,1,3),
         CID_Grupo = fct(CID_Grupo) |> fct_na_value_to_level(level = NA)
  )

#Exemplo de transformar data em mês e ano

dados <- dados |>
  mutate(
    `Data início licença` = ymd(`Data início licença`),
    mes_ano = format_ISO8601(`Data início licença`, precision = "ym")
  )


#exclui dados antes de 01/01/2005

dados <- dados |> filter(ymd(`Data início licença`) > ymd("2005-01-01"))


#Transformação dos fatores da situação funcional reduzindo os níveis
#com a criação da variável Situacao
dados <- dados |>
  mutate(
    Situacao = as.factor(`Situação funcional`
    )) |>
  mutate(
    Situacao = fct_collapse(
      Situacao,
      EFETIVO = c("EFETIVO","EFETIVO CEDIDO","EFETIVO LICENCIADO"),
      REMOVIDO = c("REMOVIDO PARA ESTE TRIBUNAL","EFETIVO REMOVIDO","REMOVIDO DEVOLVIDO AO TRIBUNAL DE ORIGEM"),
      REQUISITADO = c("REQUISITADO DEVOLVIDO","REQUISITADO"),
      INATIVO = c("INATIVO"),
      other_level = "OUTROS"

    ))

saveRDS(
  dados,
  "data/dados.RDS"
)


