#Cores Assec
cores_Assec <- c("#2D4B73","#253C59","#99B4BF","#D9B70D","#BF8D30")

#Formata número
formatar_numero <- function(x, acc = 1){
  scales::number(
    x,
    accuracy = acc,
    big.mark = ".",
    decimal.mark = ","
  )
}

#Conta linhas de uma tabela
contar_linhas <- function(tab){
  tab |>
    summarise(
      n = n()
    ) |>
    pull(n)
}
