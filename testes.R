#Idade da última vez que o servidor tomou licença dado um ou mais cids

dados |>
  filter(year(data_inicio_licenca)==2023,
         cid_grupo %in% c("A90", "J00")) |>
  group_by(codigo) |>
  summarise(
    idade = max(idade_inicio_licenca),
  ) |>
  view()


