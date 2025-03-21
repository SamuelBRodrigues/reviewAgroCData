# Mobilidade

# Deslocamento
deslocamento <- sidrar::get_sidra(api = '/t/3422/n6/all/v/1001319/p/all/c490/12829,12830/d/v1001319%202') |>
  dplyr::filter(Município %in% getAgroCData::target_cities$municipio) |>
  janitor::clean_names() |>
  dplyr::select(
    municipio_codigo, municipio, variavel, ano, tempo_habitual_de_deslocamento_para_o_trabalho, valor
  ) |>
  tidyr::pivot_wider(
    names_from = tempo_habitual_de_deslocamento_para_o_trabalho,
    values_from = valor
  ) |>
  janitor::clean_names() |>
  dplyr::mutate(
    "Percentual da população que gasta 1hora ou mais no deslocamento casatrabalho (total e por faixa de renda)" =
      mais_de_uma_hora_ate_duas_horas + mais_de_duas_horas
  )

# Gases estufa
emissao <- readr::read_csv2('https://public.tableau.com/vizql/w/SEEGMUNICPIOS2022Estatsticas/v/RANKINGGERAL/tempfile/sessions/B4342A589F304A3BA69860FE762C1821-0:0/?key=289015993&keepfile=yes&attachment=yes')
getAgroCData::target_cities
