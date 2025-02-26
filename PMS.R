

# Pacotes -----------------------------------------------------------------

library(sidrar)
library(tidyverse)



# Importação de dados -----------------------------------------------------


dados_brutos <- get_sidra(
  api = paste0("/t/8163/n1/all/v/11623,11624,11625,11626/p/all/c11046/all/",
  "c1274/56703/d/v11623%201,v11624%201,v11625%201,v11626%201")
) %>% as_tibble()


# Tratamento dos dados ----------------------------------------------------

dados <- dados_brutos %>%
    mutate(
      data = ym(`Mês (Código)`),
      variavel = recode(
        .x = `Variável`,
        "PMS - Variação mês/mês imediatamente anterior, com ajuste sazonal (M/M-1)" = "Var. % margem",
        "PMS - Variação mês/mesmo mês do ano anterior (M/M-12)" = "Var. % interanual",
        "PMS - Variação acumulada no ano (em relação ao mesmo período do ano anterior)" = "Var. % acumulada no ano",
        "PMS - Variação acumulada em 12 meses (em relação ao período anterior de 12 meses)" = "Var. % acumulada 12 meses"
      ),
      indice = recode(
        .x = `Tipos de índice`,
        "Índice de receita nominal de serviços" = "Receita Nominal",
        "Índice de volume de serviços" = "Volume"
      ),
      valor = Valor,
      .keep = "none"
    ) %>%
  drop_na()



# Visulização dos dados ---------------------------------------------------


dados %>%
  ggplot()+
  aes(x = data, y = valor, color = indice)+
  geom_line(linewidth = 1)+
  facet_wrap(facets = ~variavel, scales = "free")+
  labs(
    title = "PMS - Pesquisa mensal de serviços",
    caption = "Fonte: IBGE | Elaboração: Fundação Ecos",
    x = NULL,
    y = "%",
    color = NULL
  )