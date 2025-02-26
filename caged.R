

# Pacotes -----------------------------------------------------------------


library(ipeadatar)
library(tidyverse)



# Importação de dados -----------------------------------------------------

series <- ipeadatar::available_series()

caged_series <- list(
                    admissoes = "CAGED12_ADMISN12",
                    demissoes = "CAGED12_DESLIGN12",
                    saldo = "CAGED12_SALDON12"
                    )

admissoes <- ipeadata(code = caged_series$admissoes)
demissoes <- ipeadata(code = caged_series$demissoes)
saldo <- ipeadata(code = caged_series$saldo)



# Tratamento de dados -----------------------------------------------------

dados <- purrr::reduce(
          .x = list(
          admissoes %>% select("data" = "date", "admissoes" = "value"),
          demissoes %>% select("data" = "date", "demissoes" = "value"),
          saldo %>% select("data" = "date", "saldo" = "value")),
          .f = left_join,
          by = "data")

dados2 <- bind_rows(admissoes, demissoes, saldo) %>%
          mutate(
            data = date,
            valor = value,
            categoria = recode(
            .x = code,
            "CAGED12_ADMISN12" = "Admissões",
            "CAGED12_DESLIGN12" = "Demissões",
            "CAGED12_SALDON12" = "Saldo"
          ),
            .keep = "none") %>%
          mutate(valor = valor / 1000)

# Visualização gráfica ----------------------------------------------------

dados2 %>%
  ggplot()+
  aes(x = data, y = valor, fill = categoria)+
  geom_col()+
  facet_wrap(facets = ~categoria,
             scales = "free")+
  scale_x_date(date_labels = "%m/%y",
               breaks = "3 months")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title = "Análise CAGED",
       x = NULL,
       y = "Valores em R$ milhões",
       color = NULL,
       caption = "Fonte: IPEA")