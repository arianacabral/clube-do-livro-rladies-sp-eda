# Análise Exploratória de Dados -------------------------------------------


## Carregando os pacotes
library(dados)
library(ggplot2)

# Variação  ---------------------------------------------------------------

## EXEMPLO 1
# base de dados: diamante
# distribuição da variável quilate

ggplot(diamante, aes(x = quilate)) +
  geom_histogram() +
  theme_minimal()

ggplot(diamante, aes(x = quilate)) +
  geom_histogram(binwidth = 0.5) +
  geom_vline(aes(xintercept = mean(quilate)), color = "red") +
  theme_minimal()

## Valores típicos ----

## EXEMPLO 2
# base de dados: diamante
# distribuição da variável y (largura em mm)

{ggplot(diamante, aes(x = y)) +
  geom_histogram() +
  theme_minimal()} |>
  plotly::ggplotly()

valores_tipicos <- diamante |>
  dplyr::filter(dplyr::between(y, 3, 20))

ggplot(valores_tipicos, aes(x = y)) +
  geom_histogram(binwidth = 0.05) +
  theme_minimal()

# vendo valores atípicos com coord_cartesian()

ggplot(diamante, aes(x = y)) +
    geom_histogram() +
    theme_minimal()

# qual a diferença entre coord_cartesian() e ylim() ou xlim?

ggplot(diamante, aes(x = y)) +
  geom_histogram() +
  coord_cartesian(ylim = c(0, 50)) +
  theme_minimal() 

ggplot(diamante, aes(x = y)) +
  geom_histogram() +
  ylim(c(0, 50)) +
  theme_minimal()

df <- data.frame(
  x = seq(0, 5, 0.01)
) |>
  dplyr::mutate(
    y = sin(2*pi*x)
  )

ggplot(df, aes(x = x, y = y)) +
  geom_line()

ggplot(df, aes(x = x, y = y)) +
  geom_line() +
  ylim(c(-1.1, 0))

ggplot(df, aes(x = x, y = y)) +
  geom_line() +
  coord_cartesian(ylim = c(-1.1, 0))

# obtendo os valores incomuns com dplyr
valores_incomuns <- diamante |>
  dplyr::filter(y < 3 | y > 20)

valores_incomuns <- valores_incomuns |>
  dplyr::select(preco, x, y, z) |>
  dplyr::arrange(y)

## Lidando com valores atípicos ----

## EXEMPLO 3
# base de dados: diamante
# distribuição da variável y (largura em mm)

# eliminando a linha inteira com os valores estranhos
diamante2 <- diamante |>
  dplyr::filter(dplyr::between(y, 3, 20))

# substituindo os valores atípicos por NA
diamante2 <-  diamante |>
  dplyr::mutate(
    y = dplyr::if_else(y < 3 | y > 20, NA, y)
  )

# como o ggplot interpreta os NAs? (geom_point)
ggplot(diamante2, aes(x = x, y = y)) +
  geom_point(na.rm = TRUE) +
  theme_minimal()



diamante2 <- diamante2 |>
  dplyr::mutate(
    corte = dplyr::if_else(corte == "Ideal", NA, corte)
  )

diamante2 <- diamante2 |>
  dplyr::group_by(corte) |>
  dplyr::count()

ggplot(diamante2, aes(x = corte, y = n)) +
  geom_col()

# e se desejamos entender os valores ausentes?


## EXEMPLO 4
# base de dados: voos
# distribuição dos horários de partida programados 
# para horários cancelados e não cancelados

View(voos)

voos2 <- voos |>
  dplyr::mutate(
    cancelado = is.na(horario_saida),
    hora_saida_programada = saida_programada %/% 100,
    min_saida_programada = saida_programada %% 100,
    saida_programada_horas = hora_saida_programada + (min_saida_programada / 60),
    .after = saida_programada
  ) 

ggplot(voos2, aes(x = saida_programada_horas, color = cancelado)) +
  geom_freqpoly(binwidth = 0.2) + 
  theme_minimal()

# como melhor a visualização?
ggplot(voos2, aes(x = saida_programada_horas,
                  color = cancelado)) +
  geom_freqpoly(binwidth = 0.2) + 
  facet_wrap(vars(cancelado), ncol = 1, 
             scales = "free_y") +
  theme_minimal() 

# Covariação --------------------------------------------------------------

# Uma variável categórica e uma numérica ----

## EXEMPLO 5
# base de dados: diamante
# há associação dos preços dos diamantes com a qualidade de corte?


# como não levar em conta as frequências individuais?


# como simplificar a nossa representação?


# e se os dados não estivessem ordenados? (forcats::fct_reorder)
## EXEMPLO 6
# base de dados: milhas
# como o consumo de combustível na rodovia varia entre as classes de carros?


# e se tivessemos nomes longos nos textos dos eixos?

# 1ª opção: trocar x por y


# 2ª opção: usar coord_flip()




# Duas variáveis categóricas ----

## EXEMPLO 7
# base de dados: diamante
# há associação entre corte e cor?

# geom_count()

# dplyr + geom_tile()




# Duas variáveis númericas ----

## EXEMPLO 8
# base de dados: diamante
# há associação entre quilate e preço?


# gráfico de dispersão



# mapa de calor geom_bin2d() e geom_hex()


# discretização de uma das variáveis




# Padrões e modelos -------------------------------------------------------

## EXEMPLO 9
# base de dados: diamante
# como entender a relação entre preço e corte, eliminando a 
# influência do valor do quilate sobre o preço?

