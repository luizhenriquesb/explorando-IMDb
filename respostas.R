
# Autor: Luiz Henrique
# Contato: sbluizhenrique@gmail.com

# Pacotes -----------------------------------------------------------------

library(tidyverse)
library(knitr)
library(readxl)

# Dados -------------------------------------------------------------------

# remotes::install_github("curso-r/basesCursoR")
library(basesCursoR)

imdb <- basesCursoR::pegar_base("imdb_completa")
imdb_pessoas <- basesCursoR::pegar_base("imdb_pessoas")
imdb_avaliacoes <- basesCursoR::pegar_base("imdb_avaliacoes")

# Questão 1 ---------------------------------------------------------------

# Qual o mês do ano com o maior número de filmes? E o dia do ano?

# Mês

imdb |> 
  mutate(mes = month(as_date(data_lancamento), 
                     # Identifica o nome do mês
                     label = TRUE)
  ) |> 
  # Cria uma nova coluna chamada "total" com a quantidade de filmes em cada mês
  count(mes, name = "total") |> 
  # Coloca em ordem decrescente
  arrange(desc(total)) |>
  # Coloca em formato de tabela
  knitr::kable(
    # Define título 
    caption = "**Tabela 1: Quantidade de filmes lançados em cada mês**",
    # Define o nome das colunas
    col.names = c("Mês", "Total"),
    # Centraliza
    align = "c")


imdb |> 
  mutate(mes = month(as_date(data_lancamento), 
                     # Identifica o nome do mês
                     label = TRUE)
  ) |> 
  group_by(mes) |> 
  # Cria uma nova coluna chamada "total" com a quantidade de filmes em cada mês
  count(mes, name = "total") -> mes

theme_set(theme_minimal(base_family = "Helvetica"))

theme_update(
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  axis.line.x = element_line(color = "black", linewidth = .6),
  axis.ticks.x = element_line(color = "black", linewidth = .6),
  axis.title.y = element_blank(),
  axis.text = element_text(family = "Helvetica", size = 11, color = "black"),
  plot.margin = margin(10, 15, 10, 15),
  plot.title = element_text(face = "bold"),
  plot.background = element_rect(
    # Altera o preenchimento
    fill = "white"
  ))

mes |> 
  ggplot() +
  aes(x = mes, y = total) +
  geom_col(aes(fill = mes)) +
  scale_fill_manual(
    values = c("#F5C518", "#F5C518", "#F5C518", "#F5C518",
               "#F5C518", "#F5C518", "#F5C518", "#F5C518",
               "#F5C518", "#F5C518", "#F5C518", "#F5C518"),
    guide = "none"
  ) +
  scale_x_discrete(
    #guide = "none",
    expand = expansion(add = c(.8, .6))
  ) +
  labs(
    title = "Total de filmes",
    subtitle = "Quantidade de filmes lançados em cada mês",
    caption = "Fonte: IMDb | Curso-R"
  )

# imdb |>
#   # Separa os valores em novas colunas
#   separate_wider_delim(
#     # Coluna a ser desagregada
#     cols = data_lancamento,
#     # Delimitador
#     delim = "-",
#     # Nome das novas colunas ("ano2" porque já existem uma coluna "ano")
#     names = c("ano2", "mes", "dia"),
#     # Preenche com NA os valores ausentes
#     too_few = "align_start"
#   ) |>
#   # Cria uma nova coluna chamada "total" com a quantidade de filmes em cada mês
#   count(mes, name = "total") |>
#   # Coloca em ordem decrescente
#   arrange(desc(total)) |>
#   # Coloca em formato de tabela
#   knitr::kable(
#     # Define título 
#     caption = "**Tabela 1: Quantidade de filmes lançados em cada mês**",
#     # Define o nome das colunas
#     col.names = c("Mês", "Total"),
#     # Centraliza
#     align = "c")

# Dia

imdb |> 
  mutate(dia = day(as_date(data_lancamento))) |> 
  # Cria uma nova coluna chamada "total" com a quantidade de filmes em cada dia
  count(dia, name = "total") |> 
  # Coloca em ordem decrescente
  arrange(desc(total)) |> 
  # Coloca em formato de tabela
  knitr::kable(
    # Define título 
    caption = "**Tabela 2: Quantidade de filmes lançados em cada dia**",
    # Define o nome das colunas
    col.names = c("Dia", "Total"),
    # Centraliza
    align = "c")

dia <- imdb |> 
  mutate(dia = day(as_date(data_lancamento))) |> 
  # Cria uma nova coluna chamada "total" com a quantidade de filmes em cada dia
  count(dia, name = "total") |> 
  # Coloca em ordem decrescente
  arrange(desc(dia)) 


dia |> 
  ggplot() +
  aes(x = dia, y = total, fill = dia) +
  geom_col() +
  scale_x_date() +
  labs(
    title = "Total de filmes",
    subtitle = "Quantidade de filmes lançados em cada dia",
    caption = "Fonte: IMDb | Curso-R"
  )


# imdb |>
#   # Separa os valores em novas colunas
#   separate_wider_delim(
#     # Coluna a ser desagregada
#     cols = data_lancamento,
#     # Delimitador
#     delim = "-",
#     # Nome das novas colunas ("ano2" porque já existem uma coluna "ano")
#     names = c("ano2", "mes", "dia"),
#     # Preenche com NA os valores ausentes
#     too_few = "align_start"
#   ) |>
#   # Cria uma nova coluna chamada "total" com a quantidade de filmes em cada dia
#   count(dia, name = "total") |>
#   # Coloca em ordem decrescente
#   arrange(desc(total)) |>
#   # Coloca em formato de tabela
#   knitr::kable(
#     # Define título 
#     caption = "**Tabela 2: Quantidade de filmes lançados em cada dia**",
#     # Define o nome das colunas
#     col.names = c("Dia", "Total"),
#     # Centraliza
#     align = "c")

# Dia da semana

imdb |> 
  mutate(dia_lancamento = weekdays(as_date(data_lancamento))) |> 
  # Remove os dados ausentes
  drop_na() |> 
  count(dia_lancamento, name = "total") |> 
  # Coloca em ordem decrescente
  arrange(desc(total)) |>
  # Coloca em formato de tabela
  knitr::kable(
    # Define título 
    caption = "**Tabela 2: Quantidade de filmes lançados em cada dia da semana**",
    # Define o nome das colunas
    col.names = c("Dia da semana", "Total"),
    # Centraliza
    align = "c")

# Questão 2 ---------------------------------------------------------------

# Qual o top 5 países com mais filmes na base?

imdb |>
  # Separa os valores em novas linhas
  separate_longer_delim(
    # Coluna a ser desagregada
    cols = pais,
    # Delimitador
    delim = ", "
  ) |>
  # Cria uma nova coluna chamada "total" com a quantidade de filmes em cada país
  count(pais, name = "total") |>
  # Coloca em ordem decrescente
  arrange(desc(total)) |>
  # Seleciona os 5 primeiros
  head(n = 5) |> 
  # Coloca em formato de tabela
  knitr::kable(
    # Define título 
    caption = "**Tabela 3: Top 5 países com mais filmes**",
    # Define o nome das colunas
    col.names = c("Dia", "Total"),
    # Centraliza
    align = "c")

# Questão 3 ---------------------------------------------------------------

# Liste todas as moedas que aparecem nas colunas `orcamento` e `receita` da
# base `imdb_completa`.

imdb |>
  # Seleciona somente as colunas "orcamento" e "receita"
  select(orcamento, receita) |>
  # Separa os valores em novas colunas
  separate_wider_delim(
    # Coluna a ser desagregada
    cols = orcamento,
    # Delimitador
    delim = " ",
    # Nome das novas colunas
    names = c("moeda_orcamento", "orcamento")
  ) |>
  # Separa os valores em novas colunas
  separate_wider_delim(
    # Coluna a ser desagregada
    cols = receita,
    # Delimitador
    delim = " ",
    # Nome das novas colunas
    names = c("moeda_receita", "receita")
  ) |>
  distinct(moeda_orcamento, moeda_receita) |> 
  drop_na(moeda_orcamento, moeda_receita) |> 
  # Coloca em formato de tabela
  knitr::kable(
    # Define título 
    caption = "**Tabela 4: Moedas**",
    # Define o nome das colunas
    col.names = c("Orçamento", "Receita"),
    # Centraliza
    align = "c")

# Questão 4 ---------------------------------------------------------------

# Considerando apenas orçamentos e receitas em dólar ($), qual o gênero com
# maior lucro? E com maior nota média?

imdb |> 
  # Filtrando somentes os filmes em que as colunas 'orcamento' e 'receita' estão em dólares
  filter(
    # Detecta o "$"
    str_detect(orcamento, "\\$") &
      str_detect(receita, "\\$")
    # OBS.: é preciso usar \\ para escapar o $, já que é um caracter especial 
  ) |> 
  separate_longer_delim(
    # Coluna a ser desagregada
    cols = genero,
    # Delimitador
    delim = ", " # É preciso colocar espaço depois da vírgula
  )


# Mutate para calcular o lucro
mutate(
  # Transforma a variável 'orcamento' em dbl
  orcamento = as.numeric(orcamento),
  # Transforma a variável 'receita' em dbl
  receita = as.numeric(receita),
  # Cria uma nova coluna que armazena o lucro (receita - orcamento)
  lucro = receita - orcamento,
  # Coloca a coluna 'lucro' do lado da coluna 'receita'
  .after = receita
) |> head(5) |> view()

imdb |>
  # Separa o tipo de moeda e valor (essas duas infos. estavam juntas em uma mesma célula)
  separate_wider_delim(
    # Coluna a ser desagregada
    cols = orcamento,
    # Delimitador
    delim = " ",
    # Nome das novas colunas
    names = c("moeda_orcamento", "orcamento")
  ) |>
  # Separa o tipo de moeda e valor (essas duas infos. estavam juntas em uma mesma célula)
  separate_wider_delim(
    # Coluna a ser desagregada
    cols = receita,
    # Delimitador
    delim = " ",
    # Nome das novas colunas
    names = c("moeda_receita", "receita")
  ) |>
  # Separa os valores em novas linhas
  separate_longer_delim(
    # Coluna a ser desagregada
    cols = genero,
    # Delimitador
    delim = ", " # É preciso colocar espaço depois da vírgula
  ) |>
  # Filtra somente as linhas em que o tipo de moeda era o dólar ($)
  filter(
    # Detecta o "$"
    str_detect(moeda_orcamento, "\\$"),
    str_detect(moeda_receita, "\\$")
    # OBS.: é preciso usar \\ para escapar o $, já que é um caracter especial 
  ) |>
  # Mutate para calcular o lucro
  mutate(
    # Transforma a variável 'orcamento' em dbl
    orcamento = as.numeric(orcamento),
    # Transforma a variável 'receita' em dbl
    receita = as.numeric(receita),
    # Cria uma nova coluna que armazena o lucro (receita - orcamento)
    lucro = receita - orcamento,
    # Coloca a coluna 'lucro' do lado da coluna 'receita'
    .after = receita
  ) |>
  # Agrupa pela variável 'genero'
  group_by(genero) |>
  # Soma o lucro por gênero
  summarise(lucro_genero = sum(lucro)) |>
  # Ordena em ordem decrescente o lucro
  arrange(desc(lucro_genero)) |> 
  # Coloca em formato de tabela
  knitr::kable(
    # Define título 
    caption = "**Tabela 5: Gêneros com maiores lucros (em dólares)**",
    # Define o nome das colunas
    col.names = c("Gênero", "Lucro"),
    # Centraliza
    align = "c",
    # Define a vírgula como separador dos decimais
    format.args = list(decimal.mark = ",", big.mark = ".")
  )

# Observação: Há de se considerar duas questões que tornam a comparação
# mais complexa do que a simples diferença entre a receita e o orçamento. Primeiro,
# se um filme apresenta, por exemplo, três gêneros, cada gênero deste filme terá
# um valor correspondente de orçamento, receita e lucro. Desse modo, gêneros que
# aparecem em vários filmes, mas de forma secundária, apresentarão, inevitavelmente,
# um lucro maior. Conforme não temos como diferenciar qual o gênero principal de 
# cada filme (pois estão listados em ordem alfabética), fica somente o aviso. Em 
# segundo lugar, os valores de orçamento e receita não foram ajustados pela 
# inflação, o que invalida a comparação entre filmes de diferentes épocas.

# Aqui, vamos lidar somente com este segundo problema. Abaixo, apresentamos uma
# nova comparação com os valores de orçamento e receita reajustado pela inflação

# O primeiro passo, então, é buscar uma base de dados contendo uma série histórica
# com a inflação dos EUA. Uma rápida busca na internet e achamos o site do U.S. 
# Bureau of Labor Statistics que disponibiliza o Consumer Price Index (CPI) dos ]
# EUA desde 1913.

# série histórica com a inflação dos eua
# <https://data.bls.gov/pdq/SurveyOutputServlet>

# Base com série história do Consumer Price Index (USA)

cpi_usa <- read_excel("dados/SeriesReport-20231022214852_d55f27.xlsx", skip = 11)

cpi_usa <- cpi_usa |>
  # Transforma as colunas em linhas
  pivot_longer(
    # Colunas a serem pivotadas
    cols = Jan:Dec,
    # Nome da nova coluna contendo os meses
    names_to = "mes",
    # Nome da nova coluna contendo os valores de cada mes 
    values_to = "cpi"
  ) |> 
  # Agrupa por ano
  group_by(Year) |>
  # Faz a média co CPI por ano
  summarise(cpi_avg_annual = mean(cpi))

current_inflation <- cpi_usa |> 
  filter(Year == 2022) |> pull() # Retorna a média do CPI e 2022 (293)

# Fórmula
# qtde. $ atuais = qtde. $ ano-t x (cpi-atual/cpi-ano-t)
# Para confirmar os resultados manualmente, caso necessário:
# <https://www.bls.gov/data/inflation_calculator.htm>


# AJUSTADO PELA INFLACAO
imdb_adjusted_inf <- imdb |>
  # Separa o tipo de moeda e valor (essas duas infos. estavam juntas em uma mesma célula)
  separate_wider_delim(
    # Coluna a ser desagregada
    cols = orcamento,
    # Delimitador
    delim = " ",
    # Nome das novas colunas
    names = c("moeda_orcamento", "orcamento")
  ) |>
  # Separa o tipo de moeda e valor (essas duas infos. estavam juntas em uma mesma célula)
  separate_wider_delim(
    # Coluna a ser desagregada
    cols = receita,
    # Delimitador
    delim = " ",
    # Nome das novas colunas
    names = c("moeda_receita", "receita")
  ) |>
  # Separa os valores em novas linhas
  separate_longer_delim(
    # Coluna a ser desagregada
    cols = genero,
    # Delimitador
    delim = ", " # É preciso colocar espaço depois da vírgula
  ) |>
  # Filtra somente as linhas em que o tipo de moeda era o dólar ($)
  filter(
    # Detecta o "$"
    str_detect(moeda_orcamento, "\\$"),
    str_detect(moeda_receita, "\\$")
    # OBS.: é preciso usar \\ para escapar o $, já que é um caracter especial 
  ) |>
  
  
  # Juntando a base do imdb com a série histórica do CPI dos EUA
  left_join(cpi_usa, by = c("ano" = "Year")) -> test

test |> 
  # Mutate para calcular o lucro
  mutate(
    # Transforma a variável 'orcamento' em dbl
    orcamento = as.numeric(orcamento),
    # Transforma a variável 'receita' em dbl
    receita = as.numeric(receita),
    lucro = receita - orcamento
  ) |>
  mutate(
    # Corrige o orçamento pela inflação
    orcamento_corrigido = orcamento*(current_inflation/cpi_avg_annual),
    #.after = orcamento,
    # Corrige a receita pela inflação
    receita_corrigida = receita*(current_inflation/cpi_avg_annual),
    #.after = receita
    lucro_corrigido = receita_corrigida - orcamento_corrigido,
    lucro_corrigido2 = lucro*(current_inflation/cpi_avg_annual)
  ) |> 
  select(orcamento, receita, lucro, 
         orcamento_corrigido, receita_corrigida, lucro_corrigido, lucro_corrigido2)



# Mutate para calcular o lucro
mutate(
  # Transforma a variável 'orcamento' em dbl
  orcamento = as.numeric(orcamento),
  # Transforma a variável 'receita' em dbl
  receita = as.numeric(receita),
  
  # # Cria uma nova coluna que armazena o lucro (receita - orcamento)
  # lucro = receita - orcamento,
  # # Coloca a coluna 'lucro' do lado da coluna 'receita'
  # .after = receita
) |> 
  # Juntando a base do imdb com a série histórica do CPI dos EUA
  left_join(cpi_usa, by = c("ano" = "Year")) |> 
  mutate(
    # Corrige o orçamento pela inflação
    orcamento_corrigido = orcamento*(current_inflation/cpi_avg_annual),
    # Corrige a receita pela inflação
    receita_corrigida = receita*(current_inflation/cpi_avg_annual),
    # Corrige o lucro pela inflação
    lucro = receita_corrigida - orcamento_corrigido)














# AJUSTADO PELA INFLACAO
imdb_adjusted_inf <- imdb |>
  # Separa o tipo de moeda e valor (essas duas infos. estavam juntas em uma mesma célula)
  separate_wider_delim(
    # Coluna a ser desagregada
    cols = orcamento,
    # Delimitador
    delim = " ",
    # Nome das novas colunas
    names = c("moeda_orcamento", "orcamento")
  ) |>
  # Separa o tipo de moeda e valor (essas duas infos. estavam juntas em uma mesma célula)
  separate_wider_delim(
    # Coluna a ser desagregada
    cols = receita,
    # Delimitador
    delim = " ",
    # Nome das novas colunas
    names = c("moeda_receita", "receita")
  ) |>
  # Separa os valores em novas linhas
  separate_longer_delim(
    # Coluna a ser desagregada
    cols = genero,
    # Delimitador
    delim = ", " # É preciso colocar espaço depois da vírgula
  ) |>
  # Filtra somente as linhas em que o tipo de moeda era o dólar ($)
  filter(
    # Detecta o "$"
    str_detect(moeda_orcamento, "\\$"),
    str_detect(moeda_receita, "\\$")
    # OBS.: é preciso usar \\ para escapar o $, já que é um caracter especial 
  ) |>
  # Mutate para calcular o lucro
  mutate(
    # Transforma a variável 'orcamento' em dbl
    orcamento = as.numeric(orcamento),
    # Transforma a variável 'receita' em dbl
    receita = as.numeric(receita),
    # Cria uma nova coluna que armazena o lucro (receita - orcamento)
    lucro = receita - orcamento,
    # Coloca a coluna 'lucro' do lado da coluna 'receita'
    .after = receita
  ) |> 
  # Juntando a base do imdb com a série histórica do CPI dos EUA
  left_join(cpi_usa, by = c("ano" = "Year")) |> 
  # Mutate para corrigir os valores para o ano de 2022
  mutate(
    # Corrige o orçamento pela inflação
    orcamento_corrigido = orcamento*(current_inflation/cpi_avg_annual),
    # Corrige a receita pela inflação
    receita_corrigida = receita*(current_inflation/cpi_avg_annual),
    # Corrige o lucro pela inflação
    lucro_corrigido = lucro*(current_inflation/cpi_avg_annual))

imdb_adjusted_inf |> 
  # Agrupa pela variável 'genero'
  group_by(genero) |>
  # Soma o lucro por gênero
  summarise(lucro_genero = sum(lucro_corrigido)) |>
  # Ordena em ordem decrescente o lucro
  arrange(desc(lucro_genero)) |> 
  # Coloca em formato de tabela
  knitr::kable(
    # Define título 
    caption = "**Tabela 6: Gêneros com maiores lucros (em dólares e ajustado pela inflação)**",
    # Define o nome das colunas
    col.names = c("Gêneros", "Lucro"),
    # Centraliza
    align = "c",
    # Define a vírgula como separador dos decimais
    format.args = list(decimal.mark = ",", big.mark = ".")
  )

# Média

imdb |> 
  # Filtra somente as linhas em que o tipo de moeda era o dólar ($)
  filter(
    str_detect(orcamento, "\\$"),
    str_detect(receita, "\\$")
  ) |> 
  # Separa os valores em novas linhas
  separate_longer_delim(
    # Coluna a ser desagregada
    cols = genero,
    # Delimitador
    delim = ", " # É preciso colocar espaço depois da vírgula
  ) |> 
  # Agrupa pela variável 'genero'
  group_by(genero) |>
  # Média ponderada da nota
  summarise(nota_media = weighted.mean(nota_imdb, num_avaliacoes)) |> 
  # Ordena pela maior nota média ponderada
  arrange(desc(nota_media)) |> 
  knitr::kable(
    # Define título 
    caption = "**Tabela 7: Gêneros com as maiores notas (média ponderada pela quantidade de avaliações)**",
    # Define o nome das colunas
    col.names = c("Gêneros", "Nota média"),
    # Centraliza
    align = "c",
    # Define a vírgula como separador dos decimais
    format.args = list(decimal.mark = ",")
  )

# Questão 5 ---------------------------------------------------------------

# 5. Dentre os filmes na base `imdb_completa`, escolha o seu favorito. Então 
# faça os itens a seguir:

# Meu filme favorito
filme_fav <- imdb |> 
  filter(titulo_original == "The Grand Budapest Hotel") |> pull(titulo_original)

# a) Quem dirigiu o filme? Faça uma ficha dessa pessoa: idade (hoje em dia ou
# data de falecimento), onde nasceu, quantos filmes já dirigiu, qual o lucro 
# médio dos filmes que dirigiu (considerando apenas valores em dólar) e outras
# informações que achar interessante (base `imdb_pessoas`).

# Vamos extrair algumas informações de quem dirigiu meu filme favorito

# Nome 
direcao_nm <- imdb |> 
  # Pega meu filme favorito
  filter(titulo_original == filme_fav) |> 
  # Coloca o nome de quem dirigiu meu filme favorito em uma string
  pull(direcao)

# Idade
idade <- imdb_pessoas |> 
  # Pega o nome de quem dirigiu meu filme favorito
  filter(nome == direcao_nm) |> 
  # Cria uma coluna chamada "idade" com a idade de quem dirigiu
  mutate(idade = year(today())  - year(data_nascimento),
         # Coloca a coluna "idade" depois da coluna "data_nascimento"
         .after = data_nascimento) |> 
  # Coloca a idade em uma string
  pull(idade)

# Cidade de nascimento
cidade_nasc <- imdb_pessoas |> 
  # Pega o nome de quem dirigiu meu filme favorito
  filter(nome == direcao_nm) |> 
  # Desagrega a coluna "local_nascimento" em três níveis
  separate_wider_delim(
    cols = local_nascimento,
    delim = ", ",
    names = c("cidade", "estado", "pais")
  ) |> 
  # Coloca a cidade em uma string
  pull(cidade)

# Estado de nascimento
cidade_nasc <- imdb_pessoas |> 
  # Pega o nome de quem dirigiu meu filme favorito
  filter(nome == direcao_nm) |> 
  # Desagrega a coluna "local_nascimento" em três níveis
  separate_wider_delim(
    cols = local_nascimento,
    delim = ", ",
    names = c("cidade", "estado", "pais")
  ) |> 
  # Coloca o estado em uma string
  pull(estado)

# País de nascimento
cidade_nasc <- imdb_pessoas |> 
  # Pega o nome de quem dirigiu meu filme favorito
  filter(nome == direcao_nm) |> 
  # Desagrega a coluna "local_nascimento" em três níveis
  separate_wider_delim(
    cols = local_nascimento,
    delim = ", ",
    names = c("cidade", "estado", "pais")
  ) |> 
  # Coloca o país em uma string
  pull(pais)

# Quantos filmes já dirigiu
imdb |> 
  # Pega o nome de quem dirigiu meu filme favorito
  filter(direcao == direcao_nm) |> 
  # Conta quantos filme já dirigiu
  count(direcao) |> 
  # Coloca a qtde. de filmes que dirigiu em uma string
  pull(n)

# Lucro médio dos filmes que já digiriu (com valores ajustados pela inflação)

lucro_medio <- imdb_adjusted_inf |> 
  # Pega o nome de quem dirigiu meu filme favorito
  filter(direcao == direcao_nm) |> 
  # Agrupa por direção
  group_by(direcao) |>
  # Calcula a média do lucro (já considerando os efeitos da inflação)
  summarise(lucro_medio = mean(lucro_corrigido)) |> 
  # Coloca o lucro médio em uma string
  pull()


# b) Qual a posição desse filme no ranking de notas do IMDB? E no ranking de lucro 
# (considerando apenas valores em dólar)?

# Ranking de nota

# Id do meu filme favorito
id_filme_fav <- imdb |> 
  filter(titulo_original == filme_fav) |> 
  pull(id_filme)

ranking_nota_filme_fav <- imdb_avaliacoes |> 
  arrange(desc(nota_media_ponderada)) |> 
  mutate(ranking = dense_rank(desc(nota_media_ponderada))) |> 
  filter(id_filme == id_filme_fav) |> 
  pull(ranking)

# Ranking lucro

ranking_lucro_filme_fav <- imdb_adjusted_inf |> 
  arrange(desc(lucro_corrigido)) |> 
  mutate(ranking = dense_rank(desc(lucro_corrigido))) |> 
  group_by(id_filme) |> 
  filter(id_filme == id_filme_fav) |> 
  distinct(ranking) |> 
  pull(ranking)

# c) Em que dia esse filme foi lançado? E dia da semana? Algum outro filme foi 
# lançado no mesmo dia? Quantos anos você tinha nesse dia?

dia_lancamento <- imdb |> 
  filter(titulo_original == filme_fav) |> 
  mutate(dia_lancamento = day(data_lancamento),
         dia_semana_lancamento = weekdays(as.Date(data_lancamento))) |> 
  pull(dia_lancamento)

dia_semana_lancamento <- imdb |> 
  filter(titulo_original == filme_fav) |> 
  mutate(dia_lancamento = day(data_lancamento),
         dia_semana_lancamento = weekdays(as.Date(data_lancamento))) |> 
  pull(dia_semana_lancamento)

minha_idade <- imdb |> 
  filter(titulo_original == filme_fav) |>
  mutate(minha_idade = year(data_lancamento) - year("2001-02-09")) |> 
  pull(minha_idade)

dt_filme_fav <- imdb |> 
  filter(titulo_original == filme_fav) |> 
  pull(data_lancamento)

qtde_filmes_lancados_msm_dia <- imdb |> 
  filter(data_lancamento == dt_filme_fav) |>
  count() |> pull()



imdb |> 
  filter(data_lancamento == imdb |> 
           filter(titulo_original == filme_fav) |> 
           pull(data_lancamento)) |> 
  n_distinct("titulo_original") |> pull()

# d) Faça um gráfico representando a distribuição da nota atribuída a esse filme 
# por idade (base `imdb_avaliacoes`).

### OBJETOS ####
filme_fav <- imdb |> 
  filter(titulo_original == "The Grand Budapest Hotel") |> pull(titulo_original)

id_filme_fav <- imdb |> 
  filter(titulo_original == filme_fav) |> 
  pull(id_filme)

nota_idade <- imdb_avaliacoes |> 
  select(contains("nota_media_idade"))

imdb_avaliacoes |> 
  filter(id_filme == id_filme_fav) |> view()
##################

# INSPIRACAO
# <https://www.cedricscherer.com/2023/10/26/yet-another-how-to-on-labelling-bar-graphs-in-ggplot2/>

# Arrumando a base
df <- imdb_avaliacoes |> 
  # Selecionando somente as colunas de interesse
  select(id_filme, contains("nota_media_idade")) |> 
  # Alongando nossa base
  pivot_longer(
    cols = contains("nota_media_idade"),
    names_to = "faixa_etaria",
    values_to = "nota_media"
  ) |> 
  mutate(fx_etaria =
           case_when(
             faixa_etaria == "nota_media_idade_0_18" ~ "0 a 18 anos",
             faixa_etaria == "nota_media_idade_18_30" ~ "18 a 30 anos",
             faixa_etaria == "nota_media_idade_30_45" ~ "30 a 45 anos",
             faixa_etaria == "nota_media_idade_45_mais" ~ "Mais de 45 anos"
           )
  )

df |> 
  # Selecionando somente o filme de interesse
  filter(id_filme == id_filme_fav) |> 
  ggplot() +
  aes(x = fx_etaria, y = nota_media) +
  geom_col()

extrafont::loadfonts("win")

theme_set(theme_minimal(base_family = "Helvetica"))

theme_update(
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  axis.line.x = element_line(color = "black", linewidth = .6),
  axis.ticks.x = element_line(color = "black", linewidth = .6),
  axis.title.y = element_blank(),
  axis.text = element_text(family = "Helvetica", size = 11, color = "black"),
  plot.margin = margin(10, 15, 10, 15),
  plot.title = element_text(face = "bold"),
  plot.background = element_rect(
    # Altera o preenchimento
    fill = "grey80"
  ))

df |> 
  # Selecionando somente o filme de interesse
  filter(id_filme == id_filme_fav) |> 
  ggplot() +
  aes(x = fx_etaria, y = nota_media) +
  geom_col(aes(fill = fx_etaria)) + 
  coord_flip() + ######### talvez tenha que retirar
  facet_wrap(~ fx_etaria, ncol = 1, scales = "free_y") + 
  scale_y_continuous(
    breaks = seq(0, 10, by = 1),
    expand = c(0, 0), 
    limits = c(0, 10),
    #labels = scales::number,
    #name = "Proportion"
  ) +
  scale_x_discrete(
    guide = "none",
    expand = expansion(add = c(.8, .6))
  ) +
  theme(
    strip.text = element_text(
      hjust = 0, 
      margin = margin(1, 0, 1, 0), 
      size = rel(1.1), 
      face = "bold"
    )
  ) +
  geom_text(
    aes(
      label = paste0("  ", sprintf("%2.1f", nota_media), "  "),
      #color = "black", 
      hjust = nota_media > .05
    ),
    size = 4, 
    fontface = "bold", 
    family = "Helvetica"
  ) +
  scale_fill_manual(
    values = c("#F5C518", "#F5C518", "#F5C518", "#F5C518"),
    guide = "none"
  ) +
  labs(
    title = "The Grand Hotel Budapest",
    subtitle = "Nota média por faixa etária",
    caption = "Fonte: IMDb",
    y = "Nota média"
  )


