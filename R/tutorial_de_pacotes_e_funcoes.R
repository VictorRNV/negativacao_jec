library(dados)
library(tidyverse)
library(readxl)
library(writexl)

View(voos)

voos <- voos

voos <- voos[1:100,]

## formato serializado
saveRDS(voos, "data/voos.rds")

voos <- readRDS("data/voos.rds")

## csv
### Pelo R base
write.csv(voos, "data/voos.csv")
write.table(voos, "data/voos.csv", sep = ";")

## Tidyverse (readr)
write_csv2(voos, "data/voos.csv")

write_delim(voos, "data/voos.csv",,delim = ";")

write_delim(voos, "data/voos.csv",,delim = "\t")

write_xlsx(voos, "data/voos.xlsx")

v <- read_excel("data/voos.xlsx")

dados_iris <- dados_iris

save.image("data/base.RData")

load("data/base.RData")

## tibble

glimpse(voos)

# Como dar um id para as colunas?

voos <- rownames_to_column(voos, "id")

# Como criar uma coluna 

voos <- add_column(voos, uf = "SC", .before = 2)

## Pipe %>% (do tidyverse) ou |> (nativo)

1:10 %>%
  sum () %>%
  sqrt()

"segunda parte" %>%
  paste("primeira parte",., sep = " ")

"segunda parte" |> 
  paste("primeira parte",... = _, sep = " ")


## Dplyr

# principais funções: select, filter, count, mutate, rename, sumarize, arrange, group-by


# Função select

s1 <- voos |> 
  select(ano, mes, dia)

s2 <- voos |> 
  select (1, 5, 6)

s3 <- voos |> 
  select(1:10)

s4 <- voos |> 
  select(-horario_saida)

s5 <- voos |> 
  select(contains("horario"))

voos <- voos |> 
  rownames_to_column("id")

s6 <- voos |> 
  select(id, ends_with("hora"))

s7 <- voos |> 
  select(starts_with("atraso"))

s8 <- voos |> 
  select(year = ano, month = mes, day = dia)

# Rename (utilizada para renomear sem selecionar)

voos <- voos |> 
  rename(year = ano, month = mes, day = dia)

#Relocate (utilizada para recolocar)

voos <- voos |> 
  relocate(horario_saida, .before = 1)

voos <- voos |> 
  relocate(year, .after = month)

# Filter (olha para as linhas)

jfk <- voos |> 
  filter(origem == "JFK")

jl <- voos |> 
  filter(origem == "JFK" | origem == "LGA")

horario <- voos |> 
  filter(horario_chegada < 900)

horario <- voos |> 
  filter(horario_chegada <= 800)

horario <- voos |> 
  filter(horario_chegada <= 800)

h <- voos |> 
  filter(horario_chegada <= 800 & origem == "JFK")

# Unique

unique(voos$origem)

# Count (utilizado para contar, especialmente, variáveis categóricas)

voos |> 
  count(origem)

voos |> 
  count(origem, sort = TRUE)

voos |> 
  count(origem, destino, sort = TRUE)

# Mutate (utilizado para trasnformar colunas ou criar novas a partir do que já existe)

voos <- voos |> 
  mutate(year = as.character(year))

#multiplicar por 2

voos <- voos |> 
  mutate(month = month*2)

#juntar colunas

voos <- voos |> 
  mutate(od = paste(origem, destino, sep = "-"))

voos <- voos |> 
  mutate(od = NULL)

voos <- voos |> 
  mutate(od = paste(origem, destino, sep = "-"), .after = destino)

voos <- voos |> 
  group_by(origem) |> 
  mutate(tempo_medio = mean(tempo_voo, na.rm = TRUE),
         .after = tempo_voo)
## sumarize

sumario <- voos |> 
  ungroup() |> 
  drop_na() |> 
  group_by(origem) |> 
  summarize(quantidade = n(),
            minimo = min(tempo_voo),
            max = max(tempo_voo),
            media = mean(tempo_voo),
            desvio_padrao = sd(tempo_voo),
            mediana = median(tempo_voo))

View(sumario)


## Arrange

sumario <- sumario |> 
  arrange(quantidade)

sumario <- sumario |>
  arrange(desc(quantidade))

View(sumario)
