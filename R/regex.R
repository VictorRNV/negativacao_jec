endereco <- c("Avenida Paulista, 458, apto 1070, cep 01500-000, município de São Paulo, estado de são Paulo",
              "rua Padrão, 658, cep 01200-017, Sao Paulo, estado de Sao paulo")

# Extraindo informações do texto

str_extract(endereco, "\\d{5}-\\d{3}")

str_extract(endereco, "Paulo")

str_extract_all(endereco, "Paulo")

str_extract_all(endereco, "(?i)paulo")

df <- tibble(endereco = endereco)

df <- df |> 
  mutate(cep = str_extract(endereco, "\\d{5}-\\d{3}"))

df <- df |>
  mutate(uf = str_extract(endereco, "(?<=estado de ).+"))

# Substituindo as vírgulas por ponto  e vírgula

df <- df |>
  mutate(endereco = str_replace(endereco, ",", ";"))

df <- df |> 
  mutate(endereco2 = str_remove_all(endereco, ";"))

# str_detect

str_detect(endereco, "(?i)paulo")

str_extract_all(endereco, "(?i)s[ãa]o paulo")

cjpg <- readRDS("data/cjpg.rds")

# Alterando a coluna foro com o objetivo de tirar a palavra foro de e deixar
# somente o nome da comarca.

cjpg <- cjpg |> 
  mutate(foro = str_remove(foro, "Foro (de )?"))

# Verificar se existe a palavra ferias

cjpg <- cjpg |> 
  mutate(ferias = str_detect(julgado, "(?i)férias")) # Traduzindo o codigo:
                                                     # no código acima, criamos 
                                                     # uma coluna ferias, a partir
                                                     # da busca pela palavra ferias
                                                     # na coluna julgado. Note-se
                                                     # que usamos regex para buscar
                                                     # a palavra férias maiúscula e minúscula

# Contando em quantas observações da nova variável férias há a palavra férias

count(cjpg, ferias)

# Verificar se existe a palavra decimo terceiro ou 13º
# Para isso será utilizado o pacote quanteda e as funções kwic e corpus

corpo <- corpus(cjpg, docid = "cd_doc", text_field = "julgado")

decimo_terceiro <- kwic(corpo, "(13º|d.ecimo)", window = 10, valuetype = "regex")

cjpg <- cjpg |> 
  mutate(decimo_terceiro = str_detect(julgado, "(?i)(13º|d[ée]cimo(?!s))"))

# Vetor para testar a palavra ferias

ferias <- kwic(corpo, "férias", window = 10)

# Vetor para testar a palavra repercussão

repercussao <- kwic(corpo, "repercu*", windows = 10)

# Vetor para testar a palavra décimo terceiro

decimo <- c("décimo-terceiro", "décimos", "décimo terceiro")

str_detect(decimo, "(?i)décimo(?!s)") # Neste regex, estamos falando que a palavra
                                      # décimo terceiro pode começar com maiúscula
                                      # ou minúscula. Além disso, falamos que ela 
                                      # não pode terminar com "s" (Negative lookahead)

count(cjpg, decimo_terceiro)
