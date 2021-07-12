library(readxl)
library(dplyr)
library(purrr)
library(janitor)
library(lubridate)
library(tidyr)
library(stringr)

#' Procura a palavra ou frase do segundo vetor que melhor 
#'    se aproxima do primeiro. Particularmente útil para 
#'    comparar nomes de municípios.
#'    
#' @param x Vetor de strings de referência.
#' @param y Vetor de strings a serem buscados.
#'
#' @return vetor com as strings de y próximos
#'     de x.
#' @export
#'
#' @examples
busca_fuzzy<-function(x,y){
  
  `%>%` <- magrittr::`%>%`  
  
  
  x1 <- x %>% 
    stringi::stri_trans_general("latin-ascii") %>% 
    stringi::stri_trans_tolower() %>% 
    stringi::stri_trim_both() %>% 
    stringi::stri_replace_all_regex("\\s+","_")
  
  y1 <- y %>% 
    stringi::stri_trans_general("latin-ascii") %>% 
    stringi::stri_trans_tolower() %>% 
    stringi::stri_trim_both() %>% 
    stringi::stri_replace_all_regex("\\s+","_")
  
  purrr::map(x1, ~{
    
    a <- stringdist::stringdist(.x,y1)
    
    b <- which.min(a)
    
    d <- y[b]
    
  }) %>% 
    unlist()
  
}



# Metodologia_2020 <- read_excel("Metodologia.xlsx", 
#                           sheet = "Metodologia 2020")
# Metodologia_2021 <- read_excel("Metodologia.xlsx", 
#                             sheet = "Metodologia 2021")
# 
# 
# Metodologia_2020 <- clean_names(Metodologia_2020)
# Metodologia_2021 <- clean_names(Metodologia_2021)
# 
# 
# 
# dados_painel_full<-
#  
# Metodologia_2021 %>%
#   mutate(saldo_r_item_informacao = ifelse(item_informacao_codigo  %in% c(20,51), saldo_r_item_informacao * (-1), saldo_r_item_informacao)) %>% #créditos indisponíveis e RAP cancelado
#   #filter(mes_lancamento == "abril de 2021") %>% 
#   group_by(mes_lancamento,fase_da_despesa,nome_do_gasto) %>%
#   summarise(
#     gasto_item= sum(saldo_r_item_informacao)
#   )  %>%
#   bind_rows(Metodologia_2020 %>%
#               #filter(mes_lancamento== "dezembro de 2020") %>%
#               rename(nome_do_gasto= gastos_com_covid_19,
#                      fase_da_despesa = item_informacao) %>%
#               mutate(fase_da_despesa = ifelse(fase_da_despesa == "Previsão de Gastos", "Previsão", "Pagamento")) %>%
#               group_by(mes_lancamento,fase_da_despesa,nome_do_gasto)%>%
#               summarise(
#                 gasto_item= sum(saldo_r_item_informacao)
#               ) 
#   ) %>%
#   ungroup() %>%
#   mutate(mes_lancamento = case_when(
#     str_detect(str_to_lower( mes_lancamento),"janeiro") ~ str_c(str_sub(mes_lancamento,str_length(mes_lancamento)-4,str_length(mes_lancamento)),"01","01", sep= "-"),
#     str_detect(str_to_lower( mes_lancamento),"fevereiro") ~ str_c(str_sub(mes_lancamento,str_length(mes_lancamento)-4,str_length(mes_lancamento)),"02","01", sep= "-"),
#     str_detect(str_to_lower( mes_lancamento),"março") ~ str_c(str_sub(mes_lancamento,str_length(mes_lancamento)-4,str_length(mes_lancamento)),"03","01", sep= "-"),
#     str_detect(str_to_lower( mes_lancamento),"abril") ~ str_c(str_sub(mes_lancamento,str_length(mes_lancamento)-4,str_length(mes_lancamento)),"04","01", sep= "-"),
#     str_detect(str_to_lower( mes_lancamento),"maio") ~ str_c(str_sub(mes_lancamento,str_length(mes_lancamento)-4,str_length(mes_lancamento)),"05","01", sep= "-"),
#     str_detect(str_to_lower( mes_lancamento),"junho") ~ str_c(str_sub(mes_lancamento,str_length(mes_lancamento)-4,str_length(mes_lancamento)),"06","01", sep= "-"),
#     str_detect(str_to_lower( mes_lancamento),"julho") ~ str_c(str_sub(mes_lancamento,str_length(mes_lancamento)-4,str_length(mes_lancamento)),"07","01", sep= "-"),
#     str_detect(str_to_lower( mes_lancamento),"agosto") ~ str_c(str_sub(mes_lancamento,str_length(mes_lancamento)-4,str_length(mes_lancamento)),"08","01", sep= "-"),
#     str_detect(str_to_lower( mes_lancamento),"setembro") ~ str_c(str_sub(mes_lancamento,str_length(mes_lancamento)-4,str_length(mes_lancamento)),"09","01", sep= "-"),
#     str_detect(str_to_lower( mes_lancamento),"outubro") ~ str_c(str_sub(mes_lancamento,str_length(mes_lancamento)-4,str_length(mes_lancamento)),"10","01", sep= "-"),
#     str_detect(str_to_lower( mes_lancamento),"novembro") ~ str_c(str_sub(mes_lancamento,str_length(mes_lancamento)-4,str_length(mes_lancamento)),"11","01", sep= "-"),
#     str_detect(str_to_lower( mes_lancamento),"dezembro") ~ str_c(str_sub(mes_lancamento,str_length(mes_lancamento)-4,str_length(mes_lancamento)),"12","01", sep= "-")
#       
#   ))


################### Tratamento de agrupamentos conceituais

legislacao_covid <- read_excel("legislacao_covid.xlsx", 
                               sheet = "Agrupamento conceitual", col_types = c("text", 
                                                                               "text", "text", "date", "date", "text", 
                                                                               "text"))
legislacao_covid <- clean_names(legislacao_covid)

Painel_COVID_2020_todos_os_meses <- read_excel("Painel COVID- 2020 todos os meses.xlsx", 
                                               skip = 4)

Painel_COVID_2020_todos_os_meses <- clean_names(Painel_COVID_2020_todos_os_meses)

Metodologia_2020 <- Painel_COVID_2020_todos_os_meses 


Metodologia_2020<-
Metodologia_2020 %>%
  rename(plano_orcamentario_nome= gastos_com_covid_19_nome,
         nome_do_gasto = classificacao_no_painel,
         fase_da_despesa = conta_contabil) %>%
  mutate(fase_da_despesa = ifelse(fase_da_despesa == "DOTACAO ATUALIZADA", "Previsão","Pagamento"))

Metodologia_2021 <- read_excel("Metodologia.xlsx", 
                               sheet = "Metodologia 2021")


Metodologia_2021 <- clean_names(Metodologia_2021)


dados_painel_full<-

Metodologia_2021 %>%
  mutate(saldo_r_item_informacao = ifelse(item_informacao_codigo  %in% c(20,51), saldo_r_item_informacao * (-1), saldo_r_item_informacao)) %>% #créditos indisponíveis e RAP cancelado
  #filter(mes_lancamento == "abril de 2021") %>% 
  group_by(mes_lancamento,plano_orcamentario_nome,fase_da_despesa,nome_do_gasto, acao_governo_codigo) %>%
  summarise(
    gasto_item= sum(saldo_r_item_informacao)
  )  %>%
  bind_rows(Metodologia_2020 %>%
              #filter(mes_lancamento== "dezembro de 2020") %>%
              #rename(nome_do_gasto= gastos_com_covid_19,
                     #fase_da_despesa = item_informacao) %>%
              #mutate(fase_da_despesa = ifelse(fase_da_despesa == "Previsão de Gastos", "Previsão", "Pagamento")) %>%
              group_by(mes_lancamento, plano_orcamentario_nome, fase_da_despesa,nome_do_gasto, acao_governo_codigo)%>%
              summarise(
                gasto_item= sum(saldo_r_conta_contabil)
              ) 
  ) %>%
  ungroup() %>%
  mutate(mes_lancamento = case_when(
    str_detect(str_to_lower( mes_lancamento),"janeiro") ~ str_c(str_sub(mes_lancamento,str_length(mes_lancamento)-3,str_length(mes_lancamento)),"01","01", sep= "-"),
    str_detect(str_to_lower( mes_lancamento),"fevereiro") ~ str_c(str_sub(mes_lancamento,str_length(mes_lancamento)-3,str_length(mes_lancamento)),"02","01", sep= "-"),
    str_detect(str_to_lower( mes_lancamento),"março") ~ str_c(str_sub(mes_lancamento,str_length(mes_lancamento)-3,str_length(mes_lancamento)),"03","01", sep= "-"),
    str_detect(str_to_lower( mes_lancamento),"abril") ~ str_c(str_sub(mes_lancamento,str_length(mes_lancamento)-3,str_length(mes_lancamento)),"04","01", sep= "-"),
    str_detect(str_to_lower( mes_lancamento),"maio") ~ str_c(str_sub(mes_lancamento,str_length(mes_lancamento)-3,str_length(mes_lancamento)),"05","01", sep= "-"),
    str_detect(str_to_lower( mes_lancamento),"junho") ~ str_c(str_sub(mes_lancamento,str_length(mes_lancamento)-3,str_length(mes_lancamento)),"06","01", sep= "-"),
    str_detect(str_to_lower( mes_lancamento),"julho") ~ str_c(str_sub(mes_lancamento,str_length(mes_lancamento)-3,str_length(mes_lancamento)),"07","01", sep= "-"),
    str_detect(str_to_lower( mes_lancamento),"agosto") ~ str_c(str_sub(mes_lancamento,str_length(mes_lancamento)-3,str_length(mes_lancamento)),"08","01", sep= "-"),
    str_detect(str_to_lower( mes_lancamento),"setembro") ~ str_c(str_sub(mes_lancamento,str_length(mes_lancamento)-3,str_length(mes_lancamento)),"09","01", sep= "-"),
    str_detect(str_to_lower( mes_lancamento),"outubro") ~ str_c(str_sub(mes_lancamento,str_length(mes_lancamento)-3,str_length(mes_lancamento)),"10","01", sep= "-"),
    str_detect(str_to_lower( mes_lancamento),"novembro") ~ str_c(str_sub(mes_lancamento,str_length(mes_lancamento)-3,str_length(mes_lancamento)),"11","01", sep= "-"),
    str_detect(str_to_lower( mes_lancamento),"dezembro") ~ str_c(str_sub(mes_lancamento,str_length(mes_lancamento)-3,str_length(mes_lancamento)),"12","01", sep= "-"),
    str_detect(str_to_lower( mes_lancamento),"jan") ~ str_c(str_sub(mes_lancamento,str_length(mes_lancamento)-3,str_length(mes_lancamento)),"01","01", sep= "-"),
    str_detect(str_to_lower( mes_lancamento),"fev") ~ str_c(str_sub(mes_lancamento,str_length(mes_lancamento)-3,str_length(mes_lancamento)),"02","01", sep= "-"),
    str_detect(str_to_lower( mes_lancamento),"mar") ~ str_c(str_sub(mes_lancamento,str_length(mes_lancamento)-3,str_length(mes_lancamento)),"03","01", sep= "-"),
    str_detect(str_to_lower( mes_lancamento),"abr") ~ str_c(str_sub(mes_lancamento,str_length(mes_lancamento)-3,str_length(mes_lancamento)),"04","01", sep= "-"),
    str_detect(str_to_lower( mes_lancamento),"mai") ~ str_c(str_sub(mes_lancamento,str_length(mes_lancamento)-3,str_length(mes_lancamento)),"05","01", sep= "-"),
    str_detect(str_to_lower( mes_lancamento),"jun") ~ str_c(str_sub(mes_lancamento,str_length(mes_lancamento)-3,str_length(mes_lancamento)),"06","01", sep= "-"),
    str_detect(str_to_lower( mes_lancamento),"jul") ~ str_c(str_sub(mes_lancamento,str_length(mes_lancamento)-3,str_length(mes_lancamento)),"07","01", sep= "-"),
    str_detect(str_to_lower( mes_lancamento),"ago") ~ str_c(str_sub(mes_lancamento,str_length(mes_lancamento)-3,str_length(mes_lancamento)),"08","01", sep= "-"),
    str_detect(str_to_lower( mes_lancamento),"set") ~ str_c(str_sub(mes_lancamento,str_length(mes_lancamento)-3,str_length(mes_lancamento)),"09","01", sep= "-"),
    str_detect(str_to_lower( mes_lancamento),"out") ~ str_c(str_sub(mes_lancamento,str_length(mes_lancamento)-3,str_length(mes_lancamento)),"10","01", sep= "-"),
    str_detect(str_to_lower( mes_lancamento),"nov") ~ str_c(str_sub(mes_lancamento,str_length(mes_lancamento)-3,str_length(mes_lancamento)),"11","01", sep= "-"),
    str_detect(str_to_lower( mes_lancamento),"dez") ~ str_c(str_sub(mes_lancamento,str_length(mes_lancamento)-3,str_length(mes_lancamento)),"12","01", sep= "-")
    
  ))


#Identifica uma primeira aproximação entre as legislações citadas no TG e a que é levantada manualmente  
de_para_legislacao<-
busca_fuzzy(unique(Metodologia_2020$plano_orcamentario_nome),unique(legislacao_covid$instrumento_inicial))    



#GEra uma primeira versão da tabela com o relacionamento entre as legislações
Metodologia_2020_legislacao <- tibble(plano_orcamentario_nome= unique(Metodologia_2020$plano_orcamentario_nome),
                                      instrumento_inicial_gerado = de_para_legislacao )

#GEra um arquivo csv que será melhor refinado manualmente
Metodologia_2020_legislacao %>%
  readr::write_csv("Metodologia_2020_legislacao.csv" )


de_para_2021<-
  busca_fuzzy(unique(Metodologia_2021$plano_orcamentario_nome),unique(legislacao_covid$instrumento_inicial))    

#GEra uma primeira versão da tabela com o relacionamento entre as legislações
Metodologia_2021_legislacao <- tibble(plano_orcamentario_nome= unique(Metodologia_2021$plano_orcamentario_nome),
                                      instrumento_inicial_gerado = de_para_2021 )

#GEra um arquivo csv que será melhor refinado manualmente
Metodologia_2021_legislacao %>%
  readr::write_csv("Metodologia_2021_legislacao.csv" )



metodologia_legislacao<-
  readr::read_csv("Metodologia_2020_legislacao_editada.csv") %>%
  bind_rows(
    readr::read_csv("Metodologia_2021_legislacao_editada.csv")
  )


dados_timeline<-
dados_painel_full %>%
  mutate(nome_do_gasto = ifelse(nome_do_gasto == "Aquisição de Vacinas", "Aquisição de Vacinas e Insumos para Prevenção e Controle", nome_do_gasto)) %>%
  inner_join(metodologia_legislacao %>%
               select( -c(instrumento_inicial_gerado,Compara)) %>%
               distinct(plano_orcamentario_nome, instrumento_inicial )) %>%
  left_join(legislacao_covid )


dados_timeline %>%
  readr::write_csv("dados_timeline.csv")




#### Dados do RTN
library(readr)

# Lê arquivo com dados históricos mensais dos principais valores agregados do RTN
dados_rtn <-  read_delim("dados_rtn.csv", 
                        ";", escape_double = FALSE, col_types = cols(X1 = col_skip()), 
                        trim_ws = TRUE)


# Faz um de-para para corrigir o nome das rubricas
de_para_rtn <- read_csv("de_para_rtn.csv")

dados_timeline_rtn<-
  
 
dados_timeline %>% 
  left_join(
    dados_rtn %>%
      inner_join(de_para_rtn) %>%
      rename(mes_lancamento = Data) %>%
      mutate(mes_lancamento = as.character(mes_lancamento)) %>%
      select(conta_rtn, mes_lancamento, Valor)
  )


dados_timeline_rtn %>%
  readr::write_csv("dados_timeline_rtn.csv")


consolidado<-
dados_timeline_rtn %>%
  distinct_at(1:11) %>%
  group_by(mes_lancamento, nome_do_gasto) %>%
  summarise(
    total = sum(gasto_item)
  ) 


consolidado<-
dados_timeline%>%
  group_by(mes_lancamento, nome_do_gasto, fase_da_despesa) %>%
  summarise(
    total = sum(gasto_item)
  ) 

consolidado %>%
  ggplot2::ggplot() +
  ggplot2::geom_col (aes(x=mes_lancamento, total))

consolidado<-
Metodologia_2020 %>%
  group_by(mes_lancamento, nome_do_gasto) %>%
  summarise(
    total = sum(saldo_r_conta_contabil)
  ) 

consolidado<-
  dados_painel_full %>%
  group_by(mes_lancamento, nome_do_gasto) %>%
  summarise(
    total = sum(gasto_item)
  ) 


consolidado<-
dados_painel_full %>%
  inner_join(metodologia_legislacao %>%
               select( -c(instrumento_inicial_gerado,Compara)))  %>%
  group_by(mes_lancamento, nome_do_gasto) %>%
  summarise(
    total = sum(gasto_item)
  ) 


consolidado<-
  dados_painel_full %>%
  inner_join(metodologia_legislacao %>%
               select( -c(instrumento_inicial_gerado,Compara)) %>%
               distinct(plano_orcamentario_nome, instrumento_inicial )) %>%
  
  group_by(mes_lancamento, nome_do_gasto) %>%
  summarise(
    total = sum(gasto_item)
  ) %>%
  ungroup()

conta_legislacao<-
legislacao_covid %>%
  group_by(
    instrumento_inicial
   ) %>%
  summarise(
    n()
  )
 

linhas_nulas_agrupamento <-
  dados_timeline %>%
  filter(is.na(agrupamento)) %>%
  distinct(instrumento_inicial)
