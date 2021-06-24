#################### A3DATA - Teste Técnico Cientista de Dados =================

## Autor: Luis Henrique Velasquez Santos Porto

# rmarkdown::render("TesteTécnicoA3Data_LuisHVelasquez.Rmd", "pdf_document") # Convertendo para PDF


########## Pacotes e funções utilizadas
tab <- function(x){cbind(table(x), prop.table(table(x)))}

Limites <- function(x){
  media<-c()
  for (i in 1:1000) {
    boot<-sample(x, replace=TRUE)
    media[i] <- mean(boot, na.rm =TRUE)
  }
  LI<- quantile(media, probs=c(0.025), na.rm =TRUE)
  LS<- quantile(media, probs=c(0.975), na.rm =TRUE)
  valores<- c(LI,LS)
  valores
}

basic.np <- function(x, more=F) {
  Estatisticas <- list()
  
  Dados.Validos <- x[!is.na(x)]
  
  Estatisticas$N_validos <- length(Dados.Validos)
  Estatisticas$Média <- mean(Dados.Validos)
  Estatisticas$D.P. <- sd(Dados.Validos)
  Estatisticas$L.I.<- as.numeric(Limites(Dados.Validos)[1])
  Estatisticas$L.S.<- as.numeric(Limites(Dados.Validos)[2])
  Estatisticas$Mín. <- min(Dados.Validos)
  Estatisticas$Q1 <- fivenum(Dados.Validos)[2]
  Estatisticas$Q2<- fivenum(Dados.Validos)[3]
  Estatisticas$Q3 <- fivenum(Dados.Validos)[4]
  Estatisticas$Máx. <- max(Dados.Validos)
  Final <- unlist(Estatisticas)
  names(Final)<- c("N válidos", "Média", "D.P.","L.I", "L.S","Mín.", "1Q", "2Q", "3Q","Máx.")
  Final
}

basic.estats <- function(x, more=F) {
  stats <- list()
  
  clean.x <- x[!is.na(x)]
  
  #stats$N <- length(x)
  #stats$NAs <- stats$N-length(clean.x)
  stats$N_validos <- length(clean.x)
  stats$Média <- mean(clean.x)
  stats$E.P <- sd(clean.x)/sqrt(length(clean.x))
  stats$Q1 <- fivenum(clean.x)[2]
  stats$Q2<- fivenum(clean.x)[3]
  stats$Q3 <- fivenum(clean.x)[4]
  t1<- unlist(stats)
  names(t1)<- c("N válidos", "Média", "E.P", "1Q", "2Q", "3Q")
  t1
}

kruskal.completo <- function(y, z, more=F){
  tab<-matrix(NA, length(levels(factor(z))), 6)
  for(i in 1:length(levels(factor(z)))){ 
    desc<- tapply(y, factor(z),  basic.stats)[i]
    desc1<- unlist(desc)
    for(j in 1:6){ 
      tab[i,j] <-desc1[j]
    }
  }
  p_valor<- rep(kruskal.test(y~factor(z))$p.value, length(levels(factor(z))))
  tab <- cbind(tab, p_valor)
  colnames(tab)<- c("N", "Média", "E.P.", "1º Q.", "2º Q.", "3º Q.", "Valor-p")
  rownames(tab)<- levels(factor(z))
  
  if(!require(PMCMR)){ install.packages("PMCMR"); require(PMCMR) }
  CM <- posthoc.kruskal.nemenyi.test(y ~ factor(z), dist="Chisq")$p.value
  
  model<-list(tabela=tab, C.Multiplas=CM)
  model
}


########## Carregando os bancos de dados

Ocorrencias <- read.csv(file = "Bancos de Dados/ocorrencia_2010_2020.csv", sep = ";",
                        header = TRUE, encoding = "UTF-8")
colnames(Ocorrencias)[1] <- "codigo_ocorrencia" # Corrigindo o nome da coluna inicial

Tipo_Ocorrencias <- read.csv(file = "Bancos de Dados/ocorrencia_tipo_2010_2020.csv", sep = ";", 
                             header = TRUE, encoding = "UTF-8")
colnames(Tipo_Ocorrencias)[1] <- "codigo_ocorrencia1" # Corrigindo o nome da coluna inicial

Aeronaves <- read.csv(file = "Bancos de Dados/aeronave_2010_2020.csv", sep = ";", 
                      header = TRUE, encoding = "UTF-8")
colnames(Aeronaves)[1] <- "codigo_ocorrencia2" # Corrigindo o nome da coluna inicial

Fator_contribuinte <- read.csv(file = "Bancos de Dados/fator_contribuinte_2010_2020.csv", sep = ";", 
                               header = TRUE, encoding = "UTF-8")
colnames(Fator_contribuinte)[1] <- "codigo_ocorrencia3" # Corrigindo o nome da coluna inicial

Recomendação <- read.csv(file = "Bancos de Dados/recomendacao_2010_2020.csv", sep = ";", 
                         header = TRUE, encoding = "UTF-8")
colnames(Recomendação)[1] <- "codigo_ocorrencia4" # Corrigindo o nome da coluna inicial



########## Análisando superficialmente os bancos de dados ======================

library("tidyverse") # Uso do pipe ( "%>%"), ggplot2 e outras funções de manipulação de dados

##### Banco de dados "Ocorrencias"
Ocorrencias %>% str()
Ocorrencias %>% dim()
Ocorrencias %>% head()
Ocorrencias %>% colnames()


##### Banco de dados "Tipos de Ocorrencias"
Tipo_Ocorrencias %>% str()
Tipo_Ocorrencias %>% dim()
Tipo_Ocorrencias %>% head()
Tipo_Ocorrencias %>% colnames()


##### Banco de dados "Aeronaves"
Aeronaves %>% str()
Aeronaves %>% dim()
Aeronaves %>% head()
Aeronaves %>% colnames()


##### Banco de dados "Fator Contribuinte"
Fator_contribuinte %>% str()
Fator_contribuinte %>% dim()
Fator_contribuinte %>% head()
Fator_contribuinte %>% colnames()


##### Banco de dados "Recomendação"
Recomendação %>% str()
Recomendação %>% dim()
Recomendação %>% head()
Recomendação %>% colnames()




############### Testes Estatístico de Kruskall-Wallis ==========================

########## Verificando a normalidade da variável numérica "Nº de fatalidades"
ggplot(Aeronaves, aes(x = aeronave_fatalidades_total)) + 
  geom_density() +
  labs(title = "Gráfico de densidade para o número de fatalidades nas ocorrências",
       x = "Número de fatalidades",
       y = "Densidade") +
  scale_x_continuous(limits = c(0, 16), # Limites do eixo x no gráfico
                     breaks = seq(0,16, by = 1)) + # Intervalo de cada valor apresentado no eixo x
  theme(plot.title = element_text(hjust = 0.5)) # Centralizando o título

library("nortest") # Pacote necessário para utilizar o teste de Normalidade de Anderson-Darling (amostras > 5000 linhas)
Aeronaves$aeronave_fatalidades_total %>% ad.test() # Hipótese Nula = Os dados seguem uma dist. Normal


########## Recategorizando as variáveis que possuiam o nível "***"
Aeronaves$aeronave_tipo_veiculo2 <- case_when(Aeronaves$aeronave_tipo_veiculo == "***" ~ NA_character_,
                                              TRUE ~ as.character(Aeronaves$aeronave_tipo_veiculo))

Aeronaves$aeronave_motor_tipo2 <- case_when(Aeronaves$aeronave_motor_tipo == "***" ~ NA_character_,
                                            TRUE ~ as.character(Aeronaves$aeronave_motor_tipo))


########## Número de Fatalidade de acordo com o tipo de aeronave
kruskal.completo(Aeronaves$aeronave_fatalidades_total, Aeronaves$aeronave_tipo_veiculo2)$tabela
kruskal.completo(Aeronaves$aeronave_fatalidades_total, Aeronaves$aeronave_tipo_veiculo2)$C.Multiplas

########## Número de Fatalidades de acordo com o motor utilizado
kruskal.completo(Aeronaves$aeronave_fatalidades_total, Aeronaves$aeronave_motor_tipo2)$tabela
kruskal.completo(Aeronaves$aeronave_fatalidades_total, Aeronaves$aeronave_motor_tipo2)$C.Multiplas




############### Correlação entre as variáveis de interesse =====================
Aeronaves$aeronave_assentos %>% head() # Verificando as primeiras linhas da variável
Aeronaves$aeronave_assentos %>% class() # Verficando a classe da variável

cor.test(Aeronaves$aeronave_fatalidades_total, as.numeric(Aeronaves$aeronave_assentos), method = "spearman")


 

############### Nuvem de palavras ==============================================
library("tidytext") # Pacote necessário para extrair as palavras

##### Separando cada uma das palavras contidas nas recomendações escritas
Texto <- data.frame(id = 1:length(Recomendação$recomendacao_conteudo), 
                    Textos = Recomendação$recomendacao_conteudo)

Contagem <- Texto %>% 
  mutate(Textos = as.character(Textos)) %>% 
  unnest_tokens(Palavra, Textos) %>%   # Função que coloca em linhas cada uma das palavras de cada recomedação
  count(Palavra) 

Contagem %>% 
  arrange(desc(n)) %>% # Ordenando o banco de dados em ordem decrescente com relação as frequências
  head(50) # Apresentando as 50 palavras mais frequentes

##### Retirando palavras muito comuns dos textos, como preposições e artigos
Palavras.comuns <- c("de","a","e","que","do","da","o","os","no","em","dos","à","para","fim","com","ao","das","na",
                     "aos","as","se","seus","por","como","uma","não","pela","suas","um","durante","nos","ou",
                     "nas","às","pelo","aquele","diz","sejam")

##### Criando um banco de dados com as 50 palavras mais frequentes depois de "filtradas"
Nuvem.palavras <- Contagem %>% 
  filter(!Palavra %in% Palavras.comuns) %>% # Selecionando as palavras que não pertencem a esse vetor
  arrange(desc(n)) %>%  # ordenando em ordem decrescente
  top_n(50, n) # Selecionando as 50 palavras mais frequentes
Nuvem.palavras

##### Criando a nuvem de palavras
library("highcharter")
hchart(Nuvem.palavras, "wordcloud", hcaes(name = Palavra, weight = n)) 




############### Regressão Quassi-Poisson =======================================

### Verificando a suposição de equidispersão da variável resposta (Y)
mean(Aeronaves$aeronave_fatalidades_total)
var(Aeronaves$aeronave_fatalidades_total)

### Nesse caso, Var(Y) > E(Y) -> superdispersão  -> não satisfaz a premissa básica pra uma reg. poisson
mean(Aeronaves$aeronave_fatalidades_total)/var(Aeronaves$aeronave_fatalidades_total)


############### Juntando os bancos de dados necessários ========================
Dados.join1 <- full_join(Ocorrencias, Tipo_Ocorrencias, by = "codigo_ocorrencia1")
Dados.join2 <- full_join(Dados.join1, Aeronaves, by = "codigo_ocorrencia2")
Dados.final <- full_join(Dados.join2, Fator_contribuinte, by = "codigo_ocorrencia3")
Dados.final %>% dim()


########## Recategorizando as variáveis necessárias
Dados.final$aeronave_tipo_veiculo2 <- case_when(Dados.final$aeronave_tipo_veiculo == "***" ~ NA_character_,
                                                TRUE ~ as.character(Dados.final$aeronave_tipo_veiculo))

Dados.final$aeronave_motor_tipo2 <- case_when(Dados.final$aeronave_motor_tipo == "***" ~ NA_character_,
                                              TRUE ~ as.character(Dados.final$aeronave_motor_tipo))

Dados.final$aeronave_nivel_dano2 <- case_when(Dados.final$aeronave_nivel_dano == "***" ~ NA_character_,
                                              TRUE ~ as.character(Dados.final$aeronave_nivel_dano))

Dados.final$fator_area2 <- case_when(Dados.final$fator_area == "***" ~ NA_character_,
                                     TRUE ~ as.character(Dados.final$fator_area))


########## Criando a regressão correta para os dados

##### Modelo Inicial
modelo <- glm(aeronave_fatalidades_total ~ 
                ocorrencia_classificacao + 
                as.factor(total_aeronaves_envolvidas) +
                aeronave_tipo_veiculo2 +
                aeronave_motor_tipo2 +
                aeronave_nivel_dano2 +
                fator_area2, 
              family = quasipoisson, 
              data = Dados.final)

modelo %>% summary()

# NA = indicio de correlação com alguma outra variavel

##### Modelo Final
modelo.final <- glm(aeronave_fatalidades_total ~ 
                      # ocorrencia_classificacao + 
                      # as.factor(total_aeronaves_envolvidas) +
                      # aeronave_tipo_veiculo2 +
                      aeronave_motor_tipo2 +
                      aeronave_nivel_dano2 +
                      fator_area2, 
                    family = quasipoisson, 
                    data = Dados.final)

modelo.final %>% summary()

##### Função para obter o exponencial dos coeficientes e melhorar a interpretação do modelo
tab.log <- function(mod){
  t0 <- summary(mod)
  exp <- exp(t0$coeff[,1])
  L.I <- exp(t0$coeff[,1]-(1.96*t0$coeff[,2]))
  L.S <- exp(t0$coeff[,1]+(1.96*t0$coeff[,2]))
  I.C.95 <- c()
  for(i in 1:dim(t0$coeff)[1]) {
    I.C.95[i] <- paste("[", format(round(L.I[i], 2), nsmall=2), "; ", format(round(L.S[i], 2), nsmall=2), "]", sep="")}
  t.final <- data.frame( round(exp,2), I.C.95, round(t0$coeff[,4], 3))
  t.final[1,] <- rep(NA, dim(t.final)[2])
  colnames(t.final) <- c("Exp(B)", "I.C.-95% ", "Valor-p")
  
  return(t.final)
}

tab.log(modelo.final)
# Comparando com o tipo de motor = Jato, Dano = Destruida, Area do Fator = Humano




############### Mapas ==========================================================
Mapa.Dados <- Ocorrencias %>% 
  select(ocorrencia_uf) %>% # Selecionando a variável de interesse
  count(ocorrencia_uf) # Obtendo as frequências de cada UF
Mapa.Dados

hcmap(map = "countries/br/br-all", # Obtendo o mapa do Brasil a partir do site do Highcharter
      data = Mapa.Dados, # Bancos de dados com as informações de interesse
      value = "n", # Valores que serão utilizados no mapa (Frequências das ocorrências)
      joinBy = c("hc-a2", "ocorrencia_uf"), # Juntando o mapa aos dados a partir da variável com a UF
      name = "Total de Ocorrências") %>% # Nome que será dado aos valores apresentados do mapa
  hc_colorAxis(minColor = "#ffffff", maxColor = "#F60909") %>% # Definindo as escalas de cores do mapa
  hc_title(text = "Total de ocorrências de acordo com o Estado") # Adicionando um título ao mapa



