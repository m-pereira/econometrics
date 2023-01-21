## instalar os pacotes necessários
pckgs <-  c(
  "tidyverse","wooldridge","DataExplorer",
  "skimr","explore","devtools","remotes",
  "correlationfunnel","janitor")
install.packags("pckgs")
remotes::install_github("rfsaldanha/microdatasus")
devtools::install_github("agstn/dataxray")


## carregar os pacotes
library(wooldridge) # biblioteca com os dados
library(tidyverse) # ferramentas para análise de dados
library(janitor) # ferramentas para limpeza de dados
library(skimr) # análise de qualidade de dados 
library(dataxray) # exploração de dados automática
library(explore) # exploração de dados automática
library(DataExplorer) # exploração de dados automática
library(correlationfunnel) # análise de correlação
library(ggcorrplot) # análise de correlação

### carregar os dados
data("wage1")

## renomear e selecionar as colunas com transmute 

dados <- wage1 %>% 
  transmute(
    nivel_educacional = educ,
    salario = wage,
    anos_experiencia = exper,
    anos_no_emprego = tenure
  ) %>% tibble()

## primeiro resumo dos dados com glimpse 
dados %>% glimpse()
dados %>% head()
## avaliar qualidade dos dados com skimr
skim(dados)

## EDA automática com DataExplorer
create_report(dados, y = "salario")

## EDA automática com xray
dados %>% 
  make_xray() %>% 
  view_xray()
## EDA automática com 
dados %>% explore() 

## análise de correlação
dados %>% 
  cor() %>% 
  ggcorrplot(
    type = "lower",
    insig = "blank",
    lab = TRUE,
    digits = 3
  )

dados %>% 
  binarize(n_bins = 2) %>% 
  glimpse()


dados %>% 
  binarize(n_bins = 2) %>% 
  correlate(target = `salario__-Inf_4.65000009536743`) %>% 
  plot_correlation_funnel(interactive = TRUE)

## bonus easystats
install.packages("easystats")
library(easystats)
dados <- wage1 %>% 
  transmute(
    nivel_educacional = educ,
    salario = lwage,
    anos_experiencia = exper,
    anos_no_emprego = tenure
  ) %>% tibble()
## criação do modelo regressão
my_reg <- lm(salario ~.,data = dados)
my_reg %>% summary()
## análise de resíduos
check_model(my_reg)

## criação do modelo regressão
my_reg2 <- lm(salario ~
                nivel_educacional+
                poly(anos_experiencia,2)+
                poly(anos_no_emprego,2),
              data = dados)

my_reg2 %>% summary()
## análise de resíduos
check_model(my_reg2)

# análise de performance do modelo
my_reg %>% 
  performance() %>%
  print_md()
## comparação de performance entre os modelos
compare_performance(my_reg, my_reg2) %>%
  print_md()
# análise gráfia da comparação de performance entre os modelos
plot(compare_performance(my_reg, my_reg2))

# teste de performance entre os modelos
test_performance(my_reg, my_reg2) %>%
  print_md()
