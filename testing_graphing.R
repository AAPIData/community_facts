library(shiny)
library(data.table)
library(tidyverse)
library(readxl)
library(plotly)
# test_dta <- file.path("data", "test_dta.xlsx") 
# dta<- read_excel(test_dta)
dta<- read.csv("https://raw.githubusercontent.com/AAPIData/raw_data/master/data_factsheets/test_dta.csv")
# source_dta <- file.path("data", "source_information.xlsx") 
# source<- read_excel(source_dta)
source <- read.csv("https://raw.githubusercontent.com/AAPIData/raw_data/master/data_factsheets/source_information.csv")
# topic_lookup <-file.path("data", "topic_lookup.xlsx") 
# topic_lookup_dta <- read_excel(topic_lookup)
topic_lookup_dta <- read.csv("https://raw.githubusercontent.com/AAPIData/raw_data/master/data_factsheets/topic_lookup.csv")

topic_lookup_dta<- topic_lookup_dta %>% rename(Estimate= "var_name")


dta <- dta %>% 
#  filter(dta$group %in% input$Group) %>%
  gather(Estimate, value, -group)

percent_estimates<-dta %>%
  filter(str_detect(Estimate, 'pct_')) %>% pull(Estimate) %>% unique()

states_estimates <- dta %>%
  filter(str_detect(Estimate, 'state_')) %>% pull(Estimate) %>% unique()

selected_estimates_show<- input_checkgroup()
top_states<-input_topstates()  


dta<- dta %>% left_join(topic_lookup_dta) %>% 
#  filter(Estimate %in% selected_estimates_show) %>% 
  filter(topic_group !="Top States") %>% 
  nest(-topic_group)

test_out<- dta %>% 
  filter(topic_group == "Total Population") %>% 
  data.frame() %>% rename(pop_group = group)

test_out$value <- as.numeric(test_out$value)

hchart( test_out,type = 'bar', hcaes(y = value, group = label, x = pop_group))

testgroups <- c("Indian", "Bangladeshi","Filipino")
dta %>%
  filter(topic_group == "Total Population") %>%
  data.frame() %>% 
  rename(pop_group = group) %>% 
  filter(pop_group %in% testgroups ) %>% 
  mutate(value = as.numeric(value)) %>% 
  group_by(label) %>% 
  plot_ly(x = ~ as.character(pop_group), y = ~value)

bar_charter <- function(full_dta,topic){
  hc <- full_dta %>%
    filter(topic_group == topic) %>%
    data.frame() %>% 
    rename(pop_group = group) %>% 
    
}


bar_charter(dta,"Total Population")


data(citytemp)
citytemp2 <- citytemp %>%
  tidyr::gather(key = city, value = temperature, tokyo, london)
hchart(citytemp2, type = 'line', hcaes(y = temperature, group = city, x = month))



