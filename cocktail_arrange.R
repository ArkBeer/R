library(tidyverse)
library(magrittr)
library(furrr)

plan(multiprocess)
recipes_modified<-recipes %>% future_map(~{
  data<-data.frame(data.frame(name=.[[1]]),.[[2]],data.frame(msg=.[[4]]))
  material<-.[[3]] %>% data.frame()
  colnames(material)<-c("material", "qty", "ml", "alc", "nba_std")
  rownames(material)<-1:NROW(material)
  material %<>% mutate(name=data$name) %>% select(name,material, qty, ml, alc, nba_std)
  return(list(data,material))
})


cocktail<-recipes_modified %>% future_map(~{
  .[[1]]
}) %>% bind_rows()
material<-recipes_modified %>% future_map(~{
  .[[2]]
}) %>% bind_rows()

plan(sequential)

