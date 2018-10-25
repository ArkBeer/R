is_odd<-function(n){
  return(n%%2!=0)
}
atoi<-function(str){
  return(str %>% chartr("a-z","A-Z",.) %>% str_split("") %>% map(~{match(.,c(0:9,LETTERS)) %>% -1 %>% str_flatten()}) %>% unlist())
}
