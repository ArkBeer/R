library(tidyverse)
library(rvest)
library(httr)
options(timeout= 4000000)
urls<-c()
recipes<-list()
for(i in paste0("E",0:26)){
  page<-read_html(paste0("http://ycos2.sakura.ne.jp/Cocktail/cgi-bin/cdb_index.cgi?",i),encoding = "euc-jp")
  tmp<-page %>% html_nodes("a") %>% 
    html_attr("href")
  urls<-c(urls,tmp[-(1:27)])
}
urls<-urls[-grep(",",urls)]
urls<-urls[-grep("http://ycos.sakura.ne.jp/",urls)]

for(url in urls){
  tryCatch({
  url_text<-GET(paste0("http://ycos2.sakura.ne.jp/Cocktail/cgi-bin/",url)) %>% 
    content(as="text")
  if(!is.na(url_text)){
    abelito<-read_html(url_text)
  }else {
    abelito<-read_html(paste0("http://ycos2.sakura.ne.jp/Cocktail/cgi-bin/",url))
  }
  abelito.name<-abelito %>% html_nodes("h1") %>% html_text()
  if(abelito.name=="")next
  abelito.info<-abelito %>% html_nodes("tr") %>% .[2] %>% html_nodes("td") %>% html_text() %>% t()
  colnames(abelito.info)<- abelito %>% html_nodes("tr") %>% .[1] %>% html_nodes("th") %>% html_text()
  abelito.recipe<-c()
  for(i in 4:length(abelito %>% html_nodes("tr"))){
    temp<-abelito %>% html_nodes("tr") %>% .[i] %>% html_nodes("td") %>% html_text()
    abelito.recipe<-rbind(abelito.recipe,temp)
  }
  colnames(abelito.recipe)<-abelito %>% html_nodes("tr") %>% .[3] %>% html_nodes("th") %>% html_text()
  abelito.text<-abelito %>% html_nodes("dd") %>% .[2] %>% html_text() %>%
    str_split("\n") %>% .[[1]] %>% .[4] %>% substr(1,nchar(.)-8)
  t<-list(abelito.name,abelito.info,abelito.recipe,abelito.text)
  recipes<-c(recipes,list(t))
  print(paste0(match(url,urls)," ",url))
  },error=function(e){})
}