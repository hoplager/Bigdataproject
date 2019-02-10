#Project main script, Big Data for Social Analysis, Horn Péter
#help: https://stackoverflow.com/questions/38119447/using-r-for-webscraping-http-error-503-despite-using-long-pauses-in-program

setwd("C:/Users/T440s/OneDrive - Central European University/CEU/ECON_Pol 2. semester/Big_data/project")

#used libraries
install.packages('httr')
install.packages('RColorBrewer')
install.packages('dplyr')
install.packages('rvest')
install.packages('ggmap')
install.packages('leaflet')
install.packages('stringr')
install.packages('xml2')
install.packages('purrr')

library(httr)
library(RColorBrewer)
library(dplyr)
library(rvest)
library(ggmap)
library(leaflet)
library(stringr)
library(xml2)
library(httr)
library(purrr)


#getting URL ready (politico.com search: "economy"), scareped: 2019:02.04.

info <- GET('https://www.politico.com/search/1?q=economy&adv=true&c=0000014b-324d-d4f3-a3cb-f3ff415e0035')
print(info)


# ---------------------------------------------Creating article title dataset ----------------------------------------------------------------------------------------
# data collected at: 2019.02....
## problem with potentialy changing source data durring scraping (politico has a prevention against DOS attacks so big lags were needed)
datalist = list()
for (i in 1:1105){
  #print the link you are collecting
  print(paste("https://www.politico.com/search/",toString(1*i),toString("?q=economy&adv=true&c=0000014b-324d-d4f3-a3cb-f3ff415e0035"),sep="")) #we know where the loop is
  #get the link
  url <-read_html(paste("https://www.politico.com/search/",toString(1*i),toString("?q=economy&adv=true&c=0000014b-324d-d4f3-a3cb-f3ff415e0035"),sep="")) 
  
  title <- url %>%
    html_nodes(".summary a") %>%
    html_text()
    table(title)
  
  pol_url <- url %>%
    html_nodes(".summary a") %>%
    html_attr("href")
    table(pol_url)
    Sys.sleep(59)
  
  a <- data.frame(title,pol_url)
  datalist[[i]] <- a

}

datalist2 = list()
for (i in 842:1105){
  #print the link you are collecting
  print(paste("https://www.politico.com/search/",toString(1*i),toString("?q=economy&adv=true&c=0000014b-324d-d4f3-a3cb-f3ff415e0035"),sep="")) #we know where the loop is
  #get the link
  url <-read_html(paste("https://www.politico.com/search/",toString(1*i),toString("?q=economy&adv=true&c=0000014b-324d-d4f3-a3cb-f3ff415e0035"),sep="")) 
  
  title <- url %>%
    html_nodes(".summary a") %>%
    html_text()
  table(title)
  
  pol_url <- url %>%
    html_nodes(".summary a") %>%
    html_attr("href")
  table(pol_url)
  Sys.sleep(35)
  
  a <- data.frame(title,pol_url)
  datalist2[[i]] <- a
  
}

print(datalist2)

datalists=append(datalist, datalist2)

#creating a dataframe for titles and urls
data_title= do.call(rbind, datalists)

write.csv(data_title, file = "data_title.csv")

#removing articles, where subscription would be needed (could be bias)
data_title_nosub <- subset(data_title, grepl('https://www.politico.com/', data_title$pol_url))

write.csv(data_title_nosub, file = "data_title_nosub.csv")

#----------------------------------------------Creating article text dataset----------------------------------------------------------------------
data_title_nosub = read.csv("data_title_nosub.csv", header = TRUE)
data_title_nosub <- subset(data_title_nosub, select = c(title, pol_url))
#----------------------------------------creating a list for urls
urls_list <- list()
for (i in data_title_nosub[3]){
  urls_list <- i
}

#Have to get subsamples because of error 503 and additional bad links (error 401)
#--------------------------------------text sub-data 1
print(urls_list[2405])
urls_list <- urls_list[-2405]
#loop over urls collected above

data_main <- data.frame()
for (i in urls_list){

  link <- i
  print(link)
  url <- read_html(link)
  #print(url)
  
  text <- url %>% 
    html_nodes(".story-text") %>%
    html_text()
  #print(text)
  Sys.sleep(1)

  text <- gsub('<p class="story-text">\r\n                    ', "", text)
  
  z <- data.frame(link,text)
  data_main <- rbind(data_main,z)
  
}


#--------------------------------------text sub-data 2

data_text = read.csv("data_text.csv", header = TRUE) 
data_text <- subset(data_text, select = c(link, text))

print(urls_list[19525])

data_main2 <- data.frame()
for (i in urls_list[19525:19527]){ 
  
  link <- i
  print(link)
  url <- read_html(link)
  #print(url)
  
  text <- url %>% 
    html_nodes(".story-text") %>%
    html_text()
  #print(text)
  Sys.sleep(sample(1:5,1)*0.356)
  
  text <- gsub('<p class="story-text">\r\n                    ', "", text)
  
  z <- data.frame(link,text)
  data_main2 <- rbind(data_main2,z)
  
}
 

data_text = rbind(data_text, data_main3)

print(urls_list[2405])

write.csv(data_text, file = "data_text.csv")

#--------------------------------merging sub-text data sets

#data_text_final = rbind(data_main,data_main2)

write.csv(data_text, file = "data_text_final.csv")

#--------------------------------merging title data with text data
total = merge(data_title_nosub, data_text, by.x ="pol_url", by.y = "link")
total <- unique(total)
write.csv(total, file = "project-data_total.csv")


#-------------------------------trying to clean text
total = read.csv("project-data_total.csv", header = TRUE)
total <- subset(total, select = c(pol_url,title,text))

total$text = gsub('\\s+',' ',total$text)
total$text = gsub('.cms-textAlign-center', '', total$text)
total$text = gsub('.cms-textAlign-left', '', total$text)
total$text = gsub('.cms-textAlign-right', '', total$text)
print(total$text[3])
write.csv(total, file = "project-data_total.csv")

total = read.csv("project-data_total.csv", header = TRUE)
total <- subset(total, select = c(pol_url, title, text))

                  