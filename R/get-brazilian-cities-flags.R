library(tidyverse)
library(httr)
library(rvest)
library(data.table)

getStatesLinks <- function(){
  url <- "https://www.mbi.com.br/mbi/biblioteca/simbolopedia/"
  
  webpage <- with_config(config = config(ssl_verifypeer = 0L), GET(url, add_headers("user-agent"="APIs-Google (+https://developers.google.com/webmasters/APIs-Google.html)")))
  
  
  states_links <- webpage |> 
    read_html() |> 
    html_nodes(xpath = '//*[@id="muuri-grid"]') |>
    html_nodes('a') |> 
    html_attr('href') |> 
    unique()
  
  states_links <- paste0("https://www.mbi.com.br/",states_links)
  
  return(states_links)
}

getCitiesLinks <- function(state_url){
  
  webpage <- with_config(config = config(ssl_verifypeer = 0L), GET(state_url, add_headers("user-agent"="APIs-Google (+https://developers.google.com/webmasters/APIs-Google.html)")))
  
  cities_links <- webpage |> 
    read_html() |> 
    html_nodes(xpath = '//*[@id="muuri-grid"]') |>
    html_nodes('img') |> 
    html_attr('src')
  
  cities_links <- paste0("https://www.mbi.com.br/",cities_links)
  state <- str_remove_all(state_url,"https://www.mbi.com.br//mbi/biblioteca/simbolopedia/municipios-|-br/|estado-")
  city <- str_remove_all(cities_links,".+(?=/)|(?<=-bandeira).+|/|-bandeira|municipio-")
  file <- paste0("data/flags/",str_remove_all(cities_links,"https://www.mbi.com.br//mbi/files/media/image/simbolopedia/|\\s"))
  
  df_cities <- tibble(city,state,cities_links,file)
  
  return(df_cities)
}

# Testing
states_links <- getStatesLinks()
cities_links <- getCitiesLinks(states_links[2])


# Getting all states
states_links <- getStatesLinks()


getCityFlag <- function(city_img_link, file_name){
  download.file(city_img_link,file_name,mode = "wb")
}

getCityFlag(df_cities$cities_links[1],df_cities$file[1])

# Getting all cities foreach state
df_cities <- pmap_dfr(.l = list(state_url = states_links), .f = getCitiesLinks)

# Getting the flags from all cities
pwalk(.l = list(city_img_link = df_cities$cities_links, file_name = df_cities$file), .f = getCityFlag)


fwrite(df_cities,"data/df_cities.csv")
