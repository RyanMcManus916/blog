
library(WikipediR)
library(rvest)
library(tidyverse)
library(imager)
library(tidytuesdayR)


#tuesdata <- tidytuesdayR::tt_load(2024, week = 41)
#most_visited_nps_species_data <- tuesdata$most_visited_nps_species_data
#rm(tuesdata)

most_visited_nps_species_data<-read.csv('most_visited_nps_species_data.csv')

wikipedia_search<-function(search_word){
    # Get Wikipedia Meta Data
    meta<-WikipediR::page_info("en","wikipedia",page=search_word,clean_response=T)
    url<-meta[[1]]$fullurl
    wikipage<-rvest::session(url)
    
    # Get Image URL
    imginfo<-wikipage|>rvest::html_nodes("tr:nth-child(2) img")
    img_url<-imginfo[1]|>rvest::html_attr("src")
    img_url<-paste0('https:',img_url)
    img<-imager::load.image(img_url) #plot(axes=FALSE)
    
    # Get Intro paragraph
    wikitext<-readBin(wikipage$response$content, what = "text")
    body_text<-rvest::read_html(wikitext)|>rvest::html_elements('p')|>rvest::html_text(trim = T)
    page<-rvest::read_html(wikitext)
    nodes<- rvest::html_nodes(page,xpath='//*')
    
    h1_index<- which(rvest::html_name(nodes)=='h1')
    h2_index<- which(rvest::html_name(nodes)=='h2')[2]
    
    paragraph_contents<-''
    
    if(length(h1_index)>0 && length(h2_index)>0) {
        paragraphs_between<- nodes[(h1_index+1):(h2_index-1)]
        paragraphs<- paragraphs_between[rvest::html_name(paragraphs_between)=='p']
        paragraphs_text<- rvest::html_text(paragraphs) #[-c(1:2)])
        paragraphs_text<-gsub('\n','',paragraphs_text)
        paragraph_contents<-paste(paragraphs_text,collapse = '\n\n')
    }
    return(list('text'=paragraph_contents,'image'=img))
    
}