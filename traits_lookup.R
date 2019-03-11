rm(list=ls())
gc()

library(tidyverse)
library(rvest)
library(stringr)
library(RSelenium)

## open the remote driver session
rD<-rsDriver(port=4444L,browser="chrome")
remDr <- rD$client

# create an empty matrix to store trait data in
trait.dat<-matrix(nrow=0, ncol=17)
colnames(trait.dat)<-c("Taxon", "Raunkiaer Life Mode", "Esler Life Mode", "Regeneration", 
                       "Shade Tolerance", "Max Mean Height (m)", "Mean Height (m)", 
                       "Mean Plant Lifespan (years)", "Number of Shoot", "Spines or Burrs", 
                       "Vegetative Organs", "Leaf", "Leaf Form", "Max Mean Leaf Length (mm)", 
                       "Leaf Width (mm)", "Mean Leaf Lifespan (months)", "Leaf Longevity")

## cross-ref NVS names against farm data species codes & get preferred sci name
nvs<-read.csv("fire/CurrentNVSNames.csv", header=T, stringsAsFactors = F)
dat<-read.csv("fire/burn_list.csv", header=T, stringsAsFactors = F)
dat2<-merge(dat, nvs, by="NVSCode", all.x = T)
rm(nvs, dat)
## this is the input data for the search function
taxon.list<-dat2
## create an empty list for storing failed searches
fail.list<-list()

## this function searches the ecotraits database for the species name as given in the "preferred name" column (13 in this dataset)
for (i in 1:nrow(taxon.list)) {
  remDr$navigate("https://ecotraits.landcareresearch.co.nz/SearchForm.aspx")
  
  taxon<-taxon.list[i,14]
  
  species<- remDr$findElement(using = 'name', "SearchText")
  species$sendKeysToElement(list(taxon))
  gobutton<-remDr$findElement(using = 'name', "ScientificSearchButton")
  gobutton$clickElement()
  
  test<-remDr$findElements(using = 'partial link text',  taxon)
  
  if (length(test)==0){
    print(paste("Species not found in trait database: ", taxon))
    fail.list<-append(taxon, fail.list)
  }else{
    result<-remDr$findElement(using = 'partial link text',  taxon)
    result$clickElement()
    
    morph<-remDr$findElement(using='id', "morphCell")
    morph$clickElement()
    
    url<-remDr$getCurrentUrl()
    
    page<-readLines(url[[1]])
    start.string<-"Raunkiaer Life Mode"
    finish.string<-"</table></P>"
    
    if (length(str_subset(page, start.string))>0){
      start.line<-grep(start.string,page)
      finish.line<-grep(finish.string, page)
      trait.table<-page[start.line:finish.line]%>%.[grep("span>", .)]%>%str_replace("<span>", "")%>%
        str_replace("</span>", "")%>%str_replace("<b>", "")%>%str_replace("</b>", "")%>%
        str_replace("<font style='COLOR: lightgrey'>", "")%>%str_replace("</font>", "")%>%
        str_trim()
      even_indexes<-seq(2,length(trait.table),2)
      odd_indexes<-seq(1,length(trait.table),2)
      trait<-trait.table[odd_indexes]
      value<-trait.table[even_indexes]
      
      dat<-data.frame(trait, value)%>% apply(2, function(x) gsub("^$|^ $", NA, x))%>%.[rowSums(is.na(.)) != ncol(.),]%>%
        t()%>%.[2,]%>%append(taxon,.)
      
      trait.dat<-rbind(trait.dat, dat)
      print(paste("Species trait data added to data frame:", taxon))
    }else{
      print (paste("No morphological trait data found for species:", taxon))
      fail.list<-append(taxon, fail.list)
      fail.list<-t(t(fail.list))
    }
  }
}

# write results data to file
write.csv(trait.dat, file="traits_raw.csv", row.names = F)
write.csv(fail.list, file="failed_searches.csv", row.names=F)
