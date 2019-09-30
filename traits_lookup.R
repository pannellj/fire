# do Ctrl shift f10 to restart R
rm(list=ls())
gc()

library(tidyverse)
library(rvest)
library(stringr)
library(RSelenium)

## read in species list, cross-ref NVS names against farm data species codes & get preferred sci name
dat<-read.table("../data/all_sp_list.txt", header=T, sep="\t", stringsAsFactors = F)

# create an empty matrix to store trait data in
trait.dat<-matrix(nrow=0, ncol=17)
colnames(trait.dat)<-c("SpeciesName", "Raunkiaer Life Mode", "Esler Life Mode", "Regeneration", 
                       "Shade Tolerance", "Max Mean Height (m)", "Mean Height (m)", 
                       "Mean Plant Lifespan (years)", "Number of Shoot", "Spines or Burrs", 
                       "Vegetative Organs", "Leaf", "Leaf Form", "Max Mean Leaf Length (mm)", 
                       "Leaf Width (mm)", "Mean Leaf Lifespan (months)", "Leaf Longevity")

## this is the input data for the search function
taxon.list<-subset(dat, SpeciesName!="NA")
## create an empty list for storing failed searches
fail.list<-list()

## open the remote driver session
rsDr<-rsDriver(port=4444L, browser="firefox")
remDr <- rsDr$client

## this function searches the ecotraits database for the species name as given in the "preferred name" column (13 in this dataset)
for (i in 1:nrow(taxon.list)) {
  remDr$navigate("https://ecotraits.landcareresearch.co.nz/SearchForm.aspx")
  
  taxon<-taxon.list[i,1]
  
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
      trait<-trait.table[odd_indexes]%>%append("Taxon",.)
      value<-trait.table[even_indexes]%>%append(taxon,.)
      
      dat<-data.frame(trait, value)%>% apply(2, function(x) gsub("^$|^ $", NA, x))%>%.[rowSums(is.na(.)) != ncol(.),]
      trait<-dat[,1]
      dat<-dat%>%.[,2]%>%t()%>%as.data.frame()
      colnames(dat)<-trait
      dat<-dat[,(is.na(names(dat))==FALSE)]
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
write.csv(trait.dat, file="../data/traits_raw.csv", row.names = F)
write.csv(fail.list, file="../data/failed_searches.csv", row.names=F)
