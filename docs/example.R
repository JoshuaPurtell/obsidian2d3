library(data.table)
library(htmlwidgets)
library(igraph)
library(networkD3)
library(readr)
library(stringr)
library(stringi)
library(plyr)
library(readr)

#source files
source("~/documents/github/obsidian2d3/src/hugo_build/process_md.R")
source("~/documents/github/obsidian2d3/src/hugo_build/processing_fxns.R")

##TODO: segregate reading and write-up vaults, using flags
MyClickScript1<- '
    var n = d.name; 
    var node = n.split(" ").join("_");
    top.location = "https://www.processingstochasticities.com/obsidian_port/reading/" + node + "/"
    '
MyClickScript2<- '
    var n = d.name; 
    var node = n.split(" ").join("_");
    top.location = "https://www.processingstochasticities.com/obsidian_port/write-ups/" + node + "/"
    '
MyClickScripts<-list(MyClickScript1,MyClickScript2)
website_path<-"https://www.processingstochasticites.com/obsidian_port"
#flag vec <-c(publish word, names for destinations)
flags<-list(safety="SAFE2PUBLISH",names=c("reading","write-ups"))
push_vault("~/documents/technical","/Users/AnR/Documents/GitHub/Prosto/content/obsidian_port","~/desktop/desktop/obs2hugo_builddirs/PS",MyClickScripts,flags,FALSE,website_path)


