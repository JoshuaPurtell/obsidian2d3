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
MyClickScript2<- '
    var n = d.name; 
    var node = n.split(" ").join("_");
    top.location = "https://kind-curie-8e7995.netlify.app/obsidian_port/" + node + "/"
    '

#process_vault("~/documents/philosophy/podcasts","/Users/AnR/Documents/GitHub/TakNev/content/obsidian_port","~/desktop/desktop/obs2hugo_builddirs",MyClickScript2)
#"https://www.processingstochasticities.com/obsidian_port/"
website_path<-"https://kind-curie-8e7995.netlify.app/obsidian_port/"
push_vault("~/documents/philosophy","/Users/AnR/Documents/GitHub/TakNev/content/obsidian_port","~/desktop/desktop/obs2hugo_builddirs/TN",MyClickScript2,"SAFE2PUBLISH",TRUE,website_path)