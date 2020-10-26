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
source("~/Desktop/obsidian2d3/src/hugo_build/process_md.R")
MyClickScript2<- '
    var n = d.name; 
    var node = n.split(" ").join("_");
    top.location = "https://www.processingstochasticities.com/obsidian_port/" + node + "/"
    '

process_vault("~/desktop/blog_posts/example_directory","/Users/AnR/Documents/GitHub/casper3-hugo-starter/content/obsidian_port","~/desktop/blog_posts/example_build_directory",MyClickScript2)

