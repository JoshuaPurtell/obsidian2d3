library(igraph)
library(networkD3)
#author_edges <- data.frame(
    #paper_ref=c("On Spectral Clustering: Analysis and an algorithm"),
    #paper_source=c("Spectral partitioning works: Planar graphs and finite element meshes")
#)
library(data.table)
author_edges<-as.data.frame.matrix(fread("author_edges.csv"))
##Add new authors
author_edges[(nrow(author_edges)+1),"paper_ref"]<-"[link1](https://shiny.rstudio.com/)"
author_edges[(nrow(author_edges)),"paper_source"]<-"[link2](https://shiny.rstudio.com/)"

##Save authors

p <- simpleNetwork(author_edges, height="900px", width="200px",        
                   Source = 1,                 # column number of source
                   Target = 2,                 # column number of target
                   linkDistance = 10,          # distance between node. Increase this value to have more space between nodes
                   charge = -900,                # numeric value indicating either the strength of the node repulsion (negative value) or attraction (positive value)
                   fontSize = 14,               # size of the node names
                   fontFamily = "serif",       # font og node names
                   linkColour = "#666",        # colour of edges, MUST be a common colour for the whole graph
                   nodeColour = "#69b3a2",     # colour of nodes, MUST be a common colour for the whole graph
                   opacity = 0.9,              # opacity of nodes. 0=transparent. 1=no transparency
                   zoom = T                    # Can you zoom on the figure?
)
# with a simple click action - make the circles bigger when clicked
MyClickScript <- 
    '      d3.select(this).select("circle").transition()
.duration(750)
.attr("r", 30)
'
MyClickScript2<- 'window.location.href = "http://example.com/new_url/" + d.name + "/"
'

data(MisLinks)
data(MisNodes)

links_df <- data.frame(
    source=c(0,1),
    target=c(2,2),
    value=c(1,18)
)
nodes_df <- data.frame(
    name=c("A","B","C"),
    group=c(0,1,1),
    size=c(10,18,20)
)
fwrite(nodes_df,"author_nodes.csv")
fwrite(links_df,"author_links.csv")
nodes_df<-as.data.frame.matrix(fread("author_nodes.csv"))
links_df<-as.data.frame.matrix(fread("author_links.csv"))




fp<-forceNetwork(Links = links_df, Nodes = nodes_df,
                 Source = "source", Target = "target",
                 Value = "value", NodeID = "name",
                 Group = "group", opacity = 0.8, clickAction = MyClickScript2)
library(htmlwidgets)


saveWidget(fp, file="forcetest2.html")
fwrite(author_edges,"author_edges.csv")


#goal is to parse mk files and add to nodes and links dfs
print(getwd())
#install.packages("readr") # you only need to do this one time on your system
library(readr)
document<-read_file(paste0(getwd(),"/mt.md"))

library(stringr)
library(stringi)
links<-str_extract_all(document,"\\[\\[.+?\\]\\]")
print(links)
for (link in 1:length(links)){
    links[[link]]<-stri_trans_totitle(substr(str_replace_all(links[[link]], "[^[:alnum:]]", " "),3,nchar(links[[link]])-2))
}
print(links)
#print(strsplit(links,"\\s+"))
print(grepl("[[Sigma Field]]", document, fixed = TRUE))




###Task, write R script that goes through document and finds all links
find_links<-function(document){
    links<-str_extract_all(document,"\\[\\[.+?\\]\\]")
    for (link in 1:length(links)){
        links[[link]]<-stri_trans_totitle(substr(str_replace_all(links[[link]], "[^[:alnum:]]", " "),3,nchar(links[[link]])-2))
    }
    return(links)
}
##Task, write R script that adds links and nodes according to document
update_dict<-function(title,name_dictionary){
    if (!(document_title %in% names(name_dictionary))){
        name_dictionary[[length(name_dictionary)+1]]<-length(name_dictionary)
        names(name_dictionary)<-c(names(name_dictionary),document_title)
    }
    return(name_dictionary)
}
update_links_nodes<-function(document,document_title,links_df,nodes_df){
    links<-find_links(document)[[1]]
    names<-nodes_df$name
    #name_dictionary<-update_dict(document_title,name_dictionary)
    #nodes?
    if (document_title %in% names){
        source<-match(document_title,names)
    }else{
        source<-max(links_df$source)#length(names) #fix this
    }
    
    for (l in 1:length(links)){
        k<-1
        #links
        #name_dictionary<-update_dict(links[[l]],name_dictionary)
        if (document_title %in% names){
            target<-match(links[[l]],names)
        }else{
            target<-length(names)+k
            k=k+1
        }
        value<-3#default
        links_df<-rbind(links_df,list(source,target,value))
        
        #nodes
        if (!(links[[l]] %in% nodes_df$name)){
            name<-links[[l]]
            group<-0#default
            size<-10#default
            print(list(name,group,size))
            nodes_df<-rbind(nodes_df,list(name,group,size)) #this can be optimized
        }
    }
    
    #links
    #source=c(0,1),
    #target=c(2,2),
    #value=c(1,18)
    
    #nodes
    #name=c("A","B","C"),
    #group=c(0,1,1),
    #size=c(10,18,20)
    records<-list(links_df,nodes_df)
    return(records)
}

library(data.table)
#test
nodes_df<-as.data.frame.matrix(fread("author_nodes.csv"))
links_df<-as.data.frame.matrix(fread("author_links.csv"))
name_dictionary<-vector(mode="list", length=3)
names(name_dictionary)<-c("A","B","C")
for (i in 1:3){
    name_dictionary[[i]]<-i-1
}
document_title<-"Measure Theory"
records<-update_links_nodes(document,document_title,links_df,nodes_df)
links_df<-records[[1]]
nodes_df<-records[[2]]


