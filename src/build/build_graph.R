###Build Graph for articles webpage

##FUNCTIONS ZONE

#function to maintain_dfs
update_links_nodes_clean<-function(document,document_title,links_df,nodes_df,group){
    size<-10
    links<-unique(find_links(document)[[1]])
    if ((nrow(links_df)==0)|(nrow(nodes_df)==0)){
        #first, assign nodes
        list_to_bind<-vector(mode="list", length=(length(links)+1))
        print(group)
        print(size)
        list_to_bind[[1]]<-data.frame("name"=document_title,"group"=group,"size"=size)
        for (l in 1:length(links)){
            list_to_bind[[l+1]]<-data.frame("name"=links[[l]],"group"=group,"size"=size)
        }
        nodes_df<-rbindlist(list_to_bind)
        colnames(nodes_df)<-c("name","group","size")
        #now, do links
        
        list_to_bind<-vector(mode="list", length=(length(links)+1))
        source<-match(document_title,nodes_df$name)-1
        value<-10#standard
        for (l in 1:length(links)){
            #testdf<-data.frame("source"=source,"target"=l,"value"=value)#setDT()
            #if (nrow(match_df(links_df,testdf))==0){
            #links_df<-rbind(links_df,list(source,target,value))
            list_to_bind[[l]]<-data.frame("source"=source,"target"=match(links[[l]],nodes_df$name)-1,"value"=value)
            #print("nrow linksdf")
            #print(nrow(links_df))
            #}
        }
        links_df<-rbindlist(list_to_bind)
        colnames(links_df)<-c("source","target","value")
    }else{
        #first, add nodes if necessary
        #group<-0#default
        size<-10#default
        list_to_bind<-list()
        list_to_bind[[1]]<-nodes_df
        if (!(document_title %in% nodes_df$name)){
            list_to_bind[[2]]<-data.frame("name"=document_title,"group"=group,"size"=size)
        }
        for (l in 1:length(links)){
            if (!(links[[l]] %in% nodes_df$name)){
                list_to_bind[[length(list_to_bind)+2]]<-data.frame("name"=links[[l]],"group"=group,"size"=size)
            }
        }
        nodes_df<-rbindlist(list_to_bind)
        
        
        #then, build links
        list_to_bind<-list()
        list_to_bind[[1]]<-links_df
        value<-10#default
        for (l in 1:length(links)){
            testdf<-data.frame("source"=match(document_title,nodes_df$name)-1,"target"=match(links[[l]],nodes_df$name)-1,"value"=value)#setDT()
            if (nrow(match_df(links_df,testdf))==0){
                list_to_bind[[l+1]]<-testdf
            }
        }
        links_df<-rbindlist(list_to_bind)
    }
    return(list(links_df,nodes_df))
}

#function to initialize md files

#function to increase node size based on number of connections

#function to increase link girth based on

##PREP ZONE
library(data.table)
library(htmlwidgets)
library(igraph)
library(networkD3)
library(readr)
library(stringr)
library(stringi)
library(plyr)
nodes_df<-as.data.frame.matrix(fread("author_nodes.csv"))
links_df<-as.data.frame.matrix(fread("author_links.csv"))
links_df<-links_df[links_df$source<0,]
nodes_df<-nodes_df[nodes_df$source<0,]
MyClickScript2<- 'window.location.href = "https://www.processingstochasticities.com/hidden/" + d.name + "/"
'
##ADD ZONE
document<-read_file(paste0(getwd(),"/mt.md"))
document_title<-"Measure Theory"
records<-update_links_nodes_clean(document,document_title,links_df,nodes_df,0)
links_df<-records[[1]]
nodes_df<-records[[2]]#[4:11]

document<-read_file(paste0(getwd(),"/definitions.md"))
document_title<-"Definitions"
records<-update_links_nodes_clean(document,document_title,links_df,nodes_df,1)
links_df<-records[[1]]
nodes_df<-records[[2]]#[4:11]
fwrite(nodes_df,"author_nodes.csv")
fwrite(links_df,"author_links.csv")

##BUILD ZONE
fp<-forceNetwork(Links = links_df, Nodes = nodes_df,
                 Source = "source", Target = "target",
                 Value = "value", NodeID = "name",
                 Group = "group", opacity = 0.8, clickAction = MyClickScript2)


saveWidget(fp, file="articles.html")

