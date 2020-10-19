###Build Graph for articles webpage

##FUNCTIONS ZONE

#helpers

##find links
find_links<-function(document){
    links<-str_extract_all(document,"\\[\\[.+?\\]\\]")
    for (link in 1:length(links)){
        links[[link]]<-stri_trans_totitle(substr(str_replace_all(links[[link]], "[^[:alnum:]]", " "),3,nchar(links[[link]])-2))
    }
    return(links)
}

#function to render obsidian md files for website

##this simply strips out the [[]], which is fine for basic usage
clean_up_obsidian_md<-function(raw_path_dir,name,dest_path_dir,dest_name){
    document<-read_file(paste0(raw_path_dir,name)) #"\\[\\[.+?\\]\\]"
    clean_l<-str_replace_all(document,"\\[\\[","")
    clean_lr<-str_replace_all(clean_l,"\\]\\]","")
    write_file(clean_lr,paste0(dest_path_dir,dest_name))
    return(clean_lr)
}

##this is a bit more intensive, and builds links for webpage
convert_space_to_underscore<-function(string){
    return(str_replace(string," ","_"))
}
refactor_links_obsidian2hugo<-function(raw_path_dir,name,dest_path_dir,dest_name,website_path){
    document<-read_file(paste0(raw_path_dir,name)) #"\\[\\[.+?\\]\\]"
    clean<-str_replace_all(document,"\\[\\[(.+?)\\]\\]",paste0("\\[\\1\\]","(",website_path,"\\1",".md)"))
    clean<-str_replace_all(clean, "\\([^()]+\\.md\\)", function(x) gsub(" ", "_", x, fixed=TRUE) ) #per stack overflow's Wiktor Stribizew https://stackoverflow.com/questions/64415118/passing-function-to-r-regex-based-tools#64415319
    write_file(clean,paste0(dest_path_dir,"/",dest_name))
    return(paste0(dest_path_dir,"/",dest_name))
}


#function to maintain_dfs
build_initial_links_nodes<-function(build_dir){
    links_df <- data.frame(
        source=c(),
        target=c(),
        value=c()
    )
    nodes_df <- data.frame(
        name=c(),
        group=c(),
        size=c()
    )
    fwrite(links_df,paste0(build_dir,"/","main_links.csv"))
    fwrite(nodes_df,paste0(build_dir,"/","main_nodes.csv"))
}

#helper, credit stack exchange
simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep="", collapse=" ")
}

update_links_nodes_clean<-function(document,document_title,links_df,nodes_df,group){
    size<-10
    links<-unique(find_links(document)[[1]])
    #if no links, it should be ok to just skip
    if (length(links)>0){
        #regularize both links and document_title
        document_title<-simpleCap(document_title)
        for (i in 1:length(links)){
            links[[i]]<-simpleCap(links[[i]])
        }
        if ((nrow(links_df)==0)|(nrow(nodes_df)==0)){
            #first, assign nodes
            list_to_bind<-vector(mode="list", length=(length(links)+1))
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
    }
    
    return(list(links_df,nodes_df))
}

##Aesthetics functions

#function to increase node size based on number of connections, maybe have very connected nodes show their names by default?
concord_node_size<-function(links_df,nodes_df){
    ##need to adjust for perfect parity
    
    size_params<-rep(0,length(unique(nodes_df$name)))
    for (u in 1:length(unique(nodes_df$name))){
        matches<-links_df$target==(u-1)
        size_params[[u]]<-length(matches[matches==TRUE])
    }
    #want smallest to have size 10, largest 30, make all others linear transform
    normalized_size_params<-rep(0,length(unique(nodes_df$name)))
    minp<-min(size_params)
    maxp<-max(size_params)
    if (minp==maxp){
        adjusted_node_size<-rep(7.5,nrow(nodes_df))
    }else{
        for (u in 1:length(size_params)){
            normalized_size_params[[u]]<-12*(size_params[[u]]-minp)/(maxp-minp)+3
        }
        adjusted_node_size<-rep(0,nrow(nodes_df))
        for (u in 1:length(nodes_df$name)){
            idx<-match(nodes_df[["name"]][[u]],unique(nodes_df$name))
            adjusted_node_size[[u]]<-normalized_size_params[[idx]]
        }
        #nodes_df[,"name":=adjusted_node_size]
    }
    
    nodes_df$size<-adjusted_node_size
    return(nodes_df)
}

##TEST
#build_dir<-"~/desktop/blog_posts/example_build_directory"
#links_df<-as.data.frame.matrix(fread(paste0(build_dir,"/","main_links.csv")))
#nodes_df<-as.data.frame.matrix(fread(paste0(build_dir,"/","main_nodes.csv")))
#concord_node_size(links_df,nodes_df)

#function to increase link girth based on importance of connectivity

##wrapper

#builds tree(s) based on recursive search. Files only need to be sent to /post/ once, so do so for
#the large build but then for folder-based, topic-based, and post-based, just build the dataframes

#build dir is where we put dataframes, so as not to clutter.
#can be same as root but recommend against
process_vault<-function(root_dir,target_root_dir,build_dir){
    #first, build main
    files<-list.files(root_dir,recursive=TRUE) #these will have spaces, which we should strip
    if (!dir.exists(target_root_dir)){
        dir.create(target_root_dir)
    }
    if (!dir.exists(build_dir)){
        dir.create(build_dir)
    }
    #port normalized files to target
    target_files<-list(type="vector",length=length(files))
    for (f in 1:length(files)){
        file<-files[[f]]
        target_files[[f]]<-refactor_links_obsidian2hugo(root_dir,paste0("/",file),target_root_dir,sub('.*\\/', '', str_replace_all(file, "\\/[^()]+\\.md", function(x) gsub(" ", "_", x, fixed=TRUE))),"post/")
    }
    
    #ok, this is the tricky part
    #let's start by just building the main graph
    #make sure to build csvs in build
    if (!file.exists(paste0(build_dir,"/","main_links.csv"))){
        build_initial_links_nodes(build_dir) 
    }
    
    
    #ok, now cycle through docs
    links_df<-as.data.frame.matrix(fread(paste0(build_dir,"/","main_links.csv")))
    nodes_df<-as.data.frame.matrix(fread(paste0(build_dir,"/","main_nodes.csv")))
    group<-1
    for (f in 1:length(target_files)){
        #for now, default group is 1
        document_title_md<-sub('.*\\/', '',files[[f]]) #use raw files not target files
        document_title<-sub('\\..*', '',document_title_md) #sub('\\..*', '', x)
        outputs<-update_links_nodes_clean(read_file(paste0(root_dir,"/",files[[f]])),document_title,links_df,nodes_df,group)
        links_df<-outputs[[1]]
        nodes_df<-outputs[[2]]
        
        #print(as.data.frame.matrix(fread(paste0(build_dir,"/","main_links.csv"))))
        #print(as.data.frame.matrix(fread(paste0(build_dir,"/","main_nodes.csv"))))
    }
    
    #normalize node size
    nodes_df<-concord_node_size(links_df,nodes_df)
    fwrite(links_df,paste0(build_dir,"/","main_links.csv"))
    fwrite(nodes_df,paste0(build_dir,"/","main_nodes.csv"))
    
    
    #now, build the html
    MyClickScript2<- 'window.location.href = "https://www.processingstochasticities.com/hidden/" + d.name + "/"'
    fp<-forceNetwork(Links = links_df, Nodes = nodes_df,
                     Source = "source", Target = "target",
                     Value = "value", NodeID = "name", Nodesize = "size",
                     Group = "group", opacity = 0.8, clickAction = MyClickScript2)
    
    
    saveWidget(fp, file=paste0(target_root_dir,"/","main.html"))
    
    #TODO: build all the subset graphs
    
    #first, this is probably easier (and more important). Build node graphs
    node_dir<-paste0(target_root_dir,"/nodes")
    if (!dir.exists(node_dir)){
        dir.create(node_dir)
    }
    for (n in 1:length(unique(nodes_df$name))){
        #find all source-target pairings involving n-1
        sub_links_df<-links_df[(links_df$source==(n-1))|(links_df$target==(n-1)),]
        #now, go through and find connected
        all_neighbors<-c(sub_links_df$source,sub_links_df$target)
        unique_node_names<-unique(nodes_df$name)
        #print("subset")
        #print((match(nodes_df$name,unique_node_names)-1) %in% all_neighbors)
        sub_nodes_df<-nodes_df[(match(nodes_df$name,unique_node_names)-1) %in% all_neighbors,]
        
        ##normalize links source and target
        normalized_source<-rep(-1,nrow(sub_links_df))
        for (si in 1:length(sub_links_df$source)){
            s<-(sub_links_df$source)[[si]]
            normalized_source[[si]]<-which((nodes_df$name)[[s+1]]==(sub_nodes_df$name))[[1]]-1
        }
        normalized_target<-rep(-1,nrow(sub_links_df))
        for (ti in 1:length(sub_links_df$target)){
            t<-(sub_links_df$target)[[ti]]
            normalized_target[[ti]]<-which((nodes_df$name)[[t+1]]==(sub_nodes_df$name))[[1]]-1
        }
        sub_links_df$source<-normalized_source
        sub_links_df$target<-normalized_target
        
        #ok, now normalize node sizes and build
        sub_nodes_df<-concord_node_size(sub_links_df,sub_nodes_df)
        fp<-forceNetwork(Links = sub_links_df, Nodes = sub_nodes_df,
                         Source = "source", Target = "target",
                         Value = "value", NodeID = "name", Nodesize = "size",
                         Group = "group", opacity = 0.8, clickAction = MyClickScript2)
        
        
        saveWidget(fp, file=paste0(node_dir,"/","node",n,".html"))
    }
    #post/files
    #graphs/main.html done
    #      /nodes/node1.html
    #            /node2.html
    #      /topics/topicA.html
}

process_vault("~/desktop/blog_posts/example_directory","~/desktop/blog_posts/example_target_directory","~/desktop/blog_posts/example_build_directory")

##PREP ZONE
library(data.table)
library(htmlwidgets)
library(igraph)
library(networkD3)
library(readr)
library(stringr)
library(stringi)
library(plyr)
library(readr)


nodes_df<-as.data.frame.matrix(fread("author_nodes.csv"))
links_df<-as.data.frame.matrix(fread("author_links.csv"))
links_df<-links_df[links_df$source<0,]
nodes_df<-nodes_df[nodes_df$source<0,]
MyClickScript2<- 'window.location.href = "https://www.processingstochasticities.com/hidden/" + d.name + "/"
'
##ADD ZONE

#wd has all files
wd<-"~/desktop/blog_posts/"
document<-read_file(paste0(wd,"mt.md"))
clean_up_obsidian_md(wd,"mt.md",wd,"mt_mutated.md")
refactor_links_obsidian2hugo(wd,"mt.md",wd,"mt_mutated.md","posts/")

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

