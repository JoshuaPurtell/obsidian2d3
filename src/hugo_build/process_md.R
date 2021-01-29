## ==================== ##
## PROCESS_MD.R         ##
## ==================== ##

## ==================== ##
## These functions are  ##
## designed to prepare  ##
## 
## ==================== ##

###Build Graph for articles webpage


## Identify links in obsidian doc
find_links<-function(document){
    links<-str_extract_all(document,"\\[\\[.+?\\]\\]")
    for (link in 1:length(links)){
        links[[link]]<-stri_trans_totitle(substr(str_replace_all(links[[link]], "[^[:alnum:]]", " "),3,nchar(links[[link]])-2))
    }
    return(links)
}


##TODO: CLEAN THE "SIMPLE" FXNS AND HELPERS
convert_space_to_underscore<-function(string){
    return(str_replace_all(string," ","_"))
}
convert_underscore_to_space<-function(string){
    return(str_replace_all(string,"_"," ")) 
}
simpleCap <- function(x) {
    x<-str_replace_all(x,"'","")
    s <- strsplit(x, " ")[[1]]
    output<-paste(toupper(substring(s, 1,1)), substring(s, 2),
                  sep="", collapse=" ")
    return(output)
}
simpleCapUnder <- function(x) {
    x<-str_replace_all(x,"'","")
    s <- strsplit(x, "_")[[1]]
    output<-paste(toupper(substring(s, 1,1)), substring(s, 2),
                  sep="", collapse="_")
    return(output)
}
simpleCapUnderLink <- function(x,website_path) {
    x<-sub('.*\\/', '',x)#x<-sub('.*port\\/', '',x)
    x<-substr(x,1,nchar(x)-1)
    x<-str_replace_all(x,"'","")
    s <- strsplit(x, "_")[[1]]
    output<-paste(toupper(substring(s, 1,1)), substring(s, 2),
                  sep="", collapse="_")
    return(paste0(website_path,"/",substring(output,1,nchar(output)-3),"/)"))
}

## WRITE VERSION OF OBSIDIAN MD WITH LINKS BUILT FOR WEBSITE
refactor_links_obsidian2hugo<-function(raw_path_dir,name,dest_path_dir,dest_name,website_path){
    document<-read_file(paste0(raw_path_dir,name)) #"\\[\\[.+?\\]\\]"
    #print("document")
    #print(document)
    clean<-str_replace_all(document,"\\[\\[(.+?)\\]\\]",paste0("\\[\\1\\]","(",website_path,"/","\\1",".md)"))
    #print("link")
    #print(clean)
    clean<-str_replace_all(clean, "\\([^()]+\\.md\\)", function(x) gsub(" ", "_", x, fixed=TRUE) ) #per stack overflow's Wiktor Stribizew https://stackoverflow.com/questions/64415118/passing-function-to-r-regex-based-tools#64415319
    #print("link1")
    #print(clean)
    clean<-str_replace_all(clean, "\\([^()]+\\.md\\)", function(x) gsub("'", "", x, fixed=TRUE) )
    #print("link1.5")
    #print(clean)
    clean<-str_replace_all(clean, "https[^()]+\\.md\\)", function(x) simpleCapUnderLink(x,website_path) )
    #add frontmatter and graph
    clean<-stylize_document(clean,convert_underscore_to_space(str_sub(dest_name,1,nchar(dest_name)-3)))
    #print("link2")
    #print(clean)
    #print(paste0(dest_path_dir,"/",dest_name))
    write_file(clean,paste0(dest_path_dir,"/",dest_name))
    return(paste0(dest_path_dir,"/",dest_name))
}


## INITIALIZE DFS IF NONE EXIST (FIRST BUILD)
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

##TODO: Clean Up so that re-running does not add duplicate link rows
update_links_nodes_clean<-function(document,document_title,links_df,nodes_df,group){
    size<-10
    links<-unique(find_links(document)[[1]])
    #if no links, right now it just skips the document.
    #TODO: allow for sensible graph builds for vaults with "singleton" nodes
    
    if (length(links)>0){
        #regularize both links and document_title
        document_title<-simpleCap(document_title)
        for (i in 1:length(links)){
            links[[i]]<-simpleCap(links[[i]])
        }
        if ((nrow(links_df)==0)|(nrow(nodes_df)==0)){
            
            #first, push nodes to df
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
                list_to_bind[[l]]<-data.frame("source"=source,"target"=match(links[[l]],nodes_df$name)-1,"value"=value)
            }
            links_df<-rbindlist(list_to_bind)
            colnames(links_df)<-c("source","target","value")
        }else{
            #first, push new nodes
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
            
            
            #then, push links
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
    
    #right now makes highly connected nodes larger, but only slightly so
    size_params<-rep(0,length(unique(nodes_df$name)))
    for (u in 1:length(unique(nodes_df$name))){
        matches<-links_df$source==(u-1) #used to use target, which makes less sense
        size_params[[u]]<-length(matches[matches==TRUE])
    }
    #want smallest to have size 7.5, largest 15, make all others linear transform
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
    }
    
    nodes_df$size<-adjusted_node_size
    return(nodes_df)
}

#function to build frontmatter and add graph at the end
add_frontmatter<-function(document,document_title){
    update_document<-paste0('+++\n author = \"Anonymous\"\n title = \"',document_title,'\"\n date = ',Sys.Date(),'\n+++\n\n',document)
    return(update_document)
}
#date = "2020-08-01"
add_graph<-function(document,document_title){
    #print(convert_space_to_underscore(document_title))
    update_document<-paste0(document,'\n \n <iframe seamless src="/obsidian_port/nodes/',convert_space_to_underscore(document_title),'.html" style="width:700px; height:400px; border: 2px solid black"></iframe>')
}

stylize_document<-function(document,document_title){
    fm_added<-add_frontmatter(document,document_title)
    graph_added<-add_graph(fm_added,document_title)
    return(graph_added)
}

#TODO: add function to increase link girth based on importance of connectivity

#TODO: allow for more levels of construction. Right now, the code supports just a top-level build and atomic page
#builds. The obvious next step is to allow for intermediate folder builds. For example, if I have one folder in my
#graph for an abstract algebra textbook, and another for an algebraic topology textbook, it would be good to have a top-level
#graph that includes all nodes, then two smaller subject graphs (one for each textbook), and then atomic graphs.
#the natural question is, how would one handle these intermediate graphs? Perhaps they'd be build sort of like top-level
#and just be hosted on their own pages? I think an interesting approach to explore would be to look at 
#having a "level jump" capability, where in a given graph one would be able to click on a node and select the level
#of context they'd like to see it in. I'm not sure how to implement this in the static context, however.


#build dir is where we put dataframes, so as not to clutter.
#can be same as root but recommend against. Should not be in hugo build directory for security reasons

process_vault<-function(root_dir,target_root_dir,build_dir,click_action_script){
    #some basic checks
    if (!dir.exists(root_dir)) stop('Root Dir Not Found')
    
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
        target_files[[f]]<-refactor_links_obsidian2hugo(root_dir,paste0("/",file),target_root_dir,simpleCapUnder(sub('.*\\/', '', str_replace_all(file, "\\/[^()]+\\.md", function(x) gsub(" ", "_", x, fixed=TRUE)))),"https://www.processingstochasticities.com/obsidian_port/")#used to change to _
    }
    
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
        document_title<-simpleCap(sub('\\..*', '',document_title_md)) #sub('\\..*', '', x)
        outputs<-update_links_nodes_clean(read_file(paste0(root_dir,"/",files[[f]])),document_title,links_df,nodes_df,group)
        links_df<-outputs[[1]]
        nodes_df<-outputs[[2]]
    }
    
    #normalize node size
    nodes_df<-concord_node_size(links_df,nodes_df)
    fwrite(links_df,paste0(build_dir,"/","main_links.csv"))
    fwrite(nodes_df,paste0(build_dir,"/","main_nodes.csv"))
    
    
    #now, build the html
    fp<-forceNetwork(Links = links_df, Nodes = nodes_df,
                     Source = "source", Target = "target",
                     Value = "value", NodeID = "name", Nodesize = "size",
                     Group = "group", opacity = 0.8, clickAction = click_action_script)
    
    
    saveWidget(fp, file=paste0(target_root_dir,"/","main_graph.html"))
    
    #Build node graphs
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
                         Group = "group", opacity = 0.8, clickAction = click_action_script)
        
        saveWidget(fp, file=paste0(node_dir,"/",convert_space_to_underscore(simpleCap(unique(nodes_df$name)[[n]])),".html"))
    }
    #post/files
    #graphs/main.html done
    #      /nodes/node1.html
    #            /node2.html
    #      /topics/topicA.html
}

##TODO: build function that takes list of root_dirs and includes only md files with a special tag in them.
#all other pages which are linked to will be represented as empty

process_notes<-function(root_dir_list,target_dir,build_dir,tag,click_action_script){
    files<-c()
    for (l in 1:length(root_dir_list)){
        root_dir<-root_dir_list[[l]]
        if (!dir.exists(root_dir)) stop('Root Dir In List Not Found')
        
        
        #first, build main
        files_in_root<-list.files(root_dir,recursive=TRUE) #these will have spaces, which we should strip
        ##HOW PROCESS_NOTES DIFFERS FROM PROCESS_VAULT
        #search files
        for (f in 1:length(files_in_root)){
            document<-read_file(paste0(root_dir,"/",files_in_root[[f]]))
            if (str_detect(document,paste0("\\[\\[",tag,"\\]\\]"))){
                files<-c(files,paste0(root_dir,"/",files_in_root[[f]]))
            }
        }
    }
    if (!dir.exists(target_dir)){
        dir.create(target_dir)
    }
    if (!dir.exists(build_dir)){
        dir.create(build_dir)
    }
    
    
    
    #port normalized files to target
    target_files<-list(type="vector",length=length(files))
    for (f in 1:length(files)){
        file<-files[[f]]
        target_files[[f]]<-refactor_links_obsidian2hugo(root_dir,paste0("/",file),target_dir,simpleCapUnder(sub('.*\\/', '', str_replace_all(file, "\\/[^()]+\\.md", function(x) gsub(" ", "_", x, fixed=TRUE)))),"https://www.processingstochasticities.com/obsidian_port/")#used to change to _
    }
    
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
        document_title<-simpleCap(sub('\\..*', '',document_title_md)) #sub('\\..*', '', x)
        outputs<-update_links_nodes_clean(read_file(paste0(root_dir,"/",files[[f]])),document_title,links_df,nodes_df,group)
        links_df<-outputs[[1]]
        nodes_df<-outputs[[2]]
    }
    
    #normalize node size
    nodes_df<-concord_node_size(links_df,nodes_df)
    fwrite(links_df,paste0(build_dir,"/","main_links.csv"))
    fwrite(nodes_df,paste0(build_dir,"/","main_nodes.csv"))
    
    
    #now, build the html
    fp<-forceNetwork(Links = links_df, Nodes = nodes_df,
                     Source = "source", Target = "target",
                     Value = "value", NodeID = "name", Nodesize = "size",
                     Group = "group", opacity = 0.8, clickAction = click_action_script)
    
    
    saveWidget(fp, file=paste0(target_dir,"/","main_graph.html"))
    
    #Build node graphs
    node_dir<-paste0(target_dir,"/nodes")
    if (!dir.exists(node_dir)){
        dir.create(node_dir)
    }
    for (n in 1:length(unique(nodes_df$name))){
        #find all source-target pairings involving n-1
        sub_links_df<-links_df[(links_df$source==(n-1))|(links_df$target==(n-1)),]
        #now, go through and find connected
        all_neighbors<-c(sub_links_df$source,sub_links_df$target)
        unique_node_names<-unique(nodes_df$name)
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
                         Group = "group", opacity = 0.8, clickAction = click_action_script)
        
        saveWidget(fp, file=paste0(node_dir,"/",convert_space_to_underscore(simpleCap(unique(nodes_df$name)[[n]])),".html"))
    }
}




##TODO: build workflow so that users can choose to leave out empty nodes and represent connections involved with 
## "bypasses". For example, let's say we have nodes A,B,C connected such that A-B-C. But, B is empty — its existence
#in Obsidian owes solely to the fact that it was linked to. Then, perhaps we could allow the user to represent
#this link in the graph with an edge A-C?

##TODO: create tool which scrapes pdf scientific paper for references and then builds in "reference" links and nodes which can or can't
## be represented in the main graph

##TODO: build workflow which identifies which empty nodes are most deserving of being addressed (this makes sense in the context of articles),
## but could also work in other contexts where ideas pop-up frequently. Then, this list could be served as a webpage widget

##TODO: Add capability to color nodes by folder id in vault

##TODO: Write intelligent tag builders
