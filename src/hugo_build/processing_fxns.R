## ========================= ##
## PROCESSING_FXNS.R         ##
## ========================= ##

## ========================= ##
## These functions are       ##
## designed to transfer      ##
## obsidian pages to hugo    ##
## repo                      ##
## ========================= ##

###FXNS
## TODO:
## Write a function which eliminates duplicate rows in historical csvs
clean_dups<-function(dt,var){
    ocols<-colnames(dt)[!(colnames(dt) %in% c(var))] #non-variable cols
    dt<-dt[!(duplicated(dt[,..ocols]) ), ]
    return(dt)
}

### TODO:
## Build a safer version of process_vault
## DONE
comb_files<-function(files,root_dir,publish_word,negate){
    if (publish_word=="NA"){return(files)}else{
        safe_files<-list()
        for (i in 1:length(files)){
            doc<-read_file(paste0(root_dir,"/",files[[i]]))
            condition<-str_detect(doc,publish_word,negate=negate)
            #print(condition)
            if (condition){
                safe_files[[length(safe_files)+1]]<-files[[i]]
            }
        }
    }
    #print(safe_files)
    return(safe_files)
}
### TODO
## Incorporate GUI — will do in future

### TODO
## Write fxn that finds empty pages and initializes them 
## DONE
initialize_implied_pages<-function(node_dt,dest_root_dir){
    names<-node_dt[["name"]]
    existing_files<-list.files(dest_root_dir,recursive=TRUE)
    for (n in 1:length(names)){
        #create hypothetical name
        h_name<-paste0(gsub(" ","_",names[[n]]),".md")
        if (h_name %in% existing_files){
            next
        }else{
            document<-""
            document<-stylize_document(document,convert_underscore_to_space(str_sub(h_name,1,nchar(h_name)-3)))
            write_file(document,paste0(dest_root_dir,"/",h_name))

        }
    }
}

## TODO Concord link length
#not width ...
## DONE
concord_link_size<-function(sub_links_df,sub_nodes_df){
    #eventually we might want to make the "style" something
    #the user can input as an argument, but for now let's keep
    #it simple.

    #for now, just make link sizes which are equal differ according to gaussian w SD=mean/4
    u_link_lengths<-unique(sub_links_df$value)
    #find population of each
    u_link_length_pop<-rep(0,length(u_link_lengths))
    for (u in 1:length(u_link_length_pop)){
        u_link_length_pop[[u]]<-length(u_link_lengths[u_link_lengths==u_link_lengths[[u]]])
    }
    new_vals<-rep(0,nrow(sub_links_df))
    for (r in 1:nrow(sub_links_df)){
        u_idx<-match((sub_links_df$value)[[r]],u_link_lengths)
        new_vals[[r]]<-max((sub_links_df$value)[[r]]/4,rnorm(1,mean=(sub_links_df$value)[[r]],sd=u_link_length_pop[[u_idx]]*(sub_links_df$value)[[r]]/40))
    }
    sub_links_df[,("value"):=new_vals]
    return(sub_links_df)
}
## TODO: Add basic flags
push_vault<-function(root_dir,target_root_dir_gen,build_dir_gen,click_action_scripts,flags,negate,website_path_gen){
    
    
    publish_word<-flags[["safety"]]
    #names
    names<-flags[["names"]]
    #now go through names and append names to segregate
    if (!dir.exists(target_root_dir_gen)){
        dir.create(target_root_dir_gen)
    }
    if (!dir.exists(build_dir_gen)){
        dir.create(build_dir_gen)
    }
    for (en in 1:length(names)){
        name<-names[[en]]
        click_action_script<-click_action_scripts[[en]]
        print(click_action_script)
        target_root_dir<-paste0(target_root_dir_gen,"/",name)
        build_dir<-paste0(build_dir_gen,"/",name)
        website_path<-paste0(website_path_gen,"/",name)
        #print(c(target_root_dir,build_dir,website_path))
        ##BEGIN
        #some basic checks
        if (!dir.exists(root_dir)) stop('Root Dir Not Found')
        
        #first, build main
        files<-list.files(root_dir,recursive=TRUE) #these will have spaces, which we should strip
        
        ##APPLY FLAGS
        #safety check
        files<-comb_files(files,root_dir,publish_word,negate)
        #segregation check
        files<-comb_files(files,root_dir,paste0("label:",name),F)

        if (!dir.exists(target_root_dir)){
            dir.create(target_root_dir)
        }
        if (!dir.exists(build_dir)){
            dir.create(build_dir)
        }
        #port normalized files to target
        target_files<-vector(mode="list",length=length(files))
        for (f in 1:length(files)){
            file<-files[[f]]
            target_files[[f]]<-refactor_links_obsidian2hugo(root_dir,paste0("/",file),target_root_dir,simpleCapUnder(sub('.*\\/', '', str_replace_all(file, "\\/[^()]+\\.md", function(x) gsub(" ", "_", x, fixed=TRUE)))),website_path)#used to change to _
        }

        ##CLEAN FLAGS
        #be sure to clean labels out of files which will be published
        #safety
        for (tf in 1:length(target_files[[f]])){
            w_label<-read_file(paste0(target_files[[f]]))
            wo_label<-gsub(publish_word, "", w_label)
            write_file(wo_label,paste0(target_files[[f]]) )
        }
        #designation
        for (tf in 1:length(target_files[[f]])){
            w_label<-read_file(paste0(target_files[[f]]))
            wo_label<-gsub(paste0("label:",name), "", w_label)
            write_file(wo_label,paste0(target_files[[f]]) )
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

        #clean dups:
        links_df<-clean_dups(links_df,"value")
        nodes_df<-clean_dups(nodes_df,"size")
        #normalize node size
        nodes_df<-concord_node_size(links_df,nodes_df)
        links_df<-concord_link_size(links_df,nodes_df)
        fwrite(links_df,paste0(build_dir,"/","main_links.csv"))
        fwrite(nodes_df,paste0(build_dir,"/","main_nodes.csv"))
        
        
        #now, build the html
        fp<-forceNetwork(Links = links_df, Nodes = nodes_df,
                        Source = "source", Target = "target",
                        Value = "value", NodeID = "name", Nodesize = "size",
                        Group = "group", opacity = 0.8, clickAction = click_action_script,linkDistance = JS("function(d){return d.value * 10}"))
        
        
        saveWidget(fp, file=paste0(target_root_dir,"/","main_graph.html"))
        
        #Build node graphs
        node_dir<-paste0(target_root_dir,"/nodes")
        if (!dir.exists(node_dir)){
            dir.create(node_dir)
        }
        if (length(unique(nodes_df$name))>0){
            for (n in 1:length(unique(nodes_df$name))){
                #find all source-target pairings involving n-1
                sub_links_df<-links_df[(links_df$source==(n-1))|(links_df$target==(n-1)),]
                #now, go through and find connected
                all_neighbors<-c(sub_links_df$source,sub_links_df$target)
                unique_node_names<-unique(nodes_df$name)
                sub_nodes_df<-nodes_df[(match(nodes_df$name,unique_node_names)-1) %in% all_neighbors,]#-1
                #print(unique(nodes_df$name)[[n]])
                ##normalize links source and target
                normalized_source<-rep(-1,nrow(sub_links_df))
                if (length(sub_links_df$source)>0){
                    for (si in 1:length(sub_links_df$source)){
                                    s<-(sub_links_df$source)[[si]]
                                    normalized_source[[si]]<-which((nodes_df$name)[[s+1]]==(sub_nodes_df$name))[[1]]-1
                    }
                }else{
                    print(paste0("Warning: no sources found for ",unique(nodes_df$name)[[n]]))
                }
                
                normalized_target<-rep(-1,nrow(sub_links_df))
                if (length(sub_links_df$target)>0){
                    for (ti in 1:length(sub_links_df$target)){
                        t<-(sub_links_df$target)[[ti]]
                        normalized_target[[ti]]<-which((nodes_df$name)[[t+1]]==(sub_nodes_df$name))[[1]]-1
                    }
                }else{
                    print(paste0("Warning: no targets found for ",unique(nodes_df$name)[[n]]))
                }
                
                sub_links_df$source<-normalized_source
                sub_links_df$target<-normalized_target
                
                #ok, now normalize node sizes and build
                sub_nodes_df<-concord_node_size(sub_links_df,sub_nodes_df)

                #do we normalize link sizes? This seems crucial. Maybe need to minimize crowding.
                sub_links_df<-concord_link_size(sub_links_df,sub_nodes_df)


                fp<-forceNetwork(Links = sub_links_df, Nodes = sub_nodes_df,
                                Source = "source", Target = "target",
                                Value = "value", NodeID = "name", Nodesize = "size",
                                Group = "group", opacity = 0.8, clickAction = click_action_script,linkDistance = JS("function(d){return d.value * 10}"))
                
                saveWidget(fp, file=paste0(node_dir,"/",convert_space_to_underscore(simpleCap(unique(nodes_df$name)[[n]])),".html"))
            }
        }else{
            print("Warning: No satisfactory documents found")
        }
        

        #post/files
        #graphs/main.html done
        #      /nodes/node1.html
        #            /node2.html
        #      /topics/topicA.html

        #last but not least, initialize the empty pages which are implied to exist
        initialize_implied_pages(nodes_df,target_root_dir)
    }
    
}



