###################################################
# This script populates our javascript app with 
# network data. 
# The function PopulateApp() is the main function
# 	it takes several inputs and returns an RJson
#	object which can be written to file and loaded
# 	in the app.
###################################################

rm(list=ls())

library(igraph)
library(RColorBrewer)
library(rjson)
library(tsne)

PopulateApp <- function(node.table, igraph.object, node.table.vertex.name="Id", positions=NULL, color=NULL, size=NULL, attributes=NULL, edge.color="#171615"){
    ##############################################
    # This function populates the nodes section
    #
    # inputs:
	#	node.table = a data frame of metadata, 
	#		where each observation corresponds to
	#		a network node
	#	node.table.vertex.name = a string indicating the name of the 
	#		column in node.table indexing vertex (node) names. Defaults to 
	#		"Id" in convention with Gephi.
	#	igraph.object = an igraph object of the 
	#		network you wish to populate
	#	positions = a matrix with 2 columns and 
	#		a row per node in the network. Each
	#		row is the x, y coordinates of a node
	#		If left NULL (default) the network is 
	#		given a layout from layout.drl()
	#	color = string of the column name from 
	#		node.table to use for coloring the nodes
	#		If NULL (default) all nodes are blue
	#	size = string of the column name from node.table
	#		to use for sizing the nodes. Nodes are scaled
	#		to the size of the largest node equal to 10.
	#		If left NULL (the default) then all nodes are size 5
	#	attributes = a list where each element corresponds to a node
	#		The contents of each element will be formatted to 
	#		be the attributes object viewable by clicking on a node
	#		in the app.
	#	edge.color = string of hex color code for edges. The default
	#		is the background color of sbu_app, making edges appear
	#		invisible.
    #
    # output: a json object that can be written to 
    #   file to use to populate sbu_app
    ###############################################
    
    library(igraph)
    library(RColorBrewer)
    library(rjson)
    
    ###############################################
    # nodes object
    ###############################################

    # set up indexing of node table based on Id
    
    order <- data.frame(Id=V(igraph.object)$name, stringsAsFactors=FALSE)
    order$order <- 1:nrow(order)
    
    node.table <- merge(node.table, order, all=TRUE)
    
	if(node.table.vertex.name != "Id") warning("node.table.vertex.name != \"Id\". If using Gephi, your node table must have a column named \"Id\" indexing your node names.")
	
    rownames(node.table) <- node.table[ , node.table.vertex.name ]
    
    node.table <- node.table[ order(node.table$order) , ]
    
    # if positions is null, then use layout.drl to get positions
    # else, make sure positions is indexed correctly
    if( is.null(positions) ){
        #positions <- layout.drl(igraph.object)
        positions <- layout.fruchterman.reingold(igraph.object)
        rownames(positions) <- V(igraph.object)$name
        
        V(igraph.object)$x <- positions[ , 2 ]
        V(igraph.object)$y <- positions[ , 1 ]
    }else{
        positions <- positions[ V(igraph.object)$name , ]
        
        V(igraph.object)$x <- positions[ , 2 ]
        V(igraph.object)$y <- positions[ , 1 ]
    }
    
    # if colors is null, all colors are blue
    # else make colors based on color column
    if(is.null(color)){
        V(igraph.object)$color <- "#3333ff" # rgb(0,0,1,0.75)
    }else{
        node.table$color <- rgb(0,0,1,0.75)
        
        unique.color.feature <- unique(node.table[ , color ])
        
        brewer.colors <- brewer.pal(length(unique.color.feature), name="Paired")
        
        for(j in 1:length(brewer.colors)){
            node.table$color[ node.table[ , color ] == unique.color.feature[ j ] ] <- brewer.colors[ j ]
        }
        
        V(igraph.object)$color <-  node.table[ V(igraph.object)$name , "color" ]
    }
    
    # if size is null make everything size "5"
    # else make size based on size column
    if(is.null(size)){
        V(igraph.object)$size <- 5
    }else{
        node.table$size <- node.table[ , size ] / sum(node.table[ , size ]) * 10
        
        V(igraph.object)$size <- node.table[ V(igraph.object)$name , "size" ]
    }
    
    # if attributes is null, set attributes to be the node Id
    # else, order attributes according to node Id
    if(is.null(attributes)){
        V(igraph.object)$attributes <- as.list(V(igraph.object)$name)
    }else{
        attributes <- attributes[ V(igraph.object)$name ]
        names(attributes) <- NULL
        V(igraph.object)$attributes <- attributes
    }
    
    # make a list for the nodes object
    v.out <- vector(mode="list", length=length(V(igraph.object)$name))
    names(v.out) <- NULL
    
    for(j in 1:length(v.out)){
        v.out[[ j ]] <- list(id=V(igraph.object)$name[ j ],
                             x=V(igraph.object)$x[ j ],
                             y=V(igraph.object)$y[ j ],
                             color=V(igraph.object)$color[ j ],
                             size=V(igraph.object)$size[ j ],
                             attributes=V(igraph.object)$attributes[[ j ]])
    }
    
    ###############################################
    # edges object
    ###############################################
    e <- as.data.frame(get.edgelist(igraph.object), stringsAsFactors=FALSE)
    names(e) <- c("source", "target")
    e$label <- e$source
    e$id <- 1:nrow(e)
    e$color <- edge.color
    e$size <- 0.1
    
    e.out <- vector(mode="list", length=nrow(e))
    
    for(j in 1:nrow(e)){
        e.out[[ j ]] <- as.list(e[ j , ])
    }
    
    names(e.out) <- NULL
    
    ###############################################
    # create a json object
    ###############################################
    json.out <- list(edges=e.out, nodes=v.out)
    
    json.out <- toJSON(x=json.out)
    
    # write.table(json.out, paste(outfilepath, "data.json", sep=""), sep="", row.names=F, col.names=F, quote=F)

    return(json.out)    
}

TopicTable <- function(doc.topics, top.terms){
    ##############################################
	# This function populates an attributes object for 
	#	sbu_app from the outputs of a topic model
	#
    # Input:
    #   doc.topics = a D * K matrix of topic proportions in documents
    #   top.terms = a K * 2 dataframe with columns named "topic" and "top.terms"
    # Output:
    #   an html-formatted table for each 
    #   document. This can be used as input for 
    #   node attributes
    ##############################################
    
    library(reshape2)
    
    # rehape data for proper formatting
    doc.topics <- data.frame(document=rownames(doc.topics), doc.topics, stringsAsFactors=FALSE)
    
    doc.topics <- melt(doc.topics)
    
    names(doc.topics)[ 2:3 ] <- c("topic", "share")
    
    # add in top.terms for the doc.topics matrix
    doc.topics <- merge(doc.topics, top.terms[ , c("topic", "top.terms")], all.x=TRUE )
    
    doc.topics <- doc.topics[ doc.topics$share > 0 , ]
    
    # get topic tables
    topics <- by(data=doc.topics, INDICES=as.factor(doc.topics$document), FUN=function(x){
        topics <- x$topic
        shares <- round(x$share, 3)
        terms <- x$top.terms
        
        myorder <- order(shares, decreasing=T)
        
        topics <- topics[ myorder ]
        shares <- shares[ myorder ]
        terms <- terms[ myorder ]
        
        tablestart <- "<table border='0' cellspacing='5'><tr><th>Topic</th><th>Share</th><th>Top Terms</th></tr>"
        tableend <- "</table>"
        
        topics <- paste("<tr><td valign=\"top\">", topics, "</td>", sep="")
        shares <- paste("<td valign=\"top\">", shares, "</td>", sep="") 
        terms <- paste("<td align=\"center\" valign=\"top\">", terms, "</td>", sep="") 
        
        tablemiddle <- paste(topics, shares, terms, sep="")
        tablemiddle <- paste(tablemiddle, collapse="")
        
        result <- paste(tablestart, tablemiddle, tableend, sep="")
        
        return(result)
    })
    
    # return topics as the final result
    return(topics)    
    
}
