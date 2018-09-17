#' Dijkstra finds the shortest distance between two nodes.
#'
#' @description Dijkstra finds the shortest path to a graph
#'
#' @param df the first parameter
#' @param s  the second parameter
#'
#' @return
#' @export
#'
#'
dijkstra<-function(df,s){

  if(!is.data.frame(df)){stop("dataframe")}

  na_vec<-c(rep(NA,1,dim(wiki_graph)[1]))
  wiki_graph<-cbind(wiki_graph,na_vec)
  index<-1

  maxnod<-max(wiki_graph[,"v1"])

  small<-c(0)
  small_nod<-c(s)

  rem<-which(wiki_graph[,"v2"]==s)
  wiki_graph<-wiki_graph[-rem,]

  while(index<maxnod){
    #Startar dijkstra
    a<-which(wiki_graph[,"v1"]==s)
    wiki_graph[a,4]<-wiki_graph[a,"w"] + small[index]
    b<-which.min(wiki_graph[,"na_vec"])

    cost<- wiki_graph[b,"na_vec"]
    d<-wiki_graph[b,2]



    index<-index+1
    rem<-which(wiki_graph[,2]==d)
    wiki_graph<-wiki_graph[-rem,]
    small_nod[index]<-d
    small[index]<-cost
    s<-d}

  return(small)  }


