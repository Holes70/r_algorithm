#Pomocou nalievaní, prelievaní a vylievaní odmerajte 4 litre, keď prvý krčah má maximálnu kapacitu 5 litrov a druhý krčah má maximálnu kapacitu 2 litre. 
#Začnite z východiskového bodu, kedy v prvom krčahu je 3 litre a v druhom krčahu 0 litrov.

library(igraph)

PrvyKrcahObjem=5
DruhyKrcahObjem=2
CielovyObjem=4
g=graph.empty(n=(PrvyKrcahObjem+1)*(DruhyKrcahObjem+1), directed=TRUE)
mojVyzorGrafu=layout_in_circle

k=0
for(i in 0:PrvyKrcahObjem)
  for(j in 0:DruhyKrcahObjem) { 
    k=k+1
    V(g)[k]$x=i
    V(g)[k]$y=j
    V(g)[k]$label=paste(i, j, sep=",")
  }

plot(g, layout=layout_in_circle)

prelajPrvyDoDruheho=function(g,k) {
  if(V(g)[k]$x!=0 && V(g)[k]$y!=DruhyKrcahObjem) {
    daSaPreliat = min(V(g)[k]$x, DruhyKrcahObjem - V(g)[k]$y)
    xNovy=V(g)[k]$x-daSaPreliat
    yNovy=V(g)[k]$y+daSaPreliat
    koniecHrany=which(V(g)$label==paste(xNovy, yNovy, sep=","))
    g=add_edges(g, c(k,koniecHrany))
    cat("\n",c(k,koniecHrany))
  }
  return(g)
}

prelajDruhyDoprveho=function(g,k) {
  if(V(g)[k]$y!=0 && V(g)[k]$x!=PrvyKrcahObjem) {
    daSaPreliat = min(V(g)[k]$y, PrvyKrcahObjem - V(g)[k]$x)
    xNovy=V(g)[k]$x+daSaPreliat
    yNovy=V(g)[k]$y-daSaPreliat
    koniecHrany=which(V(g)$label==paste(xNovy, yNovy, sep=","))
    g=add_edges(g, c(k,koniecHrany))
    cat("\n",c(k,koniecHrany))
  }
  return(g)
}

nalajPrvy=function(g,k){
  if(V(g)[k]$x!=PrvyKrcahObjem) {
  xNovy=PrvyKrcahObjem
  yNovy=V(g)[k]$y
  koniecHrany=which(V(g)$label==paste(xNovy, yNovy, sep=","))
  g=add_edges(g, c(k,koniecHrany))
  cat("\n",c(k,koniecHrany))
}
return(g)
}

nalajDruhy=function(g,k){
  if(V(g)[k]$y!=DruhyKrcahObjem) {
    xNovy=V(g)[k]$x
    yNovy=DruhyKrcahObjem
    koniecHrany=which(V(g)$label==paste(xNovy, yNovy, sep=","))
    g=add_edges(g, c(k,koniecHrany))
    cat("\n",c(k,koniecHrany))
  }
  return(g)
}

vylajPrvy=function(g,k){
  if(V(g)[k]$x!=0) {
    xNovy=0
    yNovy=V(g)[k]$y
    koniecHrany=which(V(g)$label==paste(xNovy, yNovy, sep=","))
    g=add_edges(g, c(k,koniecHrany))
    cat("\n",c(k,koniecHrany))
  }
  return(g)
}

vylajDruhy=function(g,k){
  if(V(g)[k]$y!=0) {
    xNovy=V(g)[k]$x
    yNovy=0
    koniecHrany=which(V(g)$label==paste(xNovy, yNovy, sep=","))
    g=add_edges(g, c(k,koniecHrany))
    cat("\n",c(k,koniecHrany))
  }
  return(g)
}


for(k in 1:((PrvyKrcahObjem+1)*(DruhyKrcahObjem+1))) { 
    cat("\nk: ",k)
    g=prelajPrvyDoDruheho(g,k)
    g=prelajDruhyDoprveho(g,k)
    g=nalajPrvy(g,k)
    g=nalajDruhy(g,k)
    g=vylajPrvy(g,k)
    g=vylajDruhy(g,k)
}

plot(g, layout=layout_in_circle)

prehladajGrafDoSirky <- function(g,goal){
  D<-list()
  V(g)$col="green"
  prvyVrchol=which(V(g)$label==paste(3, 0, sep=","))
  D<-c(D,as_ids(V(g)[[prvyVrchol]]))
  V(g)$dist=0
  V(g)[[prvyVrchol]]$pred=NA
  while(length(D)>0) {
    uzolNaRade <- D[[1]]
    D <- D[-1]
    if(V(g)$col[uzolNaRade]=="red") next
    V(g)$col[uzolNaRade]="red"
    susedia=neighbors(g, uzolNaRade, mode = c("out"))
    susedia=unlist(V(g)[susedia][V(g)[susedia]$col!="red"])
    V(g)[setdiff(susedia,D)]$dist=V(g)[uzolNaRade]$dist+1
    V(g)[setdiff(susedia,D)]$pred=uzolNaRade
    D<-c(D,as_ids(susedia)) 
  }
  cat("vrcholy:                       ",1:vcount(g),sep='\t')
  cat("\nvzdialenosti od 1. vrchola su: ",V(g)$dist,sep='\t')
  cat("\npredchodcovia vrcholov su: ",unlist(V(g)$pred),sep='\t')
  cesta=c(goal)
  while(!is.na(V(g)[[tail(cesta,1)]]$pred))
    cesta=c(cesta,V(g)[[tail(cesta,1)]]$pred)
  cesta=rev(cesta)
  cat("\ncesta z",V(g)[[1]]$label,"do",V(g)[[goal]]$label,": ",V(g)[cesta]$label,sep=' ')
  E(g)$color="black"
  E(g,path=cesta)$color="red"
  E(g)$weight=0.1
  E(g,path=cesta)$weight=3
  V(g)$col="yellow"
  V(g)[cesta]$col="red"
  plot(g, vertex.color=V(g)$col, edge.width= E(g)$weight, layout=mojVyzorGrafu)
  title(paste("cesta", V(g)[cesta]$label, sep=" "),adj=1., line = -2)
}

ktoryVrcholChcemNajst=which(V(g)$label==paste(CielovyObjem, 0, sep=","))
prehladajGrafDoSirky(g,ktoryVrcholChcemNajst)
