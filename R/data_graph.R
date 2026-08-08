#' Run Shiny Interactive graph
#'
#' Launch the interactive Shiny graph.
#'
#' @return a graph clustered by different exams and sectors of activity
#'  for French selective engineering schools and selective higher education
#'  institutions like Ecoles Normales Superieures
#' @import tidyr
#' @import dplyr
#' @import stringr
#' @import utils
#' @export
data_graph <- function() {

  
  
  mat= read.table("http://valerierobert-maths.re/wp-content/uploads/2026/07/valerierobert-maths.re-ec-mag-ec-mag.txt", header=TRUE)
  
  n=length(mat)
  mat=mat[,c(1,2,8:n)]
  p=length(mat)
  lien_ecole=mat[,p-1]
  lien_concours=mat[,p]
  lien_concours[1:6]="https://prepas.org/index.php?rubrique=92"
  rownames(mat)=mat[,1]
  
  ecoles <- mat[,2]
  mat=mat[,3:(p-2)]
  mat[1,24]=1
  mat[1,5]=1
  mat[4,5]=1
  mat[5,5]=1
  mat[11,11]=1
  mat[4,8]=1
  mat[13,8]=1
  mat[36,8]=1
  mat[37,8]=1
  mat[23,8]=1
  mat[19,8]=1
  mat[20,8]=1
  mat[7,16]=1
  mat[8,16]=1
  mat[12,16]=1
  mat[18,16]=1
  mat[21,16]=1
  mat[48,16]=1
  
  mat[1,18]=1
  mat[11,18]=1
  mat[4,18]=1
  mat[7,18]=1
  mat[12,18]=1
  
  mat[11,7]=1 #informatique
  mat[8,7]=1
  mat[20,7]=1
  
  mat[10,17]=1
  mat[13,17]=1
  
  mat[8,4]=1
  
  mat[2,19]=1
  mat[2,23]=1
  mat[14,23]=1
  mat[89,23]=1
  mat[58,23]=1
  
  mat[23,14]=1
  mat[48,14]=1
  
  mat[23,1]=1
  mat[20,1]=1
  mat[18,1]=1
  
  mat[1,12]=1
  mat[23,12]=1
  mat[2,12]=1
  mat[7,12]=1
  mat[12,12]=1
  mat[34,12]=1
  mat[35,12]=1
  mat[100,12]=1
  
  mat[7,22]=1
  mat[20,22]=1
  
  mat[19,24]=1
  mat[9,24]=1
  mat[134,1]=1
  mat[14,10]=1
  mat[16,11]=1
  mat[16,24]=1
  mat[12,24]=1
  mat[18,24]=1
  mat[23,24]=1
  mat[14,24]=1
  mat[324,18]=0
  mat[324,17]=1
  
  
  Economie=rep(0,dim(mat)[1])
  Economie[1]=1
  Economie[11]=1
  Economie[7]=1
  Economie[8]=1
  Economie[15]=1
  Economie[339]=1
  mat=cbind(mat,Economie)
  mat=mat[,order(colnames(mat))]
  
  ecole_nodes <- data.frame(
    id = rownames(mat),
    label = rownames(mat),
    group = ecoles,          
    type = "Concours",
    lien_ecole=lien_ecole,
    lien_concours=lien_concours,
    font.color = "white"
  )
  
  
  domaine_nodes <- data.frame(
    id = colnames(mat),
    label = colnames(mat),
    group = "Domaine", stringsAsFactors = FALSE,      
    type = "Domaine",
    lien_ecole=NA,
    lien_concours=NA,
    font.color = "white"
  )
  
  # Fusionner tous les noeuds
  nodes <- rbind(ecole_nodes, domaine_nodes)
  nodes$font <- mapply(function(groupe, type) {
    if (type == "Concours") {
      list(color = "white")
    } else if (groupe == "Domaine") {
      list(color = "pink")
    } else if (type == "ecoles") {
      list(color = "red")
    } else {
      list(color = "white")
    }
  }, nodes$group, nodes$type, SIMPLIFY = FALSE)
  
  nodes$font <- lapply(nodes$group, function(g) {
    if (g == "X-ENS") {
      list(color ="#cea2e4" , size = 27)
    } else if (g == "Mines-Ponts")  {
      list(color = "#FFD700", size = 27)
    }else if (g == "Mines-Telecom")  {
      list(color = "orange", size = 27)
    }else if (g == "Centrale-Supelec")  {
      list(color = "lightgreen", size = 27)
    }else if (g == "CCINP")  {
      list(color = "lightblue", size = 27)
    }else if (g == "INP-PC-Chimie")  {
      list(color = "#0080ff", size = 27)
    }else if (g == "Banque_CCINP_ou_IF")  {
      list(color = "#2F5FD7", size = 27)
    }else if (g == "Groupe_INSA")  {
      list(color ="#2EA84F" , size = 27)
    }else if (g == "e3a-POLYTECH")  {
      list(color = "#6e9363", size = 27)
    }else if (g == "EPITA_ISME_IPSA")  {
      list(color = "#ffcc99", size = 27)
    }else if (g == "BCEAS")  {
      list(color = "#7469C1", size = 27)
    }else if (g == "Magisteres")  {
      list(color = "#9B59B6", size = 27)
    }else{
      list(color = "#FF69B4", size = 30) 
    }
    
  })
  
  
  
  edges_list <- which(mat == 1, arr.ind = TRUE)
  edges <- data.frame(
    from = rownames(mat)[edges_list[, 1]],
    to   = colnames(mat)[edges_list[, 2]]
  )
  only_domaine <-sort(domaine_nodes$id)
  only_ecoles <-sort(ecole_nodes$id)
  only_concours<-c("X-ENS","Mines-Ponts","Mines-Telecom", "Centrale-Supelec","CCINP","INP-PC-Chimie","Banque_CCINP_ou_IF","Groupe_INSA","e3a-POLYTECH", "EPITA_ISME_IPSA","BCEAS","Magisteres")
  
  group_colors <- list(
    "X-ENS" = "#cea2e4",
    "Mines-Ponts" = "#FFD700",
    "Mines-Telecom" = "orange" ,
    "Centrale-Supelec"="lightgreen",
    "CCINP"="lightblue",
    "INP-PC-Chimie"="#0080ff",
    "Banque_CCINP_ou_IF"="#2F5FD7",
    "Groupe_INSA"= "#2EA84F",
    "e3a-POLYTECH"= "#6e9363",
    "EPITA_ISME_IPSA"= "#ffcc99",
    "BCEAS"="#7469C1" ,
    "Magisteres"= "#9B59B6",
    "Domaine"="pink"
  )
  
  nodes$color <- group_colors[nodes$group]
  edges <- edges %>%
    left_join(nodes[, c("id", "color")], by = c("from" = "id")) 
  
  list(
    nodes = nodes,
    edges = edges,
    only_domaine=only_domaine,
    only_ecoles=only_ecoles,
    only_concours=only_concours
    
    
  )
  
}