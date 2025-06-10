#Input: H1, the incidence matrix of hypergraph H1(V,E1)
#       C, the number of nodes contained in each node class
#Output: H', the incidence matrix of the k-partite Hypergraph H'(V',E')

#Map the new nodes to the combinations of the original nodes.
New_C <- function(H1, C){
  rownames(H1) <- NULL
  nC <- c()
  s <- 0
  for (h in 1:length(C)){
    s <- s + C[h];
    nC <- c(nC, s)
  }
  c1 <- c()
  for (j in 1:nC[1]){
    c1 <- cbind(c1, H1[,j])
  }
  colnames(c1) <- colnames(H1)[1:nC[1]]
  c1_unique <- c1[!duplicated(c1),]#Delete duplicate lines.
  basis <- list(c1_unique)
  cc <- c()
  for (i in 1:(length(nC)-1)){
    cc <- c()
    for (j in (nC[i]+1):nC[(i+1)]){
      cc <- cbind(cc, H1[,j])
    }
    #cc: The combination of all nodes in the i-th node class.
    colnames(cc) <- colnames(H1)[(nC[i]+1):nC[i+1]];
    cc_unique <- cc[!duplicated(cc),];
    cc_unique <- as.array(cc_unique)
    if(is.na(ncol(cc_unique))){
      cc_unique <- t(cc_unique)
    }else{
      cc_unique <- cc_unique
    }
    #cc_unique: In the current loop, this node class is a non-repetitive combination of interactive nodes.
    basis[[length(basis)+1]] <- cc_unique
  }
  print(basis)
}


#New_Nodes: The corresponding list of the new node and the original node.
New_Nodes <- function(New_C, C){
  new_n <- c()
  for (i in 1:length(C)){
    n <- nrow(New_C[[i]]);
    new_n <- c(new_n, n)
  }
  #new_n: The number of nodes of each new point class.
  n <- sum(new_n)
  new_1 <- paste("v", seq(from = 1, to = n), sep = "")
  print(new_1)
  #新节点与原节点对应列表，按照点类给新节点逐个命名
  for (i in 1:length(C)){
    nn <- nrow(New_C[[i]])
    rownames(New_C[[i]]) <- new_1[1:nn]
    new_1 <- new_1[-(1:nn)]
  }
  print(New_C)
} 

#New_Incidence: H', the incidence matrix of the k-partite Hypergraph H'(V',E').
New_Incidence <- function(H1, New_Nodes, C){
  nC <- c()
  s <- 0
  for (h in 1:length(C)){
    s <- s + C[h];
    nC <- c(nC, s)
  }
  ee <- c()
  for (h in 1:nrow(H1)){
    k <- 1
    ee1 <- c()
    for (g in 1:nrow(New_Nodes[[1]])){
      if(all(H1[h,1:nC[1]] == New_Nodes[[1]][g,])){
        #Integrate the new representations of each hyperedge in the first node class.
        ee1[k] <- 1
      }else{
        ee1[k] <- 0
      }
      k <- k+1
    }
    for (i in 1:(length(C)-1)){
      for (j in 1:nrow(New_Nodes[[i+1]])){
        #Integrate the new representations of each hyperedge outside the first point class.
        if(all(H1[h,(nC[i]+1):nC[i+1]] == New_Nodes[[i+1]][j,])){
          ee1[k] <- 1
        }else{
          ee1[k] <- 0
        }
        k <- k+1
      }
    }
    ee <- rbind(ee, ee1) 
  }
  n <- ncol(ee)
  new_1 <- paste("v", seq(from = 1, to = n), sep = "")
  colnames(ee) <- new_1
  n <- nrow(ee)
  new_2 <- paste("ee", seq(from = 1, to = n), sep = "")
  rownames(ee) <- new_2
  print(ee)
}
