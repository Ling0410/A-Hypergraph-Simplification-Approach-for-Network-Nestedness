#Input: H', the incidence matrix of H'(V',E')
#       C, the number of nodes contained in each node class
#       C1, the node class indexes of the first node combination
#       C2, the node class indexes of the second node combination
#Output: G, the adjacency matirx of bipartite graph G[I, J]

New_adjacency <- function(New_Incidence, New_Nodes, C, C1, C2){
  nC <- c()
  s <- 0
  for (h in 1:length(C)){
    s <- s + C[h];
    nC <- c(nC, s)
  }
  n <- c()
  for (i in 1:length(C)){
    n <- c(n, nrow(New_Nodes[[i]]))
  }
  #nnC: The positions of the last nodes of each node class in New_Incidence.
  nnC <- c()
  s <- 0
  for (h in 1:length(n)){
    s <- s + n[h];
    nnC <- c(nnC, s)
  }
  
  cc1 <- list(1:nnC[1])
  #The positions occupied by each point class node.
  for (i in 1:(length(nnC)-1)){
    cc <- c()
    cc <- c((nnC[i]+1):nnC[(i+1)])
    cc1[[length(cc1)+1]] <- cc
  }
  
  
  ei <- c()
  #e1: The adjacency matrix of the new nodes contained in C1.
  for (i in c1){
    ei <-cbind(ei, New_Incidence[, cc1[[i]]]) 
  }
  I <- ei[!duplicated(ei),]
  #I: The correspondence between nodes and new nodes in C1.
  kkc <- c()
  for (i in c1){
    kkc <-c(kkc, cc1[[i]]) 
  }
  new_kkcc <- paste("v", kkc, sep = "")
  colnames(I) <- new_kkcc
  colnames(ei) <- new_kkcc
  new_kkcr <- paste("i", seq(from=1, to =nrow(I)), sep = "")
  rownames(I) <- new_kkcr
  
  ej <- c()
  #ej: The adjacency matrix of the new nodes contained in C2.
  for (i in C2){
    ej <-cbind(ej, New_Incidence[, cc1[[i]]]) 
  }
  J <- ej[!duplicated(ej),]
  #J: The correspondence between nodes and new nodes in C2.
  k_kkc <- c()
  for (i in C2){
    k_kkc <-c(k_kkc, cc1[[i]]) 
  }
  new_k_kkcc <- paste("v", k_kkc, sep = "")
  colnames(J) <- new_k_kkcc
  colnames(ej) <- new_k_kkcc
  new_k_kkcr <- paste("j", seq(from=1, to =nrow(J)), sep = "")
  rownames(J) <- new_k_kkcr
  
  a <- c()
  for (i in 1:nrow(ei)){
    aa <- identical(ei[i,], I[1,])
    #aa: Test whether ei[i,] is the same as I[1,].
    a <- c(a, aa)
  }
  eei <- c()
  #eei: The incidence matrix of I.
  for (j in 1:nrow(ei)){
    eeii <- c()
    a <- j
    for (i in 1:nrow(I)){
      if(identical(ei[a,], I[i,])){
        eeii[length(eeii)+1] <- 1
      }else{
        eeii[length(eeii)+1] <- 0
      }
    }
    eei <- rbind(eei, eeii)
  }
  eeic <- paste("i", seq(from=1, to =nrow(I)), sep = "")
  colnames(eei) <- eeic
  eeir <- paste("ee", seq(from=1, to =nrow(ei)), sep = "")
  rownames(eei) <- eeir
  
  b <- c()
  for (i in 1:nrow(ej)){
    bb <- identical(ej[i,], J[1,])
    b <- c(b, bb)
  }
  eej <- c()
  #eej: The incidence matrix of J.
  for (j in 1:nrow(ej)){
    eejj <- c()
    b <- j
    for (i in 1:nrow(J)){
      if(identical(ej[b,], J[i,])){
        eejj[length(eejj)+1] <- 1
      }else{
        eejj[length(eejj)+1] <- 0
      }
    }
    eej <- rbind(eej, eejj)
  }
  eejc <- paste("j", seq(from=1, to =nrow(J)), sep = "")
  colnames(eej) <- eejc
  eejr <- paste("ee", seq(from=1, to =nrow(ej)), sep = "")
  rownames(eej) <- eejr
  
  hij <- cbind(eei,eej)
  hij
  #hij: The incidence matrix represented by the nodes in I and J.
  
  G <- t(t(eei) %*% eej)
  #G: The adjacency matirx of bipartite graph G[I, J].
}
