#Input: H, the incidence matrix of hypergraph H(V,E)
#       C, the number of nodes contained in each node class
#Output: H1, the incidence matrix of H1(V,E1)

Hyperedges_Filtering <- function(H, C){  
  nC <- c()
  s <- 0
  for (h in 1:length(C)){
    s <- s + C[h];
    nC <- c(nC, s)
  }
  #nC: The positions of the last nodes of each node class.
  result_list <- c()
  for (x in 1:nrow(H)) {
    #We iterate through each row of the incidence matrix in the loop.
    sum1 <- 0
    #sum1: The number of the first class nodes that appear in each hyperedge.
    for(q in 1:nC[1]){
      sum1 <- sum1 + H[x, q]
    }
    if(sum1 == 0){
      #This hyperedge does not contain the nodes in the first node class.
      result <- c(FALSE)
    }else{
      #This hyperedge contains the nodes in the first node class.
      result <- c(TRUE)
    }
    ss <- c();
    sum <- 0;
    e <- H[x, ]
    for (i in 1:length(nC)){
      for (j in 1:nC[i]){
        sum <- sum + e[j]
      }
      #sum: The sum from the first element to the element that marks the end of the i-th class of the hyperedge e.
      ss <- c(ss, sum);
      sum <- 0
    }
    ss <- unlist(ss)
    #ss: Store the sum of the hyperedge e for each nodes class.
    for (g in 1:(length(nC)-1)){
      result <- c(result, all(ss[g+1]>ss[g]))
      
    }
    result_list <- rbind(result_list, result)
    #result_list: Whether the hyperedge contains the nodes in this node class. 
    #nrow(result_list) = nrow(H), ncol(result_list) = length(C).
    H1 <- c()
    rn_nH <- c()
    n_nH <- c()
    for(a in 1:nrow(result_list)){
      if(all(result_list[a,] == TRUE)){
        #This hyperedge encompasses all node classes.
        H1 <- rbind(H1, H[a,])
        rn_nH <- c(rn_nH, a)
      }else{
        H1 <- H1 
      }
    }
  }
  for(b in 1:length(rn_nH)){
    n_nH <- c(n_nH, rownames(H)[rn_nH[b]])
    
  }
  rownames(H1) <- n_nH
  colnames(H1) <- colnames(H)
  print(H1)
  #H1: the incidence matrix of H1(V,E1). 
}
