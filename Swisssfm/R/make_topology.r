






make_topology <- function(
	ara_id_next, 					# NA if none available
	ara_id = FALSE,					
	NA_next_ignore = FALSE			# ara_id_next not in ara_id? -> set to NA as well
){

	###############################################
	# check inputs & defaults 	
	len <- length(ara_id_next)
	if(isFALSE(ara_id[1])) ara_id <- seq(len) else{ 
		if(length(ara_id) != len) stop("Problem in make_topology: ara_id_next and ara_id must be of same length") 
		if(any(is.na(ara_id))) stop("Problem in make_topology: ara_id must not contain NAs") 
	}
	if(NA_next_ignore){
		if(any(!(ara_id_next[!is.na(ara_id_next)] %in% ara_id))) stop("Problem in make_topology: ara_id and ara_id_next mismatching")
	}else ara_id_next[!(ara_id_next %in% ara_id)] <- NA
	###############################################
	bin_link_matrix <- matrix(nrow = len, ncol = len, 0)
	ara_nr_next <- match(ara_id_next, ara_id)				# NAs returned
	for(i in 1:len) bin_link_matrix[i, ara_nr_next[i]] <- 1 # NAs skipped
	topo_matrix <- solve(diag(len) - bin_link_matrix)
	colnames(topo_matrix) <- rownames(topo_matrix) <- ara_id
	###############################################
	return(topo_matrix)
	
}

