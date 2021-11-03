






run_daily_load <- function(

	inhabitants_total = 8417700,
	hospital_beds_total = FALSE,				# Set to FALSE to ignore
	STP_id, 
	STP_fraction_hospital,
	STP_amount_inhabitants,	
	STP_amount_hospital_beds = FALSE,			# Set to FALSE to ignore
	compound_load_total, 						# [kg / a]
	compound_load_per_capita_and_day = FALSE,	# [g / d], set to FALSE to ignore
	compound_elimination_ara,					# vector with elimination fractions over treatment steps (not percentage values); set to 0 to skip a step 
	compound_excreted = 1,						# fraction excreted and discharged, set to 1 to ignore
	topo_matrix
	
){

	###############################################
	# check inputs & defaults
	if(!all(STP_id %in% colnames(topo_matrix))) stop("Problem in run_daily_load: not all STP_id present in topo_matrix")
	if(!all(colnames(topo_matrix) %in% STP_id)) stop("Problem in run_daily_load: not all topo_matrix entries present in STP_id")	
	if(!identical(length(STP_id), length(STP_fraction_hospital), length(STP_amount_inhabitants))) stop("Problem in run_daily_load: ara inputs vary in length_1")
	if(isFALSE(STP_amount_hospital_beds)) STP_amount_hospital_beds <- 0 else if(length(STP_amount_hospital_beds) != length(STP_id)) stop("Problem in run_daily_load: ara inputs vary in length_2")
	if(!identical(length(compound_load_total), length(compound_elimination_ara), length(compound_excreted))) stop("Problem in run_daily_load: stoff inputs vary in length")
	if(isFALSE(hospital_beds_total)) hospital_beds_total <- 0
	if(any((compound_elimination_ara < 0) & (compound_elimination_ara > 1))) stop("Problem in run_daily_load: compound_elimination_ara not in all [0,1]")
	topo_matrix[topo_matrix != 0] <- 1 	# in case topo_matrix contains STP id
	###############################################
	if(isFALSE(compound_load_per_capita_and_day)) compound_load_per_capita_and_day <- compound_load_total * (1 - STP_fraction_hospital) * 1000 / inhabitants_total / 365 		# [kg/a] -> [g/d]
	compound_load_per_hospital_bed_and_day <- compound_load_total * (STP_fraction_hospital) * 1000 / hospital_beds_total / 365 	# [kg/a] -> [g/d]	
	compound_elimination_ara <- prod(1 - compound_elimination_ara) # prod: if several steps combined
	load_local <- STP_amount_inhabitants * compound_load_per_capita_and_day * compound_excreted * compound_elimination_ara
	load_local <- load_local + STP_amount_hospital_beds * compound_load_per_hospital_bed_and_day * compound_excreted * compound_elimination_ara
	load_cumulated <- apply(topo_matrix, MARGIN = 2, function(x){sum(x * load_local)}, y = load_local) 
	###############################################
	return(cbind(STP_id, load_local, load_cumulated))
	
}

