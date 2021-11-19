






run_daily_load <- function( # one function run per compound

	inhabitants_total = 8417700,
	hospital_beds_total = FALSE,					# Set to FALSE to ignore
	STP_id, 
	STP_fraction_hospital= FALSE,
	STP_amount_inhabitants,	
	STP_amount_hospital_beds = FALSE,				# Set to FALSE to ignore
	compound_load_total, 							# [kg / a], set to FALSE to ignore and then use compound_load_per_capita_and_day
	compound_load_per_capita_and_day = FALSE,		# [g / E d], set to FALSE to ignore and then use compound_load_total
	compound_load_per_hospital_bed_and_day = FALSE, # [g / E d], set to FALSE to ignore and then use compound_load_total
	compound_elimination_STP,						# vector or STP-specific matrix with elimination fractions over treatment steps (not percentage values); set to 0 to skip a step 
	compound_excreted = 1,							# fraction excreted and discharged, set to 1 to ignore
	topo_matrix
	
){

	###############################################
	# check inputs & defaults
	if(!all(STP_id %in% colnames(topo_matrix))) stop("Problem in run_daily_load: not all STP_id present in topo_matrix")
	if(!all(colnames(topo_matrix) %in% STP_id)) stop("Problem in run_daily_load: not all topo_matrix entries present in STP_id")
	if(is.logical(STP_fraction_hospital)) if(!isTRUE(STP_fraction_hospital)) STP_fraction_hospital <- rep(0, length(STP_id))
	if(!identical(length(STP_id), length(STP_fraction_hospital), length(STP_amount_inhabitants))) stop("Problem in run_daily_load: STP inputs vary in length_1")
	if(is.logical(STP_amount_hospital_beds)) if(!isTRUE(STP_amount_hospital_beds)) STP_amount_hospital_beds <- 0 else if(length(STP_amount_hospital_beds) != length(STP_id)) stop("Problem in run_daily_load: STP inputs vary in length_2")
	if(!is.vector(compound_elimination_STP)) if(nrow(compound_elimination_STP) != length(STP_id)) stop("Problem in run_daily_load: when compound_elimination_STP is a matrix, its rows must ne of equal length as the number of STPs")
	if(is.logical(hospital_beds_total)) if(!isTRUE(hospital_beds_total)) hospital_beds_total <- 0
	if(any((compound_elimination_STP < 0) & (compound_elimination_STP > 1))) stop("Problem in run_daily_load: compound_elimination_STP not in all [0,1]")
	if(length(compound_load_total) > 1) stop("Problem in run_daily_load: compound_load_total should consist of one value only")
	if(length(compound_load_per_capita_and_day) > 1) stop("Problem in run_daily_load: compound_load_per_capita_and_day should consist of one value only")
	if(!is.numeric(compound_load_total) & !is.numeric(compound_load_per_capita_and_day)) stop("Problem in run_daily_load: either compound_load_total or compound_load_per_capita_and_day must be defined.")
	topo_matrix[topo_matrix != 0] <- 1 	# in case topo_matrix contains STP id
	###############################################
	if(!is.numeric(compound_load_per_capita_and_day)) compound_load_per_capita_and_day <- compound_load_total * (1 - STP_fraction_hospital) * 1000 / inhabitants_total / 365 		# [kg/a] -> [g/d]
	if(!is.numeric(compound_load_per_hospital_bed_and_day)) compound_load_per_hospital_bed_and_day <- compound_load_total * (STP_fraction_hospital) * 1000 / hospital_beds_total / 365 	# [kg/a] -> [g/d]
	if(is.vector(compound_elimination_STP)){
		compound_elimination_STP <- prod(1 - compound_elimination_STP) # prod: if several steps combined
	}else{
		compound_elimination_STP <- (1 - apply(compound_elimination_STP, MARGIN = 1, prod))
	}
	load_local <- STP_amount_inhabitants * compound_load_per_capita_and_day * compound_excreted * compound_elimination_STP
	load_local <- load_local + STP_amount_hospital_beds * compound_load_per_hospital_bed_and_day * compound_excreted * compound_elimination_STP
	load_cumulated <- apply(topo_matrix, MARGIN = 2, function(x, y){sum(x * y)}, y = load_local)
	###############################################
	result <- data.frame(as.numeric(STP_id), as.numeric(load_local), as.numeric(load_cumulated), row.names = NULL)
	names(result) <- c("STP_ID", "load_local", "load_cumulated")
	return(result)
	
}

