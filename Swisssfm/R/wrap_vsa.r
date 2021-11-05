

if(FALSE){

	ARA <- read.csv2(file = "D:/VSA/new_inputs/ARA_input_corrected.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
	ARA <- read.csv2(file = "E:/VSA/new_inputs/ARA_input_corrected.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

	STP_id <- as.character(ARA$BAFU_Abgabehoehe_2021_kurz_2.ARANR)
	STP_id_next <- as.character(ARA$ARANEXTNR)
	STP_amount_inhabitants <- as.numeric(gsub(".", "", as.character(ARA$Eang_2021), fixed = TRUE))

	STP_discharge <- as.numeric(ARA$Q347I)

	STP_discharge[STP_discharge < 0 | is.na(STP_discharge)] <- mean(STP_discharge[STP_discharge > 0 & !is.na(STP_discharge)])

	compound_name <- "Diclofenac"

	compound_load_per_capita_and_day <- 540 * 1E-6 # Diclofenac

	compound_elimination_ara <- matrix(ncol = 1, nrow = length(STP_id),  .9)
	compound_elimination_ara[ARA$MikroV %in% c("", "nicht vorhanden")] <- 0

	path_out_xlsx <- "D:/VSA/new_outputs/load_exports"
	overwrite <- TRUE
	
}

wrap_vsa <- function(
	STP_id,
	STP_id_next,
	STP_amount_inhabitants,
	local_discharge,
	STP_discharge,
	compound_name,
	compound_load_total = FALSE, 				# [kg / a]
	compound_load_per_capita_and_day,			# [g / E d], set to FALSE to ignore
	compound_load_per_hospital_bed_and_day = 0,	# [g / E d], set to FALSE to ignore
	compound_elimination_ara,					# vector with elimination fractions over treatment steps (not percentage values); set to 0 to skip a step 
	compound_excreted = 1,						# fraction excreted and discharged, set to 1 to ignore
	path_out_xlsx = FALSE,						# if FALSE, return data.frame
	overwrite = TRUE
){

	###############################################
	# check inputs & defaults #####################
	if(!is.numeric(STP_amount_inhabitants)) stop("Problem in wrap_vsa: STP_amount_inhabitants must be numeric.")
	if(!identical(length(STP_id), length(STP_id_next), length(STP_amount_inhabitants))) stop("Problem in wrap_vsa: STP_id, STP_id_next and STP_amount_inhabitants must be of equal length.")
	if(!overwrite & !is.logical(path_out_xlsx)) if(file.exists(path_out_xlsx)) stop("Problem in wrap_vsa: file at path_out_xlsx already exists; remove it or use overwrite = TRUE.")
	###############################################
	# calculate topology matrix ###################
	topo_matrix <- make_topology(
	
		STP_id_next = STP_id_next, 					# NA if none available
		STP_id = STP_id,					
		NA_next_ignore = FALSE,						# ara_id_next not in STP_id? -> set to NA as well
		insert_id_in_topo_matrix = TRUE
	
	)
	###############################################
	# calculate local and cumulative loads ########
	result_table <- run_daily_load(

		inhabitants_total = sum(STP_amount_inhabitants),
		hospital_beds_total = FALSE,				# Set to FALSE to ignore
		STP_id = STP_id,
		STP_fraction_hospital = FALSE,
		STP_amount_inhabitants = STP_amount_inhabitants,	
		STP_amount_hospital_beds = FALSE,											# Set to FALSE to ignore
		compound_load_total = FALSE, 												# [kg / a]
		compound_load_per_capita_and_day = compound_load_per_capita_and_day,		# [g / E d], set to FALSE to ignore
		compound_load_per_hospital_bed_and_day = compound_load_per_hospital_bed_and_day,
		compound_elimination_ara,					# vector or STP-specific matrix with elimination fractions over treatment steps (not percentage values); set to 0 to skip a step 
		compound_excreted = 1,						# fraction excreted and discharged, set to 1 to ignore
		topo_matrix
		
	) # [g / d]
	###############################################
	# concentration
	result_table <- cbind(result_table, 
		"conc_local" = local_discharge * result_table$load_local,
		"conc_cumulated" = local_discharge * result_table$load_cumulated
	)
	###############################################
	# fraction STP discharge  











	###############################################
	# format, export & return #####################
	
	if(is.logical(path_out_xlsx)) return(result_table) else{
	
		done_write <- try({
		
			
		
		
		
		})
		if(class(done_write) == "try-error") stop("Export of results to path_out_xlsx failed. Is this path valid? Is the file open in another software?")
	
	}
}






