

if(FALSE){

	ARA_table <- read.csv2(file = "D:/VSA/new_inputs/ARA_input_corrected.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE) # -> aus .xls in .csv umwandeln!
	#ARA <- read.csv2(file = "E:/VSA/new_inputs/ARA_input_corrected.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

	#STP_id <- as.character(ARA$BAFU_Abgabehoehe_2021_kurz_2.ARANR)
	#STP_id_next <- as.character(ARA$ARANEXTNR)
	#STP_amount_inhabitants <- as.numeric(gsub(".", "", as.character(ARA$Eang_2021), fixed = TRUE))

	#STP_discharge <- as.numeric(ARA$Q347I)

	#STP_discharge[STP_discharge < 0 | is.na(STP_discharge)] <- mean(STP_discharge[STP_discharge > 0 & !is.na(STP_discharge)])

	compound_name <- "Diclofenac"

	compound_load_per_capita_and_day <- 540 * 1E-6 # Diclofenac

	compound_load_per_hospital_bed_and_day <- 0

	compound_elimination_STP <- matrix(ncol = 1, nrow = nrow(ARA_table),  .9)
	compound_elimination_STP[ARA_table$MikroV %in% c("", "nicht vorhanden")] <- 0

	path_out_xlsx <- "D:/VSA/new_outputs"
	overwrite <- TRUE
	
	
	STP_discharge_per_capita <- 400
	
}

wrap_vsa <- function(

	ARA_table = NULL,							# Must be a data.frame if provided, overwrites all of the below STP_ arguments
	
	STP_id = NULL,
	STP_id_next = NULL,
	STP_amount_inhabitants = NULL,
	STP_local_discharge_river = NULL,			# discharge in river at STP
	STP_discharge_per_capita = 400,				# [l / E d]
	STP_amount_people_local = NULL,				# amount of people at STP
	
	compound_name,
	compound_load_total = FALSE, 				# [kg / a]
	compound_load_per_capita_and_day,			# [g / E d], set to FALSE to ignore
	compound_load_per_hospital_bed_and_day = 0,	# [g / E d], set to FALSE to ignore
	compound_elimination_STP = NULL,			# vector with elimination fractions over treatment steps (not percentage values); set to 0 to skip a step 
	compound_excreted = 1,						# fraction excreted and discharged, set to 1 to ignore
	
	add_columns_from_ARA_table = c("ARANEXTNR", "LageX", "LageY"),
	path_out_xlsx = FALSE,						# if FALSE, return data.frame
	overwrite = TRUE
	
){

	###############################################
	# -> if available, get all inputs from ARA_table
	if(!is.null(ARA_table) & !is.data.frame(ARA_table)) stop("ARA_table must be either NULL or a dataframe")
	if(!is.null(ARA_table) & is.data.frame(ARA_table)){
	
		# all required columns available?
		cols_required <- c("BAFU_Abgabehoehe_2021_kurz_2.ARANR", "ARANEXTNR", "Eang_2021", "Q347I", "MikroV", "EINWOHNER")
		if(any(is.na(match(cols_required, names(ARA_table))))){
			these_missing <- paste(cols_required[is.na(match(cols_required, names(ARA_table)))], collapse = ",")
			stop(paste0("ARA_table is missing these columns: ", these_missing))
		}
	
		# extract data from table
		STP_id <- as.character(ARA_table$BAFU_Abgabehoehe_2021_kurz_2.ARANR)
		STP_id_next <- as.character(ARA_table$ARANEXTNR)
		STP_amount_inhabitants <- as.numeric(gsub(".", "", as.character(ARA_table$Eang_2021), fixed = TRUE))
		STP_local_discharge_river <- as.numeric(ARA_table$Q347I)
		STP_local_discharge_river[STP_local_discharge_river < 0 | is.na(STP_local_discharge_river)] <- 
			mean(STP_local_discharge_river[STP_local_discharge_river > 0 & !is.na(STP_local_discharge_river)])
		STP_amount_people_local <- ARA_table$EINWOHNER
		
		
		
	}else{
	
		if(is.null(STP_id)) stop("For ARA_table = NULL, STP_id must be provided as function argument.")
		if(is.null(STP_id_next)) stop("For ARA_table = NULL, STP_id_next must be provided as function argument.")	
		if(is.null(STP_amount_inhabitants)) stop("For ARA_table = NULL, STP_amount_inhabitants must be provided as function argument.")
		if(is.null(STP_local_discharge_river)) stop("For ARA_table = NULL, STP_local_discharge_river must be provided as function argument.")
		if(is.null(STP_amount_people_local)) stop("For ARA_table = NULL, STP_amount_people_local must be provided as function argument.")
		
		#if(is.null()) stop("For ARA_table = NULL,  must be provided as function argument.")
	
	}
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
		insert_id_in_topo_matrix = FALSE
	
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
		compound_elimination_STP,					# vector or STP-specific matrix with elimination fractions over treatment steps (not percentage values); set to 0 to skip a step 
		compound_excreted = 1,						# fraction excreted and discharged, set to 1 to ignore
		topo_matrix
		
	) # [g / d]
	###############################################
	# concentration ###############################
	result_table <- cbind(result_table, 
		"conc_local" = STP_local_discharge_river * result_table$load_local,
		"conc_cumulated" = STP_local_discharge_river * result_table$load_cumulated
	)
	###############################################
	# fraction STP discharge ######################
	sewage_discharge_local <- STP_amount_people_local * STP_discharge_per_capita / 24 / 60 / 60 				# convert to [l/s]
	STP_amount_people_cumulated <- apply(topo_matrix, MARGIN = 2, function(x, y){sum(x * y)}, y = STP_amount_people_local)
	sewage_discharge_cumulated <- STP_amount_people_cumulated * STP_discharge_per_capita / 24 / 60 / 60 	# convert to [l/s]
	fract_STP_discharge_local <- STP_local_discharge_river / sewage_discharge_local
	fract_STP_discharge_cumulated <- STP_local_discharge_river / sewage_discharge_cumulated
	result_table <- cbind(result_table, 
		"fract_STP_discharge_local" = fract_STP_discharge_local,
		"fract_STP_discharge_cumulated" = fract_STP_discharge_cumulated
	)
	###############################################
	# format, export & return #####################	
	if(is.logical(path_out_xlsx)) return(result_table) else{
		if(file.exists(path_out_xlsx) & !overwrite) stop("File at path_out_xlsx already exists, and overwrite is set to FALSE")
	
		# add more STP infos to result_table
		use_cols <- match(add_columns_from_ARA_table, names(ARA_table))
		use_rows <- match(ARA_table[, "BAFU_Abgabehoehe_2021_kurz_2.ARANR"], result_table[, "STP_ID"])
		result_table <- cbind(
			"STP_ID" = result_table[, "STP_ID"], 
			ARA_table[use_rows, use_cols], 
			result_table[, names(result_table) != "STP_ID"]
		)
		result_table <- rbind(
			rep("", ncol(result_table)),
			rep("", ncol(result_table)),
			rep("", ncol(result_table)),
			rep("", ncol(result_table)),
			names(result_table),
			result_table
		)
		names(result_table) <- NULL
		result_table[2, 2] <- paste0("Compound name: ", compound_name)
		#result_table[2, 4] <- compound_name
		#result_table[3, 2] <- "Elimination:"
		#result_table[3, 4] <- compound_elimination_STP	
		done_write <- try({
		
			wb <- openxlsx:::createWorkbook()	
			openxlsx:::addWorksheet(wb, compound_name)
			openxlsx:::writeData(wb, compound_name, result_table, startCol = 2, startRow = 3, rowNames = FALSE)
			
			openxlsx:::saveWorkbook(wb, file = file.path(path_out_xlsx, paste0("STP_result_", compound_name, ".xlsx")), overwrite = TRUE)
		
		})
		if(class(done_write) == "try-error") stop("Export of results to path_out_xlsx failed. Is this path valid? Is the file open in another software?")
	
	}
	###############################################	
	
}






