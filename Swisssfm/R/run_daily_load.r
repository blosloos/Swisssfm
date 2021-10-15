






run_daily_load <- function(

	einwohner_total = 8417700,
	spitalbetten_total = FALSE,
	
	ara_id, 
	ara_anteil_spital,
	ara_anzahl_einwohner,
	ara_anzahl_spitalbetten = FALSE,
	
	stoff_menge_total, 			# [kg / a]
	stoff_elimination_ara, 
	stoff_menge_in_kanal,		# fraction excreted and discharged
	
	topo_matrix
){

	###############################################
	# check inputs & defaults
	if(!all(ara_id %in% colnames(topo_matrix))) stop("Problem in run_daily_load: not all ara_id present in topo_matrix")
	if(!identical(length(ara_id), length(ara_anteil_spital), length(ara_anzahl_einwohner))) stop("Problem in run_daily_load: ara inputs vary in length_1")
	if(isFALSE(ara_anzahl_spitalbetten)) ara_anzahl_spitalbetten <- 0 else if(length(ara_anzahl_spitalbetten) != length(ara_id)) stop("Problem in run_daily_load: ara inputs vary in length_2")
	if(!identical(length(stoff_menge_total), length(stoff_elimination_ara), length(stoff_menge_in_kanal))) stop("Problem in run_daily_load: stoff inputs vary in length")
	if(isFALSE(spitalbetten_total)) spitalbetten_total <- 0
	###############################################
	menge_pro_kopf_und_tag_total <- stoff_menge_total * (1 - ara_anteil_spital) * 1000 / einwohner_total / 365 		# [kg/a] -> [g/d]
	menge_pro_spitalbett_und_tag_total <- stoff_menge_total * (ara_anteil_spital) * 1000 / spitalbetten_total / 365 	# [kg/a] -> [g/d]
	lokal <- ara_anzahl_einwohner * menge_pro_kopf_und_tag_total * stoff_menge_in_kanal * (1 - stoff_elimination_ara)
	lokal <- lokal + ara_anzahl_spitalbetten * menge_pro_spitalbett_und_tag_total * stoff_menge_in_kanal * (1 - stoff_elimination_ara)
	kummuliert <- apply(topo_matrix, MARGIN = 2, function(x){sum(x * lokal)}, y = lokal)
	###############################################
	return(cbind(ara_id, lokal, kummuliert) 
	
}

