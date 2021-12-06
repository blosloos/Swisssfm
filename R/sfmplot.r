


sfmplot <- function(){
	  
	######################################################################################
	shiny::runApp( shinyApp(
		##################################################################################
		ui =  shinyUI(
			fluidPage(
				useShinyalert(),  # Set up shinyalert
				##########################################################################
				fluidRow(
					column(2, 
						HTML('<br><h3>Swisssfm STP map</h3>')
					),
					column(2, 
						HTML('<br>'),
						textInput("path_in_xls", "Path to wrap_vsa result file.xlsx", value = "F:/.../VSA/STP_result_Diclofenac.xlsx"),
						bsPopover("path_in_xls", 
							title = "",
							content = "", 
							placement = "right", trigger = "hover")
					),
					column(1, 
						HTML('<br><br>'),
						bsButton("open_path_in_xls", "Load", style = "success")
					)
				),
				HTML('<hr noshade = "noshade" />'),
				##########################################################################				
				tabsetPanel(			
					tabPanel("Plot",				

						navbarPage("Plot options",
							tabPanel("Hide", HTML('<hr noshade="noshade" />')),
							tabPanel("A", HTML('<br><br>')),
							tabPanel("B", HTML('<br><br>')),
							tabPanel("C", HTML('<br><br>'))							
						),
						plotOutput("STP_plane",
							click = "STP_plane_click",
							dblclick = "STP_plane_dblclick",
							hover = "STP_plane_hover",
							brush = brushOpts(
								id = "STP_plane_brush",
								resetOnNew = TRUE,
								delay = 0
							),                
							height = "700px"
						),					
						HTML('<br><br>')
					),
					tabPanel("STP table",	
						HTML('<hr noshade="noshade" />'),
						HTML('<br><br>')
					)
				)
				##########################################################################	
			)
		),
		##################################################################################

		##################################################################################
		server = function(session, input, output) {

			##############################################################################
			observe({
				input$open_path_in_xls
				if(isolate(input$open_path_in_xls)){	
					
					#sheet_name <- "ARA_Inputdaten_v1"
					
					STP_table <- try({
						openxlsx:::read.xlsx(xlsxFile = isolate(input$path_in_xls), sheet = 1)
					})
					
					if(class(STP_table) == "try-error"){
						shinyalert("Warning", STP_table[[1]], type = "error")
					}else{
					
						if(
							!any(STP_table[6, ] %in% c("STP_ID", "ARANEXTNR", "LageX", "LageY"))
						){
						
							shinyalert("Warning", "Missing columns in result table.", type = "error")
						
						}else{
						
							##############################################################
							names(STP_table) <- STP_table[6, ]
							STP_table <- STP_table[7:nrow(STP_table),, drop = FALSE]
					
					
							##############################################################					
					
						}
					}
				
				}
			})
			##############################################################################
			
			
			
			
			
			
			##############################################################################	
				
				
				
				
				
				
				
				
				
			##############################################################################	

		}
		##################################################################################
	))
	######################################################################################
	
}





