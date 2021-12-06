


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
					column(4, 
						HTML('<br>'),
						textInput("path_in_csv", "Path to wrap_vsa result file.csv", value = "F:/.../VSA/STP_result_Diclofenac.csv", width = '500px'),
						bsPopover("path_in_csv", 
							title = "",
							content = "", 
							placement = "right", trigger = "hover")
					),
					column(2, 
						HTML('<br>'),
						selectInput("csv_sep", "csv separator",
						choices = c("; (semicolon)" = ";", ", (comma)" = ",", "tab delimited" =  "\t", "white space" = ""), selected = ";")
					),				
					column(1, 
						HTML('<br><br>'),
						bsButton("open_path_in", "Load", style = "success")
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
							height = "700px",
							width  = "1300px"
						),					
						
						
						HTML('<br><br>')
					),
					tabPanel("STP table",	
						HTML('<hr noshade="noshade" />'),
						
						DT::dataTableOutput('STP_table_output'),
						
						
						HTML('<br><br>')
					)
				)
				##########################################################################	
			)
		),
		##################################################################################

		##################################################################################
		server = function(session, input, output) {

			plot_lim <- reactiveValues()
			plot_lim$xlim <- NULL
			plot_lim$ylim <- NULL

			##############################################################################
			observe({
				input$open_path_in
				if(isolate(input$open_path_in)){	
				
					path_in <- isolate(input$path_in_csv)
					if(grepl(".xlsx", path_in, fixed = TRUE)){
						STP_table <- try({
							openxlsx:::read.xlsx(xlsxFile = path_in, sheet = 1)
						})
					}else{
						STP_table <- try({
							read.csv(file = path_in, sep = isolate(input$csv_sep), stringsAsFactors = FALSE)
						})
					}
					
#STP_table <<- STP_table					
					
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
							STP_table$LageX <- as.numeric(STP_table$LageX)
							STP_table$LageY <- as.numeric(STP_table$LageY)
							##############################################################			
							output$STP_plane <- renderPlot({

								plot.new()
								plot.window(xlim = range(STP_table$LageX), ylim = range(STP_table$LageY))
								points(STP_table$LageX, STP_table$LageY, pch = 19, col = "black")

cat("\nI AM IN HERE_0")

							})
							##############################################################
							output$STP_table_output <- DT::renderDataTable({
								DT::datatable(STP_table)
							})
							##############################################################					
					
						}
					}
				
				}
			})
			##############################################################################
			

			
			##############################################################################	
							
			observeEvent(input$STP_plane_dblclick, {
				
				brush <- isolate(input$STP_plane_brush)
				if(!is.null(brush)) {
					plot_lim$xlim <- c(brush$xmin, brush$xmax)
					plot_lim$ylim <- c(brush$ymin, brush$ymax)
				}else{
					plot_lim$xlim <- NULL
					plot_lim$ylim <- NULL
				}
			
print(isolate(plot_lim$xlim))
print(isolate(plot_lim$ylim))

cat("\nI AM IN HERE_1")
				
			}, ignoreInit = TRUE)
							
			##############################################################################				
			observe({		
			
				plot_lim$xlim
				#plot_lim$ylim
				
				if(isolate(input$open_path_in_xls)){
					
					output$STP_plane <- renderPlot({
					
						
						if(is.null(isolate(plot_lim$xlim))) x_lim <- range(STP_table$LageX) else x_lim <- isolate(plot_lim$xlim)
						if(is.null(isolate(plot_lim$ylim))) y_lim <- range(STP_table$LageY) else y_lim <- isolate(plot_lim$ylim)				
						
print(x_lim)
print(y_lim)
cat("\nI AM IN HERE_2")
						
						plot.new()
						plot.window(xlim = x_lim, ylim = y_lim)
						points(STP_table$LageX, STP_table$LageY, pch = 19, col = "black")



					})
					
				}


				
			})				
			##############################################################################	

		}
		##################################################################################
	))
	######################################################################################
	
}





