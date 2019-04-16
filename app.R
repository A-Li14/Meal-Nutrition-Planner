require(shinydashboard)
require(shiny)
require(data.table)
require(readxl)
require(ggplot2)

###Build a dashboard where you can build a meal by selecting ingredients and amounts of those ingredients. 
###Give options to select a recommended serving size or by weight (in grams)

###Note taking############
#1) selectizeInput enables you to select multiple options and also remove them easily. This is very desirable.
#However, having multiple selects prevents you from choosing the quantity and units using the designed interface. 
#One solution is to change the interface so that you can select quantity and units in the cart
#
#2) How does reactiveValues work? Why can't I just define a normal variable for custom_filters and have
#the search react to changes in that variable? Actually, I wasn't able to add data to the generically defined
#custom_filters variable either for some reason. 
###app####

#ds = data.table(read_excel("ABBREV.XLSX"))
source("tidy data.R")

search_restrict_translation = data.table(UI_Option=c("Raw Food","Fresh","Meat"),Translation=c("RAW","FRSH","MEAT"))

ui <- dashboardPage(
    dashboardHeader(title="Build a Meal"),
        
    
    # tags$head(
    #     tags$style(HTML("
    #         .multicol {
    # 
    #             -webkit-column-count: 3; /* Chrome, Safari, Opera */
    # 
    #             -moz-column-count: 3; /* Firefox */
    # 
    #             column-count: 3;
    # 
    #         }
    #     "))
    # ),
    
    
    dashboardSidebar(
        sidebarMenu(
            menuItem("Main",tabName="menu"),
            menuItem("Additional Options",tabName="options")
            #sidebarSearchForm(textId = "search2",buttonId="search2_button",label="Search...")
        ),
        #sidebarLayout(
        #sidebarPanel(
          
        #),
        #mainPanel(
        uiOutput("search_foods"),
        
        ###Filter results for the search using predefined categories and custom filters
        checkboxGroupInput(inputId="limit_search",label="Narrow Food Options",
                           choices=c("Raw Food","Fresh","Meat")),
        
        ###UI for adding food to your cart
        numericInput(inputId="food_quantity",label="Quantity of Food",value=1),
        ###The selectInput should react to the food item selected and display the portions 
        ###suggested in GmWt_Desc1 and 2
        #selectInput(inputId="food_units",label="Portion Size",choices=c("100 grams","Portion Type 1","Portion Type 2")),
        uiOutput("portion_ui"),
        actionButton(inputId="add_food",label="Add Food"),
        
        actionButton(inputId="remove_food",label="Remove Food")
        #)
                
            #)
    ),
    
    dashboardBody(
        tabItems(  
            ###Search Bar to enter food items
            ###requires button to add new food item, buttons to select food quantities or manually input
            ###Add a button to search only for raw food
            tabItem(tabName="menu",
                fluidRow(
                    # box(title="Controls",
                    #     
                    #     #selectizeInput(inputId="search",label="Food Search",choices=ds[,Shrt_Desc]),
                    #     uiOutput("search_foods"),
                    #     
                    #     ###Filter results for the search using predefined categories and custom filters
                    #     checkboxGroupInput(inputId="limit_search",label="Narrow Food Options",
                    #                        choices=c("Raw Food","Fresh","Meat")),
                    #     textInput(inputId="limit_search_free_text",label="Custom Filters"),
                    #     actionButton(inputId="add_custom_filter",label="Add Custom Filter"),
                    #     actionButton(inputId="remove_custom_filters",label="Reset Custom Filters"),
                    #     
                    #     
                    #     ###UI for adding food to your cart
                    #     numericInput(inputId="food_quantity",label="Quantity of Food",value=0),
                    #     ###The selectInput should react to the food item selected and display the portions 
                    #     ###suggested in GmWt_Desc1 and 2
                    #     #selectInput(inputId="food_units",label="Portion Size",choices=c("100 grams","Portion Type 1","Portion Type 2")),
                    #     uiOutput("portion_ui"),
                    #     actionButton(inputId="add_food",label="Add Food"),
                    #     
                    #     actionButton(inputId="remove_food",label="Remove Food")
                    # ),
                    box(title="Additional Search Options",
                        textInput(inputId="limit_search_free_text",label="Custom Filters"),
                        actionButton(inputId="add_custom_filter",label="Add Custom Filter"),
                        actionButton(inputId="remove_custom_filters",label="Reset Custom Filters"),
                        textOutput("print_filters")
                    )
                ),
                
                fluidRow(
                    box(title="Nutrition Data",
                        #dataTableOutput("nutrient_table"),
                        tableOutput("nutrient_table"),
                        plotOutput("nutrient_plot")
                    )
                )
            ),
            
            tabItem(tabName="options",
                fluidRow(
                    box(width=8,
                        h4("Nutrients to Display"),
                      # tags$div(class = "multicol", checkboxGroupInput(inputId="nutrients",label="Nutrient List",inline=T,
                      #                                                 choices=nutrient_list,
                      #                                                 selected=c("Carbohydrt_(g)","Protein_(g)","Lipid_Tot_(g)","Cholestrl_(mg)","Fiber_TD_(g)","Sugar_Tot_(g)")))
                      # 
                        # checkboxGroupInput(inputId="nutrients",label="Nutrient List",inline=T,
                        #                    choices=nutrient_list,
                        #                    selected=c("Carbohydrt_(g)","Protein_(g)","Lipid_Tot_(g)","Cholestrl_(mg)","Fiber_TD_(g)","Sugar_Tot_(g)"))
                        # 
                        splitLayout(
                            checkboxGroupInput(inputId="nutrients_1",label="",choices=nutrient_list1,
                                             selected=c("Energ_Kcal","Lipid_Tot_(g)","FA_Sat_(g)","FA_Mono_(g)","FA_Poly_(g)","Cholestrl_(mg)",
                                                        "Sodium_(mg)","Carbohydrt_(g)","Fiber_TD_(g)","Sugar_Tot_(g)","Protein_(g)")
                            ),
                            checkboxGroupInput(inputId="nutrients_2",label="",choices=nutrient_list2,
                                             selected=""
                            )
                        )
                      )
                  )
              )    
          )
            
            
          
          
            ###Totals for key macronutrients
            
            
            ###ggplot, stacked bars to show the contribution of different food items to nutritional value
          
          
    )
)

server <- function(input, output) {
    #cart = data.table(Food=character(),Quantity=integer(),Unit=character())
    #custom_filter = character()
    
    sessionVars = reactiveValues(cart=data.table(Index=numeric(),Food=character(),Quantity=integer(),Unit=character()),
                                 custom_filter = character())
    
    observeEvent(input$add_food, {
        ###Search cart for the same food item and units
        #if(input$search%in%cart$Food) {
        if(input$search%in%sessionVars$cart$Food) {
            if(sessionVars$cart[Food==input$search,Unit]==input$food_units) {
                sessionVars$cart[Food==input$search,Quantity:=Quantity+input$food_quantity]
            }
        }
        ###Otherwise, add a new item to the cart
        else {
            add_item = data.table(Index=ds[,which(Shrt_Desc==input$search)],Food=ds[Shrt_Desc==input$search,Shrt_Desc],Quantity=input$food_quantity,Unit=input$food_units)
            sessionVars$cart = rbind(sessionVars$cart,add_item)
        }
    }) 
    
    output$portion_ui = renderUI(selectInput(inputId="food_units",label="Portion Size",choices=c("100 grams",unlist(ds[Shrt_Desc==input$search,c(GmWt_Desc1,GmWt_Desc2)]))))
    
    
    ###Search####
    
    ###Custom filters
    observeEvent(input$add_custom_filter,{
        print(paste("adding custom filter",input$limit_search_free_text))
        print(class(input$limit_search_free_text))
        sessionVars$custom_filter = c(sessionVars$custom_filter,input$limit_search_free_text)
        #custom_filter = c(custom_filter,input$limit_search_free_text)
        print(sessionVars$custom_filter)
    })
    observeEvent(input$remove_custom_filters,{
        print("reseting custom filter")
        sessionVars$custom_filter=character()
    })
    
    
    search_restrictions = reactive({
        search_restrict_translation[UI_Option%in%input$limit_search,Translation]
    })
    
    restricted_ds = reactive({
        restrictions = c(search_restrictions(),toupper(sessionVars$custom_filter))
        print(restrictions)
        ind = numeric()
        
        if(length(restrictions>0)) {
            filter_inds = sapply(restrictions,function(x){grep(x,ds$Shrt_Desc)})
            print(paste(length(restrictions),length(filter_inds)))
            
            filter_inds = table(unlist(filter_inds))
            ind = as.numeric(names(filter_inds)[which(filter_inds==length(restrictions))])
            print(length(ind))
        }
        else{
            ind=1:nrow(ds)
        }
        # x = which.min(sapply(filter_inds,length))
        # filter_inds[[x]]
        ds[ind,]
      
    })
    
    output$search_foods = renderUI(selectizeInput(inputId="search",label="Food Search",choices=restricted_ds()[,Shrt_Desc]))
    output$print_filters = renderText(paste(sessionVars$custom_filter,collapse=", "))
    
    ###Nutrition Data
    
    nutrition = reactive({
        #ind = which(ds$Shrt_Desc%in%cart$Food)
        req(nrow(sessionVars$cart)>0)
      
        multiplier = sapply(1:nrow(sessionVars$cart),function(x){
            if(ds[sessionVars$cart[x,Index],GmWt_Desc1]==sessionVars$cart[x,Unit]) {
                #"GmWt_Desc1"
                ds[sessionVars$cart[x,Index],GmWt_1]/100
            } else {
                #"GmWt_Desc2"
                ds[sessionVars$cart[x,Index],GmWt_2]/100
            }
        })
        
        
        # print("cart")
        # print(sessionVars$cart)
        # 
        # print("nutrition")
        # print(ds[sessionVars$cart[,Index],mget(input$nutrients)])
        # 
        # print(cart[,Index])
        # 
        # print(multiplier)
        # 
        
        cbind(sessionVars$cart[,.(Food,Quantity,Unit)],ds[sessionVars$cart[,Index],mget(c(input$nutrients_1,input$nutrients_2))]*multiplier)
        
    })
    
    nutrition_tall = reactive({
      tall = data.table(nutrition())
      tall[,Food_and_Quantity:=paste(Quantity,"\"",Unit,"\"",Food)]
      tall[,c("Quantity","Unit","Food"):=NULL]
      setcolorder(tall,c(ncol(tall),1:(ncol(tall)-1)))
      t(tall)
    })
    
    nutrition_long = reactive({
        print("nutrition()")
        print(nutrition())

        print("cart")
        print(sessionVars$cart)

        melt(nutrition(),id.vars=c("Food","Quantity","Unit"),variable.name="Nutrient",value.name="Value")
    })
    
    
    output$nutrient_table = renderTable(nutrition())
    #output$nutrient_table = renderDataTable(nutrition())
    output$nutrient_plot = renderPlot(ggplot(nutrition_long(),aes(x=Nutrient,y=Value))+geom_bar(aes(fill=Food),stat="identity")+theme(axis.text.x=element_text(angle=60,vjust=.25,hjust=.2)))
    
}

shinyApp(ui, server)



