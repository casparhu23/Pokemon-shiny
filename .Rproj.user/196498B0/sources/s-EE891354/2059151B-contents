# Load all necessary packages
library(shiny) # shiny app package
library(ggplot2) # ggplot graph package
library(data.table) # data table package
library(ggridges) # density ridges plot
library(dplyr) # dplyr package
library(plotly) # interactive plot
library(htmlwidgets) # HTML widget 
library(tidyr) # data cleaning tool
library(hrbrthemes) # ipsum package
library(scales) # scale continuous variables
library(fmsb) # make radar chart
library(RColorBrewer) # get more colors
library(summarytools) # summarize data quickly
library(rvest) # work with web scraping
library(magrittr) # structure code 
library(stringr) # string cleaning


pokemon_list <- read.csv("pokemon_list.csv") # read file

link_image <- "https://www.serebii.net/swordshield/galarpokedex.shtml" # pokemon main page

image <- read_html(link_image)%>% #scrape pokemon page
    html_nodes("a[href*='pokedex'] img.stdsprite") %>% # use css selectors
    html_attr("src")

# get the logical operator
imageFinder <- lapply(image, function(x){
    tf <- grepl("pokemon", x)  
})

# get the half link using logical operator
image_list_half <- image[unlist(imageFinder)]

# get the full link
image_list<- paste("https://www.serebii.net",
                   image_list_half, sep="")

# get rid of the duplicate pokemons
pokemon_with_picture <- pokemon_list[(pokemon_list$X!=183) & (pokemon_list$X!=329),]

# put link into dataframe
pokemon_with_picture$image <- image_list

# sort dataframe with alphabetical order
pokemon_with_picture <- pokemon_with_picture[order(pokemon_with_picture$pokemon_name),]


ui <- fluidPage(
    
    title = "Pokemon App", #name of the website
    sidebarLayout(
        sidebarPanel(
            conditionalPanel(
                'input.dataset === "Pokemon List"',
                checkboxGroupInput("show_vars", "Columns in Pokemon to show:",
                                   names(pokemon_list), selected = names(pokemon_list)),
                helpText("Click the column header to sort a column and use search textbox to search"),
            ),
            conditionalPanel(
                'input.dataset === "Pokemon Stats"',
                
                selectInput("var_1", 
                            label = "Choose a primary type to display",
                            choices = list("Grass", "Fire","Water", "Bug", "Normal",
                                           "Flying", "Dark","Electric","Ice", "Ground",
                                           "Psychic","Fighting","Steel","Ghost","Poison",
                                           "Rock","Fairy","Dragon"),
                            selected = "Grass"),
                
                # selectInput("var_2", 
                #             label = "Choose a secondary type to display",
                #             choices = list("Grass", "Fire","Water", "Bug", "Normal",
                #                            "Flying", "Dark","Electric","Ice", "Ground",
                #                            "Psychic","Fighting","Steel","Ghost","Poison",
                #                            "Rock","Fairy","Dragon","None"),
                #             selected = "None"),
                
                helpText("The graph shows all selected primary type
                  Pokemons' stats density ridges graph and traditional boxplot. For example: if you select Grass Type, 
                  a Grass/Dragon type Pokemon Applin will be included as well"),
                
            ),
            conditionalPanel(
                'input.dataset === "Pokemon Scatter"',
                
                selectInput("var_7", 
                            label = "Choose a primary type to display",
                            choices = list("Grass", "Fire","Water", "Bug", "Normal",
                                           "Flying", "Dark","Electric","Ice", "Ground",
                                           "Psychic","Fighting","Steel","Ghost","Poison",
                                           "Rock","Fairy","Dragon"),
                            selected = "Grass"),
                
                
                selectInput("var_3", 
                            label = "Choose a stats type to display as x axis",
                            choices = list("hp","attack","defence","sp_attack","sp_defence","speed"),
                            selected = "hp"),
                selectInput("var_4", 
                            label = "Choose a stats type to display as y axis",
                            choices = list("hp","attack","defence","sp_attack","sp_defence","speed"),
                            selected = "attack"),
                
                helpText("Based on the primary type you chose, this scatter plot graph allows you to analyze further. 
        You can select any two of the six stats to gain insights. And the color of the dots will be based 
        on the secondary type of that pokemon. For example: if you select attack as x axis, and defence as y axis, 
        a scatter plot attack vs defence will show up on the right side.")
                
            ),
            conditionalPanel(
                'input.dataset === "Pokemon Lollipop"',
                
                selectInput("var_8", 
                            label = "Choose a primary type to display",
                            choices = list("Grass", "Fire","Water", "Bug", "Normal",
                                           "Flying", "Dark","Electric","Ice", "Ground",
                                           "Psychic","Fighting","Steel","Ghost","Poison",
                                           "Rock","Fairy","Dragon"),
                            selected = "Grass"),
                
                selectInput("var_5", 
                            label = "Choose a stats type to display",
                            choices = list("hp","attack","defence","sp_attack","sp_defence","speed"),
                            selected = "hp"),
                
                helpText("Based on the primary type you chose, this lollipop plot graph allows you to find out 
        the actual pokemon with one of the stats performance. You can select one of the six stats to gain insights. 
        And the color of the dots will be depended on the secondary type of that pokemon. 
        For example: if you select attack, the list of primary type pokemon will appear on the right side
                 with attack stats shown.")
            ),
            conditionalPanel(
                'input.dataset === "Pokemon Radar"',
                selectInput("var_6",
                            label= "Choose a Pokemon name",
                            choices = pokemon_with_picture$pokemon_name),
                helpText("Based on the Pokemon you chose, all six stats will be shown on the radar chat below along with Pokemon image")
            )
            
        ),
        mainPanel(
            img(src='https://64.media.tumblr.com/b63f515c98bf2e548c9684f131c5a225/0fc0a6df0c93319e-fe/s128x128u_c1/6aea4141b2c205b81c94d272ac40aa4111676ac2.pnj', align = "right"),
            tabsetPanel(
                id = 'dataset',
                tabPanel("Pokemon List", h3("The following data table contains all 402 Galar Region Pokemons from Sword & Shield Pokemon Game Series."), DT::dataTableOutput("mytable1")),
                
                tabPanel("Pokemon Stats", h3("The following density ridges and box plots will be displayed based on the primary type dropdown menu in the left panel."),h4("Density Ridges Plot"),plotOutput("plot"), 
                         h4("Box Plot"), plotOutput("plot_1"),h3("The following is the summary statistics for all Pokemon types"), DT::dataTableOutput("summary")
                         ),
                
                tabPanel("Pokemon Scatter", h3("The following scatter plot will be displayed based on the primary type dropdown menu given two stats options in the left panel."), plotlyOutput("plot_2")),
                
                tabPanel("Pokemon Lollipop",h3("The following lollipop plot will be displayed based on the primary type dropdown menu given one stats option in the left panel."), plotOutput("plot_3")),
                
                tabPanel("Pokemon Radar",h3("The following radar plot will be displayed based on the pokemon name dropdown menu in the left panel."),htmlOutput("picture"),plotOutput("plot_4"),plotOutput("plot_5")) 
            )
        )
    )
)

server <- function(input, output) {
    
    pokemon_list <- read.csv("pokemon_list.csv")
    # choose columns to display 
    
    
    output$mytable1 <- DT::renderDataTable({
        
        pokemon_list_sim <- pokemon_list[,1:13]
        
        DT::datatable(pokemon_list_sim[, input$show_vars, drop = FALSE],options=list(orderClasses=TRUE))
    },height="auto") # sorted columns are colored now because CSS are attached to them
    
    
    output$summary <- DT::renderDataTable({
        
        df_pok <- as.data.frame(summary(pokemon_list[,8:13]))
        df_pok <- df_pok%>%
            separate(col=Freq,
                     into=c("Stats_Name","Value"),
                     sep=":")
        
        summary_stats <- df_pok[,2:4]
        colnames(summary_stats) <- c("Name","Stats","Value")
        
        summary_stats <- summary_stats%>%
            mutate(Value=as.numeric(Value))%>%
            pivot_wider(names_from = "Stats",
                        values_from ="Value")
    
        DT::datatable(summary_stats[,],options=list(orderClasses=FALSE))
        
    },height="auto")
    
    
    output$plot <- renderPlot({
        
        data <- pokemon_list[(pokemon_list$type_1==input$var_1),]
        data <- data %>%
            pivot_longer(cols=hp:speed,   # transform into long table
                         names_to="stats_type",
                         values_to="score")
        
        ggplot(data,aes(y=stats_type, x=score,  fill=stats_type)) +
            geom_density_ridges(alpha=0.3, stat="binline", bins=20) +
            theme_ridges()+theme_minimal()+
            theme(
                legend.position="none",
                panel.spacing = unit(0.1, "lines"),
                #strip.text.x = element_text(size = 15),
                axis.text.x = element_text(size=20),
                axis.text.y = element_text(size=20)
            ) +
            xlab("") +
            ylab("")+
            scale_x_continuous(breaks =c(0,20,40,60,80,100,120,140,160,180,200))
        
    },height=400)
    
    output$plot_1 <- renderPlot({
        
        data <- pokemon_list[(pokemon_list$type_1==input$var_1),]
        data <- data %>%
            pivot_longer(cols=hp:speed,   # transform into long table
                         names_to="stats_type",
                         values_to="score")
        
        ggplot(data,aes(x=score, y=stats_type,fill=stats_type)) +
            geom_boxplot(alpha=0.3) +
            theme_minimal()+
            theme(
                legend.position="none",
                panel.spacing = unit(0.1, "lines"),
                axis.text.x = element_text(size=20),
                axis.text.y = element_text(size=20)
            ) +
            xlab("")+
            ylab("")+
            scale_x_continuous(breaks =c(0,20,40,60,80,100,120,140,160,180,200))
        
    },height="auto")
    
    
    output$plot_2 <- renderPlotly({
        
        data_2 <- pokemon_list[pokemon_list$type_1==input$var_7,]
        x <- reactive({
            data_2[,input$var_3]
        })
        
        y <- reactive({
            data_2[,input$var_4]
        })
        
        lab_x <- list(
            title = input$var_3,
            titlefont = 6
        )
        lab_y <- list(
            title = input$var_4,
            titlefont = 6
        )
        
        
        scatter_plot <- plot_ly(data_2, 
                                x=x(),
                                y=y(),
                                colors="Set1",   #rainbow(18)
                                type = 'scatter',
                                size=5,
                                color=as.factor(data_2$type_2),
                                text = ~paste('Pokemon Name: ', pokemon_name,
                                              '</br>Secodary Type: ', type_2)
                        )
        scatter_plot <- scatter_plot %>%
            layout(xaxis = lab_x, yaxis = lab_y)

                 
    })
    
    output$plot_3 <- renderPlot({
        
        data_2 <- pokemon_list[pokemon_list$type_1==input$var_8,]
        
        data_3 <- data_2%>%
            mutate(pokemon_name=as.factor(pokemon_name))
        
        ggplot(data_3, aes_string(x=data_3$pokemon_name, y=input$var_5,color=as.factor(data_3$type_2)))+
            geom_segment(aes(xend=pokemon_name, yend=input$var_5,color=as.factor(type_2)),size=1.5) +
            geom_point(size=6) +
            theme_light() +
            coord_flip() +theme_minimal()+
            theme(
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_blank(),
                panel.border = element_blank(),
                axis.text.x = element_text(size=15, face="bold"),
                axis.text.y = element_text(size=15, face="bold"),
                axis.title=element_text(size=15,face="bold"),
                aspect.ratio = 1.2,
                legend.title = element_text(size=17,face="bold"),
                legend.text = element_text(size=15),
                axis.ticks.x = element_blank()
            )+xlab("")+ylab("")+labs(color = "Secondary Type")
            
    },height=850)
    
    output$picture <-renderText({
        
        pokemon_with_picture <- pokemon_with_picture%>%
            mutate(image=str_replace_all(image,"/small",""))
        
        pokemon_one <- pokemon_with_picture[pokemon_with_picture$pokemon_name==input$var_6,]
        
        pokemon_link <- pokemon_one$image 
        
            paste(
                '<img src="',
                pokemon_link,
                '"',',style="display: block; margin-left: auto; margin-right: auto;">',
            sep = '')
        })
    
    
    
    
    output$plot_4 <- renderPlot({
        
        
        link_image <- "https://www.serebii.net/swordshield/galarpokedex.shtml"
        
        image <- read_html(link_image)%>%
            html_nodes("a[href*='pokedex'] img.stdsprite") %>%
            html_attr("src")
        
        
        imageFinder <- lapply(image, function(x){
            tf <- grepl("pokemon", x)  # names refer to the names of the table
        })
        
        image_list_half <- image[unlist(imageFinder)]
        
        #swordshield/pokemon/small/810.png
        
        image_list<- paste("https://www.serebii.net",
                           image_list_half, sep="")
        
        pokemon_with_picture <- pokemon_list[(pokemon_list$X!=183) & (pokemon_list$X!=329),]
        
        pokemon_with_picture$image <- image_list
        
        pokemon_with_picture <- pokemon_with_picture[order(pokemon_with_picture$pokemon_name),]
        
        data_4 <- pokemon_with_picture[pokemon_with_picture$pokemon_name==input$var_6,]
        
        data_4 <- data_4 %>%
            select(3,8:13)
        rownames(data_4) <- data_4$pokemon_name
        data_4 <- data_4[,-1]
        
        max_min <- data.frame(
            hp = c(250, 0), attack = c(250, 0), defence = c(250, 0),
            sp_attack = c(250, 0), sp_defence = c(250, 0), speed = c(250, 0)
        )
        rownames(max_min) <- c("Max", "Min")
        data_5 <- rbind(max_min,data_4)
        
        radarchart(data_5, 
                   #custom polygon
                   axistype=1,
                   plwd=2, # line widths for plot data
                   pcol=4, # color of the data point
                   #custom the grid
                   cglcol="grey", # line color 
                   cglty=1,  # line type
                   axislabcol="black", # axis label color
                   caxislabels=seq(0,240,40), # axis label numbers
                   cglwd=2, # line width for radar grids
                   maxmin=T, 
                   seg=6, # the number of segment for each axis
                   pangle = 45, # default angle
                   vlcex=2.2, # font size for variable name labels
                   calcex = 1.1, # axis label size 
                   centerzero = TRUE, # start with zero
                   pfcol=scales::alpha("red",0.7), # color for filling polygons
                   plty=1, # line types for plot data
                   pty=16)
        
    },height = "auto")
    
    
    output$plot_5 <-renderPlot({
        
        pokemon_with_picture <- pokemon_with_picture[order(pokemon_with_picture$pokemon_name),]
        
        data_4 <- pokemon_with_picture[pokemon_with_picture$pokemon_name==input$var_6,]
        
        data_4 <- data_4 %>%
            select(3,8:13)
        rownames(data_4) <- data_4$pokemon_name
        data_4 <- data_4[,-1]
        
        data_4 <- data_4 %>%
            pivot_longer(cols=hp:speed,   # transform into long table
                         names_to="stats_type",
                         values_to="score")
        
        ggplot(data = data_4, aes(x = input$var_6, y= score)) + 
            geom_bar(aes(group =stats_type, fill =stats_type),alpha=0.3,
                     stat = "identity", 
                     position = "dodge")+
            coord_flip()+
            theme_minimal()+
            scale_y_continuous(breaks =c(0,10,20,30,40,50,60,70,80,90,100,
                                         110,120,130,140,150,160,170,180,190,200,
                                         210,220,230,240,250))+
            theme(panel.grid.major.x = element_blank(),
                  panel.grid.major.y = element_blank(),
                  panel.border = element_blank(),
                  panel.spacing = unit(0.1, "lines"),
                  axis.text.x = element_text(size=15,face="bold"),
                  axis.text.y = element_text(size=15,face="bold"),
                  legend.title = element_text(size=17,face="bold"),
                  legend.text = element_text(size=15),
                  axis.ticks.x = element_blank()
                  )+
            xlab("")+ylab("")
            
    },height = "auto")
    
    
}

shinyApp(ui, server)
