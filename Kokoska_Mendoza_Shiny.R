# Load data
main_frame_all <- read.csv("/Users/Stephanie/Desktop/JPSM/Fall2019/Data_Display/Project/Final Files/main_frame_all.csv",header=TRUE)

# Loading packages
library(RSocrata)
library(tidyverse)
library(shiny)
library(DT)
library(scales)

# Data prep
names(main_frame_all)[names(main_frame_all) == "CAT2_anthropology"] <- "Anthropology"
names(main_frame_all)[names(main_frame_all) == "CAT2_arts.humanities"] <- "Arts_Humanities"
names(main_frame_all)[names(main_frame_all) == "CAT2_business.econ"] <- "Business_Economics"
names(main_frame_all)[names(main_frame_all) == "CAT2_computer.science"] <- "Computer_Science"
names(main_frame_all)[names(main_frame_all) == "CAT2_decision.science"] <- "Decision_Sciences"
names(main_frame_all)[names(main_frame_all) == "CAT2_development"] <- "Development"
names(main_frame_all)[names(main_frame_all) == "CAT2_education"] <- "Education"
names(main_frame_all)[names(main_frame_all) == "CAT2_energy.engineering"] <- "Energy_Engineering"
names(main_frame_all)[names(main_frame_all) == "CAT2_health.social.science"] <- "Health_Social_Sciences"
names(main_frame_all)[names(main_frame_all) == "CAT2_library.info"] <- "Library_Information_Sciences"
names(main_frame_all)[names(main_frame_all) == "CAT2_linguistics.language"] <- "Linguistics_Language_Communications"
names(main_frame_all)[names(main_frame_all) == "CAT2_math"] <- "Math"
names(main_frame_all)[names(main_frame_all) == "CAT2_medicine"] <- "Medicine"
names(main_frame_all)[names(main_frame_all) == "CAT2_mental.health"] <- "Mental_Health"
names(main_frame_all)[names(main_frame_all) == "CAT2_neuroscience"] <- "Neuroscience"
names(main_frame_all)[names(main_frame_all) == "CAT2_nursing.health"] <- "Nursing_Health_Professions"
names(main_frame_all)[names(main_frame_all) == "CAT2_philosophy"] <- "Philosophy"
names(main_frame_all)[names(main_frame_all) == "CAT2_political.science"] <- "Political_Science"
names(main_frame_all)[names(main_frame_all) == "CAT2_psychology"] <- "Psychology"
names(main_frame_all)[names(main_frame_all) == "CAT2_science"] <- "Science"
names(main_frame_all)[names(main_frame_all) == "CAT2_social.sciences"] <- "Social_Sciences"
names(main_frame_all)[names(main_frame_all) == "CAT2_urban"] <- "Urban_Studies_Geography_Planning"

category_vars <- list("Anthropology",
                      "Arts_Humanities",
                      "Business_Economics",
                      "Computer_Science",
                      "Decision_Sciences",
                      "Development",
                      "Education",
                      "Energy_Engineering",
                      "Health_Social_Sciences",
                      "Library_Information_Sciences",
                      "Linguistics_Language_Communications",
                      "Math",
                      "Medicine",
                      "Mental_Health",
                      "Neuroscience",
                      "Nursing_Health_Professions",
                      "Philosophy",
                      "Political_Science",
                      "Psychology",
                      "Science",
                      "Social_Sciences",
                      "Urban_Studies_Geography_Planning")

category_labels <- list("Anthropology",
                        "Arts and Humanities",
                        "Business and Economics",
                        "Computer Science",
                        "Decision Sciences",
                        "Development",
                        "Education",
                        "Energy and Engineering",
                        "Health Social Science",
                        "Library and Information Sciences",
                        "Linguistics Language and Communication",
                        "Mathematics and Applied Mathematics",
                        "Medicine",
                        "Mental Health",
                        "Neuroscience",
                        "Nursing and Health Professions",
                        "Philosophy",
                        "Political Science",
                        "Psychology",
                        "Science",
                        "Social Sciences",
                        "Urban Studies and Geography Planning")


# Define UI
ui <- fluidPage(
  
  # Sidebar layout with input and output definitions 
  sidebarLayout(
    
    # Inputs
    sidebarPanel(
      
      # Select DOI of interest
      selectInput(inputId = "main_doi", 
                  label = "Seminal Article",
                  choices = list("Campbell (1950)" = 1, 
                                 "Carley (1993)" = 2, 
                                 "Conway (2005) " = 3,
                                 "Trochim (1989)" = 5), 
                  selected = 1),
      
      # Select tag word of interest
      checkboxGroupInput(inputId = "tag_word_of_interest", 
                         label = "Field of Study",
                         choiceNames = category_labels,
                         choiceValues = category_vars,
                         selected="Anthropology"),
      
      # Year slider
      sliderInput(inputId="year_slider",
                  label = "Range of Years",
                  min=1950,
                  max=2019,
                  value=c(1950,2019)),
      
      # Citation count slider
      sliderInput(inputId="citation_count_slider",
                  label = "Range of Citation Counts",
                  min=0,
                  max=50,
                  value=c(0,10)),
      
      # Cumulative Citation count slider
      sliderInput(inputId="citation_count_slider_c",
                  label = "Range of Cumulative Citation Counts",
                  min=0,
                  max=500,
                  value=c(0,100))
      
    ),
    
    # Outputs
    mainPanel(
      
      
      tabsetPanel(type = "tabs",
                  tabPanel("Counts", plotOutput("plot1")),
                  tabPanel("Cumulative Counts", plotOutput("plot2"))
      ),
      
      br(), br(),
      dataTableOutput(outputId = "table"),
      dataTableOutput(outputId = "table2")
    )
  )
)





# Define server function
server <- function(input, output) {
  
  # Create dotplot object
  output$plot1 <- renderPlot({
    plot_data1 <- 
      main_frame_all %>% 
      gather(field, citations, Anthropology:Urban_Studies_Geography_Planning) %>% 
      filter(citations == 1) %>%
      group_by(year, field, citations, main_doi) %>% 
      tally() %>%
      filter(main_doi == input$main_doi)
    
    plot_data <- plot_data1[plot_data1$field %in% input$tag_word_of_interest,]
    
    ggplot(data=plot_data) + 
      geom_point(aes(x=year, y=n, color=field),size=3) +
      ggtitle("Citation Counts of Seminal Article Over Time") +
      ylab("Citation Count") +
      xlab("Year") +
      xlim(input$year_slider) +
      ylim(input$citation_count_slider) +
      theme_bw() +
      labs(color="Field",shape="Field",size="Field") +
      theme(plot.title = element_text(size=20, face="bold"), axis.title.x = element_text(size=14), axis.title.y = element_text(size=14))
    
  })
  
  
  
  # Create plot 2 (cumulative)
  output$plot2 <- renderPlot({
    plot_data1 <- 
      main_frame_all %>% 
      gather(field, citations, Anthropology:Urban_Studies_Geography_Planning) %>% 
      filter(citations == 1) %>%
      group_by(year, field, citations, main_doi) %>% 
      tally() %>%
      filter(main_doi == input$main_doi)
    
    plot_data2 <-
      plot_data1 %>%
      arrange(field,year) %>%
      group_by(field) %>%
      mutate(cumulative = cumsum(n)) 
    
    plot_data_c <- plot_data2[plot_data2$field %in% input$tag_word_of_interest,]
    
    ggplot(data=plot_data_c) + 
      geom_point(aes(x=year, y=cumulative, color=field,shape=field),size=3) +
      geom_line(aes(x=year,y=cumulative,group=field,color=field),alpha=.4,size=1.5) +
      ggtitle("Cumulative Citation Counts of Seminal Article Over Time") +
      ylab("Cumulative Citation Count") +
      xlab("Year") +
      xlim(input$year_slider) +
      ylim(input$citation_count_slider_c) +
      theme_bw() +
      labs(color="Field",shape="Field",size="Field") +
      theme(plot.title = element_text(size=20, face="bold"), axis.title.x = element_text(size=14), axis.title.y = element_text(size=14))
    
    
  })
  
  
  
  # Create data table
  output$table <- renderDataTable({
    
    plot_data1 <- 
      main_frame_all %>%
      filter(main_doi == input$main_doi)
    
    for (i in 1:length(input$tag_word_of_interest)) {
      if (i==1) {
        plot_data <- plot_data1 %>% filter(get(input$tag_word_of_interest[i])==1) 
      } else {
        plot_data <- rbind(plot_data,plot_data1 %>% filter(get(input$tag_word_of_interest[i])==1))
      }
    }
    
    table_data <- plot_data %>% select(title,author,year,citation_count)
    for (i in 1:length(input$tag_word_of_interest)) {
      column <- NULL
      column <- plot_data %>% select(input$tag_word_of_interest[i])
      table_data <- cbind(table_data,column)
    }
    
    datatable(data = table_data %>% arrange(year), 
              options = list(pageLength = 10), 
              rownames = FALSE,
              caption="Article Information")
  }) 
  
  
  
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)


