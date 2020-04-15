library(shiny)
library(ggplot2)
require(ggrepel)
data <- read.csv("expression.csv")

colnames(data) <- c("line", "gene", "ESC.fpkm", "EpiLC.fpkm", "pvalue")
plot_exp <- ggplot(data, aes(ESC.fpkm, EpiLC.fpkm, color=pvalue))+
  geom_point(alpha=0.5)+
  scale_x_log10(breaks = c(1, 100, 10000))+
  scale_y_log10( breaks = c(1, 100, 10000))+
  theme_classic()+
  scale_color_gradient(low="grey60", high="grey90")+
  labs(x="ESC [fpkm]", y= "EpiLC [fpkm]")+
  theme(legend.position = "none",text = element_text(size=15))+
  coord_equal(ratio=1)

ui <- fluidPage(
  # Give the page a title
  titlePanel("Gene Expression in Differentiation"),
  " This data is published in ",
  tags$a(href = "https://pubmed.ncbi.nlm.nih.gov/24905168/", "Buecker et al, Cell Stem Cell, 2014"),
  
  #select a gene
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "goi",
                  label = "Choose your gene",
                  choices = data$gene),
    ),
    mainPanel(
      plotOutput("expressionplot"),
      verbatimTextOutput("stats"),
      downloadButton(outputId = "down_plot", label= "Download plot as .png")
    ))
)

server <- function(input, output) {
  plot_object <- reactive({
    plot_exp +
      geom_point(data= data[data$gene %in% input$goi,], aes(ESC.fpkm, EpiLC.fpkm), color= "firebrick", size =3)+
      geom_text_repel(data= data[data$gene %in% input$goi,], aes(ESC.fpkm, EpiLC.fpkm, label= gene), size = 5, color="black" )
  })
  output$expressionplot <- renderPlot(
    plot_object()
  )
  output$stats <- renderPrint(
    data[data$gene == input$goi, ]
  )
  ####### for download: you have to run in browser (run external), 
  #no not run in R studio, will not work with download
  output$down_plot <- downloadHandler(
    filename = function(){paste(input$goi,'.png',sep='')},
    content = function(file) {
      ggsave(file, plot = plot_object(), device = "png")
    }
  )
}
shinyApp(ui = ui, server = server)