#check out shinyFiles package for folder selection
#https://github.com/thomasp85/shinyFiles
#https://stackoverflow.com/questions/36619989/loading-data-files-with-shinyfiles
install.packages("shinyFiles")
library(shiny)
library(shinyFiles)

##########################
#TAKE 3
##########################
devtools::source_url("https://raw.githubusercontent.com/MarcusRowcliffe/CTtracking/master/CTtracking.r")

ui <- fluidPage(
  shinyDirButton("folder", "Folder select", "Please select a folder"),
  verbatimTextOutput("directorypath"),
)

server <- function(input, output) {
  volumes <- getVolumes()()
  shinyDirChoose(input, "folder", roots=volumes, allowDirCreate=FALSE)
  dir <- reactiveValues(path=volumes[1])
  observeEvent(input$folder, 
               dir$path <- normalizePath(parseDirPath(volumes, input$folder), "/")
  )
  
  output$directorypath <- renderText({
    dir$path
  })
}

shinyApp(ui, server)

##########################
#TAKE 2
##########################
library(exiftoolr)
exiftoolr::install_exiftool()

ui <- fluidPage(
  titlePanel("Dynamically generated user interface components"),
  textInput("input_text", "Input directory", width="100%"),
  fluidRow(
    
    column(3, wellPanel(
      # This outputs the dynamic UI component
      uiOutput("ui")
    )),
    
    column(7,
           tableOutput("content")
    ),
    
    column(1,
           tags$p("Choice"),
           verbatimTextOutput("choice")
    )
    
  )
)

server <- function(input, output) {
  
  dat1 <- reactive({
    dir <- gsub("\\", "/", input$input_text, fixed=TRUE)
    files <- list.files(dir, ".jpg", ignore.case=TRUE, full.names=TRUE, recursive=TRUE)
    if(length(files)==0) NULL else
      exiftoolr::exif_read(files[1])
  })
  
  output$ui <- renderUI({
    if(is.null(dat1))
      return()
    checkboxGroupInput("columns", "Pick columns", names(dat1()))
  })
  
  output$content <- renderTable({
    data.frame(Column=names(dat1()), Content=as.character(dat1()))
  }, spacing="xs")
  
  output$choice <- renderPrint({
    str(input$columns)
  })
}

shinyApp(ui, server)


##########################
#TAKE 1
##########################

ui <- fluidPage(
  mainPanel(
    textInput("inp", "whatever", width="100%"),
    fileInput("finp", "Pick an image file"),
    uiOutput("ColSel"),
    tableOutput("out")
  ))
server <- function(input, output){
  dir <- reactive({
    gsub("\\", "/", input$inp, fixed=TRUE)
  })
  
  files <- reactive({
    list.files(dir(), ".jpg", ignore.case=TRUE, full.names=TRUE, recursive=TRUE)
  })
  
  cols <- reactive({
    if(dir.exists(dir())){
      dat1 <- exiftoolr::exif_read(files()[1])
      paste0(names(dat1), " [", dat1, "]")
      } #else
#        "Can't find that folder"
  })
  
  output$ColSel <- renderUI({
    if(dir.exists(dir())){
      dat1 <- exiftoolr::exif_read(files()[1])
      lbls <- paste0(names(dat1), " [", dat1, "]")
      checkboxGroupInput("ColInpt", "Choose columns", lbls)
    }
  })
  
  dat <- reactive({
    if(dir.exists(dir())){
      res <- exiftoolr::exif_read(files(), tags=c("FileName", "FileCreateDate", "Keywords"))
      res$Keywords <- unlist(lapply(res$Keywords, paste, collapse=", "))
      res <- res[,-1]
    } else
        res <- "Can't find that folder"
    res
  })
  
  output$out <- renderTable(output$ColSel())
}
shinyApp(ui, server)

pth <- "C:/Users/rowcliffe.m/Documents/OneDriveZSL/CameraTrapping/Images/DevonPics/DCIM/100RECNX"
fl <- "C:/Users/rowcliffe.m/Documents/OneDriveZSL/CameraTrapping/Images/DevonPics/DCIM/100RECNX/IMG_0001.JPG"
exiftoolr::exif_read(pth, recursive=TRUE)
names(exiftoolr::exif_read(fl))
file.exists(pth)
dir.exists(pth)
file.exists(fl)
dir.exists(fl)
list.files(pth)
kw <- exiftoolr::exif_read(pth, recursive=TRUE, tags=c("Keywords"))
unlist(lapply(kw$Keywords, paste, collapse=", "))




