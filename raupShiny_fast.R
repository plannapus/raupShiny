library(shiny)
library(shinyWidgets)
library(mwshiny)
library(rgl)
library(shinyjs)

setwd("~/Git/raup_model") #Set the working directory containing the script and the picture folder
addResourcePath(prefix = "static", directoryPath = "static")

#Function to make the aperture shape
make_elliptic_generating_shape <- function(D,S,res=100){
  #Let's define the original ray as 1
  a <- 1
  rc <- (D+1)*a/(1-D)
  t<-seq(0,2*pi,by=pi/res)
  b <- a/S
  circle_0 <- cbind(r=rc + a*cos(t), y= b*sin(t), phi=0)
  return(circle_0)
}

#Function to coil the shape around the axis
coiling <- function(RT,W,generating_shape, turns,steps,dir="dextral"){
  PHI <- seq(0,2*pi*turns,length=steps)
  far_end <- generating_shape[1,1]
  closest_end <- approx(generating_shape[generating_shape[,1]<far_end-1,2],
                        generating_shape[generating_shape[,1]<far_end-1,1],0)$y
  D <- closest_end/far_end
  rc <- (D+1)/(1-D)
  rho <- function(theta, W, r0) r0 * W^(theta/(2*pi))
  y <- function(y0,W,theta,rc,T) y0 * W^(theta/(2*pi)) + rc*T*(W^(theta/(2*pi))-1)
  circle <- apply(generating_shape,1,
                  function(x)lapply(PHI,
                                    function(theta)cbind(r=rho(theta, W, x['r']),
                                                         y=y(x['y'],W,theta, rc,RT),
                                                         phi=theta)))
  circle <- do.call(rbind,lapply(circle,function(x)do.call(rbind,x)))
  
  #To cartesian coordinates
  if(dir=="dextral"){
    XYZ <- list(X = circle[,1] * sin(circle[,3]),
                Y = circle[,1] * cos(circle[,3]),
                Z = circle[,2])
  }else{
    XYZ <- list(X = circle[,1] * cos(circle[,3]),
                Y = circle[,1] * sin(circle[,3]),
                Z = circle[,2])
  }
  XYZ
}

#Function to transform the final shape into polygons
pt2quad <- function(pts,steps,res){
  pts$pt_n<- rep(1:(res*2+1),each=steps)
  pts$step <- 1:steps
  eg <- expand.grid(1:(2*res),1:(steps-1))
  apply(eg,1,function(x)c(which(pts$step==x[2] & pts$pt_n==x[1]),
                          which(pts$step==x[2]+1 & pts$pt_n==x[1]),
                          which(pts$step==x[2]+1 & pts$pt_n==x[1]+1),
                          which(pts$step==x[2] & pts$pt_n==x[1]+1)))
}

JS.logify <-"function logifySlider (sliderId) {
    // regular number style
    $('#'+sliderId).data('ionRangeSlider').update({
      'prettify': function (num) { return (Math.pow(10, num).toFixed(2)); }
    })
}"
JS.onload <-"$(document).ready(function() {
  setTimeout(function() {
    logifySlider('W')
  }, 5)})"

# Controller
ui_win <- list()
ui_win[["Controller"]] <- fluidPage(
  useShinyjs(),
  tags$head(tags$link(rel ="stylesheet", type="text/css", href="static/style.css")),
  tags$head(tags$script(HTML(JS.logify))),
  tags$head(tags$script(HTML(JS.onload))),
  sidebarLayout(
    sidebarPanel(
      sliderInput(input="W",
                  label=HTML('<b>Windungsexpansionsrate (W)</b><br/><em>Whorl Expansion Rate</em><br/>
                             <div class="flex"><div class="left"><img src="static/W1.png" height="50"></div>
                             <div class="right"><img src="static/W2.png" height="50"></div></div>'),
                  min = 0, max = 5, value = 0.301, step = .0001),
      sliderInput(input="D",label=HTML('<b>Nabelöffnung (D)</b><br/><em>Umbilicus opening</em><br/>
                             <div class="flex"><div class="left"><img src="static/D1.png" height="50"></div>
                             <div class="right"><img src="static/D2.png" height="50"></div></div>'), 
                  min=0, max=0.9,value=0.3,step=0.01),
      sliderInput(input="S",label=HTML('<b>Muendungsform (S)</b><br/><em>Shape of opening</em><br/>
                             <div class="flex"><div class="left"><img src="static/S1.png" height="50"></div>
                             <div class="right"><img src="static/S2.png" height="50"></div></div>'), 
                  min=0.1, max=5,value=1,step=0.01),
      sliderInput(input="RT",label=HTML('<b>Translationsrate (T)</b><br/><em>Rate of translation</em><br/>
                             <div class="flex"><div class="left"><img src="static/RT1.png" height="50"></div>
                             <div class="right"><img src="static/RT2.png" height="50"></div></div>'),
                  min=0, max=35,value=2,step=0.1),
      hr(),
      noUiSliderInput(inputId="phi", label=HTML('<div class="orientation"><b>Ausrichtung</b><br/><em>Orientation</em></div>'), min=-90,max=90,value=0,
                      update_on="change", color="#428bca", step=1, format = wNumbFormat(decimals = 0),
                      pips=list(mode="positions",values=c(0,50,100),density=18)),
      div(HTML('<div class="flex"><div class="left"><b>unter</b><br/><em>below</em></div>
          <div class="middle"><b>Muendung</b><br/><em>Opening</em></div>
          <div class="right"><b>ober</b><br/><em>above</em></div></div>')),
      width=9
    ),
    mainPanel(
      radioButtons(inputId="preselect", label=HTML("Vorausgewählte Formen<br/><em>Preselected shapes</em>"), 
                   choiceNames=list(HTML('<div class="radio"><div class="caption"><b>Kein</b><br/><em>None</em>                             <br/></div></div>'),
                                    HTML('<div class="radio">
                                         <div class="img"><img src="static/nau.png" width="50"></div>
                                         <div class="caption"><b>Perlboote</b><br/><em>Nautilus</em></div>
                                         </div>'),
                                    HTML('<div class="radio">
                                         <div class="img"><img src="static/amm.png" width="50"></div>
                                         <div class="caption"><b>Ammonit</b><br/><em>Ammonite</em></div>
                                         </div>'),
                                    HTML('<div class="radio">
                                         <div class="img"><img src="static/tow.png" width="50"></div>
                                         <div class="caption"><b>Turmschnecke</b><br/><em>Tower shell</em></div>
                                         </div>'),
                                    HTML('<div class="radio">
                                         <div class="img"><img src="static/rom.png" width="50"></div>
                                         <div class="caption"><b>Weinbergschnecke</b><br/><em>Roman snail</em></div>
                                         </div>'),
                                    HTML('<div class="radio">
                                         <div class="img"><img src="static/mus.png" width="50"></div>
                                         <div class="caption"><b>Miesmuschel</b><br/><em>Mussel</em></div>
                                         </div>'),
                                    HTML('<div class="radio">
                                         <div class="img"><img src="static/tus.png" width="50"></div>
                                         <div class="caption"><b>Kahnfuesser</b><br/><em>Tusk shell</em></div>
                                         </div>')), 
                   choiceValues=list("nul","nau","amm","tow","rom","mus","tus")),
      width=3)
  )
)

# Plot page
ui_win[["Plot"]] <- fluidPage(
  tags$head(tags$link(rel ="stylesheet", type="text/css", href="static/style.css")),
  titlePanel(HTML("Schalenwindungsmodel nach Raup<br/><em>Raup's shell coiling model</em>")),
  htmlOutput("paramstab"),
  rglwidgetOutput("coilrgl",width="80vw", height="90vh"),
)


#Calculations that are not part of the output
serv_calc <- list()

#Preselections:
presel <- data.frame(species=c("nau","amm","tow","rom","mus","tus"),
                     RT=c(0,0,12,2,0.2,0),
                     D=c(0,0.5,0,0,0,0.9),
                     W=c(3.2,1.9,1.2,2,10000,10000),
                     S=c(1.3,1,0.8,0.9,2,1),
                     turns=c(2,5,10,5,1,1))

serv_calc[[1]] <- function(input,session){
  session$onFlushed(function()runjs("logifySlider('W');"),once=FALSE) #Necessary for the log scale to still be displayed as log
  observeEvent(input$preselect,{
    x <- input$preselect
    if(!is.null(x)){
      if(x!="nul"){
        ps <- presel[presel$species==x,]
        updateSliderInput(session, "W", value=round(log(ps$W,10),4))
        updateSliderInput(session, "RT", value=ps$RT)
        updateSliderInput(session, "D", value=ps$D)
        updateSliderInput(session, "S", value=ps$S)
      }
    }
  }
  )
}

# RGL Output
serv_out <- list()
serv_out[["coilrgl"]] <- function(input,session){
  # Function to produce the shape
  mqd <- reactive({
    RT <- input$RT
    D <- input$D
    res <- 20
    W <- round(10^input$W,2)
    S <- input$S
    turns <- 5
    if(W<1.5 & RT>2) turns <- 10
    if(D>0.5) turns <- 10
    if(D<0.1 & RT==0) turns <- 2
    if(W>100) turns <- 1
    steps <- ifelse(turns>5,150,75)
    circle <- make_elliptic_generating_shape(D,S,res)
    ce <- coiling(RT,W,circle,turns,steps,"dextral")
    CE <- as.data.frame(ce)
    CE <- CE[c(2,3,1)]
    CE[,2] <- -1*CE[,2]
    qd <- pt2quad(CE,steps,res)
    mesh3d(CE,quads=qd)
    })
  # Orientation
  phi <- reactive(input$phi)
  # Actual plotting
  renderRglwidget({
    rgl.open(useNULL=TRUE)
    m <- mqd()
    rgl.viewpoint(zoom = 1, theta=0, phi=phi())
    bg3d(color="grey80")
    material3d(color="lightsalmon",emission="black",alpha=1,specular="white",ambient="black",textype="luminance")
    shade3d(m,override=TRUE)
    light3d(theta=10,phi=10)
    rglwidget()
  })
}

# The table showing the chosen params
serv_out[["paramstab"]] <- function(input,session){
  renderUI(HTML(sprintf("W=%s<br/>D=%s<br/>S=%s<br/>T=%s", round(10^input$W,2),input$D,input$S,input$RT)))
}

# Launch the app
mwsApp(ui_win = ui_win, serv_calc = serv_calc, serv_out = serv_out)
