library(shiny)
library(shinyWidgets)
library(mwshiny)
library(rgl)

setwd("~/Git/raup_model") #Set the working directory containing the script and the picture folder
addResourcePath(prefix = "static", directoryPath = "static")

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

make_elliptic_generating_shape <- function(D,S,res=100){
  #Let's define the original ray as 1
  a <- 1
  rc <- (D+1)*a/(1-D)
  t<-seq(0,2*pi,by=pi/res)
  b <- a/S
  circle_0 <- cbind(r=rc + a*cos(t), y= b*sin(t), phi=0)
  return(circle_0)
}

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

pt2quad <- function(XYZ,steps,res){
  pts <- as.data.frame(XYZ)
  pts$pt_n<- rep(1:(res*2+1),each=steps)
  pts$step <- 1:steps
  eg <- expand.grid(1:(2*res),1:(steps-1))
  qd <- apply(eg,1,function(x)rbind(pts[pts$step==x[2] & pts$pt_n==x[1],1:3],
                                    pts[pts$step==x[2]+1 & pts$pt_n==x[1],1:3],
                                    pts[pts$step==x[2]+1 & pts$pt_n==x[1]+1,1:3],
                                    pts[pts$step==x[2] & pts$pt_n==x[1]+1,1:3]))
  do.call(rbind,qd)
}

ui_win <- list()

ui_win[["Controller"]] <- fluidPage(
  tags$head(tags$link(rel ="stylesheet", type="text/css", href="static/style.css")),
  tags$head(tags$script(HTML(JS.logify))),
  tags$head(tags$script(HTML(JS.onload))),
  tags$head(tags$script(HTML('Shiny.addCustomMessageHandler("jsCode", function(message) { eval(message.value); });'))),
  sidebarLayout(
    sidebarPanel(
      # sliderTextInput(input="W",label=HTML("<b>Wirbelausdehnungsrate (W)</b><br/><em>Whorl Expansion Rate</em>"), 
      #                 choices=c(0,1,1.1,1.2,1.5,1.9,2,3.2,5,10,20,50,100,200,500,1000,10000,100000), 
      #                 selected=2, grid = TRUE),
      sliderInput(input="W",
                  label=HTML('<b>Wirbelausdehnungsrate (W)</b><br/><em>Whorl Expansion Rate</em><br/>
                             <div class="flex"><div class="left"><img src="static/placeholder.png" width="50"></div>
                             <div class="right"><img src="static/placeholder.png" width="50"></div></div>'),
                  min = 0, max = 5, value = 0.301, step = .0001),
      sliderInput(input="D",label=HTML('<b>Abstand der Muendung von der Wickelachse (D)</b><br/><em>Distance of opening from coiling axis</em><br/>
                             <div class="flex"><div class="left"><img src="static/placeholder.png" width="50"></div>
                             <div class="right"><img src="static/placeholder.png" width="50"></div></div>'), 
                  min=0, max=0.9,value=0.3,step=0.01),
      sliderInput(input="S",label=HTML('<b>Muendungsform (S)</b><br/><em>Shape of opening</em><br/>
                             <div class="flex"><div class="left"><img src="static/placeholder.png" width="50"></div>
                             <div class="right"><img src="static/placeholder.png" width="50"></div></div>'), 
                  min=0.1, max=5,value=1,step=0.01),
      sliderInput(input="RT",label=HTML('<b>Translationsrate (T)</b><br/><em>Rate of translation</em><br/>
                             <div class="flex"><div class="left"><img src="static/placeholder.png" width="50"></div>
                             <div class="right"><img src="static/placeholder.png" width="50"></div></div>'),
                  min=0, max=35,value=2,step=1),
      width=8
    ),
    mainPanel(
      radioButtons(inputId="preselect", label=HTML("Vorauswahlen:<br/><em>Preselections:</em>"), 
                   choiceNames=list(HTML('<div class="radio"><div class="caption"><b>Kein</b><br/><em>None</em>                             <br/></div></div>'),
                                    HTML('<div class="radio">
                                         <div class="img"><img src="static/placeholder.png" width="50"></div>
                                         <div class="caption"><b>Perlboote</b><br/><em>Nautilus</em></div>
                                         </div>'),
                                    HTML('<div class="radio">
                                         <div class="img"><img src="static/placeholder.png" width="50"></div>
                                         <div class="caption"><b>Ammonit</b><br/><em>Ammonite</em></div>
                                         </div>'),
                                    HTML('<div class="radio">
                                         <div class="img"><img src="static/placeholder.png" width="50"></div>
                                         <div class="caption"><b>Turmschnecke</b><br/><em>Tower shell</em></div>
                                         </div>'),
                                    HTML('<div class="radio">
                                         <div class="img"><img src="static/placeholder.png" width="50"></div>
                                         <div class="caption"><b>Weinbergschnecke</b><br/><em>Roman snail</em></div>
                                         </div>'),
                                    HTML('<div class="radio">
                                         <div class="img"><img src="static/placeholder.png" width="50"></div>
                                         <div class="caption"><b>Miesmuschel</b><br/><em>Mussel</em></div>
                                         </div>'),
                                    HTML('<div class="radio">
                                         <div class="img"><img src="static/placeholder.png" width="50"></div>
                                         <div class="caption"><b>Kahnfuesser</b><br/><em>Tusk shell</em></div>
                                         </div>')), 
                   choiceValues=list("nul","nau","amm","tow","rom","mus","tus")),
      width=4)
  )
)

ui_win[["Plot"]] <- fluidPage(
  titlePanel(HTML("Placeholder for title in German - <em>Raup's shell coiling model</em>")),
  rglwidgetOutput("coilrgl",width=800, height=800),
  tableOutput("paramstab")
)

serv_calc <- list()

presel <- data.frame(species=c("nau","amm","tow","rom","mus","tus"),
                     RT=c(0,0,12,2,0.2,0),
                     D=c(0,0.5,0,0,0,0.9),
                     W=c(3.2,1.9,1.2,2,10000,10000),
                     S=c(1.3,1,0.8,0.9,2,1),
                     turns=c(2,5,10,5,1,1))

serv_calc[[1]] <- function(input,session){
  observe({
    session$sendCustomMessage(type='jsCode', list(value = JS.onload))
    x <- input$preselect
    if(!is.null(x)){
      if(x!="nul"){
        ps <- presel[presel$species==x,]
        updateSliderInput(session, "W", value=round(log(ps$W,10),4))
        updateSliderInput(session, "RT", value=ps$RT)
        updateSliderInput(session, "D", value=ps$D)
        updateSliderInput(session, "S", value=ps$S)
        #Need to set turns too
      }
    }
  }
  )
}

serv_out <- list()
serv_out[["coilrgl"]] <- function(input,session){
  renderRglwidget({
    rgl.open(useNULL=TRUE)
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
    steps <- 10*turns
    circle <- make_elliptic_generating_shape(D,S,res)
    ce <- coiling(RT,W,circle,turns,steps,"dextral")
    qd <- pt2quad(ce,steps,res)
    qd <- qd[,c(2,3,1)]
    qd[,2] <- -1*qd[,2]
    qd <- apply(qd,2,function(x)x/max(abs(range(qd$X,qd$Y,qd$Z))))
    if(W>50){
      qd <- qd[apply(qd,1,function(x)any(abs(x)>1e-5)),]
      if(nrow(qd)%%4) qd <- rbind(matrix(0,nrow=4-nrow(qd)%%4,ncol=3),qd)
    }
    mqd <- as.mesh3d(qd,type="quads")
    rgl.viewpoint(zoom = .8, theta=90, phi=30)
    bg3d(color="grey80")
    material3d(color="lightsalmon",emission="black",alpha=1,specular="white",ambient="black",textype="luminance")
    shade3d(mqd,override=TRUE)
    light3d(theta=10,phi=10)
    rglwidget()
  })
}

serv_out[["paramstab"]] <- function(input,session){
  renderTable(data.frame(W=round(10^input$W,2),D=input$D,S=input$S,T=input$RT),
              colnames=TRUE)
}

shinyOptions("launch.browser"=TRUE)
mwsApp(ui_win = ui_win, serv_calc = serv_calc, serv_out = serv_out)
