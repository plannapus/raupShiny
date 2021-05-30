library(shiny)
library(shinyWidgets)
#For rgl part
#library(shinyRGL)
library(scales)
library(rgl)

make_elliptic_generating_shape <- function(D,S){
  #Let's define the original ray as 1
  a = 1
  rc = (D+1)*a/(1-D)
  t<-seq(0,2*pi,by=pi/100)
  b = a/S
  circle_0 = cbind(r=rc + a*cos(t), y= b*sin(t), phi=0)
  return(circle_0)
}

coiling <- function(RT,W,generating_shape, turns,resolution){
  PHI = seq(0,2*pi*turns,length=resolution)
  far_end = generating_shape[1,1]
  closest_end = approx(generating_shape[generating_shape[,1]<far_end-1,2],
                       generating_shape[generating_shape[,1]<far_end-1,1],0)$y
  D = closest_end/far_end
  rc = (D+1)/(1-D)
  rho = function(theta, W, r0) r0 * W^(theta/(2*pi))
  y = function(y0,W,theta,rc,T) y0 * W^(theta/(2*pi)) + rc*T*(W^(theta/(2*pi))-1)
  circle = apply(generating_shape,1,
                 function(x)lapply(PHI,
                                   function(theta)cbind(r=rho(theta, W, x['r']),
                                                        y=y(x['y'],W,theta, rc,RT),
                                                        phi=theta)))
  circle = do.call(rbind,lapply(circle,function(x)do.call(rbind,x)))
  
  #To cartesian coordinates
  XYZ = list(X = circle[,1] * cos(circle[,3]),
             Y = circle[,1] * sin(circle[,3]),
             Z = circle[,2])
  XYZ
}

uirgl <- fluidPage(
  titlePanel("Raup Coiling model for Mollusks"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(input="RT",label="Rate of translation (T)", min=0, max=4,value=2),
      sliderInput(input="D",label="Distance to axis (D)", min=0, max=1,value=0.3,step=0.1),
      sliderTextInput(input="W",label="Whorl Expansion (W)", 
                      choices=c(1, 2, 5, 10, 20, 50, 100, 1000, 10000), 
                      selected=2, grid = TRUE),
      sliderInput(input="S",label="Shape (S)", min=0.5, max=2,value=1,step=0.5),
      sliderInput(input="turns",label="Number of turns", min=1, max=5,value=3),
    ),
    mainPanel(
      rglwidgetOutput("coilrgl",width=800, height=800)
    )
  )
)

serverrgl <- function(input,output){
  output$coilrgl <-renderRglwidget({
    rgl.open(useNULL=TRUE)
    RT <- input$RT
    D <- input$D
    steps <- 1000
    W <- as.integer(input$W)
    S <- input$S
    turns <- input$turns
    circle <- make_elliptic_generating_shape(D,S)
    ce <- coiling(RT,W,circle,turns,steps)
    rg <- sapply(ce,range)
    m <- min(rg[1,])
    M <- max(rg[2,])
    rgl.viewpoint(zoom = .75)
    bg3d(color = "grey80")
    plot3d(ce$X,ce$Y,ce$Z,col=alpha("red",0.1),size=0.5,
                    xlim=c(m,M),ylim=c(m,M),zlim=c(m,M),xlab="",ylab="",zlab="",axes=FALSE)
    rglwidget()
  })
}

shinyApp(ui = uirgl, server = serverrgl,options=list("launch.browser"=TRUE))
#shinyApp(ui=ui,server=server)