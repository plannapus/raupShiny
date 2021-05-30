library(shiny)
library(shinyWidgets)
library(scatterplot3d)
library(animation)
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

coil_elliptic <- function(rate_of_translation, distance_to_axis, whorl_expansion, shape, turns=1, steps=1000){
  circle <- make_elliptic_generating_shape(distance_to_axis,shape)
  coiling(rate_of_translation,whorl_expansion,circle,turns,steps)
}

dots2polygon <- function(XYZ,steps){
  obj <- as.data.frame(XYZ)
  n_gen <- nrow(obj)/steps
  spl <- split(obj,rep(1:n_gen,each=steps))
  emb <- embed(c(1:n_gen,1),2)
  POLY <- do.call(rbind,apply(emb,1,function(x)rbind(spl[[x[1]]],spl[[x[2]]][steps:1,],c(NA,NA,NA))))
}

ui <- fluidPage(
  titlePanel("Raup Coiling model for Mollusks"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(input="RT",label="Rate of translation (T)", min=0, max=4,value=0),
      sliderInput(input="D",label="Distance to axis (D)", min=0, max=1,value=0.3,step=0.1),
      sliderTextInput(input="W",label="Whorl Expansion (W)", 
                      choices=c(1, 10, 100, 1000, "10 000", "100 000", "1 000 000"), 
                      selected=10, grid = TRUE),
      sliderInput(input="S",label="Shape (S)", min=0.5, max=2,value=1,step=0.5),
      sliderInput(input="turns",label="Number of turns", min=1, max=5,value=3),
    ),
    mainPanel(
      imageOutput("coilgif")
    )
  )
)

server <- function(input,output){
  output$coilgif <-renderImage({
    outfile <- tempfile(fileext='.gif')
    RT <- input$RT
    D <- input$D
    steps <- 100
    W <- as.integer(input$W)
    S <- input$S
    turns <- input$turns
    ce <- coil_elliptic(RT,D,W,S,turns,steps)
    dp <- dots2polygon(ce,steps)
    rg <- sapply(ce,range)
    m <- min(rg[1,])
    M <- max(rg[2,])
    saveGIF({
      for(i in seq(0,360,10)){
        sc3d <- scatterplot3d(ce$X,ce$Y,ce$Z,
                              xlim=c(m,M),ylim=c(m,M),zlim=c(m,M),
                              type="n",angle=i, axis=FALSE, grid=FALSE, box=FALSE)
        tPOLY <- t(apply(dp,1,function(x){S<-sc3d$xyz.convert(x[1],x[2],x[3]);cbind(S$x,S$y)}))
        polygon(tPOLY,col=alpha("red",.5),border=NA)
      }
    },file=outfile,clean=TRUE,ani.height=400,ani.width=400)
    list(src = outfile,
         contentType = 'image/gif',
         width = 400,
         height = 400
    )
  }, deleteFile = FALSE)
}