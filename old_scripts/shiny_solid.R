library(shiny)
library(shinyWidgets)
#For rgl part
library(rgl)

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

uirgl <- fluidPage(
  titlePanel("Raup Coiling model for Mollusks"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(input="RT",label="Rate of translation (T)", min=0, max=10,value=2,step=0.1),
      sliderInput(input="D",label="Distance to axis (D)", min=0, max=1,value=0.3,step=0.1),
      sliderTextInput(input="W",label="Whorl Expansion (W)", 
                      choices=c(1.1, 1.2, 1.5, 2, 5, 10, 20, 50, 100, 1000, 10000), 
                      selected=2, grid = TRUE),
      sliderInput(input="S",label="Shape (S)", min=0.1, max=3,value=1,step=0.1),
      sliderInput(input="turns",label="Number of turns", min=1, max=10,value=3),
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
    res <- 20
    W <- as.numeric(input$W)
    S <- input$S
    turns <- input$turns
    steps <- 20*turns
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
    mqd <- as.mesh3d(qd,triangles=FALSE)
    rgl.viewpoint(zoom = .8, theta=90, phi=30)
    bg3d(color="grey80")
    material3d(color="lightsalmon",emission="black",alpha=1,specular="white",ambient="black",textype="luminance")
    shade3d(mqd,override=TRUE)
    light3d(theta=10,phi=10)
    rglwidget()
  })
}

shinyApp(ui = uirgl, server = serverrgl,options=list("launch.browser"=TRUE))