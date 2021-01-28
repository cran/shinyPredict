#'
#' Makes shiny application for predictions
#'
#' @param models List of models for prediction. All models must have same y-variable and same class. Classes "lm", "glm", and "coxph" are allowed.
#' If list is named, names are used as titles.
#' @param data data.frame for prediction. Must include union of variables used in models.
#' @param path character string of path where shiny application is added
#' @param info html-file added in Info-tab, if missing creation date is shown
#' @param title Title for application
#' @param shinytheme Theme used in application
#' @note All variables must be numeric or factors.
#' @note Function creates files 'app.R', 'ModelInfo.html', and 'Models.RData' to path folder
#' Prediction are made by opening file 'app.R' in Rstudio and running it.
#' Select model in left sidebar panel. Select also x-variable values for prediction.
#' For continuous variable you may select range using slider input.
#' In Plot-tab, select variable for x-axis (in coxph-models only time-variable is possible).
#' You may choose number of predictions calculate in using slider input below plot.
#' Predictions with 95% confidence interval are shown.
#' In Data-tab data and predicted values are show.
#' Summary-tab shows the summary of current model.
#' Info-tab show additional information provided by user.
#' @note Actual data is not loaded to path folder.  'model' and 'x' are removed from lm-models.
#' It is advised to use 'model = FALSE,y=FALSE' for coxph-models,
#'  and 'x = FALSE,y=FALSE, model=FALSE' for lm- and glm-models.
#' @note In top of left sidebar panel is text input area with title 'Add plot script'.
#'  You may add styling for plot using ggplot style, e.g. '+ggtitle("Solar")+theme_bw()'
#'  adds title to plot and changes theme. Press 'SUBMIT' after typing text.
#' @concept shiny
#' @concept predict
#' @author Jari Haukka \email{jari.haukka@@helsinki.fi}
#' @return character string of path where shiny application is added
#' @export
#' @examples
#' if(interactive()){
#' library(survival)
#' data(lung)
#' lung$sex<-factor(lung$sex)
#' lung$ph.ecog<-factor(lung$ph.ecog)
#'
#' tmp.m3<-coxph(Surv( time , status )~sex+age,data=lung,model = FALSE,y=FALSE)
#' tmp.m4<-coxph(Surv( time , status )~sex+age+ph.ecog,data=lung,model = FALSE,y=FALSE)
#'
#' shinyPredict(models=list("Model 1"=tmp.m3,tmp.m4),
#'                  data=lung[,c("time","status","sex","age","ph.ecog")],path = "./",
#'                  title="Predicting lung cancer mortality")
#'
#'
#' library(splines)
#' data("airquality")
#' airquality$Month.f<-factor(airquality$Month)
#' airquality$Solar.R.f<-cut(airquality$Solar.R,3)
#'
#' tmp.m0<-glm(Ozone~Solar.R+Temp,data=airquality,x = FALSE,y=FALSE, model=FALSE)
#' tmp.m1<-glm(Ozone~Solar.R+Temp+Month.f,data=airquality,x = FALSE,y=FALSE,model=FALSE)
#' tmp.m2<-update(tmp.m1,~.-Temp+ns(Temp,knots = c(72,79,85)))
#' tmp.m2A<-update(tmp.m1,~.-Solar.R+Solar.R.f)
#' tmp.m2B<-update(tmp.m2,~.+Solar.R.f:ns(Temp,knots = c(72,79,85)))
#'
#' print(shinyPredict(models=list("Simple model"=tmp.m0,
#'                                    tmp.m1,
#'                                    "Model with splines"=tmp.m2,
#'                                    "Model with two factors"=tmp.m2A,
#'                                    "Model with interaction"=tmp.m2B),
#'                        data=airquality[,c("Ozone","Solar.R","Temp","Month.f","Solar.R.f")],
#'                        path = "./",
#'                        title="Predicting Ozone",shinytheme="paper"))
#'
#' }
shinyPredict<-function(models,data,path,info,title="Predictions",shinytheme="cerulean"){

  # Check models and remove data from model
  lv.check.models<-function(mallit){

    lv.2<-sapply(mallit,function(x){as.character(formula(x))[2]})
    if(length(unique(lv.2))!=1)stop("All model must have same dependent variable.")
    lv.2<-sapply(mallit,function(x){class(x)[1]})
    if(length(unique(lv.2))!=1)stop("All model must be of same class.")


    lv.1<-lapply(mallit,function(x){
      x$data<-NULL
      # if(class(x)=="coxph"){x$model<-x$model[,1,drop=FALSE]}
      x$x<-NULL
      x$data<-NULL
      x
    })
    lv.1
  }

  ulos.mallit<-lv.check.models(models)

  # Initial values, used with "coxph"-class
  lv.time<-""
  lv.status<-""

  # Check if models are of class "coxph", make indicator for it
  lv.i.coxph<-unique((class(ulos.mallit[[1]])%in%"coxph"))


  # Possible X-variables in prediction
  # Only time varianble is possible  for coxph models
  if(lv.i.coxph){
    # lv.xvars<-gsub(x=strsplit(x=strsplit(x=as.character(formula(ulos.mallit[[1]]))[2],split = "\\(")[[1]][2],
    #                           split=",")[[1]][1],pattern = ")",replacement = "")
    lv.xvars<-names(data)
    lv.status<-gsub(x=gsub(x=strsplit(x=as.character(formula(ulos.mallit[[1]]))[2],split = ",")[[1]][2],pattern = " ",
                           replacement = "",fixed = TRUE),pattern = ")",replacement = "",fixed = TRUE)
    lv.time<-gsub(x=gsub(x=strsplit(x=as.character(formula(ulos.mallit[[1]]))[2],split = ",")[[1]][1],pattern = "Surv",
                replacement = "",fixed = TRUE),pattern = "(",replacement = "",fixed = TRUE)


    lv.xvars<-lv.xvars[!(lv.xvars%in%lv.status)]
  }
  else {
    lv.xvars<-names(data)
    lv.xvars<-lv.xvars[!grepl(as.character(formula(ulos.mallit[[1]]))[2],lv.xvars)]
    }


  # Generate code for data generation for predictions

  ulos.code.data.gen<-lapply(lv.xvars,function(x){
    if(is.factor(data[[x]])){
      lv.0<-attributes(data[[x]])
      lv.1<-paste0(x,'=factor(input$',x,',levels=c(',
                   paste('"',lv.0$levels,'"',sep="",collapse = ","),'))')
      return(lv.1)
    }
    lv.0<-range(data[[x]],na.rm=TRUE)
    if(lv.i.coxph & x!=lv.time){
      return(paste0(x,'=input$',x,'[1]'))
    }

    lv.1<-paste0(x,'=seq(from=input$',x,'[1],to=input$',x,'[2],length=ifelse(input$',x,'[1]==input$',x,'[2],1,input$PredN))')
    lv.1
  })

  # Generate code for user interface
  # lv.model.class<-class(ulos.mallit[[1]])
  ulos.code.UI<-lapply(lv.xvars,function(x){
    if(is.factor(data[[x]])){
      lv.0<-attributes(data[[x]])
      lv.1<-paste0('checkboxGroupInput(inputId="',x,
                   '", label="',x,
                   '",\n choices=c(',paste('"',lv.0$levels,'"="',lv.0$levels,'"',sep="",collapse = ","),
                   '), selected = "',lv.0$levels[1],'")')
      return(lv.1)
    }
    lv.0<-range(data[[x]],na.rm=TRUE)
    if(lv.i.coxph){
      if(x==lv.time){
        lv.1<-paste0('sliderInput(inputId="',x,'",',
                     'label="',x,
                     '", min =0,max =',lv.0[2],' ,value =c(0,',lv.0[2],'))')
      }
      else{
        lv.1<-paste0('sliderInput(inputId="',x,'",',
                   'label="',x,
                   '", min =',lv.0[1],',max =',lv.0[2],' ,value =c(',lv.0[1],'))')
      }
    }
    else{
      lv.1<-paste0('sliderInput(inputId="',x,'",',
                 'label="',x,
                 '", min =',lv.0[1],',max =',lv.0[2],' ,value =c(',lv.0[1],",",lv.0[1],'))')
    }
    lv.1

  })

  lv.ulos<-list(models=ulos.mallit,
       xvars=lv.xvars,
       code.data.gen=paste(sapply(ulos.code.data.gen,paste),collapse=",\n"),
       code.UI=paste(sapply(ulos.code.UI,paste),collapse=",\n"))

  # Add shiny code
  lv.tmp.osa.1<-c("# Osa 1 alku", "#", "# This is a Shiny web application. You can run the application by clicking",
               "# the 'Run App' button above.", "#", "# Find out more about building applications with Shiny here:",
               "#", "#    http://shiny.rstudio.com/", "#", "library(shiny)",
               "library(ggplot2)",
               "library(DT)",
               "library(survival)",
               "library(splines)",
               "library(shinythemes)",
               "library(ggthemes)",
               "library(Epi)",
               "# Load list of models",
               "load(file=\"models.RData\")",
               "lv.malli.value<-as.character(seq(tmp.mallit))",
               "names(lv.malli.value)<-sapply(tmp.mallit,function(x){as.character(formula(x))[3]})",
               "# Time variable name",
               "lv.i.coxph<-sum(class(tmp.mallit[[1]])%in%'coxph')",
               'lv.time<-gsub(x=strsplit(x=strsplit(x=as.character(formula(tmp.mallit[[1]]))[2],split = "\\\\(")[[1]][2],
                          split=",")[[1]][1],pattern = "\\\\)",replacement = "")',
               "# X-variable names",
               "tmp.otsikko<-names(tmp.mallit)",
               "if(is.null(tmp.otsikko)){Models.otsikko<-paste0(\"Model \",seq(length(tmp.mallit)))}",
               "if(!is.null(tmp.otsikko)){Models.otsikko<-ifelse(tmp.otsikko==\"\",paste0(\"Model \",seq(length(tmp.mallit))),tmp.otsikko)}",
               "# Osa 1 loppu")

  lv.tmp.osa.1A<-c("# Osa 1A alku", "# Data frame containing predictions", "tee.dt<-function(input){",
                "    lv.dt<-expand.grid(", "# Osa 1A loppu")



  lv.tmp.osa.2<-c("# Osa 2 alku", "    )",
               "    lv.class<-class(tmp.mallit[[as.numeric(input$Model)]])",
               "    if(sum(lv.class%in%\"coxph\")>0){",
               "        lv.pred<-predict(tmp.mallit[[as.numeric(input$Model)]],newdata=lv.dt,se.fit = TRUE,type=\"survival\")",
               "    }",
               "    if(sum(lv.class%in%c(\"lm\",\"glm\"))>0){lv.pred<-predict(tmp.mallit[[as.numeric(input$Model)]],newdata=lv.dt,se.fit = TRUE,type=\"response\")}",
               "    lv.dt$Predicted<-lv.pred$fit",
               "    lv.dt$se.fit<-lv.pred$se.fit",
               "    lv.dt$Predicted.lo<-lv.pred$fit-1.96*lv.pred$se.fit",
               "    lv.dt$Predicted.hi<-lv.pred$fit+1.96*lv.pred$se.fit",
               "    if(sum(lv.class%in%\"coxph\")>0){",
               "         lv.dt$Predicted.lo<-ifelse(lv.dt$Predicted.lo<0,0,lv.dt$Predicted.lo)",
               "     }",
               "    lv.dt",
               "}",
               "# Define UI for application that draws a histogram",
               "ui <- fluidPage(theme = shinytheme(",
               paste0('"',shinytheme,'"),'),
               "    # Application title",
               "    titlePanel(",
               paste0('"',title,'"),'),
               "    # Sidebar with a slider input", "    sidebarLayout(",
               "        sidebarPanel(",
               "            textAreaInput(\"AddPlot\",label=\"Add plot script\",value=\"\",rows=3),",
               "            actionButton(\"Toteuta\", label=\"Submit\"),",
               "            radioButtons(\"Model\",label=\"Select model\",",
               "                             choices=lv.malli.value,inline=FALSE),",
               "            # Osa 2 loppu"
  )

  lv.tmp.osa.3<-c("            # Osa 3 alku",
               #"downloadButton(\"downloadPred\", \"Download predictions\")",
               "        ),",
               "        mainPanel(",
               "            tabsetPanel(id='tabs',",
               "                tabPanel(\"Plot\", list(radioButtons(\"Xvar\",label=\"Select x-variable\",",
               "                                                    choices=lv.xvars,inline=TRUE,selected=ifelse(lv.i.coxph,lv.time,lv.xvars[1])),",
               "                                       plotOutput(\"distPlot\"),",
               "            sliderInput(inputId=\"PredN\",label=\"Number of points\", min =10,max =200 ,value =10))),",
               "                tabPanel(\"Data\", dataTableOutput(\"Data\")),")
  lv.tmp.osa.3A<-c("")
  lv.tmp.osa.3B<-c(
    "                tabPanel(\"Summary\", verbatimTextOutput(\"Summary\")),",
    "                tabPanel(\"Info\",htmlOutput(\"Info\"))",
    "        )", "    )", "))", "# Define server logic required to draw a histogram",
    "server <- function(input, output, session) {",
    "",

    "    output$distPlot <- renderPlot({", "        tmp.dt<-tee.dt(input)",
    "        lv.group<-sapply(tmp.dt,function(x){is.factor(x)&(length(unique(x))>1)})",
    "        lv.group<-names(lv.group)[lv.group]",
    "        if(!is.factor(tmp.dt[[input$Xvar]])&length(lv.group)>0){",
    "            lv.txt<-paste(\"lv.p1<-ggplot(tmp.dt,aes(x=\",input$Xvar,",
    "                          \",y=Predicted,group=\",lv.group[1],\"))+geom_line(aes(linetype=\",lv.group[1],\"))\")",
    "        }",
    "        else {lv.txt<-paste(\"lv.p1<-ggplot(tmp.dt,aes(x=\",input$Xvar,\",y=Predicted))+geom_line()\")}",
    "        eval(parse(text=lv.txt))",
    "        if(is.factor(tmp.dt[[input$Xvar]])) lv.p1<-lv.p1+geom_point()",
    "        if(is.factor(tmp.dt[[input$Xvar]])){lv.p1<-lv.p1+geom_errorbar(aes(ymin = tmp.dt$Predicted.lo, ymax = tmp.dt$Predicted.hi),width = 0.2)}",
    "        else {lv.p1<-lv.p1+geom_ribbon(aes(ymin = tmp.dt$Predicted.lo, ymax = tmp.dt$Predicted.hi), alpha=.2)}",
    "        input$Toteuta",
    "        isolate(eval(parse(text=paste(\"lv.p1<-lv.p1\",input$AddPlot,collapse=\"\"))))",
    "        lv.p1",
    "    })",

    "    output$Data<-DT::renderDataTable({",
    "        tmp.dt<-tee.dt(input)",
    "    tmp.title<-'Data and predictions'",
    "DT::datatable(tmp.dt,
                           extensions = 'Buttons',escape=TRUE,
                           options = list(
                             pageLength = 50,
                             dom = 'Blfrtip',
                             buttons = list(
                               list(extend = 'copy', title = tmp.title),
                               list(extend = 'csv', title = tmp.title),
                               list(extend = 'excel', title = tmp.title),
                               list(extend = 'pdf', title = tmp.title)
                             )
                           ))",
    "    })",
    "    output$Summary<- renderPrint({",
    "     if(sum(class(tmp.mallit[[as.numeric(input$Model)]])%in%\"coxph\")>0)lv.testi<-cox.zph(tmp.mallit[[as.numeric(input$Model)]])",
    "    else{lv.testi<-NULL}",
    "        tmp.ulos.S<-list(summary(tmp.mallit[[as.numeric(input$Model)]]),lv.testi)",
    "         if(is.null(lv.testi)) {names(tmp.ulos.S)<-c('Model','')}",
    "         if(!is.null(lv.testi)) {names(tmp.ulos.S)<-c('Model','Testing PH assumption')}",
    "        cat(\"\\n Current model:\" ,Models.otsikko[as.numeric(input$Model)],\"\\n\\n\")",
    "        tmp.ulos.S",
    "    })",
    "    output$Info<-renderText({", "         HTML( scan(quiet=TRUE,file=\"ModelInfo.html\",what=\"\"))})",
    "}", "# Run the application", "shinyApp(ui = ui, server = server)")

  # Output shiny code
  sink(file=paste0(path,"/app.R"))
  cat(paste(lv.tmp.osa.1,"\n"))
  if(lv.i.coxph){cat("lv.xvars<-");dput(lv.time);}
  else{cat("lv.xvars<-");dput(lv.xvars);}
  cat(paste(lv.tmp.osa.1A,"\n"))
  cat(lv.ulos$code.data.gen)
  if(lv.i.coxph){cat("\n,",lv.status,"=0")}
  cat(paste(lv.tmp.osa.2,"\n"))
  cat(lv.ulos$code.UI)
  cat(",")
  cat(paste(lv.tmp.osa.3,"\n"))
  cat(paste(lv.tmp.osa.3B,"\n"))
  sink()
  sink(file=paste0(path,"/ModelInfo.html"))
  if(missing(info))cat("<pre>Updated: ",format(Sys.time()),"</pre>")
  else{cat(info)}
  sink()
  # Output models
  tmp.mallit<-ulos.mallit
  save(tmp.mallit,file=paste0(path,"/models.RData"))
  invisible(path)
}
