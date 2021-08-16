###

(function(input, output,session) {
  
  #simple Linear Regression
  
  csvData <- reactive({  #reactive event to store csv file data
    csvFile <- input$fileFile 
    if (is.null(csvFile)) {  
      return()  
    }
    initialData = read.csv(file=csvFile$datapath) 
    
  }) 
  
  
  #adding option to select input and clearing graph area
  observe({  
    selectFields=colnames(csvData());#saving column name
    updateSelectInput(session, "selectPredictorSlr", 
                      
                      choices = selectFields,selected = selectFields[1])#setting default of predtictor to first column
    updateSelectInput(session, "selectResponseSlr",choices = selectFields,selected = selectFields[2])#setting default of predtitor to second column
    
    output$slmGraph = renderPlot({})
    output$slmGraph2 = renderPlot({})
    
  })
  
  output$slmData=DT::renderDataTable({
    
    tempData=csvData();
    DT::datatable(tempData, options = list(lengthChange = TRUE,scrollX = TRUE)) 
    
  })
  
  observeEvent(input$buttonGo, {#triggers when go is clicked
    validate(
      need(input$selectPredictorSlr != "", ""),
      need(input$selectResponseSlr != "", "")
    )
    temp=csvData();
    temp=na.omit(temp)
    xt = temp[,input$selectPredictorSlr]
    yt=temp[,input$selectResponseSlr]
    lmFit=lm(yt~xt,data=temp)
    xlabel=input$selectPredictorSlr
    ylabel=input$selectResponseSlr
    summarySlm=summary(lmFit)
    
    output$slmGraph <- renderPlot({
      ggplot(temp,aes(y=yt,x=xt))+geom_point()+geom_smooth(method="lm")+ labs(x=xlabel, y=ylabel)
      
      #ggPredict(lmFit,se=TRUE,interactive=TRUE)
      
      
    })   
    
    output$slmGraph2 <- renderPlot({
      par(mfrow=c(2,2))
      plot(lmFit)
      #plot(predict(lmFit),residuals(lmFit))
      
    })
    
    output$slmSummary <- renderPrint({
      
      summarySlm
    })
  })
  
  #Multiple linear regression  
  csvDataLr <- reactive({  #reactive event to store csv file data
    csvFileLr <- input$fileFileLr 
    if (is.null(csvFileLr)) {  
      return()  
    }
    initialDataLr = read.csv(file=csvFileLr$datapath) 
    
  }) 
  
  #adding option to select input and clearing graph area
  observe({  
    selectFieldsLr=colnames(csvDataLr());#saving column name
    updateSelectInput(session, "selectPredictorLr",choices = selectFieldsLr)#setting default of predtictor to first column
    updateSelectInput(session, "selectResponseLr",choices = selectFieldsLr,selected = 0)#setting default of predtitor to second column
    
    output$lrGraph = renderPlot({})
    output$lrSummary = renderPrint({})
    
  })
  output$lrData=DT::renderDataTable({
    
    tempDataLr=csvDataLr();
    DT::datatable(tempDataLr, options = list(lengthChange = TRUE,scrollX = TRUE)) 
    
  })
  
  observeEvent(input$buttonGoLr, {#triggers when go is clicked
    validate(
      need(input$selectPredictorLr != "", ""),
      need(input$selectResponseLr != "", "")
    )
    
    tempLr=csvDataLr();
    tempLr=na.omit(tempLr)#data cleaning
    #xtLr = tempLr[,input$selectPredictorLr]
    #ytLr=tempLr[,input$selectResponseLr]
    form=reformulate(termlabels = input$selectPredictorLr,response = input$selectResponseLr)
    #print("1");
    lmFitLr=lm(form,data=tempLr)
    #print("2");
    #xlabel=input$selectPredictorSlr
    #ylabel=input$selectResponseSlr
    summaryLr=summary(lmFitLr)
    #summary(lmFitLr)  
    
    output$lrGraph <- renderPlot({
      #print("eeeeeeeeeeeee")
      par(mfrow=c(2,2))
      plot(lmFitLr)
      
      #plot(predict(lmFit),residuals(lmFit))
      
    })
    
    output$lrSummary <- renderPrint({
      
      summaryLr
    })  
  })
  
  #Logistic regression  
  csvDataLog <- reactive({  #reactive event to store csv file data
    csvFileLog <- input$fileFileLog 
    if (is.null(csvFileLog)) {  
      return()  
    }
    initialDataLog = read.csv(file=csvFileLog$datapath) 
    
  }) 
  
  #adding option to select input and clearing graph area
  observe({  
    selectFieldsLog=colnames(csvDataLog());#saving column name
    updateSelectInput(session, "selectPredictorLog",choices = selectFieldsLog)#setting default of predtictor to first column
    updateSelectInput(session, "selectResponseLog",choices = selectFieldsLog,selected = 0)#setting default of predtitor to second column
    
    output$logGraph = renderPlot({})
    output$logSummary = renderPrint({})
    
  })
  output$logData=DT::renderDataTable({
    
    tempDataLog=csvDataLog();
    DT::datatable(tempDataLog, options = list(lengthChange = TRUE,scrollX = TRUE)) 
    
  })
  
  observeEvent(input$buttonGoLog, {#triggers when go is clicked
    validate(
      need(input$selectPredictorLog != "", ""),
      need(input$selectResponseLog != "", "")
    )
    
    tempLog=csvDataLog();
    tempLog=na.omit(tempLog)
    #xtLog = tempLog[,input$selectPredictorLog]
    #ytLog=tempLog[,input$selectResponseLog]
    form=reformulate(termlabels = input$selectPredictorLog,response = input$selectResponseLog)
    #print("1");
    lmFitLog=glm(form,data=tempLog,family = "binomial")
    
    summaryLog=summary(lmFitLog)
    #summary(lmFitLog)  
    
    output$logGraph <- renderPlot({
      #print("eeeeeeeeeeeee")
      #lmFitLog=lmFitLog;
      accry = NULL
      thold = seq(0.1,0.9, by=.01)
      
      for (i in 1:length(thold)){
        accry[i] = Conf(lmFitLog, cutoff = thold[i])$acc
      }
      cutoff = thold[which.max(accry)]
      conf.mat = Conf(lmFitLog, cutoff = cutoff)
      layout(matrix(1:2,ncol = 2))
      plot(thold, accry, type = "l", main = "Cutoff Based on Accuracy")
      abline(h=max(accry), v = cutoff, col="red")
      fourfoldplot(conf.mat$table, main = "Confusion Matrix Plot", color=c("red","green"))
      
      
    })
    
    output$logSummary <- renderPrint({
      
      summaryLog
    })  
  })
  
 
  
   
  #descriptive stats
  csvDataDes <- reactive({  #reactive event to store csv file data
    csvFileDes <- input$fileFileDes 
    if (is.null(csvFileDes)) {  
      return()  
    }
    initialDataDes = read.csv(file=csvFileDes$datapath) 
    
  }) 
  
  #adding option to select input and clearing graph area
  observe({  
    selectFieldsDes=colnames(csvDataDes());#saving column name
    output$desSummary = renderPrint({})
    
  })
  output$desData=DT::renderDataTable({
    
    tempDataDes=csvDataDes();
    DT::datatable(tempDataDes, options = list(lengthChange = TRUE,scrollX = TRUE)) 
    
  })
  
  observeEvent(input$buttonGoDes, {#triggers when go is clicked
    
    
    tempDes=csvDataDes();
    
    summaryDes=summary(tempDes)
    #summary(lmFitDes)  
    
    
    output$desSummary <- renderPrint({
      if (is.na(summaryDes)==FALSE){
        summaryDes
        #  print(summaryDes)
      }
    })  
  })
  
  #####hypothesis mean
  observeEvent(input$buttonGoHypo, {#triggers when go is clicked
    #validate(
    #need(input$selectColumn != "", ""),
    # need(input$textMean!= "", "")
    #  )
    
    #xtHypo = tempHypo[,input$selectPredictorHypo]
    #ytHypo=tempHypo[,input$selectResponseHypo]
    
    #summaryHypo=summary(lmFitHypo)
    #summary(lmFitHypo)  
    #print(input$selectTestType)
    #print("ASdasd");
    testType=input$selectTestType
    sampleMean=(input$textSampleMean);
    sd=(input$textSD)
    sampleSize=(input$textSize)
    populationMean=(input$textPopulationMean)
    sig=(input$sliderSig)
    print(sig)
    testStatistic=(sampleMean-populationMean)/(sd/sqrt(sampleSize))
    
    
    output$hypoGraph <- renderPlot({
      #print("eeeeeeeeeeeee")
      #lmFitHypo=lmFitHypo;
      
      x= seq(-1*(populationMean+15), populationMean+15, by = 1)
      
      
      y <- dnorm(x,populationMean, sd )
      plot(x,y)
    })
    
    output$hypoSummary <- renderPrint({
      if(testType=="Upper Tailed"){
        halfAlpha=qt(1-sig,df = sampleSize-1)
        
        print(paste("Critical Value =", halfAlpha));
        
        print(paste("Test Statistic = ",testStatistic))
        
        if(testStatistic<halfAlpha){
          print("We accept the hypothesis")
        }else {
          print("We reject the hypothesis")
          
        }
      }else if(testType=="Lower Tailed"){
        halfAlpha=qt(1-sig,df = sampleSize-1)
        nhalfAlpha=-1*halfAlpha
        print(paste("Critical Value =", nhalfAlpha));
        
        print(paste("Test Statistic = ",testStatistic))
        
        if(testStatistic>=nhalfAlpha){
          print("We accept the hypothesis")
        }else {
          print("We reject the hypothesis")
          
        }
        
      } else if(testType=="Two Tailed"){
        halfAlpha=qt(1-sig/2,df = sampleSize-1)
        nhalfAlpha=-1*halfAlpha
        print(paste("Critical Values = (",halfAlpha,")(",nhalfAlpha,")"));
        
        print(paste("Test Statistic = ",testStatistic))
        
        if((testStatistic>=nhalfAlpha) && (testStatistic<=halfAlpha) ){
          print("We accept the hypothesis")
        }else {
          print("We reject the hypothesis")
          
        }
        
      }
      
    })  
  })
  
    
  #poisson model
  observeEvent(input$buttonGoPos, {#triggers when go is clicked
    count=input$sliderPos
    lambda=input$textLambdaPos
    upperX=input$textUpperXPos  
    
    
    output$posGraph <- renderPlot({
      par(mfrow=c(1,2))   
      distr=rpois(count, lambda)  
      tabu=table(distr)  
      xL=0:upperX  
      yL=dpois(xL,lambda)  
      
      barplot(tabu,col='green')  
      plot(xL,yL,type='b')  
      
    })
    
    
  })
  
  #binomial model
  observeEvent(input$buttonGoBin, {#triggers when go is clicked
    countBin=input$sliderBin
    pBin=input$textPBin
    nBin=input$textNBin
    upperXG=input$textUpperXBin  
    
    output$binGraph <- renderPlot({
      par(mfrow=c(1,2))  
      
      d <- density(rbinom(1000,nBin,pBin))  
      
      plot(d, main="Kernel Density of generated data")  
      
      polygon(d, col="green", border="blue") 
      x=0:nBin 
      plot(x,dbinom(x,nBin,pBin))  
      
    })
    
    
  })
  
  
  
})


