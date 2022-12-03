

rm(list=ls())

##Start Script


castPricingDate             <- function( df, dateStr ){
  
  dates                 <- df[ , c( dateStr) ]
  PricingDate           <- chron( as.character( dates ) )
  df[ , c( dateStr) ]   <- PricingDate
  
  df
}

prepareInput                <- function( dfReturns, pReturn, pPerRiskUnit, pSectors, pFlagSimpleName ){
  
  dfOut               <- data.frame( "PricingDate" = dfReturns$PricingDate )
  
  for( i in 1:length( pSectors ) ){
    
    thisSector <- as.character( pSectors[ i ] )
    
    if( pPerRiskUnit ){
      thisString <- paste( c( pReturn, "_", thisSector, "_perRisk" ), collapse = "" )
    } else {
      thisString <- paste( c( pReturn, "_", thisSector), collapse = "" )    
    }
    
    thisVal        <- dfReturns[ , c( thisString ) ]
    
    dfTmp          <- data.frame( "PricingDate" = dfReturns$PricingDate, "thisVal" = thisVal )
    dfOut          <- merge( dfOut, dfTmp, by = c( "PricingDate" ) )
    
    if( pFlagSimpleName ){
      colnames( dfOut )[ grep( "thisVal", colnames( dfOut ), fixed = TRUE ) ]  <- thisSector
    } else {
      colnames( dfOut )[ grep( "thisVal", colnames( dfOut ), fixed = TRUE ) ]  <- thisString      
    }
    
  }
  
  dfOut <- na.omit( dfOut )
  
  dfOut
  
}

randomWeights             <- function( pDraws, pSectors, pNSectors, pLowOmega, pHighOmega ){
  
  weights                     <- matrix( 0, pDraws, pNSectors )
  dfWeights                   <- as.data.frame( weights )
  colnames( dfWeights )       <- pSectors
  
  for( i in 1:pDraws ){
    
    thisDraw                   <- sample.int( pNSectors, pNSectors )
    thisWeight                 <- runif(pNSectors, pLowOmega, pHighOmega )
    
    # apply sum of weights equal to one
    cumSumOne                  <- cumsum( thisWeight )
    idxLessOne                 <- cumSumOne < 1
    overShoot                  <- sum( idxLessOne )
    if( overShoot < pNSectors ){
      thisWeightCapped           <- thisWeight[ idxLessOne ]
      remainingWeight            <- 1 - sum( thisWeightCapped )
      
      firstTooHigh               <- ( idxLessOne == FALSE ) * seq( 1, pNSectors, 1 )
      firstTooHigh               <- firstTooHigh[ firstTooHigh > 0 ][ 1 ]
      thisWeight[ firstTooHigh ] <- remainingWeight
      if( firstTooHigh < pNSectors ){
        thisWeight[ ( firstTooHigh + 1):pNSectors ] <- 0    
      }
    } else {
      thisWeight[ pNSectors ]  <- 1 - sum(thisWeight[1:(pNSectors-1)])
    }
    
    dfTmp                      <- data.frame( "sector" = thisDraw, "weight" = thisWeight )
    dfTmp                      <- dfTmp[ order( dfTmp$sector), ]
    
    dfWeights[ i,  ]           <- dfTmp$weight
    
    
  }
  
  dfWeights
  
}

calcDrawdown                <- function( cumReturn ){
  
  T             <- length( cumReturn )
  drawdown <- array( 0,  T )
  for( i in 1:T ){
    
    thisCumReturn   <- cumReturn[ i ]
    maxCumReturn    <- max( cumReturn[1:i] )
    drawdown[ i ]   <- maxCumReturn - thisCumReturn
    
  }
  drawdown <- 100 * drawdown
  
  drawdown
  
}

summarizeKmeansStats           <- function( clusters, kMeans2, kMeans3, kMeans4, kMeans5, kMeans6, pFeatures ){
  
  totss      <- c( kMean2$totss, kMean3$totss, kMean4$totss, kMean5$totss, kMean6$totss )
  tot_withinss  <- c( kMean2$tot.withinss, kMean3$tot.withinss, kMean4$tot.withinss, kMean5$tot.withinss, kMean6$tot.withinss )
  betweenss <- c( kMean2$betweenss, kMean3$betweenss, kMean4$betweenss, kMean5$betweenss, kMean6$betweenss )
  iter      <- c( kMean2$iter, kMean3$iter, kMean4$iter, kMean5$iter, kMean6$iter )
  size      <- c( mean( kMean2$size ), mean( kMean3$size ), mean( kMean4$size ), mean( kMean5$size), mean( kMean6$size )  )
  
  dfKMeanSummary <- data.frame( "scenario" = pFeatures, "Nclusters" = clusters, "totss"=totss, "withinss"=tot_withinss, "betweenss"=betweenss, "size" = size, "iter" = iter)
  
  
  dfKMeanSummary
}

visualizeKMeansSummary         <- function( dfKMeansSummary, variables, pRows, pCols ){
  
  op                            <- par( mfrow=c( pRows, pCols ), oma=c(1,2,2,1) )  
  uniqueScen                    <- unique( dfKMeansSummary$scenario )
  
  for( j in 1:length( variables ) ){
    
    for( i in 1:length( uniqueScen ) ){
      
      thisVariable         <- as.character( variables[ j ] )
      thisScen             <- as.character( uniqueScen[ i ] )
      idx                  <- dfKMeansSummary$scenario == thisScen
      dfPlot               <- dfKMeansSummary[ idx, ]
      
      ymin                 <- min( dfPlot[ , c( thisVariable ) ] )
      ymax                 <- max( dfPlot[ , c( thisVariable ) ] )
      
      ymin                 <- auxGetMinMax( ymin, ymax )$min
      ymax                 <- auxGetMinMax( ymin, ymax )$max
      
      plot( dfPlot$Nclusters, dfPlot[ , c( thisVariable ) ], ylim = c( ymin, ymax ), xlab = "# clusters", ylab = thisVariable, type = "l" ) 
      title( thisScen )
      grid()
    }
    
  }  
  par( op )
}


auxGetColor           <- function( number ){
  
  if( number == 1 ){
    col                 <- "blue"
  } else if( number == 2 ){
    col                 <- "red"
  } else if( number == 3 ){
    col                 <- "chartreuse4"
  } else if( number == 4 ){
    col                 <- "gold"
  } else if( number == 5 ){
    col                 <- "green"
  } else if( number == 6 ){
    col                 <- "aquamarine"
  } else if( number == 7 ){
    col                 <- "blueviolet"
  } else {
    col                 <- "black"
  }
  col
}

auxGetMinMax      <- function( min, max ){
  
  if( min < 0 ){
    min              <- min * 1.05
  } else {
    min              <- min * 0.95
  }
  
  if( max < 0 ){
    min              <- min * 0.95
  } else {
    min              <- min * 1.05
  }
  
  limes           <- list( "min" = min, "max" = max )
  limes
}


plotClusters        <- function( summaryDf, kMeans, yVariable, pVariables, pTitle, pClusterType ){
  
  if( pClusterType == "kMeans" | pClusterType == "hierarchical Kmeans" ){
    plotData              <- as.data.frame( kMeans$cluster )    
  } else if( pClusterType == "dbScan" ) {
    plotData              <- as.data.frame( kMeans$cluster )    
  } else {
    plotData              <- as.data.frame( kMeans )    
  }
  
  colnames( plotData )  <- c("cluster")
  plotData$folio        <- seq( 1,nrow(plotData),1)
  
  if( pClusterType == "dbScan" ) {
    idx                  <- plotData$cluster != 0 
    plotData             <- plotData[ idx, ]
  }
  
  plotData              <- merge( plotData, summaryDf, by = c( "folio" ) )
  ymin                  <- min( plotData[ , c(yVariable )] )
  ymax                  <- max( plotData[ , c(yVariable )] )
  ymin                  <- auxGetMinMax( ymin, ymax )$min
  ymax                  <- auxGetMinMax( ymin, ymax )$max
  uniqueClusters        <- unique( plotData$cluster )
  
  for( j in 1:length( pVariables )){
    thisVariable          <- as.character( pVariables[ j ] )
    
    xmin                  <- min( plotData[ , c( thisVariable ) ] )
    xmax                  <- max( plotData[ , c( thisVariable ) ] )
    xmin                  <- auxGetMinMax( xmin, xmax )$min
    xmax                  <- auxGetMinMax( xmin, xmax )$max
    
    for( i in 1:length( uniqueClusters ) ){
      thisCol               <- auxGetColor( i )  
      thisCluster           <- uniqueClusters[ i ]
      idx                   <- plotData$cluster == thisCluster
      plotDataTmp           <- plotData[ idx, ]
      
      
      if( i == 1 ){
        plot( plotDataTmp[ , c( thisVariable ) ], plotDataTmp[, c(yVariable)], xlim = c( xmin, xmax), ylim = c( ymin, ymax), ylab = yVariable, xlab = thisVariable, col = thisCol )
      } else {
        points( plotDataTmp[ , c( thisVariable ) ], plotDataTmp[, c(yVariable)], col = thisCol )
      }
    }
  }
  grid()
  mtext(pTitle, outer=TRUE,  cex=1, line=-0.5)
  
}

constructInspectFolio        <- function( summaryDf, kMeans, pClusterType, clusterDimensions, pUtility ){
  
  if( pClusterType == "kMeans" | pClusterType == "hierarchical Kmeans" ){
    plotData              <- as.data.frame( kMeans$cluster )    
  } else if( pClusterType == "dbScan" | pClusterType == "hDBScan") {
    plotData              <- as.data.frame( kMeans$cluster )    
  } else {
    plotData              <- as.data.frame( kMeans )    
  }
  
  colnames( plotData )  <- c("cluster")
  plotData$folio        <- seq( 1,nrow(plotData),1)
  
  plotData              <- merge( plotData, summaryDf, by = c( "folio" ) )
  
  clusters              <- unique( plotData$cluster )
  for( i in 1:length( clusters ) ){
    
    thisCluster           <- clusters[ i ]
    idx                   <- plotData$cluster == thisCluster
    plotDataTmp           <- plotData[ idx, ]
    
    cumRet                <- mean( plotDataTmp$cumRet )
    mu                    <- mean( plotDataTmp$mu )
    sigma                 <- mean( plotDataTmp$sigma )
    skew                  <- mean( plotDataTmp$skew )
    maxDD                 <- mean( plotDataTmp$maxDD )
    kurt                  <- mean( plotDataTmp$kurt )
    ir                    <- mean( plotDataTmp$ir )
    timingLoss            <- mean( plotDataTmp$lossTiming )
    utility               <- mean( plotDataTmp[ ,c( pUtility )] )
    
    
    
    dfInspectTmp          <- data.frame( "clusterType" = pClusterType, "clusterDimensions" = clusterDimensions, "cluster" = thisCluster, "nClusters" = length(clusters), "nFolios" = nrow(plotDataTmp), 
                                         "cumRet" = cumRet, "mu" = mu, "sigma" = sigma, "skew" = skew, "kurt" = kurt, "maxDD" = maxDD, "ir" = ir, "timingLoss" = timingLoss, "utility" = utility  )
    
    colnames( dfInspectTmp )[ grep( "utility", colnames( dfInspectTmp ), fixed = TRUE ) ]  <- c( pUtility )
    
    if( i == 1 ){
      dfInspect <- dfInspectTmp
    } else {
      dfInspect <- rbind( dfInspect, dfInspectTmp )
    }
  }
  
  dfInspect
  
}



normalizeData <- function( dfData ){
  
  columns <- colnames( dfData)
  columns  <- columns[ 2:length( columns ) ]
  
  for( i in 1:length( columns )){
    
    thisCol           <- as.character( columns[ i ])
    thisVal           <- dfData[ , c( thisCol )]
    mu                <- mean( thisVal )
    sigma             <- sd( thisVal )
    thisValNorm       <- ( thisVal - mu ) / sigma
    dfData <- cbind( dfData, thisValNorm )
    colnames( dfData )[ grep( "thisValNorm", colnames( dfData ), fixed = TRUE ) ]  <- paste( c( thisCol, "_norm" ), collapse = "" )
    
  }
  dfData
}

auxGetNormalizedNames        <- function( columns ){
  
  newColnames <- c()
  for( i in 1:length( columns ) ){
    thisCol <- as.character( columns[ i ] )
    newColName <- paste( c( thisCol, "_norm" ), collapse = "" )
    newColnames <- c( newColnames, newColName )  
  }
  newColnames
}

auxGetClusterDimensions       <- function( columns ){
  
  clusterDim<- c()
  for( i in 1:length( columns ) ){
    
    thisCol                  <- as.character( columns[ i ] )
    if( i == 1 ){
      clusterDim              <- thisCol
    } else {
      clusterDim               <- paste( c( clusterDim, "_", thisCol ), collapse = "" )      
    }
  }
  clusterDim
}


auxSubjectiveProbWeight         <- function( prob, pTau ){
  
  omega <- prob^pTau / (prob^pTau + (1 - prob)^pTau)^(1/pTau)  
  omega
  
}

prospectTheoryUtilityDepracated           <- function( folioReturnsDf, pGamma, pTau, pLambda, pRiskFree){
  
  folios <- unique( folioReturnsDf$folio )
  for( i in 1:length( folios ) ){
    
    thisFolio          <- folios[ i ]
    idx                <- folioReturnsDf$folio == thisFolio
    dfThisFolio        <- folioReturnsDf[ idx, ]
    
    thisReturn         <- dfThisFolio$return
    
    # get paramteres of NIG process
    nigModel           <- nigFit( thisReturn )
    param              <- nigModel$param
    param              <- as.data.frame( param )
    param$greeks       <- row.names( param )
    mu                 <- param[ param$greeks == "mu", c( "param") ]
    delta              <- param[ param$greeks == "delta", c( "param") ]
    alpha              <- param[ param$greeks == "alpha", c( "param") ]    
    beta               <- param[ param$greeks == "beta", c( "param") ]
    
    # calculate utility
    nigDensity         <- dnig( thisReturn, mu = mu, delta = delta, alpha = alpha, beta = beta )
    nigCumprob         <- pnig( thisReturn, mu = mu, delta = delta, alpha = alpha, beta = beta)
    
    positives          <- thisReturn > pRiskFree
    negatives          <- thisReturn <= pRiskFree
    
    utilityTotal       <- 0
    for( j in 1:length( thisReturn ) ){
      
      x                 <- thisReturn[ j ]
      cumProb           <- nigCumprob[ j ]
      density           <- nigDensity[ j ]
      
      if( x > pRiskFree ){
        prob              <- 1 - cumProb
        omega             <- auxSubjectiveProbWeight( prob, pTau )  
        utility           <- ( ( x - pRiskFree )^pGamma ) * omega * density
      } else {
        prob              <- cumProb
        omega             <- auxSubjectiveProbWeight( prob, pTau )  
        utility           <- - pLambda * ( ( pRiskFree - x )^pGamma ) * omega * density
      }
      
      utilityTotal <- utilityTotal + utility  
      
    }
    
    dfTmp <- data.frame( "folio" = i, "utility" = utilityTotal )
    
    if( i == 1 ){
      dfUtility <- dfTmp
    } else {
      dfUtility <- rbind( dfUtility, dfTmp )    
    }
    
  }
  
  dfUtility
  
}


prospectTheoryUtility           <- function( folioReturnsDf, pGamma, pTau, pLambda, pRiskFree){
  
  folios            <- unique( folioReturnsDf$folio )
  for( j in 1:length( folios ) ){
    
    # get vector of returns
    thisFolio          <- folios[ j ]
    idx                <- folioReturnsDf$folio == thisFolio
    dfThisFolio        <- folioReturnsDf[ idx, ]
    thisReturn         <- dfThisFolio$return
    thisReturn         <- sort( thisReturn )
    
    # get paramteres of NIG process
    nigModel           <- nigFit( thisReturn )
    param              <- nigModel$param
    param              <- as.data.frame( param )
    param$greeks       <- row.names( param )
    mu                 <- param[ param$greeks == "mu", c( "param") ]
    delta              <- param[ param$greeks == "delta", c( "param") ]
    alpha              <- param[ param$greeks == "alpha", c( "param") ]    
    beta               <- param[ param$greeks == "beta", c( "param") ]
    
    # calculate NIG probability
    nigCumprob         <- pnig( thisReturn, mu = mu, delta = delta, alpha = alpha, beta = beta)
    
    # split into positive and negative returns
    idxNeg             <- thisReturn < pRiskFree
    idxPos             <- thisReturn >= pRiskFree
    thisReturnNeg      <- thisReturn[ idxNeg ]
    thisReturnPos      <- thisReturn[ idxPos ]
    nigCumprobNeg      <- nigCumprob[ idxNeg ]
    nigCumprobPos      <- nigCumprob[ idxPos ]
    
    # negative returns
    utilityTotal       <- 0
    for( i in 2:length( thisReturnNeg ) ){
      x                <- thisReturnNeg[ i ]
      P                <- nigCumprobNeg[ i ]
      P_m1             <- nigCumprobNeg[ i - 1 ]
      dOmega           <- auxSubjectiveProbWeight( P, pTau ) - auxSubjectiveProbWeight( P_m1, pTau )
      v                <- - pLambda * ( ( pRiskFree - x )^pGamma )
      utilityTotal     <- utilityTotal + v * dOmega  
    }
    
    # positive returns
    for( i in 1:(length( thisReturnPos )-1) ){
      x                <- thisReturnPos[ i ]
      P                <- nigCumprobPos[ i ]
      P_p1             <- nigCumprobPos[ i + 1 ]
      dOmega           <- auxSubjectiveProbWeight( (1-P), pTau ) - auxSubjectiveProbWeight( (1-P_p1), pTau )
      v                <- ( x - pRiskFree )^pGamma 
      utilityTotal     <- utilityTotal + v * dOmega  
    }
    
    dfTmp              <- data.frame( "folio" = j, "thisVal" = utilityTotal )
    colnames( dfTmp )[ grep( "thisVal", colnames( dfTmp ), fixed = TRUE ) ]  <- paste( c( "CPTUtility_", as.character( 100*pLambda )), collapse = "" )
    
    if( j == 1 ){
      dfUtility <- dfTmp
    } else {
      dfUtility <- rbind( dfUtility, dfTmp )    
    }
    
  }
  
  dfUtility
  
}


getTimingOfLoss <- function( folioReturnsDf ){
  
  uniqueFolio <- unique( folioReturnsDf$folio )
  
  for( i in 1:length( uniqueFolio ) ){
    
    thisFolio           <- i
    idx                 <- folioReturnsDf$folio == thisFolio
    dfTmp               <- folioReturnsDf[ idx, ]
    dfTmp               <- dfTmp[ order( dfTmp$PricingDate), ]
    thisCumRet          <- dfTmp$cumRet
    drawdown            <- calcDrawdown( thisCumRet )
    maxDD               <- max(drawdown )
    timeIdx             <- drawdown == maxDD
    maxDDdate           <- chron( as.character( dfTmp$PricingDate[ timeIdx ] ) )
    nMonths             <- as.numeric( ( maxDDdate - pTimeMarketMaxDD ) / 30.5 )
    nMonthsAbs          <- abs( nMonths )
    
    dfOutTmp            <- data.frame( "folio" = i, "lossTimingAbs" = nMonthsAbs, "lossTiming" = nMonths )
    if( i == 1 ){
      dfOut               <- dfOutTmp
    } else {
      dfOut               <- rbind( dfOut, dfOutTmp )
    }
    
  }
  dfOut
  
}



auxCreateCluster <- function( kMeans, pClusterType ){
  
  if( pClusterType == "kMeans" | pClusterType == "hierarchical Kmeans" ){
    plotData              <- as.data.frame( kMeans$cluster )    
  } else if( pClusterType == "dbScan" | pClusterType == "hDBScan" ) {
    plotData              <- as.data.frame( kMeans$cluster )    
  } else {
    plotData              <- as.data.frame( kMeans )    
  }
  
  colnames( plotData )  <- c("cluster")
  plotData$folio        <- seq( 1,nrow(plotData),1)
  plotData
}


auxByClusterPortfolioPerformance <- function( dfFolioPerfData, folioByCluster, pUtility ){
  
  dfFolioPerfData <- merge( dfFolioPerfData, folioByCluster, by = c( "folio") )
  uniqueClusters <- unique( dfFolioPerfData$cluster )
  for( i in 1:length( uniqueClusters ) ){
    
    thisCluster <- uniqueClusters[ i ]
    
    idx<- dfFolioPerfData$cluster == thisCluster
    dfTmp <- dfFolioPerfData[ idx, ]
    
    cumRet                <- mean( dfTmp$cumRet )
    sigma                 <- mean( dfTmp$sigma )
    skew                  <- mean( dfTmp$skew )
    maxDD                 <- mean( dfTmp$maxDD )
    ir                    <- mean( dfTmp$ir )
    utility               <- mean( dfTmp[ ,c( pUtility ) ] )
    
    dfFolioPerformanceByClusterTmp <- data.frame( "cluster" = thisCluster, "cumRet" = cumRet, "sigma" = sigma, "skew" = skew, "maxDD" = maxDD, "ir" = ir, "utility" = utility )
    
    colnames( dfFolioPerformanceByClusterTmp )[ grep( "utility", colnames( dfFolioPerformanceByClusterTmp ), fixed = TRUE ) ]  <- c( pUtility )
    
    if( i == 1 ){
      dfFolioPerformanceByCluster <- dfFolioPerformanceByClusterTmp
    } else {
      dfFolioPerformanceByCluster <- rbind( dfFolioPerformanceByCluster, dfFolioPerformanceByClusterTmp )
    }
    
  }
  
  dfFolioPerformanceByCluster
  
}


dbClustSummary    <- function( dbClust, eps, minPts, clusterDimensions ){
  
  dbClust$index <- 1
  dfSummary <- as.data.frame( table(dbClust ))
  idxNoise         <- dfSummary$cluster == 0
  if( sum(idxNoise)==0){
    pctNoise         <- 0
  } else {
    pctNoise         <- dfSummary[ idxNoise, c("Freq")] / sum(dfSummary$Freq)    
  }
  pctAllCusters    <- 1- pctNoise
  idxLargest       <- dfSummary$Freq == max(dfSummary$Freq)
  pctLargest       <- dfSummary[ idxLargest, c("Freq")] / sum(dfSummary$Freq)
  dfOut            <- data.frame( "eps" = eps, "minPts" = minPts, "clusterDimensions" = clusterDimensions, "pctNoise" = pctNoise, "pctAllClusters" = pctAllCusters, "pctLargest" = pctLargest )
  dfOut
  
}




plotDbScanInspection <- function( dfDbClustSummary, clusterDimensions){
  
  plot( dfDbClustSummary$eps, dfDbClustSummary$pctAllClusters, main = clusterDimensions, xlab="epsilon", ylab="Share of dataset", type = "l", col = "blue", lwd = 3)
  points( dfDbClustSummary$eps, dfDbClustSummary$pctLargest, type = "l", col = "red", lwd = 3)
  points( dfDbClustSummary$eps, dfDbClustSummary$pctNoise, type = "l", col = "green", lwd = 3)
  grid()
  legend("bottomright", c( "All clusters", "Largest component", "Noise"), lwd = c(1,1), col = c("blue", "red", "green"))
  
}


auxByClusterSectorExposures <- function( dfSectorWeights, folioByCluster, pSectors ){
  
  dfSectorWeights   <- merge( dfSectorWeights, folioByCluster, by = c( "folio") )
  
  uniqueClusters    <- unique( dfSectorWeights$cluster )
  count             <- 0
  for( i in 1:length( uniqueClusters ) ){
    
    thisCluster   <- uniqueClusters[ i ]
    
    idx           <- dfSectorWeights$cluster == thisCluster
    dfTmp         <- dfSectorWeights[ idx, ]
    
    for( j in 1:length( pSectors ) ){
      
      thisSector    <- as.character( pSectors[ j ] )
      thisVal       <- dfTmp[ , c( thisSector ) ]
      wgt           <- mean( thisVal )
      wgt_sd        <- sd( thisVal )
      
      dfOutTmp      <- data.frame( "cluster" = thisCluster, "sector" = thisSector, "wgt" = wgt, "wgt_sd" = wgt_sd )
      
      if( count == 0 ){
        dfOut         <- dfOutTmp
      } else {
        dfOut         <- rbind( dfOut, dfOutTmp )
      }
      count         <- count + 1
    }
    
  }
  
  dfOut
}

auxCreateFolioInspection <- function( dfFolioPerfByCluster, dfSectorExposuresByCluster, clusterDimensions, pClusterType, pClusters ){
  
  dfFolioInspection                   <- merge( dfFolioPerfByCluster, dfSectorExposuresByCluster, by = c( "cluster" ) )
  dfFolioInspection$clusterDimensions <- clusterDimensions
  dfFolioInspection$clusterMethod     <- pClusterType
  dfFolioInspection$nClusters         <- pClusters
  dfFolioInspection
  
}

plotExposures <- function( dfFolioInspection, pSectors, byVariable, pRows, pCols ){
  
  op                     <- par( mfrow=c( pRows, pCols ), oma=c(1,2,2,1) )
  
  clusterDimensions      <- unique( dfFolioInspection$clusterDimensions )
  methods                <- unique( dfFolioInspection$clusterMethod )
  nClusters              <- unique( dfFolioInspection$nClusters )
  
  
  for( i in 1:length( clusterDimensions ) ){
    
    thisDimension <- as.character( clusterDimensions[ i ] )
    
    for( j in 1:length( methods ) ){
      
      thisMethod    <- as.character( methods[ j ] )
      
      for( k in 1:length( nClusters ) ){
        
        thisCluster <- nClusters[ k ]
        
        idx           <- dfFolioInspection$clusterDimensions == thisDimension & dfFolioInspection$clusterMethod == thisMethod & dfFolioInspection$nClusters == thisCluster
        if( sum( idx ) > 0 ){
          dfExposureSummaryTmp  <- dfFolioInspection[ idx, ]
          
          
          strTitle             <- paste( c( as.character(thisDimension), "\n", thisMethod, "\n", as.character(thisCluster) ), collapse = "" )
          
          plot.new(); plot.window(xlim=c(0,1),ylim=c(0,1))
          text( 0.5, 0.5, lab=strTitle, cex = 1.0)
          
          #print( paste( c( as.character( i), ", ", as.character( j), ", ", as.character( k)), collapse = ""))        
          for( l in 1:length( pSectors ) ){
            
            thisSector <- as.character( pSectors[ l ] )
            idx        <- dfExposureSummaryTmp$sector == thisSector
            dfTmp      <- dfExposureSummaryTmp[ idx, ]
            dfTmp      <- dfTmp[ order( dfTmp[ , c(byVariable)]), ]
            
            ymin                 <- min( dfTmp$wgt - dfTmp$wgt_sd )
            ymax                 <- max( dfTmp$wgt + dfTmp$wgt_sd )
            
            ymin                 <- auxGetMinMax( ymin, ymax )$min
            ymax                 <- auxGetMinMax( ymin, ymax )$max
            
            #par(mar=c(1,1,1,1) )
            plot( dfTmp[ , c(byVariable)], dfTmp$wgt, main = thisSector, ylim = c( ymin, ymax ), xlab = byVariable, ylab = "wgt", type = "b", col = "blue", pch = 19 ); grid()
            #points( dfTmp[ , c(byVariable)], dfTmp$wgt + dfTmp$wgt_sd, main = thisSector, type = "l", col = "red", lty=2 ); grid()
            #points( dfTmp[ , c(byVariable)], dfTmp$wgt - dfTmp$wgt_sd, main = thisSector, type = "l", col = "red", lty=2 ); grid()
            
          }  
        }
      }
    }
  }
  
  par( op )
  
}


auxIsFI <- function( dfData, pFI ){
  
  indexFI           <- rep( FALSE, nrow( dfData ) )
  wgtFI             <- 0
  for( i in 1:length(pFI ) ){
    
    thisFI            <- as.character( pFI[ i ] )
    idx               <- dfData$sector == thisFI
    wgtFI             <- wgtFI + dfData[ idx, c( "wgt" ) ]
    
  }
  wgtFI
  
}


plotUtility         <- function( dfUtility, pSectors, byVariable, pRows, pCols, pUtilityType, pUtility ){
  
  
  op                   <- par( mfrow=c( pRows, pCols ), oma=c(1,2,2,1) )
  
  ymin                 <- min( dfUtility[ , c( pUtility ) ] )
  ymax                 <- max( dfUtility[ , c( pUtility ) ] )
  
  ymin                 <- auxGetMinMax( ymin, ymax )$min
  ymax                 <- auxGetMinMax( ymin, ymax )$max
  
  
  for( i in 1:length( byVariable ) ){
    
    thisVar               <- as.character( byVariable[ i ] )
    dfPlot                <- dfUtility[ , c( pUtility, thisVar) ]
    plot( dfPlot[ , c( thisVar ) ], dfPlot[,c(pUtility)], xlab = thisVar, ylab = pUtility, ylim = c( ymin, ymax )  )
    grid()
    
  }
  
  par( op )
  strTitle          <- paste( c( pUtilityType, " Utility" ), collapse = "" )
  mtext( strTitle, outer=TRUE,  cex=1, line=-0.5)
  
}


auxGetFolioPerformanceMVOKelly     <- function( dfReturnDataTotRet, pSectors, weightsVec, nameOptim ){
  
  portRet <- 0
  nSectors <- length( pSectors )
  for( i in 1:nSectors ){
    
    thisSector                  <- as.character( pSectors[ j ] )
    thisWeight                  <- weightsVec[ i ]
    portRet                     <- portRet + dfReturnDataTotRet[ , c( thisSector ) ] * thisWeight
    
  }
  portRet                     <- 1 + portRet / 100
  cumRet                      <- cumprod(( portRet ) )
  drawdown                    <- calcDrawdown( cumRet )
  
  # summary stats
  folioRet                    <- portRet - 1
  T                           <- length( folioRet )
  mu                          <- mean( folioRet )
  sigma                       <- sd( folioRet ) 
  skew                        <- skewness( folioRet )    
  kurt                        <- kurtosis( folioRet )
  maxDD                       <- max( drawdown )
  
  dfOut                       <- data.frame( "nameOptim" = nameOptim, "cumRet" = tail( cumRet, 1 ), "mu" = mu, "sigma" = sigma, "ir" = mu / sigma, "skew" = skew, "kurt" = kurt, "maxDD" = maxDD )
  
  dfOut  
  
}

calcAllExposures                     <- function( dfFolioPerf, pNobs, pUtility ){
  
  
  # 4: c( "ir", "skew", kurt" )
  columns              <- c( "ir", "skew", "kurt" )
  clusterDimensions    <- auxGetClusterDimensions( columns )
  if( pNormalized ) columns <- auxGetNormalizedNames( columns )
  pClusters            <- 5
  test                 <- dfFolioPerf[ 1:pNobs, columns ]
  
  pClusterType         <- "kMeans"
  kMean                <- kmeans( test, pClusters ) 
  folioByCluster       <- auxCreateCluster( kMean, pClusterType )
  dfFolioPerfByCluster <- auxByClusterPortfolioPerformance( dfFolioPerf, folioByCluster, pUtility )                                           
  dfSectorExposuresByCluster <- auxByClusterSectorExposures( dfSectorExposures, folioByCluster, pSectors )
  dfFolioInspectionTmp <- auxCreateFolioInspection( dfFolioPerfByCluster, dfSectorExposuresByCluster, clusterDimensions, pClusterType, pClusters )
  dfFolioInspection    <- dfFolioInspectionTmp
  
  pClusterType         <- "hierarchical"
  hc                   <- hclust( dist(test))
  hc                   <- cutree(hc, k = pClusters)
  folioByCluster       <- auxCreateCluster( hc, pClusterType )
  dfFolioPerfByCluster <- auxByClusterPortfolioPerformance( dfFolioPerf, folioByCluster, pUtility )                                           
  dfSectorExposuresByCluster <- auxByClusterSectorExposures( dfSectorExposures, folioByCluster, pSectors )
  dfFolioInspectionTmp <- auxCreateFolioInspection( dfFolioPerfByCluster, dfSectorExposuresByCluster, clusterDimensions, pClusterType, pClusters )
  dfFolioInspection    <- rbind( dfFolioInspection, dfFolioInspectionTmp )
  
  pClusterType         <- "dbScan"
  minPts               <- 6
  knnCalibEps          <- 0.34   # 0.2   / 0.1
  db                   <- dbscan( test, eps = knnCalibEps, minPts = minPts )
  folioByCluster       <- auxCreateCluster( db, pClusterType )
  dfFolioPerfByCluster <- auxByClusterPortfolioPerformance( dfFolioPerf, folioByCluster, pUtility )                                           
  dfSectorExposuresByCluster <- auxByClusterSectorExposures( dfSectorExposures, folioByCluster, pSectors )
  dfFolioInspectionTmp <- auxCreateFolioInspection( dfFolioPerfByCluster, dfSectorExposuresByCluster, clusterDimensions, pClusterType, pClusters )
  idx                  <- dfFolioInspectionTmp$cluster != 0
  dfFolioInspectionTmp <- dfFolioInspectionTmp[ idx, ]
  dfFolioInspection    <- rbind( dfFolioInspection, dfFolioInspectionTmp )
  
  
  
  # 5: columns              <-  c(  "cumRet", "sigma", "skew", "kurt" );       
  columns              <-  c(  "cumRet", "sigma", "skew", "kurt" );       
  clusterDimensions    <- auxGetClusterDimensions( columns )
  if( pNormalized ) columns <- auxGetNormalizedNames( columns )
  pClusters            <- 8
  test                 <- dfFolioPerf[ 1:pNobs, columns ]
  
  pClusterType         <- "kMeans"
  kMean                <- kmeans( test, pClusters ) 
  kMean                <- kmeans( test, pClusters ) 
  folioByCluster       <- auxCreateCluster( kMean, pClusterType )
  dfFolioPerfByCluster <- auxByClusterPortfolioPerformance( dfFolioPerf, folioByCluster, pUtility )                                           
  dfSectorExposuresByCluster <- auxByClusterSectorExposures( dfSectorExposures, folioByCluster, pSectors )
  dfFolioInspectionTmp <- auxCreateFolioInspection( dfFolioPerfByCluster, dfSectorExposuresByCluster, clusterDimensions, pClusterType, pClusters )
  dfFolioInspection    <- rbind( dfFolioInspection, dfFolioInspectionTmp )
  
  pClusterType         <- "hierarchical"
  hc                   <- hclust( dist(test))
  hc                   <- cutree(hc, k = pClusters)
  folioByCluster       <- auxCreateCluster( hc, pClusterType )
  dfFolioPerfByCluster <- auxByClusterPortfolioPerformance( dfFolioPerf, folioByCluster, pUtility )                                           
  dfSectorExposuresByCluster <- auxByClusterSectorExposures( dfSectorExposures, folioByCluster, pSectors )
  dfFolioInspectionTmp <- auxCreateFolioInspection( dfFolioPerfByCluster, dfSectorExposuresByCluster, clusterDimensions, pClusterType, pClusters )
  dfFolioInspection    <- rbind( dfFolioInspection, dfFolioInspectionTmp )
  
  pClusterType         <- "dbScan"
  minPts               <- 8
  knnCalibEps          <- 0.34   #0.28    /0.165
  db                   <- dbscan( test, eps = knnCalibEps, minPts = minPts )
  folioByCluster       <- auxCreateCluster( db, pClusterType )
  dfFolioPerfByCluster <- auxByClusterPortfolioPerformance( dfFolioPerf, folioByCluster, pUtility )                                           
  dfSectorExposuresByCluster <- auxByClusterSectorExposures( dfSectorExposures, folioByCluster, pSectors )
  dfFolioInspectionTmp <- auxCreateFolioInspection( dfFolioPerfByCluster, dfSectorExposuresByCluster, clusterDimensions, pClusterType, pClusters )
  idx                  <- dfFolioInspectionTmp$cluster != 0
  dfFolioInspectionTmp <- dfFolioInspectionTmp[ idx, ]
  dfFolioInspection    <- rbind( dfFolioInspection, dfFolioInspectionTmp )
  
  dfFolioInspection
  
}









library( chron )
library( gridExtra )
library( GeneralizedHyperbolic )
library( moments )
require( graphics )
library( factoextra )
library( clustertend )
library( NbClust )
library( fpc )
library( dbscan )
library( quadprog )
library( clues)
library( xlsx)




# Set Parameters for input/output data
# file names
# SPECIFY YOUR OWN PATH
pPath <- "C:\\Users\\SPECIFY YOUR OWN PATH"
pPath                       <- "C:\\Users\\zayde\\OneDrive\\Documents\\ZA Documents\\Papers\\20200601 ML Cluster Analysis Portfolio Construction"
pPathOut                    <- pPath

# this file contains the monthly return data which is used to construct and calibrate portfolios
pFileReturns                <- "R_returnData_withEquity_truncated.csv"

# these pdf files are for inspection and research of clustered portfolios only
pPdfFileNameNonSphericalClusterInspection  <- "R_nonSphericalCluster_inspection"
pPdfFileNameFolioResults    <- "R_folioPerformance_inspection"
pPdfFileNameFolioExposures  <- "R_folioExposures_inspection"
pPdfFileNameNonSphericalOptimalNCluster <- "R_NonSphericalOptimalNCluster"
pPdfFileNameSphericalClusterInspection <- "R_SphericalCluster_inspection"
pFileInspectFolioPerformance <- "R_inspectFolioPerformance"

# these files are for inspection of utilities associated with resulting portfolios from the cluster analysis
pFileNameFolioCompositionXLS <- "R_folioCompositionSortedByUtility"
pdfFileNameInspectUtil       <- "R_inspectUtilityFunction"

# return parameters
pReturn                     <- "trt"
pPerRiskUnit                <- FALSE
pSectors                    <- c( "ust3m", "ust10", "tips", "mbs", "agency", "usIGCredit", "usHYCredit", "cmbs", "em", "intlBond", "spx", "intlEq", "emStock", "reit" )
nSectors                    <- length( pSectors )
pFI                         <- c( "ust3m", "ust10", "tips", "mbs", "agency", "usIGCredit", "usHYCredit", "cmbs", "em", "intlBond" )
pFTQ                        <- c( "ust3m", "ust10" )
pFIRates                    <- c( "ust3m", "ust10", "tips", "mbs", "agency" )
pFICredit                   <- c( "usIGCredit", "usHYCredit", "cmbs", "em", "intlBond" )
pFeatures                   <- c( "cumRet", "mu", "sigma", "ir", "skew", "kurt", "maxDD", "lossTimingAbs" )
pNSectors                   <- length( pSectors )
pTimeMarketMaxDD            <- chron( as.character( "2/27/2009" ) )
pTimeMarket2ndMaxDD         <- chron( as.character( "9/30/2002" ) )
pPerRiskUnit                <- FALSE    # analyze portfolios in terms of their total returns, and not returns per unit of risk, i.e. information ratios

# prepare data
pFlagSimplifyName           <- TRUE

# random draw parameters
pDraws                      <- 10000
pLowOmega                   <- 0
pHighOmega                  <- 0.6

# parameters prospect theory and NIG
pRiskFree                   <- 1.51 / 100 / 12 # US Bill 1 month
pGamma                      <- 0.88
pLambda                     <- 2.25
pTau                        <- 0.65

# plotting parameters
pRows                       <- 2
pCols                       <- 2


# GET DATA
fileName                    <- paste( c( pPath, "\\", pFileReturns  ), collapse ="" )
dfReturns                   <- read.csv( fileName )
dfReturns                   <- castPricingDate( dfReturns, "PricingDate" )
dfReturnDataTotRet          <- prepareInput( dfReturns, pReturn, pPerRiskUnit, pSectors, pFlagSimplifyName )


# CALCULATE PORTFOLIO FROM MEAN VARIANCE

# get expected returns
dfTmp                     <- dfReturnDataTotRet
dfTmp$PricingDate         <- NULL
dfMean                    <- as.data.frame(lapply(dfTmp, mean))
  
# get correlation matrix
corr                      <- cor( dfTmp)

# MVO
pTargetReturns            <- c( 0.48, 0.52, 0.59, 0.76, 0.69 )
targetReturnNames         <- c( "gamma_5_ir_48", "gamma_3_5_ir_52", "gamma_3_ir_59","gamma_1_ir_76", "gamma_1_ir_69" )
  
Dmat                      <- as.matrix( cov( dfTmp) )
dvec                      <- rep( 0 / ncol(dfMean), ncol(dfMean) )   # is the linear term of the program
a1                        <- rep( 0, nSectors )
for( i in 1:nSectors ){
  a1[ i ]                   <- dfMean[[i]]
}
a2                        <- rep( 1, nSectors )
Amat                      <- matrix( c(a1, a2 ), nrow = nSectors, ncol = 2 )
Amat                      <- cbind( Amat, diag( x = 1, nSectors, nSectors ) )
bvec                      <- c( 0.5, 1, rep( 0, nSectors ))
meq                       <- 2
  
dfMVO                     <- data.frame( "sector" = pSectors )
for( j in 1:length( pTargetReturns ) ){
    
  thisReturn                <- pTargetReturns[ j ]
  thisName                  <- targetReturnNames[ j ]
  bvec[ 1 ]                 <- thisReturn
  mvo                       <- solve.QP(Dmat,dvec,Amat,bvec=bvec, meq = meq )
  weightsMvo                <- as.data.frame( mvo$solution )  
  colnames( weightsMvo )[ grep( "mvo$solution", colnames( weightsMvo ), fixed = TRUE ) ]  <- thisName
  dfMVO                     <- cbind( dfMVO, weightsMvo )
  dfMVOPerfTmp              <- auxGetFolioPerformanceMVOKelly( dfReturnDataTotRet, pSectors, mvo$solution, thisName )
  if( j == 1 ){
    dfMVOPerf                 <- dfMVOPerfTmp
  } else {
    dfMVOPerf                 <- rbind( dfMVOPerf, dfMVOPerfTmp )
  }
}
  

# Kelly criterion
Dmat                        <- as.matrix( cov( dfTmp) )
dvec                        <- rep( 0, nSectors )     # mean normalized
for( i in 1:nSectors ){
  dvec[ i ]                   <- ( dfMean[[i]] - pRiskFree * 100 )
}
a2                        <- rep( 1, nSectors )
Amat                      <- matrix( c(a2 ), nrow = nSectors, ncol = 1 )
Amat                      <- cbind( Amat, diag( x = 1, nSectors, nSectors ) )
bvec                      <- c( 1, rep( 0, nSectors ))
meq                       <- 2

kelly                     <- solve.QP(Dmat,dvec,Amat,bvec=bvec, meq = meq )
dfKelly                   <- data.frame( "sector" = pSectors )
weightsKelly              <- as.data.frame( kelly$solution )  
dfKelly                   <- cbind( dfKelly, weightsKelly )
dfKellyPerf               <- auxGetFolioPerformanceMVOKelly( dfReturnDataTotRet, pSectors, kelly$solution, "kelly" )

# condition number
e                         <- eigen( Dmat )
conditionNumber           <- e$values[ 1 ] / tail(e$values, 1)



# GENERATE PORTFOLIO WEIGHTS RANDOMLY TO CREATE PORTFOLIOS FOR CLUSTER ANALYSIS

# the following data frames are created for further cluster analysis:
#   dfWeights: randomized weights for assets under consideration, without leverage, shorting
#   summaryDf: tracks metrics for portfolio returns, average, sigma, information ratio, skew, kurtosis and maximum drawdown
#   folioReturnsDf: tracks dynamics of portfolio over time

dfWeights                    <- randomWeights( pDraws, pSectors, pNSectors, pLowOmega, pHighOmega )
nPortfolios                  <- nrow( dfWeights )
  
sectors                      <- colnames( dfWeights )
  
for( i in 1:nPortfolios ){
    
  thisFolio                   <- dfWeights[ i, ]  
  portRet                     <- 0
  for( j in 1:pNSectors ){
      
    thisSector                  <- as.character( sectors[ j ] )
    thisWeight                  <- thisFolio[ , c( thisSector ) ]
    portRet                     <- portRet + dfReturnDataTotRet[ , c( thisSector ) ] * thisWeight
      
  }
    
  portRet                     <- 1 + portRet / 100
  cumRet                      <- cumprod(( portRet ) )
  drawdown                    <- calcDrawdown( cumRet )
  dfTmp                       <- data.frame( "PricingDate" = dfReturnDataTotRet$PricingDate, "folio" = i, "return" = portRet - 1, "cumRet" = cumRet )
    
  # summary stats
  folioRet                    <- portRet - 1
  T                           <- length( folioRet )
  mu                          <- mean( folioRet )
  sigma                       <- sd( folioRet ) 
  skew                        <- skewness( folioRet )    
  kurt                        <- kurtosis( folioRet )
  maxDD                       <- max( drawdown )
    
  summaryDfTmp                <- data.frame( "folio" = i, "cumRet" = tail( cumRet, 1 ), "mu" = mu, "sigma" = sigma, "ir" = mu / sigma, "skew" = skew, "kurt" = kurt, "maxDD" = maxDD )
    
  # put dfs together
  if( i == 1 ){
    folioReturnsDf              <- dfTmp
    summaryDf                   <- summaryDfTmp
  } else {
    folioReturnsDf              <- rbind( folioReturnsDf, dfTmp )
    summaryDf                   <- rbind( summaryDf, summaryDfTmp )
  }
    
}
  
# get loss timing
folioReturnsDf                <- castPricingDate( folioReturnsDf, "PricingDate" )
dfTimingOfLoss                <- getTimingOfLoss( folioReturnsDf )  
summaryDf                     <- merge( summaryDf, dfTimingOfLoss, by = c( "folio") )
  


# CALCULATE UTILITES ASSOCIATED WITH RANDOMIZED PORTFOLIOS - USE AS SPECIFICATIONS:
# - PROSPECT THEORY
# - CRRA UTILITY
# - QUADRATIC UTILITY

# don't run code in if() statement, but instead load the data directly from dfUtility.csv

if (FALSE){
  pLambda                       <- 2.25
  dfUtility                     <- prospectTheoryUtility( folioReturnsDf, pGamma, pTau, pLambda, pRiskFree)   
  pLambda                       <- 1
  dfUtilityTmp                  <- prospectTheoryUtility( folioReturnsDf, pGamma, pTau, pLambda, pRiskFree)   
  tmpUtility                    <- dfUtilityTmp$CPTUtility_100
  dfUtility                     <- cbind( dfUtility, tmpUtility )
  colnames( dfUtility )[ grep( "tmpUtility", colnames( dfUtility ), fixed = TRUE ) ]  <- c( "CPTUtility_100")
  pLambda                       <- 1.5
  dfUtilityTmp                  <- prospectTheoryUtility( folioReturnsDf, pGamma, pTau, pLambda, pRiskFree)   
  tmpUtility                    <- dfUtilityTmp$CPTUtility_150
  dfUtility                     <- cbind( dfUtility, tmpUtility )
  colnames( dfUtility )[ grep( "tmpUtility", colnames( dfUtility ), fixed = TRUE ) ]  <- c( "CPTUtility_150")
  dfUtility                     <- cbind( dfUtility, dfUtilityTmp$CPTUtility_150 )
  
  
  # crra utility
  tmpUtility                    <- log( summaryDf$cumRet )
  dfUtility                     <- cbind( dfUtility, tmpUtility )
  colnames( dfUtility )[ grep( "tmpUtility", colnames( dfUtility ), fixed = TRUE ) ]  <- c( "crraUtility_1")
  
  tmpUtility                    <- ( 1 / (1-2)) * summaryDf$cumRet^(1-2)
  dfUtility                     <- cbind( dfUtility, tmpUtility )
  colnames( dfUtility )[ grep( "tmpUtility", colnames( dfUtility ), fixed = TRUE ) ]  <- c( "crraUtility_2")
  
  tmpUtility                    <- ( 1 / (1-3)) * summaryDf$cumRet^(1-3)
  dfUtility                     <- cbind( dfUtility, tmpUtility )
  colnames( dfUtility )[ grep( "tmpUtility", colnames( dfUtility ), fixed = TRUE ) ]  <- c( "crraUtility_3")
  
  tmpUtility                    <- ( 1 / (1-5)) * summaryDf$cumRet^(1-5)
  dfUtility                     <- cbind( dfUtility, tmpUtility )
  colnames( dfUtility )[ grep( "tmpUtility", colnames( dfUtility ), fixed = TRUE ) ]  <- c( "crraUtility_5")
  
  
  # quadratic utility
  tmpUtility                    <- summaryDf$mu - ( 1 / 2 ) * summaryDf$sigma^2
  dfUtility                     <- cbind( dfUtility, tmpUtility )
  colnames( dfUtility )[ grep( "tmpUtility", colnames( dfUtility ), fixed = TRUE ) ]  <- c("quadUtility_1")
  
  tmpUtility                    <- summaryDf$mu - ( 2 / 2 ) * summaryDf$sigma^2
  dfUtility                     <- cbind( dfUtility, tmpUtility )
  colnames( dfUtility )[ grep( "tmpUtility", colnames( dfUtility ), fixed = TRUE ) ]  <- c("quadUtility_2")
  
  tmpUtility                    <- summaryDf$mu - ( 3 / 2 ) * summaryDf$sigma^2
  dfUtility                     <- cbind( dfUtility, tmpUtility )
  colnames( dfUtility )[ grep( "tmpUtility", colnames( dfUtility ), fixed = TRUE ) ]  <- c("quadUtility_3")
  
  tmpUtility                    <- summaryDf$mu - ( 5 / 2 ) * summaryDf$sigma^2
  dfUtility                     <- cbind( dfUtility, tmpUtility )
  colnames( dfUtility )[ grep( "tmpUtility", colnames( dfUtility ), fixed = TRUE ) ]  <- c("quadUtility_5")
}


# read data sets instead of running the code in above two if() statements
if( TRUE ){
  
  fileNameIn                   <- paste( c( pPath, "\\folioReturnsDf.csv" ), collapse ="" )
  folioReturnsDf               <- read.csv( fileNameIn )
  
  fileNameIn                   <- paste( c( pPath, "\\summaryDf.csv" ), collapse ="" )
  summaryDf                    <- read.csv( fileNameIn )
  summaryDf                    <- normalizeData( summaryDf )
  
  
  fileNameIn                   <- paste( c( pPath, "\\dfWeights.csv" ), collapse ="" )
  dfWeights                    <- read.csv( fileNameIn )
  
  fileNameIn                   <- paste( c( pPath, "\\dfUtility.csv" ), collapse ="" )
  dfUtility                    <- read.csv( fileNameIn )
  
}

# last steps of data preparation for cluster and portfolio analysis
pNobs                <- 10000
pNormalized          <- TRUE
dfFolioPerf          <- summaryDf[1:pNobs, ]
dfSectorExposures    <- dfWeights[ 1:pNobs, ]
dfSectorExposures$folio <- seq( 1, nrow(dfSectorExposures),1)
dfFolioPerf          <- merge( dfFolioPerf, dfUtility, by = c( "folio"))



# CALCULATE HOPKINS STATISTICS (TEST FOR WHETHER DATA CAN BE APPLIED TO CLUSTER ANALYSIS IN THE FIRST PLACE)

if( FALSE ){
  
  
  features <- list( 
    c( "ir", "maxDD" ),
    c( "ir", "skew" ),
    c( "ir", "kurt" ),
    c( "ir", "skew", "maxDD" ),
    c( "ir", "skew", "kurt"),
    c( "ir", "mu", "sigma", "skew", "maxDD" ),
    c( "ir", "mu", "sigma", "skew", "kurt" ),
    c( "cumRet", "sigma", "skew", "maxDD" ),
    c( "cumRet", "sigma", "skew", "kurt" ),
    c( "mu", "sigma", "skew", "maxDD" ),
    c( "mu", "sigma", "skew", "kurt" )
  )
  
  pNobs         <- c( 10000 )
  pNHopkins     <- c( 0.05, 0.1 )
  
  count         <- 0
  for( i in 1:length(pNobs ) ){
    nobs <- pNobs[ i ]    
    for( j in 1:length( features )){
      
      columns              <- features[[j]]
      clusterDimensions    <- auxGetClusterDimensions( columns )
      if( pNormalized ) columns <- auxGetNormalizedNames( columns )
      test                 <- summaryDf[ 1:nobs, columns ]
      
      for( k in 1:length( pNHopkins )){
        
        hopkinsSample <- pNHopkins[ k ] * nobs
        hpk           <- hopkins( test, hopkinsSample )
        
        dfTmp <- data.frame( "clusterDimension" = clusterDimensions, "nobs" = nobs, "hopkinsN" = hopkinsSample, "hopkinsStat" = hpk$H )      
        if( count == 0 ){
          dfHopkins <- dfTmp
        } else {
          dfHopkins <- rbind( dfHopkins, dfTmp )
        }
        count <- count + 1 
      }
    }
  }
  fileNameOut                   <- paste( c( pPath, "\\hopkinsStat.csv" ), collapse ="" )
  write.csv(x = dfHopkins, file = fileNameOut, row.names = FALSE, na = "")
  
}


# INSPECT UTILITY
if( FALSE ){
  pdfFileName <- paste( c( pPath, "\\", pdfFileNameInspectUtil, "_", as.character(pNobs), ".pdf" ), collapse = "" )
  pdf( pdfFileName )
  plot.new(); plot.window(xlim=c(0,1),ylim=c(0,1))
  titleStr                      <- paste( c( "Inspect Utility Functions", "\n", as.character( pNobs), " observations" ), collapse = "" )
  text( 0.5, 0.5, lab=titleStr, cex = 1.2)
  plot.new(); plot.window(xlim=c(0,1),ylim=c(0,1))
  titleStr                      <- paste( c( "Cumulative Prospect Theory", "\n", "NIG process" ), collapse = "" )
  text( 0.5, 0.5, lab=titleStr, cex = 1.2)
  
  plotXvariables               <- c( "mu", "sigma", "skew", "kurt", "cumRet", "maxDD" )
  plotUtility( dfFolioPerf, pSectors, plotXvariables, pRows, pCols, "CPT", "CPTUtility_225")
  plotUtility( dfFolioPerf, pSectors, plotXvariables, pRows, pCols, "CPT", "CPTUtility_150")
  plotUtility( dfFolioPerf, pSectors, plotXvariables, pRows, pCols, "CPT", "CPTUtility_100")
  
  plot.new(); plot.window(xlim=c(0,1),ylim=c(0,1))
  titleStr                      <- paste( c( "CRRA Utility" ), collapse = "" )
  text( 0.5, 0.5, lab=titleStr, cex = 1.2)
  
  plotUtility( dfFolioPerf, pSectors, plotXvariables, pRows, pCols, "CRRA", "crraUtility_1")
  #plotUtility( dfFolioPerf, pSectors, plotXvariables, pRows, pCols, "CRRA", "crraUtility_2")
  plotUtility( dfFolioPerf, pSectors, plotXvariables, pRows, pCols, "CRRA", "crraUtility_3")
  plotUtility( dfFolioPerf, pSectors, plotXvariables, pRows, pCols, "CRRA", "crraUtility_5")
  
  plot.new(); plot.window(xlim=c(0,1),ylim=c(0,1))
  titleStr                      <- paste( c( "Quadratic Utility" ), collapse = "" )
  text( 0.5, 0.5, lab=titleStr, cex = 1.2)
  
  plotUtility( dfFolioPerf, pSectors, plotXvariables, pRows, pCols, "Quadratic", "quadUtility_1")
  #plotUtility( dfFolioPerf, pSectors, plotXvariables, pRows, pCols, "Quadratic", "quadUtility_2")
  plotUtility( dfFolioPerf, pSectors, plotXvariables, pRows, pCols, "Quadratic", "quadUtility_3")
  plotUtility( dfFolioPerf, pSectors, plotXvariables, pRows, pCols, "Quadratic", "quadUtility_5")
  
  
  dev.off()  
}





# CLUSTER ANALYSES
# reduce pNobs for quick inspections and iterations
# reset pNobs to 10k afterwards

pNobs                         <- 2000
pNormalized                   <- TRUE


# OPTIMAL NUMBER CLUSTERS

if( pNormalized ){
  pdfFileName <- paste( c( pPdfFileNameNonSphericalOptimalNCluster, "_", as.character( pNobs ), "_norm.pdf" ), collapse = "" )
} else {
  pdfFileName <- paste( c( pPdfFileNameNonSphericalOptimalNCluster, "_", as.character( pNobs ), ".pdf" ), collapse = "" )
}
pdfFileName <- paste( c( pPath, "\\", pdfFileName ), collapse = "" )
pdf( pdfFileName )



# for reader's reference, we tested various clustering strategies of the simulated portfolios
if( FALSE ){
  
  # 1. c( "ir", "maxDD" )
  columns              <- c( "ir", "maxDD" )
  clusterDimensions    <- auxGetClusterDimensions( columns )
  if( pNormalized ) columns <- auxGetNormalizedNames( columns )
  test                 <- summaryDf[ 1:pNobs, columns ]
  
  # for kMeans
  plot.new(); plot.window(xlim=c(0,1),ylim=c(0,1))
  titleStr                      <- paste( c( "k-Means Cluster - ", "\n", "Optimal # clusters",  "\n", clusterDimensions,  "\n(", as.character(pNobs), " sample)" ), collapse = "" )
  text( 0.5, 0.5, lab=titleStr, cex = 1.2)
  
  fviz_nbclust( test, kmeans, method = c("wss"))
  if( pNobs < 5000 ){
    fviz_nbclust( test, kmeans, method = c("gap_stat"))  
  }
  fviz_nbclust( test, kmeans, method = c("silhouette"))
  
  # for horizontal cut in hierarchical clustering
  plot.new(); plot.window(xlim=c(0,1),ylim=c(0,1))
  titleStr                      <- paste( c( "Hierarchical Cluster - ", "\n", "Optimal # clusters",  "\n", clusterDimensions,  "\n(", as.character(pNobs), " sample)" ), collapse = "" )
  text( 0.5, 0.5, lab=titleStr, cex = 1.2)
  
  fviz_nbclust( test, hcut, method = c("wss"))
  if( pNobs < 5000 ){
    fviz_nbclust( test, kmeans, method = c("gap_stat"))  
  }
  fviz_nbclust( test, hcut, method = c("silhouette"))
  
  # majority rule
  plot.new(); plot.window(xlim=c(0,1),ylim=c(0,1))
  titleStr                      <- paste( c( "Majority rule - ", "\n", "Optimal # clusters",  "\n", clusterDimensions,  "\n(", as.character(pNobs), " sample)" ), collapse = "" )
  text( 0.5, 0.5, lab=titleStr, cex = 1.2)
  if( pNobs < 5000 ){
    aa            <- NbClust( test, method="complete", index="all")
    fviz_nbclust( aa,ggtheme=theme_minimal())
  }
  
  
  
  # 2. c( "ir", "skew", "maxDD"  )
  columns              <- c( "ir", "skew", "maxDD" )
  clusterDimensions    <- auxGetClusterDimensions( columns )
  if( pNormalized ) columns <- auxGetNormalizedNames( columns )
  test                 <- summaryDf[ 1:pNobs, columns ]
  
  # for kMeans
  plot.new(); plot.window(xlim=c(0,1),ylim=c(0,1))
  titleStr                      <- paste( c( "k-Means Cluster - ", "\n", "Optimal # clusters",  "\n", clusterDimensions,  "\n(", as.character(pNobs), " sample)" ), collapse = "" )
  text( 0.5, 0.5, lab=titleStr, cex = 1.2)
  
  fviz_nbclust( test, kmeans, method = c("wss"))
  if( pNobs < 5000 ){
    fviz_nbclust( test, kmeans, method = c("gap_stat"))  
  }
  fviz_nbclust( test, kmeans, method = c("silhouette"))
  
  # for horizontal cut in hierarchical clustering
  plot.new(); plot.window(xlim=c(0,1),ylim=c(0,1))
  titleStr                      <- paste( c( "Hierarchical Cluster - ", "\n", "Optimal # clusters",  "\n", clusterDimensions,  "\n(", as.character(pNobs), " sample)" ), collapse = "" )
  text( 0.5, 0.5, lab=titleStr, cex = 1.2)
  
  fviz_nbclust( test, hcut, method = c("wss"))
  if( pNobs < 5000 ){
    fviz_nbclust( test, kmeans, method = c("gap_stat"))  
  }
  fviz_nbclust( test, hcut, method = c("silhouette"))
  
  # majority rule
  plot.new(); plot.window(xlim=c(0,1),ylim=c(0,1))
  titleStr                      <- paste( c( "Majority rule - ", "\n", "Optimal # clusters",  "\n", clusterDimensions,  "\n(", as.character(pNobs), " sample)" ), collapse = "" )
  text( 0.5, 0.5, lab=titleStr, cex = 1.2)
  if( pNobs < 5000 ){
    aa            <- NbClust( test, method="complete", index="all")
    fviz_nbclust( aa,ggtheme=theme_minimal())
  }
  
  
  
  
  # 3. c( "cumRet", "sigma", "skew", "maxDD" )
  columns              <- c( "cumRet", "sigma", "skew", "maxDD" )
  clusterDimensions    <- auxGetClusterDimensions( columns )
  if( pNormalized ) columns <- auxGetNormalizedNames( columns )
  test                 <- summaryDf[ 1:pNobs, columns ]
  
  # for kMeans
  plot.new(); plot.window(xlim=c(0,1),ylim=c(0,1))
  titleStr                      <- paste( c( "k-Means Cluster - ", "\n", "Optimal # clusters",  "\n", clusterDimensions,  "\n(", as.character(pNobs), " sample)" ), collapse = "" )
  text( 0.5, 0.5, lab=titleStr, cex = 1.2)
  
  fviz_nbclust( test, kmeans, method = c("wss"))
  if( pNobs < 5000 ){
    fviz_nbclust( test, kmeans, method = c("gap_stat"))  
  }
  fviz_nbclust( test, kmeans, method = c("silhouette"))
  
  # for horizontal cut in hierarchical clustering
  plot.new(); plot.window(xlim=c(0,1),ylim=c(0,1))
  titleStr                      <- paste( c( "Hierarchical Cluster - ", "\n", "Optimal # clusters",  "\n", clusterDimensions,  "\n(", as.character(pNobs), " sample)" ), collapse = "" )
  text( 0.5, 0.5, lab=titleStr, cex = 1.2)
  
  fviz_nbclust( test, hcut, method = c("wss"))
  if( pNobs < 5000 ){
    fviz_nbclust( test, kmeans, method = c("gap_stat"))  
  }
  fviz_nbclust( test, hcut, method = c("silhouette"))
  
  # majority rule
  plot.new(); plot.window(xlim=c(0,1),ylim=c(0,1))
  titleStr                      <- paste( c( "Majority rule - ", "\n", "Optimal # clusters",  "\n", clusterDimensions,  "\n(", as.character(pNobs), " sample)" ), collapse = "" )
  text( 0.5, 0.5, lab=titleStr, cex = 1.2)
  if( pNobs < 5000 ){
    aa            <- NbClust( test, method="complete", index="all")
    fviz_nbclust( aa,ggtheme=theme_minimal())
  }
  
}


# 4. c( "ir", "skew", "kurt"  )
columns              <- c( "ir", "skew", "kurt" )
clusterDimensions    <- auxGetClusterDimensions( columns )
if( pNormalized ) columns <- auxGetNormalizedNames( columns )
test                 <- summaryDf[ 1:pNobs, columns ]

# for kMeans
plot.new(); plot.window(xlim=c(0,1),ylim=c(0,1))
titleStr                      <- paste( c( "k-Means Cluster - ", "\n", "Optimal # clusters",  "\n", clusterDimensions,  "\n(", as.character(pNobs), " sample)" ), collapse = "" )
text( 0.5, 0.5, lab=titleStr, cex = 1.2)

fviz_nbclust( test, kmeans, method = c("wss"))
if( pNobs < 5000 ){
  fviz_nbclust( test, kmeans, method = c("gap_stat"))  
}
fviz_nbclust( test, kmeans, method = c("silhouette"))

# for horizontal cut in hierarchical clustering
plot.new(); plot.window(xlim=c(0,1),ylim=c(0,1))
titleStr                      <- paste( c( "Hierarchical Cluster - ", "\n", "Optimal # clusters",  "\n", clusterDimensions,  "\n(", as.character(pNobs), " sample)" ), collapse = "" )
text( 0.5, 0.5, lab=titleStr, cex = 1.2)

fviz_nbclust( test, hcut, method = c("wss"))
if( pNobs < 5000 ){
  fviz_nbclust( test, kmeans, method = c("gap_stat"))  
}
fviz_nbclust( test, hcut, method = c("silhouette"))

# majority rule
plot.new(); plot.window(xlim=c(0,1),ylim=c(0,1))
titleStr                      <- paste( c( "Majority rule - ", "\n", "Optimal # clusters",  "\n", clusterDimensions,  "\n(", as.character(pNobs), " sample)" ), collapse = "" )
text( 0.5, 0.5, lab=titleStr, cex = 1.2)
if( pNobs < 5000 ){
  aa            <- NbClust( test, method="complete", index="all")
  fviz_nbclust( aa,ggtheme=theme_minimal())
}


# 5. c( "cumRet", "sigma", "skew", "maxDD" )
columns              <- c( "cumRet", "sigma", "skew", "kurt" )
clusterDimensions    <- auxGetClusterDimensions( columns )
if( pNormalized ) columns <- auxGetNormalizedNames( columns )
test                 <- summaryDf[ 1:pNobs, columns ]

# for kMeans
plot.new(); plot.window(xlim=c(0,1),ylim=c(0,1))
titleStr                      <- paste( c( "k-Means Cluster - ", "\n", "Optimal # clusters",  "\n", clusterDimensions,  "\n(", as.character(pNobs), " sample)" ), collapse = "" )
text( 0.5, 0.5, lab=titleStr, cex = 1.2)

fviz_nbclust( test, kmeans, method = c("wss"))
if( pNobs < 5000 ){
  fviz_nbclust( test, kmeans, method = c("gap_stat"))  
}
fviz_nbclust( test, kmeans, method = c("silhouette"))

# for horizontal cut in hierarchical clustering
plot.new(); plot.window(xlim=c(0,1),ylim=c(0,1))
titleStr                      <- paste( c( "Hierarchical Cluster - ", "\n", "Optimal # clusters",  "\n", clusterDimensions,  "\n(", as.character(pNobs), " sample)" ), collapse = "" )
text( 0.5, 0.5, lab=titleStr, cex = 1.2)

fviz_nbclust( test, hcut, method = c("wss"))
if( pNobs < 5000 ){
  fviz_nbclust( test, kmeans, method = c("gap_stat"))  
}
fviz_nbclust( test, hcut, method = c("silhouette"))

# majority rule
plot.new(); plot.window(xlim=c(0,1),ylim=c(0,1))
titleStr                      <- paste( c( "Majority rule - ", "\n", "Optimal # clusters",  "\n", clusterDimensions,  "\n(", as.character(pNobs), " sample)" ), collapse = "" )
text( 0.5, 0.5, lab=titleStr, cex = 1.2)
if( pNobs < 5000 ){
  aa            <- NbClust( test, method="complete", index="all")
  fviz_nbclust( aa,ggtheme=theme_minimal())
}




dev.off()








# NON-SPHERICAL CLUSTERING ANALYSIS

pNobs                         <- 1000
pNormalized                   <- TRUE

if( pNormalized ){
  pdfFileName <- paste( c( pPdfFileNameNonSphericalClusterInspection, "_", as.character( pNobs ), "_norm.pdf" ), collapse = "" )
} else {
  pdfFileName <- paste( c( pPdfFileNameNonSphericalClusterInspection, "_", as.character( pNobs ), ".pdf" ), collapse = "" )
}
pdfFileName <- paste( c( pPath, "\\", pdfFileName ), collapse = "" )
pdf( pdfFileName )


plot.new(); plot.window(xlim=c(0,1),ylim=c(0,1))
titleStr                      <- paste( c( "k-Means Cluster - ", "\n", "Cluster visualization",  "\n", "(", as.character( pNobs ), " sample)" ), collapse = "" )
text( 0.5, 0.5, lab=titleStr, cex = 2.5)


op                            <- par( mfrow=c( pRows, pCols ), oma=c(1,2,2,1) )
clusters                      <- seq( 2,6,1)
plotYvariable                 <- "ir"
plotXvariables                <- c( "sigma", "skew", "kurt", "maxDD" )
pClusterType                  <- "kMeans"

# K-MEANS CLUSTERING
columns              <- c( "ir", "maxDD" )
clusterDimensions    <- auxGetClusterDimensions( columns )
if( pNormalized ) columns <- auxGetNormalizedNames( columns )
test                 <- summaryDf[ 1:pNobs, columns ]
kMean2<-kmeans( test, 2)
kMean3<-kmeans( test, 3); plotClusters( summaryDf, kMean3, plotYvariable, plotXvariables, paste( c( 3, " ", clusterDimensions ), collapse=""), pClusterType )
kMean4<-kmeans( test, 4); plotClusters( summaryDf, kMean4, plotYvariable, plotXvariables, paste( c( 4, " ", clusterDimensions ), collapse=""), pClusterType )
kMean5<-kmeans( test, 5); plotClusters( summaryDf, kMean5, plotYvariable, plotXvariables, paste( c( 5, " ", clusterDimensions ), collapse=""), pClusterType )
kMean6<-kmeans( test, 6); plotClusters( summaryDf, kMean6, plotYvariable, plotXvariables, paste( c( 6, " ", clusterDimensions ), collapse=""), pClusterType )
dfKMeansSummary      <- summarizeKmeansStats( clusters, kMeans2, kMeans3, kMeans4, kMeans5, kMeans6, clusterDimensions )


columns              <- c( "ir", "skew", "maxDD" )
clusterDimensions    <- auxGetClusterDimensions( columns )
if( pNormalized ) columns <- auxGetNormalizedNames( columns )
test                 <- summaryDf[ 1:pNobs, columns ]
kMean3<-kmeans( test, 3); plotClusters( summaryDf, kMean3, plotYvariable, plotXvariables, paste( c( 3, " ", clusterDimensions ), collapse=""), pClusterType )
kMean4<-kmeans( test, 4); plotClusters( summaryDf, kMean4, plotYvariable, plotXvariables, paste( c( 4, " ", clusterDimensions ), collapse=""), pClusterType )
kMean5<-kmeans( test, 5); plotClusters( summaryDf, kMean5, plotYvariable, plotXvariables, paste( c( 5, " ", clusterDimensions ), collapse=""), pClusterType )
kMean6<-kmeans( test, 6); plotClusters( summaryDf, kMean6, plotYvariable, plotXvariables, paste( c( 6, " ", clusterDimensions ), collapse=""), pClusterType )
dfKMeansSummary      <- rbind( dfKMeansSummary, summarizeKmeansStats( clusters, kMeans2, kMeans3, kMeans4, kMeans5, kMeans6, clusterDimensions ) )

columns              <- c( "cumRet", "sigma", "skew", "maxDD" )
clusterDimensions    <- auxGetClusterDimensions( columns )
if( pNormalized ) columns <- auxGetNormalizedNames( columns )
test                 <- summaryDf[ 1:pNobs, columns ]
kMean2<-kmeans( test, 2)
kMean3<-kmeans( test, 3); plotClusters( summaryDf, kMean3, plotYvariable, plotXvariables, paste( c( 3, " ", clusterDimensions ), collapse=""), pClusterType )
kMean4<-kmeans( test, 4); plotClusters( summaryDf, kMean4, plotYvariable, plotXvariables, paste( c( 4, " ", clusterDimensions ), collapse=""), pClusterType )
kMean5<-kmeans( test, 5); plotClusters( summaryDf, kMean5, plotYvariable, plotXvariables, paste( c( 5, " ", clusterDimensions ), collapse=""), pClusterType )
kMean6<-kmeans( test, 6); plotClusters( summaryDf, kMean6, plotYvariable, plotXvariables, paste( c( 6, " ", clusterDimensions ), collapse=""), pClusterType )
dfKMeansSummary      <- rbind( dfKMeansSummary, summarizeKmeansStats( clusters, kMeans2, kMeans3, kMeans4, kMeans5, kMeans6, clusterDimensions ) )

par(op )



# HIERARCHICAL CLUSTERING
pClusterType         <- "hierarchical"

plot.new(); plot.window(xlim=c(0,1),ylim=c(0,1))
titleStr             <- paste( c( "Hierarchical Cluster - ", "\n", "Cluster visualization",  "\n", "(", as.character(pNobs), " sample)"), collapse = "" )
text( 0.5, 0.5, lab=titleStr, cex = 2.5)



op                            <- par( mfrow=c( pRows, pCols ), oma=c(1,2,2,1) )


columns              <- c( "ir", "maxDD" )
clusterDimensions    <- auxGetClusterDimensions( columns )
if( pNormalized ) columns <- auxGetNormalizedNames( columns )
test                 <- summaryDf[ 1:pNobs, columns ]
hc                   <- hclust( dist(test))
clust3               <- cutree(hc, k = 3); plotClusters( summaryDf, clust3, plotYvariable, plotXvariables, paste( c( 3, " ", clusterDimensions ), collapse=""), pClusterType ) 
clust4               <- cutree(hc, k = 4); plotClusters( summaryDf, clust4, plotYvariable, plotXvariables, paste( c( 4, " ", clusterDimensions ), collapse=""), pClusterType ) 
clust5               <- cutree(hc, k = 5); plotClusters( summaryDf, clust5, plotYvariable, plotXvariables, paste( c( 5, " ", clusterDimensions ), collapse=""), pClusterType ) 
clust6               <- cutree(hc, k = 6); plotClusters( summaryDf, clust6, plotYvariable, plotXvariables, paste( c( 6, " ", clusterDimensions ), collapse=""), pClusterType ) 

columns              <- c( "ir", "skew", "maxDD" )
clusterDimensions    <- auxGetClusterDimensions( columns )
if( pNormalized ) columns <- auxGetNormalizedNames( columns )
test                 <- summaryDf[ 1:pNobs, columns ]
hc                   <- hclust( dist(test))
clust3               <- cutree(hc, k = 3); plotClusters( summaryDf, clust3, plotYvariable, plotXvariables, paste( c( 3, " ", clusterDimensions ), collapse=""), pClusterType ) 
clust4               <- cutree(hc, k = 4); plotClusters( summaryDf, clust4, plotYvariable, plotXvariables, paste( c( 4, " ", clusterDimensions ), collapse=""), pClusterType ) 
clust5               <- cutree(hc, k = 5); plotClusters( summaryDf, clust5, plotYvariable, plotXvariables, paste( c( 5, " ", clusterDimensions ), collapse=""), pClusterType ) 
clust6               <- cutree(hc, k = 6); plotClusters( summaryDf, clust6, plotYvariable, plotXvariables, paste( c( 6, " ", clusterDimensions ), collapse=""), pClusterType ) 


columns              <- c( "cumRet", "sigma", "skew", "maxDD" )
clusterDimensions    <- auxGetClusterDimensions( columns )
if( pNormalized ) columns <- auxGetNormalizedNames( columns )
test                 <- summaryDf[ 1:pNobs, columns ]
hc                   <- hclust( dist(test))
clust3               <- cutree(hc, k = 3); plotClusters( summaryDf, clust3, plotYvariable, plotXvariables, paste( c( 3, " ", clusterDimensions ), collapse=""), pClusterType ) 
clust4               <- cutree(hc, k = 4); plotClusters( summaryDf, clust4, plotYvariable, plotXvariables, paste( c( 4, " ", clusterDimensions ), collapse=""), pClusterType ) 
clust5               <- cutree(hc, k = 5); plotClusters( summaryDf, clust5, plotYvariable, plotXvariables, paste( c( 5, " ", clusterDimensions ), collapse=""), pClusterType ) 
clust6               <- cutree(hc, k = 6); plotClusters( summaryDf, clust6, plotYvariable, plotXvariables, paste( c( 6, " ", clusterDimensions ), collapse=""), pClusterType ) 


par(op )


# summary of kMeans
plot.new(); plot.window(xlim=c(0,1),ylim=c(0,1))
titleStr                      <- paste( c( "k-Means Cluster - ", "\n", "Model inspections",  "\n", "(", as.character(pNobs), " sample)", "\n", "loss function"  ), collapse = "" )
text( 0.5, 0.5, lab=titleStr, cex = 2.5)

op                            <- par( mfrow=c( pRows, pCols ), oma=c(1,2,2,1) )
visualizeKMeansSummary( dfKMeansSummary, c( "withinss", "betweenss", "size", "iter"), pRows, pCols )
par( op )


dev.off()





# SPHERICAL CLUSTERING
pNobs                <- 10000
epsilonVec           <- seq( 0, 1, 0.02 )

# DBSCAN 1
columns              <- c( "ir", "maxDD"  );    
clusterDimensions    <- auxGetClusterDimensions( columns )
pClusterType         <- "dbscan"

# create clusters
if( pNormalized ) columns <- auxGetNormalizedNames( columns )
minPts               <- 4
test                 <- dfFolioPerf[ 1:pNobs, columns]
kNNdistplot(test, k=minPts)
knnCalibEps          <- 0.11
titleStr             <- paste( c( as.character(clusterDimensions), "\nDBSCAN", "\n", "minPts: ", as.character(minPts), ",  knnCalibEps: ",   as.character( knnCalibEps) ),collapse = "" ) 
db                   <- dbscan( test, eps = knnCalibEps, minPts = minPts )
fviz_cluster( db, data = test, main = titleStr, stand = FALSE, ellipse = TRUE , outlier.shape = 10, labelsize = 1)

for( i in 1:length( epsilonVec ) ){
  
  thisEps             <- epsilonVec[ i ]
  db                  <- dbscan( test, eps = thisEps, minPts = minPts )
  dbClust             <- as.data.frame( db$cluster )
  names(dbClust )     <- c( "cluster")
  dfDbClustSummaryTmp <- dbClustSummary( dbClust, thisEps, minPts, clusterDimensions )
  if( i == 1){
    dfDbClustSummary    <- dfDbClustSummaryTmp
  } else {
    dfDbClustSummary    <- rbind( dfDbClustSummary, dfDbClustSummaryTmp )
  }
  
}
title <- paste( c( clusterDimensions, ", minPts: ", as.character( minPts )), collapse = "")
plotDbScanInspection( dfDbClustSummary, title )

# OPTICS
res <- optics( test, eps = 1, minPts = minPts )
titleStr             <- paste( c( as.character(clusterDimensions), "\nOTPICS"), collapse = "" ) 
plot( res, main=titleStr)
abline( h = knnCalibEps, col = "red")

# HDBSCAN
res <- hdbscan( test, minPts )
plot( res)
plot(test, col=res$cluster+1L, cex = .5)




# DBSCAN 2
columns              <- c( "ir", "skew", "maxDD"  );    
clusterDimensions    <- auxGetClusterDimensions( columns )
pClusterType         <- "dbscan"


# create clusters
if( pNormalized ) columns <- auxGetNormalizedNames( columns )
test                 <- dfFolioPerf[ 1:pNobs, columns]
minPts               <- 6
kNNdistplot(test, k=minPts)
knnCalibEps          <- 0.20    # 0.21  vs 0.1
titleStr             <- paste( c( as.character(clusterDimensions), "\nDBSCAN", "\n", "minPts: ", as.character(minPts), ",  knnCalibEps: ",   as.character( knnCalibEps) ),collapse = "" ) 
db                   <- dbscan( test, eps = knnCalibEps, minPts = minPts )
fviz_cluster( db, data = test, main = titleStr, stand = FALSE, ellipse = TRUE , outlier.shape = 10, labelsize = 1)


for( i in 1:length( epsilonVec ) ){
  
  thisEps             <- epsilonVec[ i ]
  db                  <- dbscan( test, eps = thisEps, minPts = minPts )
  dbClust             <- as.data.frame( db$cluster )
  names(dbClust )     <- c( "cluster")
  dfDbClustSummaryTmp <- dbClustSummary( dbClust, thisEps, minPts, clusterDimensions )
  if( i == 1){
    dfDbClustSummary    <- dfDbClustSummaryTmp
  } else {
    dfDbClustSummary    <- rbind( dfDbClustSummary, dfDbClustSummaryTmp )
  }
  
}
title <- paste( c( clusterDimensions, ", minPts: ", as.character( minPts )), collapse = "")
plotDbScanInspection( dfDbClustSummary, title )

# OPTICS
res <- optics( test, eps = 10, minPts = 3*minPts )
titleStr             <- paste( c( as.character(clusterDimensions), "\nOTPICS"), collapse = "" ) 
plot( res, main=titleStr)
abline( h = knnCalibEps, col = "red")

# HDBSCAN
res <- hdbscan( test, minPts )
plot( res)
plot(test, col=res$cluster+1L, cex = .5)






# DBSCAN 3
columns              <- c(  "cumRet", "sigma", "skew", "maxDD" );    
clusterDimensions    <- auxGetClusterDimensions( columns )
pClusterType         <- "dbscan"


# create clusters
if( pNormalized ) columns <- auxGetNormalizedNames( columns )
test                 <- dfFolioPerf[ 1:pNobs, columns]
minPts               <- 8
kNNdistplot(test, k=minPts)
knnCalibEps          <- 0.165     #0.28
titleStr             <- paste( c( as.character(clusterDimensions), "\nDBSCAN", "\n", "minPts: ", as.character(minPts), ",  knnCalibEps: ",   as.character( knnCalibEps) ),collapse = "" ) 
db                   <- dbscan( test, eps = knnCalibEps, minPts = minPts )
fviz_cluster( db, data = test, main = titleStr, stand = FALSE, ellipse = TRUE , outlier.shape = 10, labelsize = 1)

for( i in 1:length( epsilonVec ) ){
  
  thisEps             <- epsilonVec[ i ]
  db                  <- dbscan( test, eps = thisEps, minPts = minPts )
  dbClust             <- as.data.frame( db$cluster )
  names(dbClust )     <- c( "cluster")
  dfDbClustSummaryTmp <- dbClustSummary( dbClust, thisEps, minPts, clusterDimensions )
  if( i == 1){
    dfDbClustSummary    <- dfDbClustSummaryTmp
  } else {
    dfDbClustSummary    <- rbind( dfDbClustSummary, dfDbClustSummaryTmp )
  }
  
}
title <- paste( c( clusterDimensions, ", minPts: ", as.character( minPts )), collapse = "")
plotDbScanInspection( dfDbClustSummary, title )

# OPTICS
res <- optics( test, eps = 1, minPts = minPts )
titleStr             <- paste( c( as.character(clusterDimensions), "\nOTPICS"), collapse = "" ) 
plot( res, main=titleStr)
abline( h = knnCalibEps, col = "red")

# HDBSCAN
res <- hdbscan( test, minPts )
plot( res)
plot(test, col=res$cluster+1L, cex = .5)



# DBSCAN 4
columns              <- c(  "ir", "skew", "kurt" );    
clusterDimensions    <- auxGetClusterDimensions( columns )
pClusterType         <- "dbscan"


# create clusters
if( pNormalized ) columns <- auxGetNormalizedNames( columns )
test                 <- dfFolioPerf[ 1:pNobs, columns]
minPts               <- 6
kNNdistplot(test, k=minPts)
knnCalibEps          <- 0.34     #0.28
titleStr             <- paste( c( as.character(clusterDimensions), "\nDBSCAN", "\n", "minPts: ", as.character(minPts), ",  knnCalibEps: ",   as.character( knnCalibEps) ),collapse = "" ) 
db                   <- dbscan( test, eps = knnCalibEps, minPts = minPts )
fviz_cluster( db, data = test, main = titleStr, stand = FALSE, ellipse = TRUE , outlier.shape = 10, labelsize = 1)

for( i in 1:length( epsilonVec ) ){
  
  thisEps             <- epsilonVec[ i ]
  db                  <- dbscan( test, eps = thisEps, minPts = minPts )
  dbClust             <- as.data.frame( db$cluster )
  names(dbClust )     <- c( "cluster")
  dfDbClustSummaryTmp <- dbClustSummary( dbClust, thisEps, minPts, clusterDimensions )
  if( i == 1){
    dfDbClustSummary    <- dfDbClustSummaryTmp
  } else {
    dfDbClustSummary    <- rbind( dfDbClustSummary, dfDbClustSummaryTmp )
  }
  
}
title <- paste( c( clusterDimensions, ", minPts: ", as.character( minPts )), collapse = "")
plotDbScanInspection( dfDbClustSummary, title )

# OPTICS
res <- optics( test, eps = 1, minPts = minPts )
titleStr             <- paste( c( as.character(clusterDimensions), "\nOTPICS"), collapse = "" ) 
plot( res, main=titleStr)
abline( h = knnCalibEps, col = "red")

# HDBSCAN
res <- hdbscan( test, minPts )
plot( res)
plot(test, col=res$cluster+1L, cex = .5)



# DBSCAN 5
columns              <- c(  "cumRet", "sigma","skew", "kurt" )    
clusterDimensions    <- auxGetClusterDimensions( columns )
pClusterType         <- "dbscan"


# create clusters
if( pNormalized ) columns <- auxGetNormalizedNames( columns )
test                 <- dfFolioPerf[ 1:pNobs, columns]
minPts               <- 8
#kNNdistplot(test, k=minPts)
knnCalibEps          <- 0.34     #0.28
titleStr             <- paste( c( as.character(clusterDimensions), "\nDBSCAN", "\n", "minPts: ", as.character(minPts), ",  knnCalibEps: ",   as.character( knnCalibEps) ),collapse = "" ) 
db                   <- dbscan( test, eps = knnCalibEps, minPts = minPts )
fviz_cluster( db, data = test, main = titleStr, stand = FALSE, ellipse = TRUE , outlier.shape = 10, labelsize = 1)

for( i in 1:length( epsilonVec ) ){
  
  thisEps             <- epsilonVec[ i ]
  db                  <- dbscan( test, eps = thisEps, minPts = minPts )
  dbClust             <- as.data.frame( db$cluster )
  names(dbClust )     <- c( "cluster")
  dfDbClustSummaryTmp <- dbClustSummary( dbClust, thisEps, minPts, clusterDimensions )
  if( i == 1){
    dfDbClustSummary    <- dfDbClustSummaryTmp
  } else {
    dfDbClustSummary    <- rbind( dfDbClustSummary, dfDbClustSummaryTmp )
  }
  
}
title <- paste( c( clusterDimensions, ", minPts: ", as.character( minPts )), collapse = "")
plotDbScanInspection( dfDbClustSummary, title )

# OPTICS
res <- optics( test, eps = 1, minPts = minPts )
titleStr             <- paste( c( as.character(clusterDimensions), "\nOTPICS"), collapse = "" ) 
plot( res, main=titleStr)
abline( h = knnCalibEps, col = "red")

# HDBSCAN
res <- hdbscan( test, minPts )
plot( res)
plot(test, col=res$cluster+1L, cex = .5)




# SUMMARIZE HDBSCAN
if( FALSE ){
  
  pNobs                <- 10000
  pListDBScan          <- list( c( "ir", "maxDD" ), c( "ir", "skew", "maxDD" ), c(  "cumRet", "sigma","skew", "maxDD" ), c( "ir", "skew", "kurt" ), c(  "cumRet", "sigma","skew", "kurt" )    )
  minPts               <- c( 8, 7, 6, 5, 4, 3 )
  count                <- 0
  for( i in 1:length( pListDBScan ) ){
    
    columns            <- pListDBScan[[i]]
    if( pNormalized ) columns <- auxGetNormalizedNames( columns )
    clusterDimensions    <- auxGetClusterDimensions( columns )
    test               <- dfFolioPerf[ 1:pNobs, columns]
    
    for( j in 1:length( minPts ) ){
      
      thisMinPt           <- minPts[ j ]
      db                  <- hdbscan( test, thisMinPt )
      plotData            <- as.data.frame( db$cluster )  
      uniqueClusters      <- unique(plotData$`db$cluster`)
      noise               <- sum( plotData$`db$cluster` == 0 )
      dfTmp               <- data.frame( "clusterDim" = clusterDimensions, "minPts" = thisMinPt, "numClusters" = length( uniqueClusters) - 1, "pctNoise" = 100 * noise / pNobs )   
      if( count == 0 ){
        dfHDBSCAN           <- dfTmp
      } else {
        dfHDBSCAN           <- rbind( dfHDBSCAN, dfTmp )
      }
      count               <- count + 1
    }
    
  }
  
  fileNameOut          <- paste( c( pPath, "\\", "R_HDBSCAN.csv" ), collapse ="" )
  write.csv(x = dfHDBSCAN, file = fileNameOut, row.names = FALSE, na = "")
  
}









# VISUALIZE SPHERICAL CLUSTERING ANALYSIS
pNobs                         <- 10000
pClusterType                  <- "dbScan"
plotYvariable                 <- "ir"
plotXvariables                <- c( "sigma", "skew", "kurt", "maxDD" )


if( pNormalized ){
  pdfFileName <- paste( c( pPdfFileNameSphericalClusterInspection, "_", as.character( pNobs ), "_norm.pdf" ), collapse = "" )
} else {
  pdfFileName <- paste( c( pPdfFileNameSphericalClusterInspection, "_", as.character( pNobs ), ".pdf" ), collapse = "" )
}
pdfFileName <- paste( c( pPath, "\\", pdfFileName ), collapse = "" )
pdf( pdfFileName )


plot.new(); plot.window(xlim=c(0,1),ylim=c(0,1))
titleStr                      <- paste( c( "DBSCAN Cluster - ", "\n", "Cluster visualization",  "\n", "(", as.character( pNobs ), " sample)" ), collapse = "" )
text( 0.5, 0.5, lab=titleStr, cex = 2.5)


op                            <- par( mfrow=c( pRows, pCols ), oma=c(1,2,2,1) )


# DBSCAN 1
columns              <- c( "ir", "maxDD"  );    
clusterDimensions    <- auxGetClusterDimensions( columns )

# create clusters and plot
if( pNormalized ) columns <- auxGetNormalizedNames( columns )
minPts               <- 4
test                 <- dfFolioPerf[ 1:pNobs, columns]
knnCalibEps          <- 0.11
db                   <- dbscan( test, eps = knnCalibEps, minPts = minPts )
plotClusters( summaryDf, db, plotYvariable, plotXvariables, paste( c( clusterDimensions, ", minPts: ", as.character( minPts), " eps: ", as.character( knnCalibEps) ), collapse=""), pClusterType ) 


# DBSCAN 2
columns              <- c( "ir", "skew", "maxDD"  );    
clusterDimensions    <- auxGetClusterDimensions( columns )

# create clusters and plot
if( pNormalized ) columns <- auxGetNormalizedNames( columns )
minPts               <- 6
test                 <- dfFolioPerf[ 1:pNobs, columns]
knnCalibEps          <- 0.20
db                   <- dbscan( test, eps = knnCalibEps, minPts = minPts )
plotClusters( summaryDf, db, plotYvariable, plotXvariables, paste( c( clusterDimensions, ", minPts: ", as.character( minPts), " eps: ", as.character( knnCalibEps) ), collapse=""), pClusterType ) 


# DBSCAN 3
columns              <-  c(  "cumRet", "sigma", "skew", "maxDD" );       
clusterDimensions    <- auxGetClusterDimensions( columns )

# create clusters and plot
if( pNormalized ) columns <- auxGetNormalizedNames( columns )
minPts               <- 8
test                 <- dfFolioPerf[ 1:pNobs, columns]
knnCalibEps          <- 0.28
db                   <- dbscan( test, eps = knnCalibEps, minPts = minPts )
plotClusters( summaryDf, db, plotYvariable, plotXvariables, paste( c( clusterDimensions, ", minPts: ", as.character( minPts), " eps: ", as.character( knnCalibEps) ), collapse=""), pClusterType ) 

par( op )


dev.off()






# PORTFOLIO INSPECTION
pNobs           <- 10000
pUtility        <- "CPTUtility_100"



# folio performance by cluster
if(FALSE){
  # 1: c( "ir", "maxDD" )
  columns              <- c( "ir", "maxDD" )
  clusterDimensions    <- auxGetClusterDimensions( columns )
  if( pNormalized ) columns <- auxGetNormalizedNames( columns )
  pClusters            <- 5
  test                 <- dfFolioPerf[ 1:pNobs, columns ]
  
  pClusterType         <- "kMeans"
  kMean                <- kmeans( test, pClusters ) 
  dfInspect            <- constructInspectFolio( dfFolioPerf, kMean, pClusterType, clusterDimensions, pUtility )
  
  pClusterType         <- "hierarchical"
  hc                   <- hclust( dist(test))
  hc                   <- cutree(hc, k = pClusters)
  dfInspect            <- rbind( dfInspect, constructInspectFolio( dfFolioPerf, hc, pClusterType, clusterDimensions, pUtility ) )
  
  pClusterType         <- "dbScan"
  minPts               <- 4
  knnCalibEps          <- 0.11
  db                   <- dbscan( test, eps = knnCalibEps, minPts = minPts )
  dfInspect            <- rbind( dfInspect, constructInspectFolio( dfFolioPerf, db, pClusterType, clusterDimensions, pUtility ) )
}


# 2: c( "ir", "skew", maxDD" )
columns              <- c( "ir", "skew", "maxDD" )
clusterDimensions    <- auxGetClusterDimensions( columns )
if( pNormalized ) columns <- auxGetNormalizedNames( columns )
pClusters            <- 8
test                 <- dfFolioPerf[ 1:pNobs, columns ]

pClusterType         <- "kMeans"
kMean                <- kmeans( test, pClusters ) 
dfInspect            <- constructInspectFolio( dfFolioPerf, kMean, pClusterType, clusterDimensions, pUtility )

pClusterType         <- "hierarchical"
hc                   <- hclust( dist(test))
hc                   <- cutree(hc, k = pClusters)
dfInspect            <- rbind( dfInspect, constructInspectFolio( dfFolioPerf, hc, pClusterType, clusterDimensions, pUtility ) )

pClusterType         <- "dbScan"
minPts               <- 6
knnCalibEps          <- 0.2    #0.2    / 0.1
db                   <- dbscan( test, eps = knnCalibEps, minPts = minPts )
dfInspect            <- rbind( dfInspect, constructInspectFolio( dfFolioPerf, db, pClusterType, clusterDimensions, pUtility ) )





# 3: columns              <-  c(  "cumRet", "sigma", "skew", "maxDD" );       
columns              <-  c(  "cumRet", "sigma", "skew", "maxDD" );       
clusterDimensions    <- auxGetClusterDimensions( columns )
if( pNormalized ) columns <- auxGetNormalizedNames( columns )
pClusters            <- 8
test                 <- dfFolioPerf[ 1:pNobs, columns ]

pClusterType         <- "kMeans"
kMean                <- kmeans( test, pClusters ) 
dfInspect            <- rbind( dfInspect, constructInspectFolio( dfFolioPerf, kMean, pClusterType, clusterDimensions, pUtility ) )

pClusterType         <- "hierarchical"
hc                   <- hclust( dist(test))
hc                   <- cutree(hc, k = pClusters)
dfInspect            <- rbind( dfInspect, constructInspectFolio( dfFolioPerf, hc, pClusterType, clusterDimensions, pUtility ) )

pClusterType         <- "dbScan"
minPts               <- 8
knnCalibEps          <- 0.28    #0.28   / 0.165
db                   <- dbscan( test, eps = knnCalibEps, minPts = minPts )
dfInspect            <- rbind( dfInspect, constructInspectFolio( dfFolioPerf, db, pClusterType, clusterDimensions, pUtility ) )

if(FALSE){
  pClusterType         <- "hDBScan"
  minPts               <- 4
  db                   <- hdbscan( test, minPts )
  dfInspect            <- rbind( dfInspect, constructInspectFolio( dfFolioPerf, db, pClusterType, clusterDimensions, pUtility ) )
}



# 4: c( "ir", "skew", kurt" )
columns              <- c( "ir", "skew", "kurt" )
clusterDimensions    <- auxGetClusterDimensions( columns )
if( pNormalized ) columns <- auxGetNormalizedNames( columns )
pClusters            <- 5
test                 <- dfFolioPerf[ 1:pNobs, columns ]

pClusterType         <- "kMeans"
kMean                <- kmeans( test, pClusters ) 
dfInspect            <- rbind( dfInspect, constructInspectFolio( dfFolioPerf, kMean, pClusterType, clusterDimensions, pUtility ) )

pClusterType         <- "hierarchical"
hc                   <- hclust( dist(test))
hc                   <- cutree(hc, k = pClusters)
dfInspect            <- rbind( dfInspect, constructInspectFolio( dfFolioPerf, hc, pClusterType, clusterDimensions, pUtility ) )

pClusterType         <- "dbScan"
minPts               <- 6
knnCalibEps          <- 0.34
db                   <- dbscan( test, eps = knnCalibEps, minPts = minPts )
dfInspect            <- rbind( dfInspect, constructInspectFolio( dfFolioPerf, db, pClusterType, clusterDimensions, pUtility ) )

if(FALSE){
  pClusterType         <- "hDBScan"
  minPts               <- 5
  db                   <- hdbscan( test, minPts )
  dfInspect            <- rbind( dfInspect, constructInspectFolio( dfFolioPerf, db, pClusterType, clusterDimensions, pUtility ) )
}



# 5: columns              <-  c(  "cumRet", "sigma", "skew", "kurt" );       
columns              <-  c(  "cumRet", "sigma", "skew", "kurt" );       
clusterDimensions    <- auxGetClusterDimensions( columns )
if( pNormalized ) columns <- auxGetNormalizedNames( columns )
pClusters            <- 8
test                 <- dfFolioPerf[ 1:pNobs, columns ]

pClusterType         <- "kMeans"
kMean                <- kmeans( test, pClusters ) 
dfInspect            <- rbind( dfInspect, constructInspectFolio( dfFolioPerf, kMean, pClusterType, clusterDimensions, pUtility ) )

pClusterType         <- "hierarchical"
hc                   <- hclust( dist(test))
hc                   <- cutree(hc, k = pClusters)
dfInspect            <- rbind( dfInspect, constructInspectFolio( dfFolioPerf, hc, pClusterType, clusterDimensions, pUtility ) )

pClusterType         <- "dbScan"
minPts               <- 8
knnCalibEps          <- 0.34    #0.28   / 0.165
db                   <- dbscan( test, eps = knnCalibEps, minPts = minPts )
dfInspect            <- rbind( dfInspect, constructInspectFolio( dfFolioPerf, db, pClusterType, clusterDimensions, pUtility ) )


# add more info & get rid of noise
dfInspect$utilityType<- pUtility
idx                  <- dfInspect$cluster > 0
dfInspect            <- dfInspect[ idx, ]

fileNameOut          <- paste( c( pPath, "\\", pFileInspectFolioPerformance, pUtility, ".csv" ), collapse ="" )
write.csv(x = dfInspect, file = fileNameOut, row.names = FALSE, na = "")


# print to pdf
if( pNormalized ){
  pdfFileName <- paste( c( pPdfFileNameFolioResults, "_", as.character( pNobs ), "_norm_", pUtility, ".pdf" ), collapse = "" )
} else {
  pdfFileName <- paste( c( pPdfFileNameFolioResults, "_", as.character( pNobs ), "_", pUtility, ".pdf" ), collapse = "" )
}
pdfFileName <- paste( c( pPath, "\\", pdfFileName ), collapse = "" )
pdf( pdfFileName )



# SUMMARIZE PORTFOLIO PERFORMANCE BY CLUSTER
plot.new(); plot.window(xlim=c(0,1),ylim=c(0,1))
titleStr                      <- paste( c( "Folio performance", "\n", "by Cluster", "\n", "(", as.character(pNobs), " sample)" ), collapse = "" )
text( 0.5, 0.5, lab=titleStr, cex = 2.5)
op                            <- par( mfrow=c( pRows, pCols ), oma=c(1,2,2,1) )

plotXvariables                <- c( "sigma", "skew", "kurt", "maxDD", "timingLoss", "cumRet", pUtility )
# plotXvariables                <- c( "sigma", "skew", "kurt", "maxDD", "timingLoss", "cumRet", "utility" )
plotYvariables                <- c( "ir", pUtility )
# plotYvariables                <- c( "ir", "cumRet", "utility" )
uniqueClusterDimensions       <- unique( dfInspect$clusterDimensions )
clusterTypes                  <- unique( dfInspect$clusterType )

for( i in 1:length( clusterTypes )){
  
  thisClusterType <- as.character( clusterTypes[ i ] )
  
  for( j in 1:length( uniqueClusterDimensions )){
    
    thisClusterDim   <- as.character( uniqueClusterDimensions[ j ] )
    
    idx                <- dfInspect$clusterType == thisClusterType & dfInspect$clusterDimensions == thisClusterDim 
    if( sum(idx)>0){
      plotData           <- dfInspect[ idx, ]
      
      for( k in 1:length( plotYvariables)){
        
        thisYVariable   <- as.character( plotYvariables[ k ] )
        
        ymin                  <- min( plotData[ , c(thisYVariable)] )
        ymax                  <- max( plotData[ , c(thisYVariable)] )
        ymin                  <- auxGetMinMax( ymin, ymax )$min
        ymax                  <- auxGetMinMax( ymin, ymax )$max
        
        for( l in 1:length( plotXvariables )){
          #print( c( as.character(i),", ", as.character(j), ",", as.character(k)), collapse = "")       
          thisXVariable     <- as.character( plotXvariables[ l ] )        
          plotData          <- plotData[ order( plotData[ , c( thisYVariable ) ]), ]                  
          #par(mar=c(1,1,1,1))
          plot( plotData[ , c( thisXVariable)], plotData[ , c( thisYVariable) ], xlab = thisXVariable, ylab = thisYVariable, ylim = c(ymin, ymax ), type = "b", col = "blue" )
        }
        grid()
        pTitle <- paste( c( "clusterType: ", thisClusterType, ", clusterDim: ", thisClusterDim ), collapse = "" )
        mtext(pTitle, outer=TRUE,  cex=1, line=-0.5)
      }      
    }
  }
}


par( op )


dev.off()




# SECTOR EXPOSURES

pNobs                <- 10000


# folio performance by cluster

if(FALSE ){
  # 1: c( "ir", "maxDD" )
  columns              <- c( "ir", "maxDD" )
  clusterDimensions    <- auxGetClusterDimensions( columns )
  if( pNormalized ) columns <- auxGetNormalizedNames( columns )
  pClusters            <- 5
  test                 <- dfFolioPerf[ 1:pNobs, columns ]
  
  pClusterType         <- "kMeans"
  kMean                <- kmeans( test, pClusters ) 
  folioByCluster       <- auxCreateCluster( kMean, pClusterType )
  dfFolioPerfByCluster <- auxByClusterPortfolioPerformance( dfFolioPerf, folioByCluster, pUtility )                                           
  dfSectorExposuresByCluster <- auxByClusterSectorExposures( dfSectorExposures, folioByCluster, pSectors )
  dfFolioInspection    <- auxCreateFolioInspection( dfFolioPerfByCluster, dfSectorExposuresByCluster, clusterDimensions, pClusterType, pClusters )
  
  pClusterType         <- "hierarchical"
  hc                   <- hclust( dist(test))
  hc                   <- cutree(hc, k = pClusters)
  folioByCluster       <- auxCreateCluster( hc, pClusterType )
  dfFolioPerfByCluster <- auxByClusterPortfolioPerformance( dfFolioPerf, folioByCluster, pUtility )                                           
  dfSectorExposuresByCluster <- auxByClusterSectorExposures( dfSectorExposures, folioByCluster, pSectors )
  dfFolioInspectionTmp <- auxCreateFolioInspection( dfFolioPerfByCluster, dfSectorExposuresByCluster, clusterDimensions, pClusterType, pClusters )
  dfFolioInspection    <- rbind( dfFolioInspection, dfFolioInspectionTmp )
  
  pClusterType         <- "dbScan"
  minPts               <- 4
  knnCalibEps          <- 0.11
  db                   <- dbscan( test, eps = knnCalibEps, minPts = minPts )
  folioByCluster       <- auxCreateCluster( db, pClusterType )
  dfFolioPerfByCluster <- auxByClusterPortfolioPerformance( dfFolioPerf, folioByCluster, pUtility )                                           
  dfSectorExposuresByCluster <- auxByClusterSectorExposures( dfSectorExposures, folioByCluster, pSectors )
  dfFolioInspectionTmp <- auxCreateFolioInspection( dfFolioPerfByCluster, dfSectorExposuresByCluster, clusterDimensions, pClusterType, pClusters )
  idx                  <- dfFolioInspectionTmp$cluster != 0
  dfFolioInspectionTmp <- dfFolioInspectionTmp[ idx, ]
  dfFolioInspection    <- rbind( dfFolioInspection, dfFolioInspectionTmp )
  
  pClusterType         <- "hDBScan"
  minPts               <- 5
  db                   <- hdbscan( test, minPts )
  folioByCluster       <- auxCreateCluster( db, pClusterType )
  dfFolioPerfByCluster <- auxByClusterPortfolioPerformance( dfFolioPerf, folioByCluster, pUtility )                                           
  dfSectorExposuresByCluster <- auxByClusterSectorExposures( dfSectorExposures, folioByCluster, pSectors )
  dfFolioInspectionTmp <- auxCreateFolioInspection( dfFolioPerfByCluster, dfSectorExposuresByCluster, clusterDimensions, pClusterType, pClusters )
  idx                  <- dfFolioInspectionTmp$cluster != 0
  dfFolioInspectionTmp <- dfFolioInspectionTmp[ idx, ]
  dfFolioInspection    <- rbind( dfFolioInspection, dfFolioInspectionTmp )
}




# 2: c( "ir", "skew", maxDD" )
columns              <- c( "ir", "skew", "maxDD" )
clusterDimensions    <- auxGetClusterDimensions( columns )
if( pNormalized ) columns <- auxGetNormalizedNames( columns )
pClusters            <- 8
test                 <- dfFolioPerf[ 1:pNobs, columns ]

pClusterType         <- "kMeans"
kMean                <- kmeans( test, pClusters ) 
folioByCluster       <- auxCreateCluster( kMean, pClusterType )
dfFolioPerfByCluster <- auxByClusterPortfolioPerformance( dfFolioPerf, folioByCluster, pUtility )                                           
dfSectorExposuresByCluster <- auxByClusterSectorExposures( dfSectorExposures, folioByCluster, pSectors )
dfFolioInspection    <- auxCreateFolioInspection( dfFolioPerfByCluster, dfSectorExposuresByCluster, clusterDimensions, pClusterType, pClusters )


pClusterType         <- "hierarchical"
hc                   <- hclust( dist(test))
hc                   <- cutree(hc, k = pClusters)
folioByCluster       <- auxCreateCluster( hc, pClusterType )
dfFolioPerfByCluster <- auxByClusterPortfolioPerformance( dfFolioPerf, folioByCluster, pUtility )                                           
dfSectorExposuresByCluster <- auxByClusterSectorExposures( dfSectorExposures, folioByCluster, pSectors )
dfFolioInspectionTmp <- auxCreateFolioInspection( dfFolioPerfByCluster, dfSectorExposuresByCluster, clusterDimensions, pClusterType, pClusters )
dfFolioInspection    <- rbind( dfFolioInspection, dfFolioInspectionTmp )

pClusterType         <- "dbScan"
minPts               <- 6
knnCalibEps          <- 0.2   # 0.2   / 0.1
db                   <- dbscan( test, eps = knnCalibEps, minPts = minPts )
folioByCluster       <- auxCreateCluster( db, pClusterType )
dfFolioPerfByCluster <- auxByClusterPortfolioPerformance( dfFolioPerf, folioByCluster, pUtility )                                           
dfSectorExposuresByCluster <- auxByClusterSectorExposures( dfSectorExposures, folioByCluster, pSectors )
dfFolioInspectionTmp <- auxCreateFolioInspection( dfFolioPerfByCluster, dfSectorExposuresByCluster, clusterDimensions, pClusterType, pClusters )
idx                  <- dfFolioInspectionTmp$cluster != 0
dfFolioInspectionTmp <- dfFolioInspectionTmp[ idx, ]
dfFolioInspection    <- rbind( dfFolioInspection, dfFolioInspectionTmp )

if(FALSE){
  pClusterType         <- "hDBScan"
  minPts               <- 5
  db                   <- hdbscan( test, minPts )
  folioByCluster       <- auxCreateCluster( db, pClusterType )
  dfFolioPerfByCluster <- auxByClusterPortfolioPerformance( dfFolioPerf, folioByCluster, pUtility )                                           
  dfSectorExposuresByCluster <- auxByClusterSectorExposures( dfSectorExposures, folioByCluster, pSectors )
  dfFolioInspectionTmp <- auxCreateFolioInspection( dfFolioPerfByCluster, dfSectorExposuresByCluster, clusterDimensions, pClusterType, pClusters )
  idx                  <- dfFolioInspectionTmp$cluster != 0
  dfFolioInspectionTmp <- dfFolioInspectionTmp[ idx, ]
  dfFolioInspection    <- rbind( dfFolioInspection, dfFolioInspectionTmp )
}






# 3: columns              <-  c(  "cumRet", "sigma", "skew", "maxDD" );       
columns              <-  c(  "cumRet", "sigma", "skew", "maxDD" );       
clusterDimensions    <- auxGetClusterDimensions( columns )
if( pNormalized ) columns <- auxGetNormalizedNames( columns )
pClusters            <- 8
test                 <- dfFolioPerf[ 1:pNobs, columns ]

pClusterType         <- "kMeans"
kMean                <- kmeans( test, pClusters ) 
kMean                <- kmeans( test, pClusters ) 
folioByCluster       <- auxCreateCluster( kMean, pClusterType )
dfFolioPerfByCluster <- auxByClusterPortfolioPerformance( dfFolioPerf, folioByCluster, pUtility )                                           
dfSectorExposuresByCluster <- auxByClusterSectorExposures( dfSectorExposures, folioByCluster, pSectors )
dfFolioInspectionTmp <- auxCreateFolioInspection( dfFolioPerfByCluster, dfSectorExposuresByCluster, clusterDimensions, pClusterType, pClusters )
dfFolioInspection    <- rbind( dfFolioInspection, dfFolioInspectionTmp )

pClusterType         <- "hierarchical"
hc                   <- hclust( dist(test))
hc                   <- cutree(hc, k = pClusters)
folioByCluster       <- auxCreateCluster( hc, pClusterType )
dfFolioPerfByCluster <- auxByClusterPortfolioPerformance( dfFolioPerf, folioByCluster, pUtility )                                           
dfSectorExposuresByCluster <- auxByClusterSectorExposures( dfSectorExposures, folioByCluster, pSectors )
dfFolioInspectionTmp <- auxCreateFolioInspection( dfFolioPerfByCluster, dfSectorExposuresByCluster, clusterDimensions, pClusterType, pClusters )
dfFolioInspection    <- rbind( dfFolioInspection, dfFolioInspectionTmp )

pClusterType         <- "dbScan"
minPts               <- 8
knnCalibEps          <- 0.28   #0.28    /0.165
db                   <- dbscan( test, eps = knnCalibEps, minPts = minPts )
folioByCluster       <- auxCreateCluster( db, pClusterType )
dfFolioPerfByCluster <- auxByClusterPortfolioPerformance( dfFolioPerf, folioByCluster, pUtility )                                           
dfSectorExposuresByCluster <- auxByClusterSectorExposures( dfSectorExposures, folioByCluster, pSectors )
dfFolioInspectionTmp <- auxCreateFolioInspection( dfFolioPerfByCluster, dfSectorExposuresByCluster, clusterDimensions, pClusterType, pClusters )
idx                  <- dfFolioInspectionTmp$cluster != 0
dfFolioInspectionTmp <- dfFolioInspectionTmp[ idx, ]
dfFolioInspection    <- rbind( dfFolioInspection, dfFolioInspectionTmp )

if(FALSE){
  pClusterType         <- "hDBScan"
  minPts               <- 4
  db                   <- hdbscan( test, minPts )
  folioByCluster       <- auxCreateCluster( db, pClusterType )
  dfFolioPerfByCluster <- auxByClusterPortfolioPerformance( dfFolioPerf, folioByCluster, pUtility )                                           
  dfSectorExposuresByCluster <- auxByClusterSectorExposures( dfSectorExposures, folioByCluster, pSectors )
  dfFolioInspectionTmp <- auxCreateFolioInspection( dfFolioPerfByCluster, dfSectorExposuresByCluster, clusterDimensions, pClusterType, pClusters )
  idx                  <- dfFolioInspectionTmp$cluster != 0
  dfFolioInspectionTmp <- dfFolioInspectionTmp[ idx, ]
  dfFolioInspection    <- rbind( dfFolioInspection, dfFolioInspectionTmp )
}


# 4: c( "ir", "skew", kurt" )
columns              <- c( "ir", "skew", "kurt" )
clusterDimensions    <- auxGetClusterDimensions( columns )
if( pNormalized ) columns <- auxGetNormalizedNames( columns )
pClusters            <- 5
test                 <- dfFolioPerf[ 1:pNobs, columns ]

pClusterType         <- "kMeans"
kMean                <- kmeans( test, pClusters ) 
folioByCluster       <- auxCreateCluster( kMean, pClusterType )
dfFolioPerfByCluster <- auxByClusterPortfolioPerformance( dfFolioPerf, folioByCluster, pUtility )                                           
dfSectorExposuresByCluster <- auxByClusterSectorExposures( dfSectorExposures, folioByCluster, pSectors )
dfFolioInspectionTmp <- auxCreateFolioInspection( dfFolioPerfByCluster, dfSectorExposuresByCluster, clusterDimensions, pClusterType, pClusters )
dfFolioInspection    <- rbind( dfFolioInspection, dfFolioInspectionTmp )

pClusterType         <- "hierarchical"
hc                   <- hclust( dist(test))
hc                   <- cutree(hc, k = pClusters)
folioByCluster       <- auxCreateCluster( hc, pClusterType )
dfFolioPerfByCluster <- auxByClusterPortfolioPerformance( dfFolioPerf, folioByCluster, pUtility )                                           
dfSectorExposuresByCluster <- auxByClusterSectorExposures( dfSectorExposures, folioByCluster, pSectors )
dfFolioInspectionTmp <- auxCreateFolioInspection( dfFolioPerfByCluster, dfSectorExposuresByCluster, clusterDimensions, pClusterType, pClusters )
dfFolioInspection    <- rbind( dfFolioInspection, dfFolioInspectionTmp )

pClusterType         <- "dbScan"
minPts               <- 6
knnCalibEps          <- 0.34   # 0.2   / 0.1
db                   <- dbscan( test, eps = knnCalibEps, minPts = minPts )
folioByCluster       <- auxCreateCluster( db, pClusterType )
dfFolioPerfByCluster <- auxByClusterPortfolioPerformance( dfFolioPerf, folioByCluster, pUtility )                                           
dfSectorExposuresByCluster <- auxByClusterSectorExposures( dfSectorExposures, folioByCluster, pSectors )
dfFolioInspectionTmp <- auxCreateFolioInspection( dfFolioPerfByCluster, dfSectorExposuresByCluster, clusterDimensions, pClusterType, pClusters )
idx                  <- dfFolioInspectionTmp$cluster != 0
dfFolioInspectionTmp <- dfFolioInspectionTmp[ idx, ]
dfFolioInspection    <- rbind( dfFolioInspection, dfFolioInspectionTmp )

if(FALSE){
  pClusterType         <- "hDBScan"
  minPts               <- 5
  db                   <- hdbscan( test, minPts )
  folioByCluster       <- auxCreateCluster( db, pClusterType )
  dfFolioPerfByCluster <- auxByClusterPortfolioPerformance( dfFolioPerf, folioByCluster, pUtility )                                           
  dfSectorExposuresByCluster <- auxByClusterSectorExposures( dfSectorExposures, folioByCluster, pSectors )
  dfFolioInspectionTmp <- auxCreateFolioInspection( dfFolioPerfByCluster, dfSectorExposuresByCluster, clusterDimensions, pClusterType, pClusters )
  idx                  <- dfFolioInspectionTmp$cluster != 0
  dfFolioInspectionTmp <- dfFolioInspectionTmp[ idx, ]
  dfFolioInspection    <- rbind( dfFolioInspection, dfFolioInspectionTmp )
}


# 5: columns              <-  c(  "cumRet", "sigma", "skew", "kurt" );       
columns              <-  c(  "cumRet", "sigma", "skew", "kurt" );       
clusterDimensions    <- auxGetClusterDimensions( columns )
if( pNormalized ) columns <- auxGetNormalizedNames( columns )
pClusters            <- 8
test                 <- dfFolioPerf[ 1:pNobs, columns ]

pClusterType         <- "kMeans"
kMean                <- kmeans( test, pClusters ) 
kMean                <- kmeans( test, pClusters ) 
folioByCluster       <- auxCreateCluster( kMean, pClusterType )
dfFolioPerfByCluster <- auxByClusterPortfolioPerformance( dfFolioPerf, folioByCluster, pUtility )                                           
dfSectorExposuresByCluster <- auxByClusterSectorExposures( dfSectorExposures, folioByCluster, pSectors )
dfFolioInspectionTmp <- auxCreateFolioInspection( dfFolioPerfByCluster, dfSectorExposuresByCluster, clusterDimensions, pClusterType, pClusters )
dfFolioInspection    <- rbind( dfFolioInspection, dfFolioInspectionTmp )

pClusterType         <- "hierarchical"
hc                   <- hclust( dist(test))
hc                   <- cutree(hc, k = pClusters)
folioByCluster       <- auxCreateCluster( hc, pClusterType )
dfFolioPerfByCluster <- auxByClusterPortfolioPerformance( dfFolioPerf, folioByCluster, pUtility )                                           
dfSectorExposuresByCluster <- auxByClusterSectorExposures( dfSectorExposures, folioByCluster, pSectors )
dfFolioInspectionTmp <- auxCreateFolioInspection( dfFolioPerfByCluster, dfSectorExposuresByCluster, clusterDimensions, pClusterType, pClusters )
dfFolioInspection    <- rbind( dfFolioInspection, dfFolioInspectionTmp )

pClusterType         <- "dbScan"
minPts               <- 8
knnCalibEps          <- 0.34   #0.28    /0.165
db                   <- dbscan( test, eps = knnCalibEps, minPts = minPts )
folioByCluster       <- auxCreateCluster( db, pClusterType )
dfFolioPerfByCluster <- auxByClusterPortfolioPerformance( dfFolioPerf, folioByCluster, pUtility )                                           
dfSectorExposuresByCluster <- auxByClusterSectorExposures( dfSectorExposures, folioByCluster, pSectors )
dfFolioInspectionTmp <- auxCreateFolioInspection( dfFolioPerfByCluster, dfSectorExposuresByCluster, clusterDimensions, pClusterType, pClusters )
idx                  <- dfFolioInspectionTmp$cluster != 0
dfFolioInspectionTmp <- dfFolioInspectionTmp[ idx, ]
dfFolioInspection    <- rbind( dfFolioInspection, dfFolioInspectionTmp )

if(FALSE){
  pClusterType         <- "hDBScan"
  minPts               <- 4
  db                   <- hdbscan( test, minPts )
  folioByCluster       <- auxCreateCluster( db, pClusterType )
  dfFolioPerfByCluster <- auxByClusterPortfolioPerformance( dfFolioPerf, folioByCluster, pUtility )                                           
  dfSectorExposuresByCluster <- auxByClusterSectorExposures( dfSectorExposures, folioByCluster, pSectors )
  dfFolioInspectionTmp <- auxCreateFolioInspection( dfFolioPerfByCluster, dfSectorExposuresByCluster, clusterDimensions, pClusterType, pClusters )
  idx                  <- dfFolioInspectionTmp$cluster != 0
  dfFolioInspectionTmp <- dfFolioInspectionTmp[ idx, ]
  dfFolioInspection    <- rbind( dfFolioInspection, dfFolioInspectionTmp )
}


dfFolioInspection$utilityType <-  pUtility





# summarize high-level portfolios by clustering technique
pNumberInspectFolio   <- 15
pWeightThreshold      <- 0.02
uniqueClusterDim      <- unique( dfFolioInspection$clusterDimensions )
uniqueClusterMethod   <- unique( dfFolioInspection$clusterMethod )

for( i in 1:length( uniqueClusterDim ) ){
  
  thisDim               <- as.character( uniqueClusterDim[ i ] )
  
  for( j in 1:length( uniqueClusterMethod ) ){
    
    thisMethod            <- as.character( uniqueClusterMethod[ j ] )  
    idx                   <- dfFolioInspection$clusterMethod == thisMethod & dfFolioInspection$clusterDimensions == thisDim 
    
    if( sum( idx ) > 0 ){
      
      thisDf                <- dfFolioInspection[ idx, ]
      thisDf                <- thisDf[ order( thisDf[, c(pUtility )], decreasing = TRUE), ]
      
      thisClusterNum        <- unique( thisDf$cluster )
      if( length( thisClusterNum ) > pNumberInspectFolio  ){
        thisClusterNum        <- thisClusterNum[ 1:pNumberInspectFolio ]
      }
      
      for( k in 1:length( thisClusterNum ) ){
        
        thisCluster           <- thisClusterNum[ k ]
        idx                   <- thisDf$cluster == thisCluster
        thisDfInspect         <- thisDf[ idx, ]
        
        # summarizes number of exposures
        idx                   <- thisDfInspect$wgt > pWeightThreshold
        numPositions          <- sum( idx )
        dfTmp                 <- data.frame( "utility" = k, "utilityType" = pUtility, "clusterDim" = thisDim, "numberExposures" = numPositions )    
        
        if( k == 1 ){
          dfNumExposureTmp      <- dfTmp
        } else {
          dfNumExposureTmp      <- rbind( dfNumExposureTmp, dfTmp )
        }
        
        #print(paste(c(as.character(i),", ",as.character(j), ", ", as.character(k)), collapse = ""))    
        # summarize FI vs Equity
        if( is.na( thisCluster ) ){
          pctFI                 <- NA
          pctEq                 <- NA
          strSector             <- NA
        } else {
          pctFI                 <- round( 100 * auxIsFI( thisDfInspect, pFI ), 0 )
          pctEq                 <- 100 - pctFI
          strSector             <- paste( c( as.character( pctFI ), "/", as.character( pctEq )), collapse = "" )
        }
        
        dfTmp                 <- data.frame( "utility" = k, "utilityType" = pUtility, "clusterDim" = thisDim, "FIvsEquity" = strSector )    
        if( k == 1 ){
          dfSectorSummaryTmp      <- dfTmp
        } else {
          dfSectorSummaryTmp      <- rbind( dfSectorSummaryTmp, dfTmp )
        }
        
        # summarize composition of FI
        if( is.na( thisCluster ) ){
          pctFTQ                <- NA
          pctFIRates            <- NA
          pctFICredit           <- NA
          strFIComp             <- NA
        } else {
          pctFTQ                <- round( 100 * auxIsFI( thisDfInspect, pFTQ ), 0 )
          pctFIRates            <- round( 100 * auxIsFI( thisDfInspect, pFIRates ), 0 )
          pctFICredit           <- round( 100 * auxIsFI( thisDfInspect, pFICredit ), 0 )
          strFIComp             <- paste( c( as.character( pctFTQ ), "/", as.character( pctFIRates ), "/", as.character( pctFICredit )), collapse = "" )
        }
        
        dfTmp                 <- data.frame( "utility" = k, "utilityType" = pUtility, "clusterDim" = thisDim, "FIComposition" = strFIComp )    
        if( k == 1 ){
          dfFISummaryTmp      <- dfTmp
        } else {
          dfFISummaryTmp      <- rbind( dfFISummaryTmp, dfTmp )
        }
        
      }
      colnames( dfNumExposureTmp )[ grep( "numberExposures", colnames( dfNumExposureTmp ), fixed = TRUE ) ]  <- thisMethod  
      colnames( dfSectorSummaryTmp )[ grep( "FIvsEquity", colnames( dfSectorSummaryTmp ), fixed = TRUE ) ]   <- thisMethod
      colnames( dfFISummaryTmp )[ grep( "FIComposition", colnames( dfFISummaryTmp ), fixed = TRUE ) ]        <- thisMethod  
      
      if(FALSE){
        nCluster                <- nrow( dfNumExposureTmp )
        
        if( nCluster < pNumberInspectFolio ){
          
          auxUtility              <- seq( nCluster + 1, pNumberInspectFolio, 1 )
          auxUtilityType          <- rep( pUtility, pNumberInspectFolio - nCluster )
          auxClusterDim           <- rep( thisDim, pNumberInspectFolio - nCluster  )
          auxMethod               <- rep( NA, pNumberInspectFolio - nCluster )
          
          dfAux                   <- data.frame( "utility" = auxUtility, "utilityType" = auxUtilityType, "clusterDim" = auxClusterDim, "auxMethod" = auxMethod)
          colnames( dfAux )[ grep( "auxMethod", colnames( dfAux ), fixed = TRUE ) ]  <- thisMethod
          
          dfNumExposureTmp       <- rbind( dfNumExposureTmp, dfAux )
          dfSectorSummaryTmp     <- rbind( dfSectorSummaryTmp, dfAux )
          dfFISummaryTmp         <- rbind( dfFISummaryTmp, dfAux )
          
        }
        
      }
      
      if( j == 1 ){
        dfNumExposure          <- dfNumExposureTmp
        dfSectorSummary        <- dfSectorSummaryTmp
        dfFISummary            <- dfFISummaryTmp
      } else {
        dfNumExposure          <- merge( dfNumExposure, dfNumExposureTmp, by = c( "utility", "utilityType", "clusterDim" ) )
        dfSectorSummary        <- merge( dfSectorSummary, dfSectorSummaryTmp, by = c( "utility", "utilityType", "clusterDim" ) )
        dfFISummary            <- merge( dfFISummary, dfFISummaryTmp, by = c( "utility", "utilityType", "clusterDim" ) )
      }
      
    }
  }
  
  if( i == 1 ){
    dfNumExposureOut        <- dfNumExposure
    dfSectorSummaryOut      <- dfSectorSummary
    dfFISummaryOut          <- dfFISummary
  } else {
    dfNumExposureOut        <- rbind( dfNumExposureOut, dfNumExposure )
    dfSectorSummaryOut      <- rbind( dfSectorSummaryOut, dfSectorSummary )
    dfFISummaryOut          <- rbind( dfFISummaryOut, dfFISummary )
  }
  
}


fileName <- paste( c( pPathOut, "\\", pFileNameFolioCompositionXLS, pUtility, ".xlsx"), collapse = "" )
write.xlsx( dfNumExposureOut, fileName, sheetName = "Number of Positions", row.names = FALSE, append = FALSE )
write.xlsx( dfSectorSummaryOut, fileName, sheetName = "FI vs Equity" , row.names = FALSE, append = TRUE)
write.xlsx( dfFISummaryOut, fileName, sheetName = "FI Composition - FTQ - Rates - Credit", row.names = FALSE, append = TRUE )



# plot exposures and print to pdf

if( pNormalized ){
  pdfFileName <- paste( c( pPdfFileNameFolioExposures, "_", as.character(pNobs), "_norm_", pUtility, ".pdf" ), collapse = "" )
} else {
  pdfFileName <- paste( c( pPdfFileNameFolioExposures, "_", as.character(pNobs), "_", pUtility, ".pdf" ), collapse = "" )
}
pdfFileName <- paste( c( pPath, "\\", pdfFileName ), collapse = "" )
pdf( pdfFileName )

plot.new(); plot.window(xlim=c(0,1),ylim=c(0,1))
titleStr                      <- paste( c( "Sector exposures", "\n", "by Cluster",  "\n", "(", as.character(pNobs), " sample)" ), collapse = "" )
text( 0.5, 0.5, lab=titleStr, cex = 1.5)


#plotExposures( dfFolioInspection, pSectors, "cumRet", pRows, pCols )
#plotExposures( dfFolioInspection, pSectors, "ir", pRows, pCols )
plotExposures( dfFolioInspection, pSectors, pUtility, pRows, pCols )



dev.off()




# SIMILARITY TESTS

# within cluster dimensions comparison
  
# 2: c( "ir", "skew", maxDD" )
columns              <- c( "ir", "skew", "maxDD" )
clusterDimensions    <- auxGetClusterDimensions( columns )
if( pNormalized ) columns <- auxGetNormalizedNames( columns )
pClusters            <- 8
test                 <- dfFolioPerf[ 1:pNobs, columns ]
  
pClusterType         <- "kMeans"
kMean                <- kmeans( test, pClusters )$cluster
  
pClusterType         <- "hierarchical"
hc                   <- hclust( dist(test))
hc                   <- cutree(hc, k = pClusters)
  
pClusterType         <- "dbScan"
minPts               <- 6
knnCalibEps          <- 0.2    #0.2    / 0.1
db                   <- dbscan( test, eps = knnCalibEps, minPts = minPts )$cluster
  
# kMeans vs hc
dfSimilarityTmp      <- as.data.frame( adjustedRand( kMean, hc) )
colnames( dfSimilarityTmp )[ grep( colnames(dfSimilarityTmp), colnames( dfSimilarityTmp ), fixed = TRUE ) ]  <- "indexVal"
dfSimilarityTmp$method <- rownames( dfSimilarityTmp )
dfSimilarityTmp$clusterDim <- clusterDimensions
dfSimilarityTmp$clusterMeth <- "kMeans_hc"
dfSimilarity         <- dfSimilarityTmp
  
# kMeans vs db
dfSimilarityTmp      <- as.data.frame( adjustedRand( kMean, db) )
colnames( dfSimilarityTmp )[ grep( colnames(dfSimilarityTmp), colnames( dfSimilarityTmp ), fixed = TRUE ) ]  <- "indexVal"
dfSimilarityTmp$method <- rownames( dfSimilarityTmp )
dfSimilarityTmp$clusterDim <- clusterDimensions
dfSimilarityTmp$clusterMeth <- "kMeans_db"
dfSimilarity         <- rbind( dfSimilarity, dfSimilarityTmp )
  
# hc vs db
dfSimilarityTmp      <- as.data.frame( adjustedRand( kMean, db) )
colnames( dfSimilarityTmp )[ grep( colnames(dfSimilarityTmp), colnames( dfSimilarityTmp ), fixed = TRUE ) ]  <- "indexVal"
dfSimilarityTmp$method <- rownames( dfSimilarityTmp )
dfSimilarityTmp$clusterDim <- clusterDimensions
dfSimilarityTmp$clusterMeth <- "hc_db"
dfSimilarity         <- rbind( dfSimilarity, dfSimilarityTmp )

  
  
  
# 3: columns              <-  c(  "cumRet", "sigma", "skew", "maxDD" );       
columns              <-  c(  "cumRet", "sigma", "skew", "maxDD" );       
clusterDimensions    <- auxGetClusterDimensions( columns )
if( pNormalized ) columns <- auxGetNormalizedNames( columns )
pClusters            <- 8
test                 <- dfFolioPerf[ 1:pNobs, columns ]
  
pClusterType         <- "kMeans"
kMean                <- kmeans( test, pClusters )$cluster
  
pClusterType         <- "hierarchical"
hc                   <- hclust( dist(test))
hc                   <- cutree(hc, k = pClusters)
  
pClusterType         <- "dbScan"
minPts               <- 8
knnCalibEps          <- 0.28    #0.2    / 0.1
db                   <- dbscan( test, eps = knnCalibEps, minPts = minPts )$cluster
  
# kMeans vs hc
dfSimilarityTmp      <- as.data.frame( adjustedRand( kMean, hc) )
colnames( dfSimilarityTmp )[ grep( colnames(dfSimilarityTmp), colnames( dfSimilarityTmp ), fixed = TRUE ) ]  <- "indexVal"
dfSimilarityTmp$method <- rownames( dfSimilarityTmp )
dfSimilarityTmp$clusterDim <- clusterDimensions
dfSimilarityTmp$clusterMeth <- "kMeans_hc"
dfSimilarity         <- rbind( dfSimilarity, dfSimilarityTmp )
  
# kMeans vs db
dfSimilarityTmp      <- as.data.frame( adjustedRand( kMean, db) )
colnames( dfSimilarityTmp )[ grep( colnames(dfSimilarityTmp), colnames( dfSimilarityTmp ), fixed = TRUE ) ]  <- "indexVal"
dfSimilarityTmp$method <- rownames( dfSimilarityTmp )
dfSimilarityTmp$clusterDim <- clusterDimensions
dfSimilarityTmp$clusterMeth <- "kMeans_db"
dfSimilarity         <- rbind( dfSimilarity, dfSimilarityTmp )
  
# hc vs db
dfSimilarityTmp      <- as.data.frame( adjustedRand( kMean, db) )
colnames( dfSimilarityTmp )[ grep( colnames(dfSimilarityTmp), colnames( dfSimilarityTmp ), fixed = TRUE ) ]  <- "indexVal"
dfSimilarityTmp$method <- rownames( dfSimilarityTmp )
dfSimilarityTmp$clusterDim <- clusterDimensions
dfSimilarityTmp$clusterMeth <- "hc_db"
dfSimilarity         <- rbind( dfSimilarity, dfSimilarityTmp )
  
  
  
# 4: c( "ir", "skew", kurt" )
columns              <- c( "ir", "skew", "kurt" )
clusterDimensions    <- auxGetClusterDimensions( columns )
if( pNormalized ) columns <- auxGetNormalizedNames( columns )
pClusters            <- 5
test                 <- dfFolioPerf[ 1:pNobs, columns ]
  
pClusterType         <- "kMeans"
kMean                <- kmeans( test, pClusters )$cluster
  
pClusterType         <- "hierarchical"
hc                   <- hclust( dist(test))
hc                   <- cutree(hc, k = pClusters)
  
pClusterType         <- "dbScan"
minPts               <- 6
knnCalibEps          <- 0.34    #0.2    / 0.1
db                   <- dbscan( test, eps = knnCalibEps, minPts = minPts )$cluster
  
# kMeans vs hc
dfSimilarityTmp      <- as.data.frame( adjustedRand( kMean, hc) )
colnames( dfSimilarityTmp )[ grep( colnames(dfSimilarityTmp), colnames( dfSimilarityTmp ), fixed = TRUE ) ]  <- "indexVal"
dfSimilarityTmp$method <- rownames( dfSimilarityTmp )
dfSimilarityTmp$clusterDim <- clusterDimensions
dfSimilarityTmp$clusterMeth <- "kMeans_hc"
dfSimilarity         <- rbind( dfSimilarity, dfSimilarityTmp )
  
# kMeans vs db
dfSimilarityTmp      <- as.data.frame( adjustedRand( kMean, db) )
colnames( dfSimilarityTmp )[ grep( colnames(dfSimilarityTmp), colnames( dfSimilarityTmp ), fixed = TRUE ) ]  <- "indexVal"
dfSimilarityTmp$method <- rownames( dfSimilarityTmp )
dfSimilarityTmp$clusterDim <- clusterDimensions
dfSimilarityTmp$clusterMeth <- "kMeans_db"
dfSimilarity         <- rbind( dfSimilarity, dfSimilarityTmp )
  
# hc vs db
dfSimilarityTmp      <- as.data.frame( adjustedRand( kMean, db) )
colnames( dfSimilarityTmp )[ grep( colnames(dfSimilarityTmp), colnames( dfSimilarityTmp ), fixed = TRUE ) ]  <- "indexVal"
dfSimilarityTmp$method <- rownames( dfSimilarityTmp )
dfSimilarityTmp$clusterDim <- clusterDimensions
dfSimilarityTmp$clusterMeth <- "hc_db"
dfSimilarity         <- rbind( dfSimilarity, dfSimilarityTmp )
  
  
  
# 5: columns              <-  c(  "cumRet", "sigma", "skew", "kurt" );       
columns              <-  c(  "cumRet", "sigma", "skew", "kurt" );       
clusterDimensions    <- auxGetClusterDimensions( columns )
if( pNormalized ) columns <- auxGetNormalizedNames( columns )
pClusters            <- 8
test                 <- dfFolioPerf[ 1:pNobs, columns ]
  
pClusterType         <- "kMeans"
kMean                <- kmeans( test, pClusters )$cluster
  
pClusterType         <- "hierarchical"
hc                   <- hclust( dist(test))
hc                   <- cutree(hc, k = pClusters)

pClusterType         <- "dbScan"
minPts               <- 8
knnCalibEps          <- 0.34   #0.2    / 0.1
db                   <- dbscan( test, eps = knnCalibEps, minPts = minPts )$cluster
  
# kMeans vs hc
dfSimilarityTmp      <- as.data.frame( adjustedRand( kMean, hc) )
colnames( dfSimilarityTmp )[ grep( colnames(dfSimilarityTmp), colnames( dfSimilarityTmp ), fixed = TRUE ) ]  <- "indexVal"
dfSimilarityTmp$method <- rownames( dfSimilarityTmp )
dfSimilarityTmp$clusterDim <- clusterDimensions
dfSimilarityTmp$clusterMeth <- "kMeans_hc"
dfSimilarity         <- rbind( dfSimilarity, dfSimilarityTmp )
  
# kMeans vs db
dfSimilarityTmp      <- as.data.frame( adjustedRand( kMean, db) )
colnames( dfSimilarityTmp )[ grep( colnames(dfSimilarityTmp), colnames( dfSimilarityTmp ), fixed = TRUE ) ]  <- "indexVal"
dfSimilarityTmp$method <- rownames( dfSimilarityTmp )
dfSimilarityTmp$clusterDim <- clusterDimensions
dfSimilarityTmp$clusterMeth <- "kMeans_db"
dfSimilarity         <- rbind( dfSimilarity, dfSimilarityTmp )
  
# hc vs db
dfSimilarityTmp      <- as.data.frame( adjustedRand( kMean, db) )
colnames( dfSimilarityTmp )[ grep( colnames(dfSimilarityTmp), colnames( dfSimilarityTmp ), fixed = TRUE ) ]  <- "indexVal"
dfSimilarityTmp$method <- rownames( dfSimilarityTmp )
dfSimilarityTmp$clusterDim <- clusterDimensions
dfSimilarityTmp$clusterMeth <- "hc_db"
dfSimilarity         <- rbind( dfSimilarity, dfSimilarityTmp )
  
  
  
# between cluster dimensions comparison
  
# c( "ir", "skew", kurt / maxDD" )
columns              <- c( "ir", "skew", "maxDD" )
clusterDimensions    <- auxGetClusterDimensions( columns )
if( pNormalized ) columns <- auxGetNormalizedNames( columns )
pClusters1           <- 8
test1                <- dfFolioPerf[ 1:pNobs, columns ]
  
columns              <- c( "ir", "skew", "kurt" )
clusterDimensions    <- auxGetClusterDimensions( columns )
if( pNormalized ) columns <- auxGetNormalizedNames( columns )
pClusters2           <- 5
test2                <- dfFolioPerf[ 1:pNobs, columns ]
  
pClusterType         <- "kMeans"
kMean1               <- kmeans( test1, pClusters1 )$cluster
kMean2               <- kmeans( test2, pClusters2 )$cluster
  
pClusterType         <- "hierarchical"
hc1                  <- hclust( dist(test1))
hc1                  <- cutree(hc1, k = pClusters1)
hc2                  <- hclust( dist(test2))
hc2                  <- cutree(hc2, k = pClusters2)
  
pClusterType         <- "dbScan"
minPts               <- 6
knnCalibEps          <- 0.2    #0.2    / 0.1
db1                  <- dbscan( test1, eps = knnCalibEps, minPts = minPts )$cluster
  
minPts               <- 6
knnCalibEps          <- 0.34    #0.2    / 0.1
db2                  <- dbscan( test2, eps = knnCalibEps, minPts = minPts )$cluster
  
  
dfSimilarityTmp      <- as.data.frame( adjustedRand( kMean1, kMean2) )
colnames( dfSimilarityTmp )[ grep( colnames(dfSimilarityTmp), colnames( dfSimilarityTmp ), fixed = TRUE ) ]  <- "indexVal"
dfSimilarityTmp$method <- rownames( dfSimilarityTmp )
dfSimilarityTmp$clusterDim <- "ir_skew_maxDD/kurt"
dfSimilarityTmp$clusterMeth <- "kMean_kMean"
dfSimilarity         <- rbind( dfSimilarity, dfSimilarityTmp )
  
dfSimilarityTmp      <- as.data.frame( adjustedRand( hc1, hc2) )
colnames( dfSimilarityTmp )[ grep( colnames(dfSimilarityTmp), colnames( dfSimilarityTmp ), fixed = TRUE ) ]  <- "indexVal"
dfSimilarityTmp$method <- rownames( dfSimilarityTmp )
dfSimilarityTmp$clusterDim <- "ir_skew_maxDD/kurt"
dfSimilarityTmp$clusterMeth <- "hc_hc"
dfSimilarity         <- rbind( dfSimilarity, dfSimilarityTmp )
  
dfSimilarityTmp      <- as.data.frame( adjustedRand( db1, db2) )
colnames( dfSimilarityTmp )[ grep( colnames(dfSimilarityTmp), colnames( dfSimilarityTmp ), fixed = TRUE ) ]  <- "indexVal"
dfSimilarityTmp$method <- rownames( dfSimilarityTmp )
dfSimilarityTmp$clusterDim <- "ir_skew_maxDD/kurt"
dfSimilarityTmp$clusterMeth <- "db_db"
dfSimilarity         <- rbind( dfSimilarity, dfSimilarityTmp )
  
  
  
# c( "cumRet", "sigma", "skew", kurt / maxDD" )
columns              <- c(  "cumRet", "sigma", "skew", "maxDD" )
clusterDimensions    <- auxGetClusterDimensions( columns )
if( pNormalized ) columns <- auxGetNormalizedNames( columns )
pClusters1           <- 8
test1                <- dfFolioPerf[ 1:pNobs, columns ]
  
columns              <- c(  "cumRet", "sigma", "skew", "kurt" )
clusterDimensions    <- auxGetClusterDimensions( columns )
if( pNormalized ) columns <- auxGetNormalizedNames( columns )
pClusters2           <- 8
test2                <- dfFolioPerf[ 1:pNobs, columns ]
  
pClusterType         <- "kMeans"
kMean1               <- kmeans( test1, pClusters1 )$cluster
kMean2               <- kmeans( test2, pClusters2 )$cluster
  
pClusterType         <- "hierarchical"
hc1                  <- hclust( dist(test1))
hc1                  <- cutree(hc1, k = pClusters1)
hc2                  <- hclust( dist(test2))
hc2                  <- cutree(hc2, k = pClusters2)
  
pClusterType         <- "dbScan"
minPts               <- 8
knnCalibEps          <- 0.28    #0.2    / 0.1
db1                  <- dbscan( test1, eps = knnCalibEps, minPts = minPts )$cluster
  
minPts               <- 8
knnCalibEps          <- 0.34    #0.2    / 0.1
db2                  <- dbscan( test2, eps = knnCalibEps, minPts = minPts )$cluster
  
  
dfSimilarityTmp      <- as.data.frame( adjustedRand( kMean1, kMean2) )
colnames( dfSimilarityTmp )[ grep( colnames(dfSimilarityTmp), colnames( dfSimilarityTmp ), fixed = TRUE ) ]  <- "indexVal"
dfSimilarityTmp$method <- rownames( dfSimilarityTmp )
dfSimilarityTmp$clusterDim <- "cumRet_sigma_skew_maxDD/kurt"
dfSimilarityTmp$clusterMeth <- "kMean_kMean"
dfSimilarity         <- rbind( dfSimilarity, dfSimilarityTmp )
  
dfSimilarityTmp      <- as.data.frame( adjustedRand( hc1, hc2) )
colnames( dfSimilarityTmp )[ grep( colnames(dfSimilarityTmp), colnames( dfSimilarityTmp ), fixed = TRUE ) ]  <- "indexVal"
dfSimilarityTmp$method <- rownames( dfSimilarityTmp )
dfSimilarityTmp$clusterDim <- "cumRet_sigma_skew_maxDD/kurt"
dfSimilarityTmp$clusterMeth <- "hc_hc"
dfSimilarity         <- rbind( dfSimilarity, dfSimilarityTmp )
  
dfSimilarityTmp      <- as.data.frame( adjustedRand( db1, db2) )
colnames( dfSimilarityTmp )[ grep( colnames(dfSimilarityTmp), colnames( dfSimilarityTmp ), fixed = TRUE ) ]  <- "indexVal"
dfSimilarityTmp$method <- rownames( dfSimilarityTmp )
dfSimilarityTmp$clusterDim <- "cumRet_sigma_skew_maxDD/kurt"
dfSimilarityTmp$clusterMeth <- "db_db"
dfSimilarity         <- rbind( dfSimilarity, dfSimilarityTmp )
  
  
  
  
fileNameOut               <- paste( c( pPath, "\\clusterSimilarityTests.csv" ), collapse ="" )
write.csv(x = dfSimilarity, file = fileNameOut, row.names = FALSE, na = "")




# SECTOR WEIGHTS BY SCENARIO
pNobs           <- 10000
pUtilities           <- c( "CPTUtility_225", "CPTUtility_150", "CPTUtility_100", "crraUtility_1", "crraUtility_3", "crraUtility_5", "quadUtility_1", "quadUtility_3", "quadUtility_5" )
count           <- 0
for( i in 1:length( pUtilities ) ){
  
  pUtility          <- as.character( pUtilities[ i ] )
  dfFolioInspection <- calcAllExposures( dfFolioPerf, pNobs, pUtility )
  
  uniqueMethods     <- unique( dfFolioInspection$clusterMethod )
  uniqueDimensions  <- unique( dfFolioInspection$clusterDimensions )
  
  for( j in 1:length( uniqueMethods ) ){
    
    thisMethod        <- as.character( uniqueMethods[ j ] )
    idx               <- dfFolioInspection$clusterMethod == thisMethod
    dfFolioInspectionTmp <- dfFolioInspection[ idx, ]
    
    for( k in 1:length( uniqueDimensions ) ){
      
      thisDim           <- as.character( uniqueDimensions[ k ] )
      idx               <- dfFolioInspectionTmp$clusterDimensions == thisDim
      dfFolioInspectionTmpTmp <- dfFolioInspectionTmp[ idx, ]
      
      maxUtil           <- max(dfFolioInspectionTmpTmp[ , c( pUtility ) ])
      idx               <- dfFolioInspectionTmpTmp[ , c( pUtility )] == maxUtil
      dfFolioInspectionTmpTmp  <- dfFolioInspectionTmpTmp[ idx, ]
      colnames( dfFolioInspectionTmpTmp )[ grep( pUtility, colnames( dfFolioInspectionTmpTmp ), fixed = TRUE ) ]  <- "utility"
      
      # transpose
      dfOutTmp          <- data.frame( "clusterMethod" = thisMethod, "clusterDimension" = thisDim, "utility" = pUtility )
      columnNames       <- dfFolioInspectionTmpTmp$sector
      dfOutTmp2         <- as.data.frame( t( dfFolioInspectionTmpTmp$wgt ) )
      colnames( dfOutTmp2 ) <- columnNames
      dfOutTmp          <- cbind( dfOutTmp, dfOutTmp2 )
      
      
      if( count == 0 ){
        dfSectorWeights   <- dfOutTmp
      } else {
        dfSectorWeights   <- rbind( dfSectorWeights, dfOutTmp )
      }
      count <- count + 1
      
    }
  }
  
}


fileNameOut               <- paste( c( pPath, "\\R_optimalSectorWeights.csv" ), collapse ="" )
write.csv(x = dfSectorWeights, file = fileNameOut, row.names = FALSE, na = "")



