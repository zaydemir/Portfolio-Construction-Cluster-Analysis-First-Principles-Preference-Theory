

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


constructFolioPerformance  <- function( dfReturnData, dfWeights ){
  
  sectors                      <- colnames( dfWeights )  
  nPortfolios                  <- nrow( dfWeights )
  
  for( i in 1:nPortfolios ){
    
    thisFolio                   <- dfWeights[ i, ]  
    portRet                     <- 0
    for( j in 1:pNSectors ){
      
      thisSector                  <- as.character( sectors[ j ] )
      thisWeight                  <- thisFolio[ , c( thisSector ) ]
      portRet                     <- portRet + dfReturnData[ , c( thisSector ) ] * thisWeight
      
    }
    
    portRet                     <- 1 + portRet / 100
    cumRet                      <- cumprod(( portRet ) )
    drawdown                    <- calcDrawdown( cumRet )
    dfTmp                       <- data.frame( "PricingDate" = dfReturnData$PricingDate, "folio" = i, "return" = portRet - 1, "cumRet" = cumRet )
    
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
  
  # prepare output
  listOut <- list( "folioReturnsDf" = folioReturnsDf,
                   "summaryDf"      = summaryDf
  )
  
  listOut
  
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

calcSimFolioPerformance <- function( dfPerformance, pMethod, clusterDimensions, pUtility ){
  
  return                      <- dfPerformance$return
  
  cumRet                      <- cumprod(( return ) )
  drawdown                    <- calcDrawdown( cumRet )
  
  # summary stats
  mu                          <- mean( return )
  sigma                       <- sd( return )
  skew                        <- skewness( return )    
  kurt                        <- kurtosis( return )
  maxDD                       <- max( drawdown )
  
  summaryDf                <- data.frame( "utilityType" = pUtility, "method" = pMethod, "clusterDimension" = clusterDimensions, "cumRet" = tail( cumRet, 1 ), "mu" = mu, "sigma" = sigma, "ir" = mu / sigma, "skew" = skew, "kurt" = kurt, "maxDD" = maxDD )
  
  summaryDf
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
    
    mu                    <- mean( dfTmp$mu )
    utility               <- mean( dfTmp[ ,c( pUtility ) ] )
    
    dfFolioPerformanceByClusterTmp <- data.frame( "cluster" = thisCluster, "mu" = mu, "utility" = utility )
    
    colnames( dfFolioPerformanceByClusterTmp )[ grep( "utility", colnames( dfFolioPerformanceByClusterTmp ), fixed = TRUE ) ]  <- c( pUtility )
    
    if( i == 1 ){
      dfFolioPerformanceByCluster <- dfFolioPerformanceByClusterTmp
    } else {
      dfFolioPerformanceByCluster <- rbind( dfFolioPerformanceByCluster, dfFolioPerformanceByClusterTmp )
    }
    
  }
  
  dfFolioPerformanceByCluster
  
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

sectorSummary <- function( dfReturns, pSectors, pStamp, pTimeMarketMaxDD ){
  
  for( i in 1:length( pSectors ) ){
    
    thisSector                  <- as.character( pSectors[ i ] )
    thisRet                     <- dfReturns[ , c( thisSector ) ]
    
    # get stats
    mu                          <- mean( thisRet )
    sigma                       <- sd( thisRet )
    ir                          <- mu / sigma
    skew                        <- skewness( thisRet )    
    kurt                        <- kurtosis( thisRet )
    
    
    # prepare data
    portRet                     <- 1 + thisRet / 100
    cumRet                      <- cumprod( ( portRet ) )
    drawdown                    <- calcDrawdown( cumRet )
    maxDD                       <- max( drawdown )
    timeIdx                     <- drawdown == maxDD
    if( pStamp == "PricingDate"){
      maxDDdate                   <- chron( as.character( dfReturns$PricingDate[ timeIdx ] ) )
      nMonths                     <- as.numeric( ( maxDDdate - pTimeMarketMaxDD ) / 30.5 )
    } else {
      nMonths                   <- dfReturns$month[ timeIdx ]
      nMonths                   <- nMonths - pTimeMarketMaxDD
    }
    
    dfTmp                       <- data.frame( "sector" = thisSector, "mu" = mu, "sigma" = sigma, "ir" = ir, "skew" = skew, "kurt" = kurt, "cumRet" = tail( cumRet, 1 ), "maxDD" = maxDD, "lossTiming" = nMonths )    
    if( i == 1 ){
      dfOut                       <- dfTmp
    } else {
      dfOut                       <- rbind( dfOut, dfTmp )
    }
  }
  dfOut
}

simFullReturns <- function( pTsim, nSectors, lambda, psi, chi, alpha.bar, mu, gamma, Amat ){
  
  W                           <- rgig( pTsim, chi = chi, psi = psi, lambda = lambda )
  muNorm                      <- rep( 0, nSectors )
  sigmaNorm                   <- diag( 1, nSectors, nSectors )
  Z                           <- rnorm( pTsim * nSectors, 0, 1 )
  
  simReturns                  <- matrix( 0, pTsim, nSectors)
  for( i in 1:pTsim ){
    
    Wscal_i                     <- W[ i ]
    Zvec_i                      <- Z[ ( (i-1 )*nSectors + 1):(i*nSectors) ]
    retVec_i                    <- mu + Wscal_i * gamma + sqrt( Wscal_i ) * Amat %*% Zvec_i
    simReturns[i,]              <- retVec_i
    
  }
  
  dfSim                       <- as.data.frame( simReturns)
  colnames(dfSim)             <- pSectors
  dfSim$PricingDate           <- seq( 1, pTsim, 1 )
  
  dfSim
  
}

simCreateBenchmarkDDmonth         <- function( dfSimData, thisSector ){
  
  thisRet                  <- dfSimData[ , c( thisSector) ]
  
  portRet                  <- 1 + thisRet / 100
  cumRet                   <- cumprod(( portRet ) )
  drawdown                 <- calcDrawdown( cumRet )
  maxDD                    <- max(drawdown )
  timeIdx                  <- drawdown == maxDD
  
  dfSimData$PricingDate[ timeIdx ]  
}


simGetTimingOfLoss            <- function( folioReturnsDf ){
  
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
    maxDDdate           <- dfTmp$PricingDate[ timeIdx ]
    nMonths             <- as.numeric( ( maxDDdate - pTimeMarketMaxDD )  )
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

simCalcUtility <- function(folioReturnsDf, summaryDf ){
  
  # CPT utility and NIG
  pLambda                       <- 2.25
  dfUtility                     <- prospectTheoryUtility( folioReturnsDf, pGamma, pTau, pLambda, pRiskFree)  
  pLambda                       <- 1
  dfUtilityTmp                  <- prospectTheoryUtility( folioReturnsDf, pGamma, pTau, pLambda, pRiskFree)  
  dfUtility                     <- merge( dfUtility, dfUtilityTmp, by = c( "folio") )
  pLambda                       <- 1.5
  dfUtilityTmp                  <- prospectTheoryUtility( folioReturnsDf, pGamma, pTau, pLambda, pRiskFree)  
  dfUtility                     <- merge( dfUtility, dfUtilityTmp, by = c( "folio") )
  
  
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
  
  dfUtility  
}


clustGetNumberOfCluster   <- function( test, pClusters, pMethod ){
  
  if( pMethod == "hierarchical" ){
    aa            <- NbClust( test, method="complete", index="all")
  } else {
    aa            <- NbClust( test, method=pMethod, index="all")    
  }
  numClustSim   <- ceiling( mean(aa$Best.nc[1,]) )
  if( numClustSim < pClusters ){
    numClust  <- pClusters
  } else {
    numClust  <- numClustSim
  }
  
  list( "numCluster" = numClust,
        "calibNumCluster" = numClustSim )
  
}


getMaxUtilPortfolioClusterBased <- function( dfPerf, dfSectorExposures, kMean, pNClusters, pClusterType, clusterDimensions, pUtility, pSectors ){
  
  folioByCluster               <- auxCreateCluster( kMean, pClusterType )
  dfFolioPerfByCluster         <- auxByClusterPortfolioPerformance( dfPerf, folioByCluster, pUtility )                                          
  dfSectorExposuresByCluster   <- auxByClusterSectorExposures( dfSectorExposures, folioByCluster, pSectors )
  dfFolioInspectionTmp         <- auxCreateFolioInspection( dfFolioPerfByCluster, dfSectorExposuresByCluster, clusterDimensions, pClusterType, pNClusters )
  
  # get max util folio
  maxUtil                      <- max(dfFolioInspectionTmp[ , c( pUtility ) ])
  idx                          <- dfFolioInspectionTmp[ , c( pUtility )] == maxUtil
  dfFolioInspectionTmp         <- dfFolioInspectionTmp[ idx, ]
  colnames( dfFolioInspectionTmp )[ grep( pUtility, colnames( dfFolioInspectionTmp ), fixed = TRUE ) ]  <- "utility"
  dfFolioInspectionTmp$utilityType <- pUtility
  
  dfFolioInspectionTmp
}


getRPC                       <- function( dfPerf, dfSectorExposures, numClust, pClusterType, pRunOptimalNumberCluster, clusterDimensions, pNobs, pUtilityVec, pSectors ){
  
  if( pRunOptimalNumberCluster ){
    numClust                      <- clustGetNumberOfCluster( dfFolioPerf, numClust, pClusterType )  
  } else {
    numClust                      <- numClust
  }
  test                          <- dfPerf[ 1:pNobs, columns ]
  if( pClusterType == "kMeans" ){
    clusterFolio                   <- kmeans( test, numClust )    
  } else {
    hc                          <- hclust( dist(test), method = "complete" )
    clusterFolio                <- cutree(hc, k = numClust)
  }
  
  for( i in 1:length( pUtilityVec ) ){
    pUtility                      <- pUtilityVec[ i ]  
    folioOptimTmp                 <- getMaxUtilPortfolioClusterBased( dfPerf, dfSectorExposures, clusterFolio, numClust, pClusterType, clusterDimensions, pUtility, pSectors )
    
    if( i == 1 ){
      folioOptim                  <- folioOptimTmp
    } else {
      folioOptim                  <- rbind( folioOptim, folioOptimTmp )
    }
  }
  
  folioOptim
  
}

auxAddDfPlaceHoldersOptim  <- function( dfMVO, pColNamesSim ){
  
  colNamesMVO <- colnames( dfMVO )
  dfMVOout <- data.frame( matrix( ncol = length( pColNamesSim), nrow = nrow( dfMVO )))
  colnames(dfMVOout)<-pColNamesSim
  for( i in 1:length(pColNamesSim)){
    
    thisColName <- as.character( pColNamesSim[i] )
    isCovered <- sum( thisColName == colNamesMVO ) > 0
    if( isCovered ){
      dfMVOout[ , c(thisColName)] <- dfMVO[ , c(thisColName)]
    } else {
      dfMVOout[ , c(thisColName)] <- 999
    }
    
  }
  dfMVOout
}

errorHandlingOptim <- function( Dmat,dvec,Amat,bvec, meq){
  
  tryCatch(
    mvo                       <- solve.QP(Dmat,dvec,Amat,bvec=bvec, meq = meq ),
    error=function(e) FALSE
  )
}


getMVO <- function( dfSimData, pTargetRet, pSectors, nSectors ){
  
  # get expected returns
  dfTmp                     <- dfSimData
  dfTmp$PricingDate         <- NULL
  dfMean                    <- as.data.frame(lapply(dfTmp, mean))
  
  # MVO
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
  
  bvec[ 1 ]                 <- pTargetRet
  out                       <- errorHandlingOptim( Dmat,dvec,Amat,bvec, meq)
  if( length( out ) > 1 ){
    mvo                       <- out
    weightsMvo                <- as.data.frame( mvo$solution )  
    colnames( weightsMvo )[ grep( "mvo$solution", colnames( weightsMvo ), fixed = TRUE ) ]  <- "wgt"
    dfMVO                     <- cbind( dfMVO, weightsMvo )
    dfMVO$clusterMethod       <- "MVO"
    
  } else {
    dfMVO                     <- data.frame()
    print( paste( c( "MVO failed\n"), collapse = ""))
  }
  
  dfMVO
  
}

getKelly <- function( dfSimData, pSectors, nSectors ){
  
  dfTmp                     <- dfSimData
  dfTmp$PricingDate         <- NULL
  dfMean                    <- as.data.frame(lapply(dfTmp, mean))
  
  Dmat                        <- as.matrix( cov( dfTmp ) )
  dvec                        <- rep( 0, nSectors )     # mean normalized
  for( i in 1:nSectors ){
    dvec[ i ]                   <- ( dfMean[[i]] - pRiskFree * 100 )
  }
  a2                        <- rep( 1, nSectors )
  Amat                      <- matrix( c(a2 ), nrow = nSectors, ncol = 1 )
  Amat                      <- cbind( Amat, diag( x = 1, nSectors, nSectors ) )
  bvec                      <- c( 1, rep( 0, nSectors ))
  meq                       <- 2
  out                       <- errorHandlingOptim( Dmat,dvec,Amat,bvec, meq)
  if( length( out ) > 1 ){  
    kelly                     <- solve.QP(Dmat,dvec,Amat,bvec=bvec, meq = meq )
    dfKelly                   <- data.frame( "sector" = pSectors )
    dfKelly$wgt               <- kelly$solution
    dfKelly$clusterMethod     <- "Kelly"
  } else {
    dfKelly                   <- data.frame()
    print( paste( c( "Kelly failed\n"), collapse = ""))
  }
  
  dfKelly    
}

auxAddDfPlaceHoldersRet     <- function( dfFolioPerformance, pClusterDimension, pClusterMethod, pNClusters, pUtilityType ){
  
  dfFolioPerformance$clusterDimension <- pClusterDimension
  dfFolioPerformance$clusterMethod    <- pClusterMethod
  dfFolioPerformance$nClusters        <- pNClusters
  dfFolioPerformance$utilityType      <- pUtilityType
  
  dfFolioPerformance
  
}

auxGetSimFolioReturns      <- function( dfFolioOptim, dfSimReturn, pClusterDimension, pClusterMethod, pNClusters, pUtilityType ){
  
  dfWeightsTmp                <- as.data.frame(  t(dfFolioOptim$wgt))
  colnames(dfWeightsTmp)      <- dfFolioOptim$sector
  sector                      <- dfFolioOptim$sector
  listOutputTmp               <- constructFolioPerformance( dfSimReturn, dfWeightsTmp )
  dfFolioPerformance         <- listOutputTmp$folioReturnsDf  
  dfFolioPerformance         <- auxAddDfPlaceHoldersRet( dfFolioPerformance, pClusterDimension, pClusterMethod, pNClusters, pUtilityType )  
  
  dfFolioPerformance
  
}

simSumarizeFolioPerformanceBySim   <- function( folioPerformance, weightTracker, pUtilityVec, pSectors ){
  
  uniqueClusterDimensions     <- unique( folioPerformance$clusterDimension )
  uniqueMethods               <- unique( folioPerformance$clusterMethod )
  countSummary                <- 0
  
  for( i in 1:length( uniqueMethods ) ){
    
    thisMethod                  <- as.character( uniqueMethods[ i ] )
    
    if( thisMethod == "MVO" || thisMethod == "Kelly" ){
      
      idx                         <- folioPerformance$clusterMethod == thisMethod
      if( sum( idx ) ){
        folioPerformanceTmp         <- folioPerformance[ idx, ]
        simPerformanceSummaryTmp    <- calcSimFolioPerformance( folioPerformanceTmp, thisMethod, "na", "na" )
        
        idx                         <- weightTracker$clusterMethod == thisMethod
        weightTrackerTmp            <- weightTracker[ idx, ]
        simPerformanceSummaryTmp2   <- calcSimweightTracker( weightTrackerTmp, pSectors )
        
        simPerformanceSummaryTmp    <- cbind( simPerformanceSummaryTmp, simPerformanceSummaryTmp2 )
        
        if( countSummary == 0 ){
          simPerformanceSummary       <- simPerformanceSummaryTmp
        } else {
          simPerformanceSummary       <- rbind( simPerformanceSummary, simPerformanceSummaryTmp )
        }
        countSummary                <- countSummary + 1
      }
      
    } else {
      
      for( k in 1:length( pUtilityVec ) ){
        
        thisUtility                 <- as.character( pUtilityVec[ k ] )
        
        for( m in 1:length( uniqueClusterDimensions ) ){
          
          thisClusterDim              <- as.character( uniqueClusterDimensions[ m ] )
          
          idx1                        <- folioPerformance$clusterMethod == thisMethod
          idx2                        <- folioPerformance$utilityType == thisUtility
          idx3                        <- folioPerformance$clusterDimension == thisClusterDim
          idx                         <- idx1 & idx2 & idx3
          if( sum(idx) > 0 ){
            folioPerformanceTmp         <- folioPerformance[ idx, ]
            simPerformanceSummaryTmp    <- calcSimFolioPerformance( folioPerformanceTmp, thisMethod, thisClusterDim, thisUtility )
            
            idx                         <- weightTracker$clusterMethod == thisMethod
            weightTrackerTmp            <- weightTracker[ idx, ]
            simPerformanceSummaryTmp2   <- calcSimweightTracker( weightTrackerTmp, pSectors )
            
            simPerformanceSummaryTmp    <- cbind( simPerformanceSummaryTmp, simPerformanceSummaryTmp2 )
            
            if( countSummary == 0 ){
              simPerformanceSummary       <- simPerformanceSummaryTmp
            } else {
              simPerformanceSummary       <- rbind( simPerformanceSummary, simPerformanceSummaryTmp )
            }
            countSummary                <- countSummary + 1
          }
        }
      }
    }
  }
  
  simPerformanceSummary
  
}


calcSimweightTracker   <- function( weightTrackerDf, pSectors ){
  
  # calc TO
  uniqueOptimMonth            <- unique( weightTrackerDf$optimMonth )
  NRebal                      <- length( uniqueOptimMonth ) - 1
  TO                          <- 0
  for( i in 2:NRebal ){
    
    month_t                     <- uniqueOptimMonth[ i ]
    month_t_1                   <- uniqueOptimMonth[ i - 1 ]
    
    wgt_t                       <- weightTrackerDf[ weightTrackerDf$optimMonth == month_t, c( "sector", "wgt" ) ]
    wgt_t_1                     <- weightTrackerDf[ weightTrackerDf$optimMonth == month_t_1, c( "sector", "wgt" ) ]
    colnames(wgt_t)[ colnames(wgt_t) == 'wgt']     <- 'wgt_t'
    colnames(wgt_t_1)[ colnames(wgt_t_1) == 'wgt'] <- 'wgt_t_1'
    wgt_delta                   <- merge( wgt_t, wgt_t_1, by = c( "sector" ) )
    
    to_t                        <- sum( abs(wgt_delta$wgt_t - wgt_delta$wgt_t_1) )
    TO <- TO + to_t
    
  }
  
  # calc SSPW
  SSPW                        <- sum(weightTrackerDf$wgt^2) / NRebal
  
  # calc average weight
  wgtAverages                 <- aggregate( weightTrackerDf$wgt, by = list( weightTrackerDf$sector ), mean )
  sectors                     <- wgtAverages$Group.1
  wgtAverages                 <- as.data.frame( t( wgtAverages[,-1] ) )
  colnames( wgtAverages )     <- sectors
  wgtAverages                 <- wgtAverages[ , pSectors ]
  
  TOdf                        <- data.frame( "TO" = TO, "SSPW" = SSPW )
  TOdf                        <- cbind( TOdf, wgtAverages )
  
  TOdf
}


auxCreateKeyForSimSummary <- function( utilityType, method, clusterDimension ){
  key                         <- paste( c( as.character( method ), "_", as.character( clusterDimension ), "_", as.character( utilityType ) ), collapse = "" )
  key
}




library( chron )
library( gridExtra )
library( GeneralizedHyperbolic )
library( moments )
require( graphics )
library( clustertend )
library( NbClust )
library( fpc )
library( dbscan )
library( quadprog )
library( clues)
library( optimx )
library( ghyp )
library(tictoc)
library( xlsx )
library( writexl )




# Set Parameters for input/output data
# file names
pPath <- "C:\\Users\\SPECIFY YOUR OWN PATH"
pPathOut                    <- pPath
pFileReturns                <- "R_returnData_withEquity_truncated.csv"





# return parameters
pReturn                     <- "trt"
pPerRiskUnit                <- FALSE
pFlagSimplifyName           <- TRUE
pSectors                    <- c( "ust3m", "ust10", "tips", "mbs", "agency", "usIGCredit", "usHYCredit", "cmbs", "em", "intlBond", "spx", "intlEq", "emStock", "reit" )
nSectors                    <- length( pSectors )
pFI                         <- c( "ust3m", "ust10", "tips", "mbs", "agency", "usIGCredit", "usHYCredit", "cmbs", "em", "intlBond" )
pFTQ                        <- c( "ust3m", "ust10" )
pFIRates                    <- c( "ust3m", "ust10", "tips", "mbs", "agency" )
pFIRates                    <- c( "ust3m", "ust10", "mbs", "agency" )
pFICredit                   <- c( "usIGCredit", "usHYCredit", "cmbs", "em", "intlBond" )
pFICredit                   <- c( "usIGCredit", "usHYCredit", "cmbs", "intlBond" )
pNSectors                   <- length( pSectors )
pUtilityVec                 <- c( "CPTUtility_100", "CPTUtility_150", "CPTUtility_225", "crraUtility_1", "crraUtility_3", "crraUtility_5", "quadUtility_1", "quadUtility_3", "quadUtility_5" )

# prepare data
fileName                    <- paste( c( pPath, "\\", pFileReturns  ), collapse ="" )
dfReturns                   <- read.csv( fileName )
dfReturns                   <- castPricingDate( dfReturns, "PricingDate" )
pPerRiskUnit                <- FALSE
dfReturnDataTotRet          <- prepareInput( dfReturns, pReturn, pPerRiskUnit, pSectors, pFlagSimplifyName )



# EM fit of data
X                           <- dfReturnDataTotRet
X$PricingDate               <- NULL
EMalgo                      <- fit.NIGmv( X )
attrEMalgo                  <- attributes( EMalgo )

lambda                      <- attrEMalgo$lambda
psi                         <- attrEMalgo$psi
chi                         <- attrEMalgo$chi
alpha.bar                   <- attrEMalgo$alpha.bar
mu                          <- attrEMalgo$mu
sigma                       <- attrEMalgo$sigma
gamma                       <- attrEMalgo$gamma
Amat                        <- t( chol( sigma ) )


# portfolio simulation paramters
pDraws                      <- 1000               # 10000
pLowOmega                   <- 0
pHighOmega                  <- 0.6

# parameters prospect theory and NIG
pRiskFree                   <- 1.51 / 100 / 12 # US Bill 1 month
pGamma                      <- 0.88
pLambda                     <- 2.25
pTau                        <- 0.65

# cluster parameters
pNormalized                 <- TRUE
pNobs                       <- 1000            # 10000
pNobsNumClusters            <- 1000
pNClustersIR                <- 5
pNClustersCumRet            <- 8
pRunOptimalNumberCluster    <- FALSE



# Monte Carlo simulations
pRepeatSim                  <- 2                          # do the sims pRepeatSim times (make use of cloud for parallel processing, set pRepeatSim = 10000)
pTsim                       <- 10 * 12                    # ten years into the future
pTsimLookback               <- 5 * 12                     # look back 5 years to calibrate portfolio optimizations
pSimRebalance               <- 3                          # rebalance every 3 months
nSimFolioRuns               <- ( pTsim - pTsimLookback ) / pSimRebalance + 1

dfColnames                  <- c( "utilityType", "method", "clusterDimension","cumRet", "mu", "sigma", "ir", "skew", "kurt", "maxDD", "TO", "SSPW", pSectors )
simPerformanceSummary       <- data.frame(matrix(vector(), 0, length(dfColnames), dimnames=list(c(), dfColnames)), stringsAsFactors=F)

for( ss in 1:pRepeatSim ){
  
  dfSim                       <- simFullReturns( pTsim, nSectors, lambda, psi, chi, alpha.bar, mu, gamma, Amat )
  
  # SUMMARY STATS OF NIG SIMULATIONS
  pTimeMarketMaxDD            <- simCreateBenchmarkDDmonth( dfSim, "spx" )
  dfSimTmp                    <- dfSim
  colnames(dfSimTmp)[colnames(dfSimTmp) == 'PricingDate'] <- 'month'
  summarySimTmp               <- sectorSummary( dfSimTmp, pSectors, "month", pTimeMarketMaxDD )
  summarySimTmp$simulation    <- ss
  if( ss == 1 ){
    summarySim                  <- summarySimTmp
  } else {
    summarySim                  <- rbind( summarySim, summarySimTmp )
  }
  
  # CORRELATIONS OF NIG SIMULATIONS
  Xsim                        <- dfSim
  Xsim$PricingDate            <- NULL
  corSimTmp                   <- as.data.frame( cor( Xsim ) )
  corSimTmp$sector            <- rownames( corSimTmp )
  corSimTmp$simulation        <- ss
  if( ss == 1 ){
    corSim                      <- corSimTmp
  } else {
    corSim                      <- rbind( corSim, corSimTmp )
  }
  
  
  # RUN OPTIMIZATIONS AND PNL
  tic( "run sim optim and perf calc")
  countPerf                   <- 0
  for( j in 1:(nSimFolioRuns-1) ){    
    
    print( paste( c( as.character( j)) ), collapse="")
    
    # construct sample for portfolio weights calibration
    firstRowOptim               <- (j - 1 ) * pSimRebalance + 1
    lastRowOptim                <- firstRowOptim + pTsimLookback - 1
    dfSimTmpOptim               <- dfSim[ firstRowOptim:lastRowOptim, ]
    
    # extract sample for portfolio return calculation based on those weights
    firstRowReturn              <- lastRowOptim + 1
    lastRowReturn               <- firstRowReturn + pSimRebalance - 1
    dfSimTmpReturn              <- dfSim[ firstRowReturn:lastRowReturn, ]
    
    
    # GENERATE PORTFOLIOs
    pTimeMarketMaxDD            <- simCreateBenchmarkDDmonth( dfSimTmpOptim, "spx" )
    dfWeights                   <- randomWeights( pDraws, pSectors, pNSectors, pLowOmega, pHighOmega )
    listOutput                  <- constructFolioPerformance( dfSimTmpOptim, dfWeights )
    folioReturnsDf              <- listOutput$folioReturnsDf
    summaryDf                   <- listOutput$summaryDf
    summaryDf                   <- normalizeData( summaryDf )
    
    # get loss timing
    dfTimingOfLoss              <- simGetTimingOfLoss( folioReturnsDf )  
    summaryDf                   <- merge( summaryDf, dfTimingOfLoss, by = c( "folio") )
    
    # calc utility
    dfUtility                   <- simCalcUtility(folioReturnsDf, summaryDf )
    
    # put together
    dfFolioPerf                 <- summaryDf[1:pNobs, ]
    dfSectorExposures           <- dfWeights[ 1:pNobs, ]
    dfSectorExposures$folio     <- seq( 1, nrow(dfSectorExposures),1)
    dfFolioPerf                 <- merge( dfFolioPerf, dfUtility, by = c( "folio"))
    
    
    # PORTFOLIO OPTIMIZATIONS
    
    # RPC
    # a) cluster with c( "ir", "skew", "kurt" )
    columns                     <- c( "ir", "skew", "kurt" )
    clusterDimensions           <- auxGetClusterDimensions( columns )
    if( pNormalized ) columns   <- auxGetNormalizedNames( columns )
    pClusterType                <- "kMeans"
    folioOptim                  <- getRPC( dfFolioPerf, dfSectorExposures, pNClustersIR, pClusterType, pRunOptimalNumberCluster, clusterDimensions, pNobs, pUtilityVec, pSectors )
    pClusterType                <- "hierarchical"  
    folioOptim                  <- rbind( folioOptim, getRPC( dfFolioPerf, dfSectorExposures, pNClustersIR, pClusterType, pRunOptimalNumberCluster, clusterDimensions, pNobs, pUtilityVec, pSectors ) )
    
    # b) cluster with c(  "cumRet", "sigma", "skew", "kurt" )
    columns                     <- c(  "cumRet", "sigma", "skew", "kurt" )
    clusterDimensions           <- auxGetClusterDimensions( columns )
    if( pNormalized ) columns   <- auxGetNormalizedNames( columns )
    pClusterType                <- "kMeans"
    folioOptim                  <- rbind( folioOptim, getRPC( dfFolioPerf, dfSectorExposures, pNClustersCumRet, pClusterType, pRunOptimalNumberCluster, clusterDimensions, pNobs, pUtilityVec, pSectors ) )
    pClusterType                <- "hierarchical"  
    folioOptim                  <- rbind( folioOptim, getRPC( dfFolioPerf, dfSectorExposures, pNClustersCumRet, pClusterType, pRunOptimalNumberCluster, clusterDimensions, pNobs, pUtilityVec, pSectors ) )
    
    # MVO & Kelly
    pColNamesSim                <- colnames(folioOptim)
    if( FALSE) {
      idx                         <- folioOptim$sector == "usIGCredit"
      targetRet                   <- mean( folioOptim$mu[idx] )
      dfMVO                       <- getMVO( dfSimTmpOptim, targetRet, pSectors, nSectors )
      if( nrow( dfMVO ) > 0 ){
        dfMVO                       <- auxAddDfPlaceHoldersOptim( dfMVO, pColNamesSim )
        folioOptim                  <- rbind( folioOptim, dfMVO )
      }
      
    }
    
    dfKelly                     <- getKelly( dfSimTmpOptim, pSectors, nSectors )
    if( nrow( dfKelly ) > 0 ){
      dfKelly                     <- auxAddDfPlaceHoldersOptim( dfKelly, pColNamesSim )
      folioOptim                  <- rbind( folioOptim, dfKelly )
    }
    
    # CONSTRUCT MONTHLY PORTFOLIO PERFORMANCE & KEEP TRACK OF WEIGHTS
    uniqueClusterDimensions     <- unique( folioOptim$clusterDimensions )
    uniqueMethods               <- unique( folioOptim$clusterMethod )
    
    
    for( i in 1:length( uniqueMethods ) ){
      
      thisMethod                  <- as.character( uniqueMethods[ i ] )
      
      if( thisMethod == "MVO" || thisMethod == "Kelly" ){
        
        idx                         <- folioOptim$clusterMethod == thisMethod
        if( sum( idx ) ){
          folioOptimTmp               <- folioOptim[ idx, ]
          folioPerformanceTmp         <- auxGetSimFolioReturns( folioOptimTmp, dfSimTmpReturn, "na", thisMethod, NA, "na" )
          weightTrackerTmp            <- folioOptimTmp
          weightTrackerTmp$optimMonth <- lastRowOptim
          
          if( countPerf == 0 ){
            folioPerformance            <- folioPerformanceTmp
            weightTracker               <- weightTrackerTmp
          } else {
            folioPerformance            <- rbind( folioPerformance, folioPerformanceTmp )
            weightTracker               <- rbind( weightTracker, weightTrackerTmp )
          }
          countPerf                   <- countPerf + 1
        }
        
      } else {
        
        for( k in 1:length( pUtilityVec ) ){
          
          thisUtility <- as.character( pUtilityVec[ k ] )
          
          for( m in 1:length( uniqueClusterDimensions ) ){
            
            thisClusterDim              <- as.character( uniqueClusterDimensions[ m ] )
            
            idx1                        <- folioOptim$clusterMethod == thisMethod
            idx2                        <- folioOptim$utilityType == thisUtility
            idx3                        <- folioOptim$clusterDimensions == thisClusterDim
            idx                         <- idx1 & idx2 & idx3
            if( sum(idx) > 0 ){
              folioOptimTmp               <- folioOptim[ idx, ]
              folioPerformanceTmp         <- auxGetSimFolioReturns( folioOptimTmp, dfSimTmpReturn, thisClusterDim, thisMethod, folioOptimTmp$nClusters[ 1 ], thisUtility )
              weightTrackerTmp            <- folioOptimTmp
              weightTrackerTmp$optimMonth <- lastRowOptim
              
              if( countPerf == 0 ){
                folioPerformance            <- folioPerformanceTmp
                weightTracker               <- weightTrackerTmp
              } else {
                folioPerformance            <- rbind( folioPerformance, folioPerformanceTmp )
                weightTracker               <- rbind( weightTracker, weightTrackerTmp )
              }
              countPerf                   <- countPerf + 1
            }
          }
        }
      }
    }
  }
  toc()
  
  folioPerformance               <- folioPerformance[ order( folioPerformance$clusterDimension, folioPerformance$clusterMethod,folioPerformance$utilityType, folioPerformance$PricingDate ), ]
  weightTracker                  <- weightTracker[ order( weightTracker$clusterDimension, weightTracker$clusterMethod,weightTracker$utilityType, weightTracker$optimMonth ), ]
  
  # performance summary
  simPerformanceSummarySim       <- simSumarizeFolioPerformanceBySim( folioPerformance, weightTracker, pUtilityVec, pSectors )   
  simPerformanceSummary          <- rbind( simPerformanceSummary, simPerformanceSummarySim )
  
}



if( FALSE ){
  fileName <- paste( c( pPathOut, "\\folioPerformance.csv"), collapse = "" )  
  write.csv( folioPerformanceSim, fileName )
  
  fileName <- paste( c( pPathOut, "\\weightTracker.csv"), collapse = "" )  
  write.csv( weightTrackerSim, fileName )
}




# SUMMARIZE NIG SIMULATIONS: MOMENTS
# summary of moments
pTimeMarketMaxDD            <- chron( as.character( "2/27/2009" ) )
summaryHistData             <- sectorSummary( dfReturnDataTotRet, pSectors, "PricingDate", pTimeMarketMaxDD )
summarySimOverall           <- aggregate( x = summarySim[ , c( "mu", "sigma", "ir", "skew", "kurt", "cumRet", "maxDD", "lossTiming") ], by = list( summarySim$sector ), FUN = "mean" )



# SUMMARIZE NIG SIMULATIONS: CORR
X                           <- dfReturnDataTotRet
X$PricingDate               <- NULL
corHist                     <- cor( X )
corSimAverage               <- aggregate( x = corSim[ , pSectors ], by = list( corSim$sector ), FUN = "mean" )

for( i in 1:length( pSectors ) ){
  thisSector                  <- as.character( pSectors[ i ] )
  idx                         <- corSimAverage$Group.1 == thisSector
  thisRow <- corSimAverage[ idx, ]
  if( i == 1 ){
    corSimAverageSorted       <- thisRow    
  } else {
    corSimAverageSorted       <- rbind( corSimAverageSorted, thisRow )
  }
}
corSimAverageSortedOut      <- corSimAverageSorted
corSimAverageSorted$Group.1 <- NULL
deltaCorr                   <- corHist - corSimAverageSorted


# WRITE TO XLS
if( FALSE ){
  fileName <- paste( c( pPathOut, "\\summaryStatsSimulationMoments", ".xlsx"), collapse = "" )
  write.xlsx( summaryHistData, fileName, sheetName = "Summary Hist", row.names = FALSE, append = FALSE )
  write.xlsx( summarySimOverall, fileName, sheetName = "Summary Sim" , row.names = FALSE, append = TRUE)
  write.xlsx( deltaSummary, fileName, sheetName = "Summary Delta (Hist-Sim)", row.names = FALSE, append = TRUE )
  write.xlsx( corHist, fileName, sheetName = "Corr Hist", row.names = FALSE, append = TRUE )
  write.xlsx( corSimAverageSortedOut, fileName, sheetName = "Corr Sim", row.names = FALSE, append = TRUE )
  write.xlsx( deltaCorr, fileName, sheetName = "Corr Delta (Hist-Sim)", row.names = FALSE, append = TRUE )
}


# SUMMARIZE PORTFOLIO PERFORMANCE
key                         <- mapply( auxCreateKeyForSimSummary, simPerformanceSummary$method, simPerformanceSummary$clusterDimension, simPerformanceSummary$utilityType )
simPerformanceSummary$key   <- key
columNamesSummary           <- c( "cumRet", "mu", "sigma", "ir", "skew", "kurt", "kurt", "maxDD", "TO", "SSPW", pSectors )
simSummary                  <- aggregate( simPerformanceSummary[ , columNamesSummary ], by = list( simPerformanceSummary$key ), mean )


