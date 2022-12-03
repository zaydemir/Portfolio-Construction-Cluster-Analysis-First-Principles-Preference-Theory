rm(list=ls())

##Start Script


castPricingDate             <- function( df, dateStr ){
  
  dates                 <- df[ , c( dateStr) ]
  PricingDate           <- chron( as.character( dates ) )
  df[ , c( dateStr) ]   <- PricingDate
  
  df
}

prepareInput                <- function( dfReturns, pReturn, pPerRiskUnit, pSectors, pExcessReturn, pRiskFreeSector, pFlagSimplifyName ){
  
  dfOut               <- data.frame( "PricingDate" = dfReturns$PricingDate )
  if( pRiskFreeSector == "" ){
    retBenchmark        <- rep( 0, nrow( dfOut) )
  } else {
    if( pPerRiskUnit ){
      thisString          <- paste( c( pReturn, "_", pRiskFreeSector, "_perRisk" ), collapse = "" )
    } else {
      thisString          <- paste( c( pReturn, "_", pRiskFreeSector), collapse = "" )   
    }
    retBenchmark        <- dfReturns[ , c( thisString ) ]
  }
  for( i in 1:length( pSectors ) ){
    thisSector <- as.character( pSectors[ i ] )
    if( pPerRiskUnit ){
      thisString <- paste( c( pReturn, "_", thisSector, "_perRisk" ), collapse = "" )
    } else {
      thisString <- paste( c( pReturn, "_", thisSector), collapse = "" )   
    }
    thisVal        <- dfReturns[ , c( thisString ) ] - retBenchmark
    dfTmp          <- data.frame( "PricingDate" = dfReturns$PricingDate, "thisVal" = thisVal )
    dfOut          <- merge( dfOut, dfTmp, by = c( "PricingDate" ) )
    if( pFlagSimplifyName ){
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


constructFolioPerformance  <- function( dfReturnData, dfWeights, pNSectors ){
  
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
    drawdown[ i ]   <- ( maxCumReturn - thisCumReturn )/maxCumReturn
    
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
    colnames( dfTmp )[ grep( "thisVal", colnames( dfTmp ), fixed = TRUE ) ]  <- paste( c( "CPTUtility_", as.character( 100*pLambda ), "lambda_", as.character( 100*pTau),"tau"), collapse = "" )
    
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
    if( sum( timeIdx ) > 1 ){
      timeIdx             <- rep( FALSE, length( timeIdx))
      timeIdx[ 1 ]        <- TRUE
    }
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
  seed                        <- sample(.Machine$integer.max, size = 1) 
  set.seed( seed )
  W                           <- rgig( pTsim, chi = chi, psi = psi, lambda = lambda )
  muNorm                      <- rep( 0, nSectors )
  sigmaNorm                   <- diag( 1, nSectors, nSectors )
  set.seed( seed )
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
  if( sum( timeIdx ) > 1 ){
    timeIdx             <- rep( FALSE, length( timeIdx))
    timeIdx[ 1 ]        <- TRUE
  }
  dfSimData$PricingDate[ timeIdx ]  
}


simGetTimingOfLoss            <- function( folioReturnsDf, pTimeMarketMaxDD ){
  
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
    if( sum( timeIdx ) > 1 ){
      timeIdx             <- rep( FALSE, length( timeIdx))
      timeIdx[ 1 ]        <- TRUE
    }
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

simCalcUtility <- function(folioReturnsDf, summaryDf, cptGamma, cptTau, cptLambda, cptRiskFree ){
  
  # CPT utility and NIG
  cptLambda                     <- 2.25
  cptTau                        <- 0.3
  dfUtility                     <- prospectTheoryUtility( folioReturnsDf, cptGamma, cptTau, cptLambda, cptRiskFree )  
  
  cptLambda                     <- 2.25
  cptTau                        <- 0.65
  dfUtilityTmp                  <- prospectTheoryUtility( folioReturnsDf, cptGamma, cptTau, cptLambda, cptRiskFree )  
  dfUtility                     <- merge( dfUtility, dfUtilityTmp, by = c( "folio") )
  
  cptLambda                     <- 1.0
  cptTau                        <- 0.22
  dfUtilityTmp                  <- prospectTheoryUtility( folioReturnsDf, cptGamma, cptTau, cptLambda, cptRiskFree )  
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


errorHandlingSimulation <- function( paramListNIG, paramListFolioWeigthsDraw, paramListProspectTheory, paramListCluster, paramListMCsim, pUtilityVec, pSectors, nSectors ){
  
  tryCatch(
    simOutut                       <- simulatePortfolioConstruction(paramListNIG, paramListFolioWeigthsDraw, paramListProspectTheory, paramListCluster, paramListMCsim, pUtilityVec, pSectors, nSectors),
    error=function(e) FALSE
  )
}




simulatePortfolioConstruction    <- function( paramListNIG, paramListFolioWeigthsDraw, paramListProspectTheory, paramListCluster, paramListMCsim, pUtilityVec, pSectors, nSectors ){
  
  # EXTRACT PARAMETERS
  
  # extract params NIG
  lambda                      <- paramListNIG$lambda
  psi                         <- paramListNIG$psi
  chi                         <- paramListNIG$chi
  alpha.bar                   <- paramListNIG$alpha.bar
  mu                          <- paramListNIG$mu
  gamma                       <- paramListNIG$gamma
  Amat                        <- paramListNIG$Amat
  
  # extract params folio weights
  nDraws                      <- paramListFolioWeigthsDraw$nDraws
  lowOmega                    <- paramListFolioWeigthsDraw$lowOmega
  highOmega                   <- paramListFolioWeigthsDraw$highOmega
  
  # extract params prospect theory
  cptRiskFree                 <- paramListProspectTheory$cptRiskFree
  cptGamma                    <- paramListProspectTheory$cptGamma
  cptLambda                   <- paramListProspectTheory$cptLambda
  cptTau                      <- paramListProspectTheory$cptTau
  
  # extact params for cluster analysis 
  clustNormalized	            <- paramListCluster$clustNormalized
  clustNobs	                  <- paramListCluster$clustNobs
  clustNobsNumClusters	      <- paramListCluster$clustNobsNumClusters
  clustNclustersIR           	<- paramListCluster$clustNclustersIR
  clustNclustersCumRet	      <- paramListCluster$clustNclustersCumRet
  clustRunOptimalNumberCluster<- paramListCluster$clustRunOptimalNumberCluster
  
  # extract params for MC simulations and portfolio simulations
  mcTsim                      <- paramListMCsim$mcTsim
  mcTsimLookback              <- paramListMCsim$mcTsimLookback
  mcSimRebalance              <- paramListMCsim$mcSimRebalance
  mcNSimFolioRuns             <- ( mcTsim - mcTsimLookback ) / mcSimRebalance + 1
  
  
  
  # GENERATE MULTIVARIATE NIG PATHS
  # generate time series 
  dfSim                       <- simFullReturns( mcTsim, nSectors, lambda, psi, chi, alpha.bar, mu, gamma, Amat )
  
  # SUMMARY STATS OF NIG SIMULATIONS
  pTimeMarketMaxDD            <- simCreateBenchmarkDDmonth( dfSim, "spx" )
  dfSimTmp                    <- dfSim
  colnames(dfSimTmp)[colnames(dfSimTmp) == 'PricingDate'] <- 'month'
  summarySim                  <- sectorSummary( dfSimTmp, pSectors, "month", pTimeMarketMaxDD )
  
  # CORRELATIONS OF NIG SIMULATIONS
  Xsim                        <- dfSim
  Xsim$PricingDate            <- NULL
  corSim                      <- as.data.frame( cor( Xsim ) )
  corSim$sector               <- rownames( corSim )
  
  
  # RUN OPTIMIZATIONS AND PNL
  countPerf                   <- 0
  #mcNSimFolioRuns<-2
  for( j in 1:(mcNSimFolioRuns-1) ){          #nSimFolioRuns-1
    
    #print( paste( c( as.character( j)) ), collapse="")
    
    # construct sample for portfolio weights calibration
    firstRowOptim               <- (j - 1 ) * mcSimRebalance + 1
    lastRowOptim                <- firstRowOptim + mcTsimLookback - 1
    dfSimTmpOptim               <- dfSim[ firstRowOptim:lastRowOptim, ]
    
    # extract sample for portfolio return calculation based on those weights
    firstRowReturn              <- lastRowOptim + 1
    lastRowReturn               <- firstRowReturn + mcSimRebalance - 1
    dfSimTmpReturn              <- dfSim[ firstRowReturn:lastRowReturn, ]
    
    
    # GENERATE PORTFOLIOs
    pTimeMarketMaxDD            <- simCreateBenchmarkDDmonth( dfSimTmpOptim, "spx" )
    dfWeights                   <- randomWeights( nDraws, pSectors, nSectors, lowOmega, highOmega )
    listOutput                  <- constructFolioPerformance( dfSimTmpOptim, dfWeights, nSectors )
    folioReturnsDf              <- listOutput$folioReturnsDf
    summaryDf                   <- listOutput$summaryDf
    summaryDf                   <- normalizeData( summaryDf )
    
    # get loss timing
    dfTimingOfLoss              <- simGetTimingOfLoss( folioReturnsDf, pTimeMarketMaxDD )  
    summaryDf                   <- merge( summaryDf, dfTimingOfLoss, by = c( "folio") )
    
    # calc utility
    dfUtility                   <- simCalcUtility( folioReturnsDf, summaryDf, cptGamma, cptTau, cptLambda, cptRiskFree )
    
    # put together
    dfFolioPerf                 <- summaryDf[1:clustNobs, ]
    dfSectorExposures           <- dfWeights[ 1:clustNobs, ]
    dfSectorExposures$folio     <- seq( 1, nrow(dfSectorExposures),1)
    dfFolioPerf                 <- merge( dfFolioPerf, dfUtility, by = c( "folio"))
    
    
    # PORTFOLIO OPTIMIZATIONS
    
    # RPC
    # a) cluster with c( "ir", "skew", "kurt" )
    columns                     <- c( "ir", "skew", "kurt" )
    clusterDimensions           <- auxGetClusterDimensions( columns )
    if( clustNormalized ) columns   <- auxGetNormalizedNames( columns )
    pClusterType                <- "kMeans"
    folioOptim                  <- getRPC( dfFolioPerf, dfSectorExposures, columns, clustNclustersIR, pClusterType, clustRunOptimalNumberCluster, clusterDimensions, clustNobs, pUtilityVec, pSectors )
    pClusterType                <- "hierarchical"  
    folioOptim                  <- rbind( folioOptim, getRPC( dfFolioPerf, dfSectorExposures, columns, clustNclustersIR, pClusterType, clustRunOptimalNumberCluster, clusterDimensions, clustNobs, pUtilityVec, pSectors ) )
    
    # b) cluster with c(  "cumRet", "sigma", "skew", "kurt" )
    columns                     <- c(  "cumRet", "sigma", "skew", "kurt" )
    clusterDimensions           <- auxGetClusterDimensions( columns )
    if( clustNormalized ) columns   <- auxGetNormalizedNames( columns )
    pClusterType                <- "kMeans"
    folioOptim                  <- rbind( folioOptim, getRPC( dfFolioPerf, dfSectorExposures, columns, clustNclustersCumRet, pClusterType, clustRunOptimalNumberCluster, clusterDimensions, clustNobs, pUtilityVec, pSectors ) )
    pClusterType                <- "hierarchical"  
    folioOptim                  <- rbind( folioOptim, getRPC( dfFolioPerf, dfSectorExposures, columns, clustNclustersCumRet, pClusterType, clustRunOptimalNumberCluster, clusterDimensions, clustNobs, pUtilityVec, pSectors ) )
    
    # MVO & Kelly & 1/N
    pColNamesSim                <- colnames(folioOptim)
    if( TRUE) {
      
      targetRet                   <- auxGetMvoTarget( dfSimTmpOptim )
      dfMVO                       <- getMVO( dfSimTmpOptim, targetRet, pSectors, nSectors )
      if( nrow( dfMVO ) > 0 ){
        dfMVO                       <- auxAddDfPlaceHoldersOptim( dfMVO, pColNamesSim )
        folioOptim                  <- rbind( folioOptim, dfMVO )
      }
      
    }
    
    dfKelly                     <- getKelly( dfSimTmpOptim, pSectors, nSectors, cptRiskFree )
    if( nrow( dfKelly ) > 0 ){
      dfKelly                     <- auxAddDfPlaceHoldersOptim( dfKelly, pColNamesSim )
      folioOptim                  <- rbind( folioOptim, dfKelly )
    }
    
    df1_N                       <- get1_N( pSectors, nSectors )
    df1_N                       <- auxAddDfPlaceHoldersOptim( df1_N, pColNamesSim )
    folioOptim                  <- rbind( folioOptim, df1_N )
    
    # CONSTRUCT MONTHLY PORTFOLIO PERFORMANCE & KEEP TRACK OF WEIGHTS
    uniqueClusterDimensions     <- unique( folioOptim$clusterDimensions )
    uniqueMethods               <- unique( folioOptim$clusterMethod )
    
    for( i in 1:length( uniqueMethods ) ){
      
      thisMethod                  <- as.character( uniqueMethods[ i ] )
      
      if( thisMethod == "MVO" || thisMethod == "Kelly" || thisMethod == "1_N" ){
        
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
  
  folioPerformance               <- folioPerformance[ order( folioPerformance$clusterDimension, folioPerformance$clusterMethod,folioPerformance$utilityType, folioPerformance$PricingDate ), ]
  weightTracker                  <- weightTracker[ order( weightTracker$clusterDimension, weightTracker$clusterMethod,weightTracker$utilityType, weightTracker$optimMonth ), ]
  
  # performance summary
  simPerformanceSummarySim       <- simSumarizeFolioPerformanceBySim( folioPerformance, weightTracker, pUtilityVec, pSectors )   
  
  simulationListOut              <- list(
    "simPerformanceSummary"    = simPerformanceSummarySim,
    "summarySim"               = summarySim,
    "corSim"                   = corSim
  )
  
  simulationListOut
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


getRPC                       <- function( dfPerf, dfSectorExposures, columns, numClust, pClusterType, pRunOptimalNumberCluster, clusterDimensions, pNobs, pUtilityVec, pSectors ){
  
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

auxGetMvoTarget <- function( dfSimData ){
  
  dfTmp                     <- dfSimData
  dfTmp$PricingDate         <- NULL
  targetRet                    <- mean( unlist( lapply( dfTmp, mean ) ) )
  
  targetRet
  
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

getKelly <- function( dfSimData, pSectors, nSectors, pRiskFree ){
  
  dfTmp                     <- dfSimData
  dfTmp$PricingDate         <- NULL
  dfMean                    <- as.data.frame(lapply(dfTmp, mean))
  
  Dmat                        <- as.matrix( cov( dfTmp ) )
  dvec                        <- rep( 0, nSectors )     # mean normalized
  for( i in 1:nSectors ){
    dvec[ i ]                   <- ( dfMean[[i]] - pRiskFree * 100 )
  }
  dvec                      <- -dvec
  a2                        <- rep( 1, nSectors )
  Amat                      <- matrix( c(a2 ), nrow = nSectors, ncol = 1 )
  Amat                      <- cbind( Amat, diag( x = 1, nSectors, nSectors ) )
  bvec                      <- c( 1, rep( 0, nSectors ))
  meq                       <- 1
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

get1_N <- function( pSectors, nSectors ){
  
  df1_N <- data.frame( "sector" = pSectors, "wgt" = 1/nSectors, "clusterMethod" = "1_N" )
  df1_N
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
  listOutputTmp               <- constructFolioPerformance( dfSimReturn, dfWeightsTmp, nSectors )
  dfFolioPerformance          <- listOutputTmp$folioReturnsDf  
  dfFolioPerformance          <- auxAddDfPlaceHoldersRet( dfFolioPerformance, pClusterDimension, pClusterMethod, pNClusters, pUtilityType )  
  
  dfFolioPerformance
  
}

simSumarizeFolioPerformanceBySim   <- function( folioPerformance, weightTracker, pUtilityVec, pSectors ){
  
  uniqueClusterDimensions     <- unique( folioPerformance$clusterDimension )
  uniqueMethods               <- unique( folioPerformance$clusterMethod )
  countSummary                <- 0
  
  for( i in 1:length( uniqueMethods ) ){
    
    thisMethod                  <- as.character( uniqueMethods[ i ] )
    
    if( thisMethod == "MVO" || thisMethod == "Kelly" || thisMethod == "1_N" ){
      
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
          
          idxPerf1                    <- folioPerformance$clusterMethod == thisMethod
          idxPerf2                    <- folioPerformance$utilityType == thisUtility
          idxPerf3                    <- folioPerformance$clusterDimension == thisClusterDim
          idxPerf                     <- idxPerf1 & idxPerf2 & idxPerf3
          
          idxPerf1                    <- folioPerformance$clusterMethod == thisMethod
          idxPerf2                    <- folioPerformance$utilityType == thisUtility
          idxPerf3                    <- folioPerformance$clusterDimension == thisClusterDim
          idxPerf                     <- idxPerf1 & idxPerf2 & idxPerf3
          if( sum(idxPerf) > 0 ){
            folioPerformanceTmp         <- folioPerformance[ idxPerf, ]
            simPerformanceSummaryTmp    <- calcSimFolioPerformance( folioPerformanceTmp, thisMethod, thisClusterDim, thisUtility )
            
            idxWgt1                     <- weightTracker$clusterMethod == thisMethod
            idxWgt2                     <- weightTracker$utilityType == thisUtility
            idxWgt3                     <- weightTracker$clusterDimension == thisClusterDim
            idxWgt                      <- idxWgt1 & idxWgt2 & idxWgt3
            weightTrackerTmp            <- weightTracker[ idxWgt, ]
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


cleanUpConcatenateCsv                       <- function( pFilePrefix, pBucket = "" ){
  
  thisBucket                  <- aws.s3::get_bucket( pBucket )
  files                       <- aws.s3::get_bucket( thisBucket, prefix = pFilePrefix )
  tmpSimRandomNames           <- lapply( files, aws.s3::get_object)
  nFiles                      <- length( tmpSimRandomNames )
  for( i in 1:nFiles ){
    dfTmp                       <-   read.csv(text = rawToChar( tmpSimRandomNames[i]$Contents) ) 	
    if( i == 1 ){
      dfSimConcat                 <- dfTmp
    } else {
      dfSimConcat                 <- rbind( dfSimConcat, dfTmp )
    }
    
  }
    
  dfSimConcat

}


# dont need below because AMI already has it installed
if(FALSE){
  install.packages("chron")
  install.packages("gridExtra")
  install.packages("GeneralizedHyperbolic")
  install.packages("moments")
  install.packages("clustertend")
  install.packages("NbClust")
  install.packages("fpc")
  install.packages("dbscan")
  install.packages("quadprog")
  install.packages("clues")
  install.packages("optimx")
  install.packages("ghyp")
  install.packages("tictoc")
  install.packages("xlsx")
  install.packages("writexl")
  install.packages("foreach")
  install.packages("doParallel")
  install.packages("aws.s3")
  install.packages("aws.ec2metadata")
  install.packages("reticulate")
}


library(chron)
library(gridExtra)
library(GeneralizedHyperbolic)
library(moments)
require(graphics)
library(clustertend)
library(NbClust)
library(fpc)
library(dbscan)
library(quadprog)
library(clues)
library(optimx)
library(ghyp)
library(tictoc)
library(writexl)
library(foreach)
require(iterators)
require(parallel)
library(doParallel)
library(aws.s3)
library(aws.ec2metadata)
library(reticulate)

if( TRUE ){
  Sys.setenv("AWS_ACCESS_KEY_ID" = "",
             "AWS_SECRET_ACCESS_KEY" = "",
             "AWS_DEFAULT_REGION" = "")
}




# Set Parameters for input/output data
pPath                       <- "PATH TO YOUR LOCAL DRIVE"
pFileReturns                <- "R_returnData_withEquity.csv"
pS3_bucket                  <- "your bucket name"
pS3_readFile                <- pFileReturns
pS3_writeFileSimSummary     <- "R_simSummary"
pS3_writeFileSimPerformance <- "R_simPerformance"
pS3_writeFileSimCor         <- "R_simCorr"
pS3_writeFileSimTimer       <- "R_timer"

# return parameters
pReturn                     <- "trt"
pPerRiskUnit                <- FALSE
pFlagSimplifyName           <- TRUE
pExcessReturn               <- FALSE
pRiskFreeSector             <- ""     # empty string indicates no reference sector, ie. looking at total returns, altenative is eg. pRiskFreeSector <- "ust3m"
pSectors                    <- c( "ust3m", "ust10", "tips", "mbs", "agency", "usIGCredit", "usHYCredit", "cmbs", "em", "intlBond", "spx", "intlEq", "emStock", "reit" )
nSectors                    <- length( pSectors )


# read file from S3
dfReturns                   <- read.csv(text = rawToChar(aws.s3::get_object(object = pS3_readFile, bucket = pS3_bucket ) ) )
dfReturns                   <- castPricingDate( dfReturns, "PricingDate" )
pPerRiskUnit                <- FALSE
dfReturnDataTotRet          <- prepareInput( dfReturns, pReturn, pPerRiskUnit, pSectors, pExcessReturn, pRiskFreeSector, pFlagSimplifyName )


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
paramListNIG                <- list( 
  "lambda"	    =	lambda,
  "psi"	        =	psi,
  "chi"	        = chi,
  "alpha.bar"	  =	alpha.bar,
  "mu"	        =	mu,
  "sigma"	      =	sigma,
  "gamma"	      =	gamma,
  "Amat"	      =	Amat
)


# portfolio draw parameters
pDraws                      <- 10000               # 10000
pLowOmega                   <- 0
pHighOmega                  <- 0.6
paramListFolioWeigthsDraw   <- list(
  "nDraws"      = pDraws,
  "lowOmega"    = pLowOmega,
  "highOmega"   = pHighOmega
)


# parameters prospect theory and NIG
pRiskFree                   <- 1.51 / 100 / 12 # US Bill 1 month
pGamma                      <- 0.88
pLambda                     <- 2.25
pTau                        <- 0.65
paramListProspectTheory     <- list(
  "cptRiskFree" = pRiskFree,
  "cptGamma"    = pGamma,
  "cptLambda"   = pLambda,
  "cptTau"      = pTau
)


# parameters for clustering
pNormalized                 <- TRUE
pNobs                       <- 10000            # 10000
pNobsNumClusters            <- 1000
pNClustersIR                <- 5
pNClustersCumRet            <- 8
pRunOptimalNumberCluster    <- FALSE
paramListCluster            <- list(
  "clustNormalized"	  =	pNormalized,
  "clustNobs"	        =	pNobs,
  "clustNobsNumClusters"=	pNobsNumClusters,
  "clustNclustersIR"	=	pNClustersIR,
  "clustNclustersCumRet"=	pNClustersCumRet,
  "clustRunOptimalNumberCluster"=	pRunOptimalNumberCluster
)

pUtilityVec                 <- c( 
                                  "CPTUtility_225lambda_65tau", "CPTUtility_225lambda_30tau", "CPTUtility_100lambda_22tau", 
                                  "crraUtility_1", "crraUtility_3", "crraUtility_5", 
                                  "quadUtility_1", "quadUtility_3", "quadUtility_5" 
                                )


# Monte Carlo simulations
pTsim                       <- 12 * 12                    # twelve years into the future
pTsim                       <- 5 * 12
pTsimLookback               <- 5 * 12                     # look back 5 years to calibrate portfolio optimizations
pTsimLookback               <- 3 * 12 
pSimRebalance               <- 3                          # rebalance every 3 months
nSimFolioRuns               <- ( pTsim - pTsimLookback ) / pSimRebalance + 1
paramListMCsim              <- list(
  mcTsim              = pTsim,  
  mcTsimLookback      = pTsimLookback,
  mcSimRebalance      = pSimRebalance,
  mcNSimFolioRuns     = nSimFolioRuns
)



# MULTICORE PROCESSING
pPackages <- c( 
  'chron',
  'gridExtra',
  'GeneralizedHyperbolic',
  'moments',
  'clustertend',
  'NbClust',
  'fpc',
  'dbscan',
  'quadprog',
  'clues',
  'optimx',
  'ghyp'
)


args                        <- commandArgs(trailingOnly = TRUE)
reticulate::use_virtualenv("~/r_simulation/env")          # this is AMI related, has R libraries and Python libraries; reticulate is an R interface to Py
source_python("terminate_instance.py")      
pCores                      <- args[1]                    # ZA leave as is, but later override pCores with my detectCores; see 2 lines further down
pLoops                      <- args[2]
pCores                      <- detectCores()

# parallel processing
for( i in 1:pLoops ){
  
  print( paste( c( "Loop number ", as.character( i ) ), collapse = "" ) )
  tic()
  registerDoParallel( cores = pCores )
  listSimOutput <- foreach( i = 1:pCores,
                            .packages = pPackages    ) %dopar% errorHandlingSimulation( paramListNIG, paramListFolioWeigthsDraw, paramListProspectTheory, paramListCluster, paramListMCsim, pUtilityVec, pSectors, nSectors )  
  
  
  # save the data
  dfColnames                  <- c( "utilityType", "method", "clusterDimension","cumRet", "mu", "sigma", "ir", "skew", "kurt", "maxDD", "TO", "SSPW", pSectors )
  simPerformanceSummary       <- data.frame(matrix(vector(), 0, length(dfColnames), dimnames=list(c(), dfColnames)), stringsAsFactors=F)
  dfColnames                  <- c( "sector", "mu", "sigma", "ir", "skew", "kurt", "cumRet", "maxDD", "lossTiming")
  summarySim                  <- data.frame(matrix(vector(), 0, length(dfColnames), dimnames=list(c(), dfColnames)), stringsAsFactors=F)
  dfColnames                  <- c( pSectors, "sector" )
  corSim                      <- data.frame(matrix(vector(), 0, length(dfColnames), dimnames=list(c(), dfColnames)), stringsAsFactors=F)
  
  for( i in 1:pCores ){
    
    # extract information from list
    thisList                    <- listSimOutput[[i]]
    if( length( thisList ) > 1 ){
      simPerformanceSummaryTmp    <- thisList$simPerformanceSummary
      summarySimTmp               <- thisList$summarySim
      corSimTmp                   <- thisList$corSim
      
      # concatenate simulation results
      simPerformanceSummary       <- rbind( simPerformanceSummary, simPerformanceSummaryTmp )
      summarySim                  <- rbind( summarySim, summarySimTmp )
      corSim                      <- rbind( corSim, corSimTmp )
    }
  }
  
  # write the files to S3 bucket
  seed                        <- sample(.Machine$integer.max, size = 1) 
  set.seed( seed )
  x1 <- runif(1, 0, 100 )
  seed                        <- sample(.Machine$integer.max, size = 1) 
  set.seed( seed )
  x2 <- runif(1, 0, 100 )
  instance_id                 <- metadata$instance_id()   
  fileNameRandom              <-  paste(c(as.character(x1), "_", as.character(x2), "_", instance_id), collapse = "")
  
  fileNamePerformance         <- paste( c( pS3_writeFileSimPerformance, "_", fileNameRandom, ".csv" ), collapse = "" )
  aws.s3::s3write_using( simPerformanceSummary, FUN = write.csv, object = fileNamePerformance, bucket = pS3_bucket)
  
  fileNameSummary             <- paste( c( pS3_writeFileSimSummary, "_", fileNameRandom, ".csv" ), collapse = "" )
  aws.s3::s3write_using( summarySim, FUN = write.csv, object = fileNameSummary, bucket = pS3_bucket)
  
  fileNameCor                 <- paste( c( pS3_writeFileSimCor, "_", fileNameRandom, ".csv" ), collapse = "" )
  aws.s3::s3write_using( corSim, FUN = write.csv, object = fileNameCor, bucket = pS3_bucket)
  
  # keep timing
  keepTime                    <- toc()
  elapseT                     <- keepTime$toc - keepTime$tic
  dfElapse                    <- as.data.frame( elapseT )
  fileNameTimer               <- paste(c(pS3_writeFileSimTimer, "_", fileNameRandom, ".csv"), collapse = "")
  aws.s3::s3write_using(dfElapse, FUN = write.csv, object = fileNameTimer, bucket = pS3_bucket )
  
}

terminate_instance(metadata$instance_id())

# concatenate simulation results
if( FALSE ){
  dfSimPerformance              <- cleanUpConcatenateCsv( pS3_writeFileSimPerformance, pS3_bucket )
  dfSimSummary                  <- cleanUpConcatenateCsv( pS3_writeFileSimSummary, pS3_bucket )
  dfSimCor                      <- cleanUpConcatenateCsv( pS3_writeFileSimCor, pS3_bucket )
  dfSimTimer                    <- cleanUpConcatenateCsv( pS3_writeFileSimTimer, pS3_bucket )
  
  
  fileName <- paste( c( pPath, "\\debug_performance.csv"), collapse="")
  write.csv( dfSimPerformance, fileName )
  
  fileName <- paste( c( pPath, "\\debug_summary.csv"), collapse="")
  write.csv( dfSimSummary, fileName )

  fileName <- paste( c( pPath, "\\debug_corr.csv"), collapse="")
  write.csv( dfSimCor, fileName )

  fileName <- paste( c( pPath, "\\debug_timer.csv"), collapse="")
  write.csv( dfSimTimer, fileName )
  
}




