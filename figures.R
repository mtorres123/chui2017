#' Produce figures
#'
#' Functions to produce figures for Chui et al 2017
#'
#' plotYear produces a temporal heatmap of disaster frequency by country-year
#' and disaster type; a temporal heatmap and line graph of attack frequency 
#' by year (all countries)
#'
#' plotMonth produces a temporal heatmap of disaster frequency by country-month
#' and disaster type; a temporal heatmap and line graph of attack frequency
#' by year (all countries).  Month values range from 1 to 12.
#'
#' plotGHA produces a temporal heatmap of disaster frequency by year, 
#' and a temporal heatmap of attack frequency by year for Ghana.
#' 
#' plotIND produces a temporal heatmap of disaster frequency by year,
#' and a temporal heatmap of attack frequency by year for India. 
#' 
#' plotKEN produces a temporal heatmap of disaster frequency by year,
#' and a temporal heatmap of attack frequency by year for Kenya.
#' 
#' plotTZA produces a temporal heatmap of disaster frequency by year,
#' and a temporal heatmap of attack frequency by year for Tanzania.
#' 
#' plotYEM produces a temporal heatmap of disaster frequency by year,
#' and a temporal heatmap of attack frequency by year for Yemen
#' 
#' plotPHL produces a temporal heatmap of disaster frequency by year,
#' and a temporal heatmap of attack frequency by year for Philippines.
#' 
#' plotNCOM produces a temporal heatmap of attack frequency by year
#' for USNORTHCOM regions
#' 
#' plotSCOM produces a temporal heatmap of attack frequency by year
#' for USSOUTHCOM regions
#' 
#' plotACOM produces a temporal heatmap of attack frequency by year
#' for USAFRICOM regions
#' 
#' plotECOM produces a temporal heatmap of attack frequency by year
#' for USEUCOM regions
#' 
#' plotCCOM produces a temporal heatmap of attack frequency by year
#' for USCENTCOME regions
#' 
#' plotPCOM produces a temporal heatmap of attack frequency by year
#' for USPACOM regions
#'
#' @param o Character vector specifying output file(s).
#' @param form Character variable specifying output format:
#'   \describe{
#'     \item{pdf}{Adobe Portable document format}
#'     \item{png}{Portable network graphics}
#'     \item{jpeg}{Joint Photographic Experts Group}
#'     \item{eps}{Encapsulated postscript}
#'     \item{tiff}{Tagged image file format}
#'     \item{bmp}{Bitmap}
#' }
#' @return imperative function--writes files to disk and returns \code{NULL}.
#' @name figures
NULL

#' @rdname plotYear
plotYear <- function(o, form='pdf') {
  ## remdat  
  # load dfrEmDat
    years = year(dfrEmDat$Start.date)
    dfrEmDat$Disaster.type <- factor(dfrEmDat$Disaster.type)
    
  # Group country, year, and disaster type
    cdt <- count(dfrEmDat,c("Country","Disaster.type"))
    cyrd <- count(dfrEmDat,c("Country","years","Disaster.type"))
    
  # Plot heat map of disasters by year by country  
  pdf(o)  
    g <- ggplot(cyrd,aes(x=years,y=Disaster.type))
    g + geom_point() + facet_grid(Country~.) + ylab("Disaster type")
  dev.off()
  
  ## rgtd
  # load dfrGtd
  yrat <- count(dfrGtd,c("iyear","attacktype1_txt"))
  yrat$attacktype1_txt <- factor(yrat$attacktype1_txt)
  
  # Colors
  mycol <- colorRampPalette(rev(brewer.pal(11,"Spectral")))
  setcol <- brewer.pal(9,"Set1")
  names(setcol) <- levels(yrat$attacktype1_txt)
  colscale <- scale_color_manual(name="attack type",values=setcol)
  
  # Plot heat map of attacks by year: all countries
  pdf(o)
  ggplot(yrat,aes(x=iyear)) +
    geom_line(aes(y=attacktype1_txt,color=freq),size=1) + theme_dark() +
    scale_color_gradientn(colors=mycol(100),trans="log") + 
    labs(x="year", 
         y="attack type",
         title="Frequency of attack type by year for all countries")
  dev.off()
  
  # plot line graph of attack by year: all countries
  pdf(o)
  ggplot(yrat,aes(x=iyear)) +
    geom_line(aes(y=freq,color=attacktype1_txt),size=1) +
    colscale + 
    labs(x="year",
         color="attack type",
         title="Frequency of attack type by year for all countries")
  dev.off()
}

#' @rdname plotMonth
plotMonth <- function(o, form='pdf') {
  ## remdat
  # load dfrEmDat
    months = month(dfrEmDat$Start.date)
    cmod <- data.frame(
      "Country" = dfrEmDat$Country,
      "month" = months,
      "disaster" = dfrEmDat$Disaster.type
    )
    cmod1 <- count(cmod,c("Country","month","disaster"))
  
  # Plot heat map of disasters by month by country
  pdf(o)
  gg <- ggplot(cmod1,aes(x=month,y=disaster))
  gg + geom_point() + facet_grid(.~Country) + ylab("Disaster type")
  dev.off()
  
  ## rgtd
  # load dfrGtd
  moat <- count(dfrGtd,c("imonth","attacktype1_txt"))
  moat$attacktype1_txt <- factor(moat$attacktype1_txt)
  
  # Colors
  mycol <- colorRampPalette(rev(brewer.pal(11,"Spectral")))
  setcol <- brewer.pal(9,"Set1")
  names(setcol) <- levels(yrat$attacktype1_txt)
  colscale <- scale_color_manual(name="attack type",values=setcol)
  
  # Plot heat map of attacks by year: all countries
  pdf(o)
  ggplot(moat,aes(x=imonth)) +
    geom_line(aes(y=attacktype1_txt,color=freq),size=1) + theme_dark() +
    scale_color_gradientn(colors=mycol(100),trans="log") +
    labs(x="month",
         y="attack type",
         title="Frequency of attack type by month for all countries")
  dev.off()
  
  # Plot line graph of attacks by year: all countries
  pdf(o)
  ggplot(moat,aes(x=imonth)) +
    geom_line(aes(y=freq,color=attacktype1_txt),size=1) +
    colscale + scale_y_log10("freq, log10") +
    labs(x="month",
         color="attack type",
         title="Frequency of attack types by month for all countries")
  dev.off()
}

#' @rdname plotGHA
plotGHA <- function(o, form='pdf') {
  ## remdat
  # load dfrEmDat
  cyrd <- count(dfrEmDat,c("Country","years","Disaster.type"))
  ghana.yr <- cyrd[which(cyrd$Country == "Ghana"), ]
  
  # Generate time series from points
  ldis <- unlist(levels(dfrEmDat$Disaster.type))
  yrvec <- min(years):max(years)
  len <- length(yrvec)*length(ldis)
  yrbuild <- c()
  for (i in seq_along(yrvec)){
    yrbuild <- append(yrbuild,c(rep(yrvec[i],11)))
  }
  
  ghana.cont <- data.frame(
    Country = rep("Ghana",len),
    years = yrbuild,
    Disaster.type = rep(ldis,len/11)
  )
  ghana.cont <- merge(ghana.cont,ghana.yr,by=c("Country","years","Disaster.type"),all=T)
  ghana.cont[is.na(ghana.cont)] <- 0
  
  # Plot heat map of disasters by year
  pdf(o)
    par(mar = c(4.5,4.5,1.5,1))
    ggplot(ghana.cont,aes(x=years)) +
      geom_line(aes(y=Disaster.type,color=freq),size=1) +
      scale_color_distiller(palette="Spectral") +
      labs(color="freq",title="Ghana,years") +
      ylab("disaster type") + theme_bw()
  dev.off()
  
  ## rgtd
  # load dfrGtd
  cyrat <- count(dfrGtd,c("country_txt","iyear","attacktype1_txt"))
  phl.yr <- cyrat[which(cyrat$country_txt == "Philippines"), ]
  
  # Plot heat map of attacks by year
  pdf(o)
  ggplot(gha.yr,aes(x=iyear)) + 
    geom_line(aes(y=attacktype1_txt,color=freq),size=1) +
    scale_color_gradientn(colors=mycol(100)) +
    labs(x="year",
         y="attack type",
         title="Ghana,years")
  dev.off()
}

#' @rdname plotIND
plotIND <- function(o, form='pdf') {
  ## remdat
  # load dfrEmDat
  cyrd <- count(dfrEmDat,c("Country","years","Disaster.type"))
  india.yr <- cyrd[which(cyrd$Country == "India"), ]
  
  # Generate time series from points  
  ldis <- unlist(levels(dfrEmDat$Disaster.type))
  yrvec <- min(years):max(years)
  len <- length(yrvec)*length(ldis)
  yrbuild <- c()
  for (i in seq_along(yrvec)){
    yrbuild <- append(yrbuild,c(rep(yrvec[i],11)))
  }

  india.cont <- data.frame(
    Country = rep("India",len),
    years = yrbuild,
    Disaster.type = rep(ldis,len/11)
  )
  india.cont <- merge(india.cont,india.yr,by=c("Country","years","Disaster.type"),all=T)
  india.cont[is.na(india.cont)] <- 0
  
  # Plot heat map of disasters by year
  pdf(o)
    par(mar = c(4.5,4.5,1.5,1))
    ggplot(india.cont,aes(x=years))+
      geom_line(aes(y=Disaster.type,color=freq),size=1.5) +
      scale_color_distiller(palette="Spectral") +
      labs(color="freq",title="India,years") +
      ylab("disaster type") + theme_bw()
  dev.off()
  
  ## rgtd
  # load dfrGtd
  cyrat <- count(dfrGtd,c("country_txt","iyear","attacktype1_txt"))
  ind.yr <- cyrat[which(cyrat$country_txt == "India"), ]
  
  # Plot heat map of attacks by year
  pdf(o)
  ggplot(ind.yr,aes(x=iyear)) + 
    geom_line(aes(y=attacktype1_txt,color=freq),size=1) +
    scale_color_gradientn(colors=mycol(100),trans="log") +
    labs(x="year",
         y="attack type",
         title="India,years")
  dev.off()
}

#' @rdname plotKEN
plotKEN <- function(o, form='pdf') {
  ## remdat
  # load dfrEmdat
  cyrd <- count(dfrEmDat,c("Country","years","Disaster.type"))
  kenya.yr <- cyrd[which(cyrd$Country == "Kenya"), ]
  
  # Generate time series from points
  ldis <- unlist(levels(dfrEmDat$Disaster.type))
  yrvec <- min(years):max(years)
  len <- length(yrvec)*length(ldis)
  yrbuild <- c()
  for (i in seq_along(yrvec)){
    yrbuild <- append(yrbuild,c(rep(yrvec[i],11)))
  }

  kenya.cont <- data.frame(
    Country = rep("Kenya",len),
    years = yrbuild,
    Disaster.type = rep(ldis,len/11)
  )
  kenya.cont <- merge(kenya.cont,india.yr,by=c("Country","years","Disaster.type"),all=T)
  kenya.cont[is.na(kenya.cont)] <- 0
  
  # Plot heat map of disasters by year
  pdf(o)
    par(mar = c(4.5,4.5,1.5,1))
    ggplot(kenya.cont,aes(x=years))+
      geom_line(aes(y=Disaster.type,color=freq),size=1.5) +
      scale_color_distiller(palette="Spectral") +
      labs(color="freq",title="Kenya,years") +
      ylab("disaster type") + theme_bw()
  dev_off()
  
  ## rgtd
  # load dfrGtd
  cyrat <- count(dfrGtd,c("country_txt","iyear","attacktype1_txt"))
  ken.yr <- cyrat[which(cyrat$country_txt == "Kenya"), ]
  
  # Plot heat map of attacks by year
  pdf(o)
  ggplot(ken.yr,aes(x=iyear)) + 
    geom_line(aes(y=attacktype1_txt,color=freq),size=1) +
    scale_color_gradientn(colors=mycol(100),trans="log") +
    labs(x="year",
         y="attack type",
         title="Kenya,years")
  dev.off()
}

#' @rdname plotTZA
plotTZA <- function(o, form='pdf') {
  ## remdat
  # load dfrEmDat
  cyrd <- count(dfrEmDat,c("Country","years","Disaster.type"))
  tanza.yr <- cyrd[which(cyrd$Country == "Tanzania, United Republic of"), ]
  
  # Generate time series from points
  ldis <- unlist(levels(dfrEmDat$Disaster.type))
  yrvec <- min(years):max(years)
  len <- length(yrvec)*length(ldis)
  yrbuild <- c()
  for (i in seq_along(yrvec)){
    yrbuild <- append(yrbuild,c(rep(yrvec[i],11)))
  }

  tanza.cont <- data.frame(
    Country = rep("Tanzania, United Republic of",len),
    years = yrbuild,
    Disaster.type = rep(ldis,len/11)
  )
  tanza.cont <- merge(tanza.cont,tanza.yr,by=c("Country","years","Disaster.type"),all=T)
  tanza.cont[is.na(tanza.cont)] <- 0
  
  # Plot heat map of disasters by year
  pdf(o)
    par(mar = c(4.5,4.5,1.5,1))
    ggplot(tanza.cont,aes(x=years))+
      geom_line(aes(y=Disaster.type,color=freq),size=1.5) +
      scale_color_distiller(palette="Spectral") +
      labs(color="freq",title="Tanzania,years") +
      ylab("disaster type") + theme_bw()
  dev_off()

  ## rgtd
  # load dfrGtd
  cyrat <- count(dfrGtd,c("country_txt","iyear","attacktype1_txt"))
  tza.yr <- cyrat[which(cyrat$country_txt == "Tanzania"), ]
  
  # Plot heat map of attacks by year
  pdf(o)
  ggplot(tza.yr,aes(x=iyear)) + 
    geom_line(aes(y=attacktype1_txt,color=freq),size=1) +
    scale_color_gradientn(colors=mycol(100),trans="log") +
    labs(x="year",
         y="attack type",
         title="Tanzania,years")
  dev.off()
}

#' @rdname plotYEM
plotYEM <- function(o, form='pdf') {
  ## remdat
  # load dfrEmDat
  cyrd <- count(dfrEmDat,c("Country","years","Disaster.type"))
  yemen.yr <- cyrd[which(cyrd$Country == "Yemen"), ]
  
  # Generate time series from points
  ldis <- unlist(levels(dfrEmDat$Disaster.type))
  yrvec <- min(years):max(years)
  len <- length(yrvec)*length(ldis)
  yrbuild <- c()
  for (i in seq_along(yrvec)){
    yrbuild <- append(yrbuild,c(rep(yrvec[i],11)))
  }

  yemen.cont <- data.frame(
    Country = rep("Yemen",len),
    years = yrbuild,
    Disaster.type = rep(ldis,len/11)
  )
  yemen.cont <- merge(yemen.cont,yemen.yr,by=c("Country","years","Disaster.type"),all=T)
  yemen.cont[is.na(yemen.cont)] <- 0
  
  # Plot heat map of disaster by year
  pdf(o)
    par(mar = c(4.5,4.5,1.5,1))
    ggplot(yemen.cont,aes(x=years))+
      geom_line(aes(y=Disaster.type,color=freq),size=1.5) +
      scale_color_distiller(palette="Spectral") +
      labs(color="freq",title="Yemen,years") +
      ylab("disaster type") + theme_bw()
  dev.off()
  
  ## rgtd
  # load dfrGtd
  cyrat <- count(dfrGtd,c("country_txt","iyear","attacktype1_txt"))
  yem.yr <- cyrat[which(cyrat$country_txt == "Yemen"), ]
  
  # Plot heat map of attacks by year
  pdf(o)
  ggplot(yem.yr,aes(x=iyear)) + 
    geom_line(aes(y=attacktype1_txt,color=freq),size=1) +
    scale_color_gradientn(colors=mycol(100),trans="log") +
    labs(x="year",
         y="attack type",
         title="Yemen,years")
  dev.off()
}

#' @rdname plotPHL
plotPHL <- function(o, form='pdf') {
  ## remdat
  cyrd <- count(dfrEmDat,c("Country","years","Disaster.type"))
  phili.yr <- cyrd[which(cyrd$Country == "Philippines (the)"), ]
  
  # Generate time series from points
  ldis <- unlist(levels(dfrEmDat$Disaster.type))
  yrvec <- min(years):max(years)
  len <- length(yrvec)*length(ldis)
  yrbuild <- c()
  for (i in seq_along(yrvec)){
    yrbuild <- append(yrbuild,c(rep(yrvec[i],11)))
  }

  phili.cont <- data.frame(
    Country = rep("Philippines (the)",len),
    years = yrbuild,
    Disaster.type = rep(ldis,len/11)
  )
  phili.cont <- merge(phili.cont,phili.yr,by=c("Country","years","Disaster.type"),all=T)
  phili.cont[is.na(phili.cont)] <- 0
  
  # Plot heat map of disasters by year 
  pdf(o)
    par(mar = c(4.5,4.5,1.5,1))
    ggplot(phili.cont,aes(x=years))+
      geom_line(aes(y=Disaster.type,color=freq),size=1.5) +
      scale_color_distiller(palette="Spectral") +
      labs(color="freq",title="Philippines,years") +
      ylab("disaster type") + theme_bw()
  dev_off()

  ## rgtd
  cyrat <- count(dfrGtd,c("country_txt","iyear","attacktype1_txt"))
  phl.yr <- cyrat[which(cyrat$country_txt == "Philippines"), ]
  
  # Plot heat map of attacks by year
  pdf(o)
  ggplot(phl.yr,aes(x=iyear)) +
    geom_line(aes(y=attacktype1_txt,color=freq),size=1) +
    scale_color_gradientn(colors=mycol(100),trans="log") +
    labs(x="year",
         y="attack type",
         title="Philippines,years")
  dev.off()
}

#' @rdname plotNCOM
plotNCOM <- function(o, form='pdf') {
  ## rgtd
  # load dfrGtd
  usnorthcom <- dfrGtd[which(dfrGtd$region_txt == "North America"),]
  usn <- count(usnorthcom,c("country_txt","iyear","attacktype1_txt"))
  
  # Plot heatmap of attacks by year
  pdf(o)
  ggplot(usn,aes(x=iyear)) + theme_dark() +
    geom_line(aes(y=attacktype1_txt,color=freq),size=1) +
    scale_color_gradientn(colors=mycol(100),trans="log") +
    labs(x="year",
         y="attack type",
         title="USNORTHCOM,year")
  dev.off()
}

#' @rdname plotSCOM
plotSCOM <- function(o, form='pdf') {
  ## rgtd
  # load dfrGtd
  ussouthcom <- rbind.fill(
    dfrGtd[which(dfrGtd$region_txt == "Central America & Caribbean"), ],
    dfrGtd[which(dfrGtd$region_txt == "South America"), ]
  )
  uss <- count(ussouthcom,c("country_txt","iyear","attacktype1_txt"))
  
  # Plot heatmap of attack by year
  pdf(o)
  ggplot(uss,aes(x=iyear)) + theme_dark() +
    geom_line(aes(y=attacktype1_txt,color=freq),size=1) +
    scale_color_gradientn(colors=mycol(100),trans="log") +
    labs(x="year",
         y="attack type",
         title="USSOUTHCOM,year")
  dev.off()
}

#' @rdname plotACOM
plotACOM <- function(o, form='pdf') {
  ## rgtd
  # load dfrGtd
  usafricom <- dfrGtd[which(dfrGtd$region_txt == "Sub-Saharan Africa"), ]
  usa <- count(usafricom,c("country_txt","iyear","attacktype1_txt"))
  
  # Plot heatmap of attack by year
  pdf(o)
  ggplot(usa,aes(x=iyear)) + theme_dark() +
    geom_line(aes(y=attacktype1_txt,color=freq),size=1) +
    scale_color_gradientn(colors=mycol(100),trans="log") +
    labs(x="year",
         y="attack type",
         title="USAFRICOM,year")
  dev.off()
}

#' @rdname plotECOM
plotECOM <- function(o, form='pdf') {
  ## rgtd
  # load dfrGtd
  useucom <- rbind.fill(
    dfrGtd[which(dfrGtd$region_txt == "Eastern Europe"), ],
    dfrGtd[which(dfrGtd$region_txt == "Western Europe"), ]
  )
  use <- count(useucom,c("country_txt","iyear","attacktype1_txt"))
  
  # Plot heatmap of attack by year
  pdf(o)
  ggplot(use,aes(x=iyear)) + theme_dark() +
    geom_line(aes(y=attacktype1_txt,color=freq),size=1) +
    scale_color_gradientn(colors=mycol(100),trans="log") +
    labs(x="year",
         y="attack type",
         title="USEUCOM,year")
  dev.off()
}

#' @rdname plotCCOM
plotCCOM <- function(o, form='pdf') {
  ## rgtd
  # load dfrGtd
  uscentcom <- dfrGtd[which(dfrGtd$region_txt == "Middle East & North Africa"), ]
  usc <- count(uscentcom,c("country_txt","iyear","attacktype1_txt"))
  
  # Plot heatmap of attack by year
  pdf()
  ggplot(usc,aes(x=iyear)) + theme_dark() +
    geom_line(aes(y=attacktype1_txt,color=freq),size=1) +
    scale_color_gradientn(colors=mycol(100),trans="log") +
    labs(x="year",
         y="attack type",
         title="USCENTCOM,year")
  dev.off()
}

#' @rdname plotPCOM
plotPCOM <- function(o, form='pdf') {
  ## rgtd
  # load dfrGtd
  uspacom <- rbind.fill(
    dfrGtd[which(dfrGtd$region_txt == "Australasia & Oceania"), ],
    dfrGtd[which(dfrGtd$region_txt == "Central Asia"), ],
    dfrGtd[which(dfrGtd$region_txt == "East Asia"), ],
    dfrGtd[which(dfrGtd$region_txt == "South Asia"), ],
    dfrGtd[which(dfrGtd$region_txt == "Southeast Asia"), ]
  )
  usp <- count(uspacom,c("country_txt","iyear","attacktype1_txt"))
  
  # Plot heatmap of attack by year
  pdf(o)
  ggplot(usp,aes(x=iyear)) + theme_dark() +
    geom_line(aes(y=attacktype1_txt,color=freq),size=1) +
    scale_color_gradientn(colors=mycol(100),trans="log") +
    labs(x="year",
         y="attack type",
         title="USPACOM,year")
  dev.off()
}