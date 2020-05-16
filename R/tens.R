# --------------------------------------------------------------
# R Data libraries
# --------------------------------------------------------------
require(data.table, warn.conflicts=FALSE)
require(ggplot2, warn.conflicts=FALSE)
require(igraph, warn.conflicts=FALSE)
require(TSstudio, warn.conflicts=FALSE)
require(plotly, warn.conflicts=FALSE)
require(dtw, warn.conflicts=FALSE)
require(ContourFunctions, warn.conflicts=FALSE)

# --------------------------------------------------------------
# Machine specific
# --------------------------------------------------------------
hostname <- tolower(Sys.info()["nodename"])
print(hostname)
getwd()


# --------------------------------------------------------------
# Globals
# --------------------------------------------------------------

secsPerMin  <- 60
minsPerHour <- 60
hoursPerDay <- 24
minsPerDay  <- minsPerHour * hoursPerDay
daysPerWeek <- 7
minsPerWeek <- daysPerWeek * minsPerDay
accuracy    <- 2
IfactorM    <- 1000/3600


# --------------------------------------------------------------
# Read INRIX
# --------------------------------------------------------------
i <- fread("InData/inrix_225285973_1071883675_oct2018.csv")

i[, c("reference",
      "segmenttype",
      "registered",
      "speedbucket",
      "cvalue",
      "score") := NULL]

timeConvert <- function(dt) {
    dt[, travel_time_secs := traveltimeminutes * secsPerMin]
    dt[, speed := speed * IfactorM]
    dt[, average := average * IfactorM]
    return(dt)
}


inrixNorth = i[segmentid == 225285973,]
print(paste("No. of rows in INRIX North", nrow(inrixNorth)))
inrixSouth = i[segmentid == 1071883675,]
print(paste("No. of rows in INRIX South", nrow(inrixSouth)))


#inrix_ts <- as.numeric(as.POSIXct(timestamputc))

inrixNorth[, Epoch_inrix := as.numeric(as.POSIXct(timestamputc)) - min(as.numeric(as.POSIXct(timestamputc)))]

timeConvert(inrixNorth)


#inrixNorth[, travel_time_secs := traveltimeminutes * secsPerMin]
#inrixNorth[, speed := speed * IfactorM]
#inrixNorth[, average := average * IfactorM ]

inrixSouth[, Epoch_inrix := as.numeric(as.POSIXct(timestamputc)) - min(as.numeric(as.POSIXct(timestamputc)))]

#inrixSouth[, travel_time_secs := traveltimeminutes * secsPerMin]
#inrixSouth[, speed := speed * IfactorM]
#inrixSouth[, average := average * IfactorM]


# --------------------------------------------------------------
# Mcs
# --------------------------------------------------------------

m <- fread("InData/mcs_1159_1162_oct2018.csv")

mcsNorth <- m[fk_id == 1159]
print(paste("No. rows in mcs North", nrow(mcsNorth)))
mcsSouth <- m[fk_id == 1162]
print(paste("No. rows in mcs South", nrow(mcsSouth)))

mcsNorth[, Epoch_mcs := as.numeric(as.POSIXct(date)) - min(as.numeric(as.POSIXct(date)))]
mcsSouth[, Epoch_mcs := as.numeric(as.POSIXct(date)) - min(as.numeric(as.POSIXct(date)))]

mcsNorth <- mcsNorth[mcsNorth$used_lanes == "{1,1,1,1}"]
mcsSouth <- mcsSouth[mcsSouth$used_lanes == "{1,1,1,1}"]


# --------------------------------------------------------------
# Mcs + INRIX
# --------------------------------------------------------------

minElem <- which.min(
    c(nrow(mcsSouth),
      nrow(mcsNorth),
      nrow(inrixNorth),
      nrow(inrixSouth)))

minRows <- min(
    c(nrow(mcsSouth),
      nrow(mcsNorth),
      nrow(inrixNorth),
      nrow(inrixSouth)))

inrixNorth <- inrixNorth[1:minRows]	 	# North
inrixSouth <- inrixSouth[1:minRows]		# South  

mcsNorth <- mcsNorth[1:minRows]			# North
mcsSouth <- mcsSouth[1:minRows]			# South

# --------------------------------------------------------------
# Mcs Density (%)
# --------------------------------------------------------------

mcsNorth[, Density := flow / (minsPerHour * speed)]
mcsSouth[, Density := flow / (minsPerHour * speed)]

# --------------------------------------------------------------
# Write out CSV files
# --------------------------------------------------------------
write.table(
    inrixNorth,
    "OutData/inrixNorth.csv",
    row.names=F,
    quote=F,
    sep=";")

write.table(
    mcsNorth,
    "OutData/mcsNorth.csv",
    row.names=F,
    quote=F,
    sep=";")

write.table(
    inrixSouth,
    "OutData/inrixSouth.csv",
    row.names=F,
    quote=F,
    sep=";")

write.table(
    mcsSouth,
    "OutData/mcsSouth.csv",
    row.names=F,
    quote=F,
    sep=";")


# --------------------------------------------------------------
#			Make matrices
# --------------------------------------------------------------
mns <- mcsNorth$speed
mss <- mcsSouth$speed
ins <- inrixNorth$speed
iss <- inrixSouth$speed

traffic <- cbind(mns, mss, ins, iss)
trafficOneDay <- traffic[1:minsPerDay, 1:ncol(traffic)]

mns_c <- round(cor(mns, ins), accuracy)
mss_c <- round(cor(mns, iss), accuracy)
ins_c <- round(cor(mss, ins), accuracy)
iss_c <- round(cor(mss, iss), accuracy)

trafficCorrelation <- matrix(
    c(mns_c,
      mss_c,
      ins_c,
      iss_c),
    nrow=2,
    ncol=2,
    byrow=TRUE,
    dimnames = list(
        c("mcsNorthC",
          "mcsSouthC"),
        c("inrixNorthC",
          "inrixSouthC")))

print(trafficCorrelation)


# --------------------------------------------------------------
#			1. Mcs and INRIX speed plot
# --------------------------------------------------------------

mcs_speed   <- mcsNorth$speed[1:minsPerDay]
inrix_speed <- inrixNorth$speed[1:minsPerDay]

pdf("Pdf/Mcs_INRIX_speed.pdf", width=14, height=7)

Title = "24 hour speeds on Mcs 1162 \
         and INRIX 225285973 segments \
         (Monday Oct. 1st 2018, 00:00-23:46)"

plot(
    mcs_speed,
    type="l",
    ylab="Speed (km / hr)",
    main=Title,
    xaxt="n",
    lwd=2,
    xlab="Hour",
    col="black",
    cex=1.5)

lines(inrix_speed,
      type="l",
      lwd=2,
      col="red")

xtick <- seq(
    0,
    minsPerDay,
    by=minsPerHour)

axis(
    side=1,
    at=xtick,
    labels=FALSE)

text(
    x=xtick,
    par("usr")[3],
    labels=c(1:hoursPerDay),
    pos=1,
    xpd=T)

abline(
    v=xtick,
    col="grey",
    lty=2)

abline(
    v=xtick[1],
    col="blue",
    lwd=2)

abline(
    v=xtick[24],
    col="blue",
    lwd=2)

legend(
    "top",
    legend=c("mcsNorth", "inrixNorth"),
    col=c("black", "red"),
    lty=1:1,
    cex=2,
    lwd=2)

invisible(dev.off())

# --------------------------------------------------------------
#			2. 1 day speed difference
# --------------------------------------------------------------
diffs <- mcs_speed-inrix_speed
Title = "Differences in speed MCS-INRIX"


pdf("Pdf/INRIX_Mcs_speed_differences_20181001.pdf", width=14, height=7)

plot(
    diffs,
    pch=15,
    main=Title,
    xaxt="n",
    xlab="Hour",
    col="blue",
    cex.lab=1.5,
    ylab="Speed (km/hr)")

xtick <- seq(
    0,
    minsPerDay,
    by=minsPerHour)

axis(
    side=1,
    at=xtick,
    labels=FALSE)

text(
    x=xtick,
    par("usr")[3],
    labels=c(1:hoursPerDay),
    pos=1,
    xpd=T)

abline(
    v=xtick,
    col="grey",
    lty=2)

abline(
    v=xtick[1],
    col="red",
    lwd=2)

abline(
    v=xtick[24],
    col="red",
    lwd=2)

abline(
    h=0,
    col="red",
    lwd=1)

legend(
    "topleft",
    legend=c("Time differences"),
    col=c("blue"),
    cex=1.2,
    pch=15)


invisible(dev.off())
# --------------------------------------------------------------
#			3. 1 day Mcs and INRIX[average] 
# --------------------------------------------------------------
mcs_speed     <- mcsNorth$speed[1:minsPerDay]
inrix_average <- inrixNorth$average[1:minsPerDay]


pdf("Pdf/Mcs_INRIX_average_20181001.pdf", width=14, height=7)

Title = "24 hour average speeds on Mcs 1162 and INRIX 225285973 segments (2018-10-01 (Mon) 00:00-23:46)"

plot(
    mcs_speed,
    type="l",
    ylab="Speed (km / hr)",
    main=Title,
    xaxt="n",
    xlab="Hour",
    ylim=c(0, max(inrix_speed)),
    col="black")

lines(
    inrix_average,
    type="l",
    col="red")
  
xtick <- seq(
    0,
    minsPerDay,
    by=minsPerHour)

axis(
    side=1,
    at=xtick,
    labels=FALSE)

text(
    x=xtick,
    par("usr")[3],
    labels=c(1:hoursPerDay),
    pos=1,
    xpd=T)
  
abline(
    v=xtick,
    col="grey",
    lty=2)

abline(
    v=xtick[1],
    col="blue",
    lwd=2)

abline(
    v=xtick[24],
    col="blue",
    lwd=2)

legend(
    "bottomleft",
    legend=c(
        "mcsNorth",
        "inrixNorth"),
    col=c(
        "black",
        "red"),
    lty=1:1,
    cex=1.5,
    lwd=2)


invisible(dev.off())
# --------------------------------------------------------------
# 1 day speed (average) comparisons
# --------------------------------------------------------------
diffs <- mcs_speed-inrix_average

Title = "Average speed differences \
         (MCS[SPEED] - INRIX[AVERAGE] \
         2018-10-01, 00:00 to 23:46)"

pdf("Pdf/McsINRIX_average_speeds_20181001.pdf", width=14, height=7)

plot(diffs,
     pch=15,
     ylab="Speed (km/hr)",
     main=Title,
     xaxt="n",
     xlab="Hour",
     col="blue")

xtick <- seq(
    0,
    minsPerDay,
    by=minsPerHour)

axis(
    side=1,
    at=xtick,
    labels=FALSE)

text(
    x=xtick,
    par("usr")[3],
    labels=c(1:minsPerHour),
    pos=1,
    xpd=T)

abline(
    v=xtick,
    col="grey",
    lty=2)

abline(
    v=xtick[1],
    col="blue",
    lwd=2)

abline(
    v=xtick[24],
    col="blue",
    lwd=2)

abline(
    h=0,
    col="red",
    lwd=1)

legend(
    "top",
    legend=c("Diffs."),
    col="blue",
    pch=15)

invisible(dev.off())

# --------------------------------------------------------------
#			1 day speed density
# --------------------------------------------------------------
pdf("Pdf/InrixNorth_SpeedKDE.pdf", width=14, height=7)
Title="Kernel density estimates of speed distributions" 


plot(
    density(inrixNorth$speed),
    main=Title,
    xlab="Speed (km / hr)",
    col="black",
    ylim=c(0, 0.25),
    lwd=2)

lines(
    density(mcsNorth$speed),
    col="red",
    main=Title,
    xlab="Speed (km / hr)",
    lwd=3)

legend(
    "topleft",
    legend=c(
        "mcsNorth",
        "inrixNorth"),
    col=c(
        "black",
        "red"),
    lty=1:1,
    cex=2,
    lwd=3)

invisible(dev.off())

# --------------------------------------------------------------
# 7 day density
# --------------------------------------------------------------
data = mcsNorth$Density[1:minsPerWeek]

Title="7 day density Mcs North \
         (Start: Monday 1st Oct. 2018, \
         00:00 - End: Sunday 8th Oct. 2018, 13:18)"

pdf("Pdf/Mcs_density_Oct_2018.pdf", width=14, height=7)

plot(
    data,
    type="l",
    ylab="Density (% Max)",
    main=Title,
    xaxt="n",
    cex.lab=1.5,
    cex=1.5)

xtick <- seq(
    0,
    minsPerWeek,
    by=minsPerDay)

axis(
    side=1,
    at=xtick,
    labels = FALSE)

text(
    x=xtick,
    par("usr")[3],
    labels=c("Monday",
             "Tuesday",
             "Wednesday",
             "Thursday",
             "Friday",
             "Saturday",
             "Sunday"),
    xtick, 
    pos=1,
    xpd=TRUE)

abline(
    v=xtick,
    col="grey",
    lty=2)

abline(
    v=xtick[1],
    col="blue",
    lwd=2)

abline(
    v=xtick[minsPerWeek],
    col="blue",
    lwd=2)

legend(
    "topleft",
    legend=c("Density"),
    col=c("black"),
    lty=1:1,
    cex=2,
    lwd=1)


invisible(dev.off())


# --------------------------------------------------------------
# INRIX North (x) Vs. INRIX South (y)
# --------------------------------------------------------------
pdf("Pdf/INRIX_NorthSouth.pdf")

speeds <- cbind(
    inrixNorth[1:minsPerDay]$speed,
    inrixSouth[1:minsPerDay]$speed)

plot(
    speeds,
    xlab="INRIX North speed",
    ylab="INRIX South speed")

abline(
    a=1,
    b=1,
    col="red",
    lwd=2)

legend(
    "topleft",
    legend=c("Speed","Linear fit"),
    col=c(
        "black",
        "red"),
    lty=1:1,
    cex=1.5) 

invisible(dev.off())


# --------------------------------------------------------------
# Contour plots hour
# --------------------------------------------------------------
pdf("Pdf/contour_hour.pdf")


filled.contour(
    1:minsPerDay,
    1:ncol(trafficOneDay),
    trafficOneDay,
    xlab="Hour",
    axes=F)

xtick <- seq(
    1,
    minsPerDay,
    by=minsPerHour)

axis(
    side=1,
    at=xtick,
    labels=FALSE)


invisible(dev.off())
# --------------------------------------------------------------
# Log density Vs. Speed
# --------------------------------------------------------------

filename <- "logDensity_Speed_day"
format <- "pdf"
no  <- 9
x   <- "logDensity"
y   <- "Speed"
int <- "day"

SundayStart <- 6*minsPerDay
SundayEnd   <- 7*minsPerDay
MondayStart <- 1
MondayEnd   <- 1*minsPerDay


pdf("Pdf/logDensity_Speed_day.pdf")

plot(
    log(mcsNorth$Density[MondayStart:MondayEnd]),
    mcsNorth$speed[MondayStart:MondayEnd],
    col="red",
    pch=19,
    cex=1,
    ylab="Speed (km/hr)",
    xlab="Log density (veh/hr)")

points(
    log(mcsNorth$Density[SundayStart:SundayEnd]),
    mcsNorth$speed[SundayStart:SundayEnd],
    col="blue",
    pch=18,
    cex=1)

legend(
    "topright",
    legend=c("Monday", "Sunday"),
    col=c("red", "blue"),
    pch=c(19, 19),
    cex=1.5)

invisible(dev.off())

# --------------------------------------------------------------
# Four time series (speed)
# --------------------------------------------------------------

pdf("Pdf/Four_time_series.pdf", width=28, height=7)

ts.plot(
    trafficOneDay,
    col=c(rep("blue"),
          rep("black"),
          rep("red"),
          rep("green")),
    xlab="1 day in minutes (60 * 24 = 1440)",
        ylab="Speed (km/hr)",
    lwd=2)

legend(
    "bottomleft",
    legend=c("Mcs North",
             "Mcs South",
             "INRIX North",
             "INRIX South"),
    col=c("red",
          "blue",
          "green",
          "black"),
    lty=1:1,
    lwd=2,
    cex=1.5)

invisible(dev.off())


# --------------------------------------------------------------
# Four time series (averaged)
# --------------------------------------------------------------
bw <- 10

pdf("Pdf/Four_times_series_averages.pdf", width=28, height=7)

plot(
    running.mean(
        trafficOneDay[1:nrow(trafficOneDay),1],
        bw),
    main="Averaged speeds (bandwidth = 10)",
    xlab="1 day in minutes (60 * 24 = 1440))",
    ylab="Speed (km/hr)",
    type="l",
    col="blue",
    lwd=2,
    lty=1)


lines(
    running.mean(
        trafficOneDay[1:nrow(trafficOneDay), 2],
        bw),
    type="l",
    col="black",
    lwd=2,
    lty=1)

lines(
    running.mean(
        trafficOneDay[1:nrow(trafficOneDay), 3],
        bw),
    type="l",
    col="red",
    lwd=2,
    lty=1)

lines(running.mean(trafficOneDay[1:nrow(trafficOneDay), 4], bw),
      type="l",
      col="green",
      lwd=2,
      lty=1)

legend("bottomleft",
       legend=c("Mcs North",
                "Mcs South",
                "INRIX North",
                "INRIX South"),
       col=c(
           "red",
           "blue",
           "green",
           "black"),
       lty=1:1,
       lwd=2,
       cex=1.5)


invisible(dev.off())
# --------------------------------------------------------------
# Time lagged correlation
# --------------------------------------------------------------

x <- mcsNorth[1:minsPerDay, summary(speed), by=used_lanes]

lane_speeds <- as.vector(x$V1)
lane_names  <- as.character(x$used_lanes)
lane_pos    <- 1:length(x$used_lanes)


pdf("Pdf/barplot_perlane.pdf",
    width=14,
    height=14)

barplot(
    lane_speeds,
    ylab="Speed (km/hr)",
    xaxt="n",
    srt=45,
    adj=1,
    xpd=T,
    col=("#3CA0D0"),
    main="Average Speeds per lane (1 day)",
    cex.names=1.0,
    cex.lab=1.5,
    las=2,
    space=1,
    xlab="Lanes")

text(x=seq(1.5, 48.5, by=2),
    y=6,
    srt=90,
    adj=1,
    xpd=TRUE,
    labels=lane_names,
    cex=1.2)


invisible(dev.off())
# ----------------------------------------------------------------
# Density plot
# ----------------------------------------------------------------
densityNorth <- mcsNorth$Density[1:minsPerDay]
bw <- 50


pdf("Pdf/densityNorth_day.pdf",
    width=14,
    height=14)

plot(running.mean(densityNorth, binwidth=bw),
     main="Vehicles per km (2018-10-01, Monday)",
     xlab="Hours in the day",
     ylab="Density (veh / km)",
     col="red",
     type="l",
     lwd=3,
     xaxt="n",
     cex.lab=1.5,
     cex=2)

xtick <- seq(
    0,
    minsPerWeek,
    by=minsPerHour)


axis(
    side=1,
    at=xtick,
    labels=FALSE)

text(
    x=xtick,
    par("usr")[3], # find out what this does
    labels=c(1:hoursPerDay),
    pos=1,
    xpd=T)


abline(
    v=minsPerHour * 6.3,
    col="grey",
    lwd=2,
    lty=2)

abline(
    v=minsPerHour * 20.0,
    col="grey",
    lwd=2,
    lty=2)

abline(
    h=1.15,
    col="grey",
    lwd=1,
    lty=1)

abline(
    h=2.42,
    col="grey",
    lwd=1,
    lty=1)

legend(
    "bottom",
    legend=c("Density (bw 50)"),
    lty=1,
    lwd=3,
    cex=2,
    col="red")

invisible(dev.off())


# --------------------------------------------------------------
# Time lagged correlation
# --------------------------------------------------------------

mcsNorth_speed <- as.ts(trafficOneDay[,1])
inrixNorth_speed <- as.ts(trafficOneDay[,3])


# ----------------------------------------------------------------
# Time evolution (1-10)
# ----------------------------------------------------------------

pdf("Pdf/evolution_mcs_inrix10.pdf", width=14, height=14)

plot(
    main="Epoch time interval (10 mins)",	  	
    mcsNorth$Epoch_mcs[1:10],
    type="b",
    ylab="Epoch (sec)",
    col="black",
    cex.lab=1.5,
    xlab="Line number")

lines(
    inrixNorth$Epoch_inrix[1:10],
    type="b",
    col="red")


legend(
    "topleft",
    legend=c("MCS Epoch",
             "INRIX epoch"),
    col=c("black",
          "red"),
    lty=1:1,
    lwd=3)


invisible(dev.off())

# ----------------------------------------------------------------
# Time evolution (1-60)
# ----------------------------------------------------------------

pdf("Pdf/evolution_mcs_inrix60.pdf", width=14, height=14)

plot(
    main="Epoch time interval (1 hour)",	
    mcsNorth$Epoch_mcs[1:60],
    type="b",
    ylab="Epoch (sec)",
    col="black",
    cex.lab=1.5,
    xlab="Line number")

lines(
    inrixNorth$Epoch_inrix[1:60],
    type="b",
    col="red")

legend(
    "topleft",
    legend=c("MCS Epoch",
             "INRIX epoch"),
    col=c("black",
          "red"),
    lty=1:1,
    lwd=3,	  
    cex=1.5)


invisible(dev.off())

# ----------------------------------------------------------------
# Time evolution (1-1440)
# ----------------------------------------------------------------

pdf("Pdf/evolution_mcs_inrix1440.pdf", width=14, height=14)

plot(
    main="Epoch time interval (1 day)",	
    mcsNorth$Epoch_mcs[1:1440],
    type="b",
    ylab="Epoch (sec)",
    col="black",
    cex.lab=1.5,
    xlab="Line number")

lines(
    inrixNorth$Epoch_inrix[1:1440],
    type="b",
    col="red")

legend(
    "topleft",
    legend=c("MCS Epoch",
             "INRIX epoch"),
    col=c("black",
          "red"),
    lty=1:1,
    lwd=3,
    cex=1.5)


invisible(dev.off())

# ----------------------------------------------------------------
# Dynamic time warping
# ----------------------------------------------------------------

query <- mcsNorth[1:minsPerDay]$Epoch_mcs
reference <- inrixNorth[1:minsPerDay]$Epoch_inrix
alignment <-dtw(query, reference, keep=TRUE);


pdf("Pdf/dtw.pdf", width=14, height=14)
contour(query, reference, alignment$costMatrix, col=terrain.colors(100), xlab="Query (Mcs) noisy", ylab="Reference (INRIX)")
invisible(dev.off())


# ----------------------------------------------------------------
# Convert
# ----------------------------------------------------------------

system2("./Python/convertall-py")
