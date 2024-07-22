###################### allgemeine Einstellungen für die R-Teile im MSI-Buch ########################

## Allgemeine Optionen -----------------------------------------------------------------------------
options(encoding = "utf8",            # Encoding festlegen
        SweaveHooks =
        list(                           # und jede Grafik wird wie folgt formatiert
             fig = function(){
                 par(las = 1, # always horizontal axis labels
                     cex.lab = 1, # increase font height for labels
                     mar = c(4, 4.5, 0.2, 0.5)+0.1, # reduce top and right margins
                     xaxs="r", yaxs="r", # regular axis styles
                     font.lab = 1       # label font unchanged
                     )}
             ),
        myColour = FALSE,               # keine Farbe in den Grafiken
        width=60,                       # only 60 characters in R output
        continue="  ",                  # use two blanks instead of "+" for
                                        # continuation
        SweaveSyntax="SweaveSyntaxNoweb"
        )

## temporäres File für latex-Ausgaben:
latexTempFile <- tempfile ()

## Fonts -------------------------------------------------------------------------------------------
CM <- Type1Font("CM",
                file.path("Schriften", "metrics",
                          c("fcsr8a.afm", "fcsb8a.afm", "fcmri8a.afm", "fcmbi8a.afm", "cmsyase.afm"))
                ## encoding = file.path ("Schriften", "8r-mod.enc") # das funktioniert nicht!

                ## sans serif font for plain or bold text (labels, axes, ...),
                ## regular font for italic or bold-italic text (math)
                )
pdfFonts(CM = CM)
postscriptFonts(CM = CM)

ps.options (family = "CM")

## Funktionen  -------------------------------------------------------------------------------------

## Funktion zum einfachen Outsourcen von Code - funktioniert nicht mit Sweave!!!
outsource <- function ( # loads save file or creates and writes output objects to file, if it does not exist already
                       file, # character, name of the save file (without .RData-suffix
                       output,    # character vector of output object names from
                       code,        # the code chunk {wrapped in curly brackets}
                       path = "Daten"   # path of file
                       )
{
    saveFile <- file.path (path,
                           paste (file, "RData", sep = "."))

    existent <- file.exists (saveFile)
    readable <- existent && file.access (saveFile, mode = 4)

    if (!readable)
    {
        code
        save (list = output, file = saveFile)
    }
    load (saveFile, .GlobalEnv)
}

## weglassen der letzten num Elemente
allButLast <- function (vector, num = 1)
    vector[-(length (vector) - (0:(num-1)))]

## Halb-Normalverteilung, d.h. Verteilung von |Normal(mean, sd^2)| : _hnorm

rhnorm <- function (n, mean = 0, sd = 1)
{
    abs (rnorm (n = n, mean = mean, sd = sd))
}

dhnorm <- function (x, mean = 0, sd = 1, log = FALSE)
{
    val <- dnorm (x, mean = mean, sd = sd, log = FALSE) +
        dnorm (-x, mean = mean, sd = sd, log = FALSE)

    if (log)
        return (log (val))
    else
        return (val)
}

phnorm <- function (q, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
{
    val <- ifelse (q > 0,
                   pnorm (q, mean = mean, sd = sd) - pnorm (-q, mean = mean, sd = sd),
                   0)
    if (!lower.tail)
        val <- 1 - val
    if (log.p)
        return (log (val))
    else
        return (val)
}

qhnorm <- function (p, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
{
    if (log.p)
        p <- exp (p)

    target <- function (q)
        phnorm (q, mean = mean, sd = sd, lower.tail = lower.tail, log.p = FALSE) - p

    unirootObj <- uniroot (target, lower = 0, upper = 1/sqrt (.Machine$double.eps))
    unirootObj$root
}

meanHnorm <- function (mean = 0, sd = 1)
{
    tmp <- mean / sd

    val <- sqrt (2) * sd * exp (-1/2 * tmp^2) / sqrt (pi) +
        mean * (2 * pnorm (tmp) - 1)

    val
}

varHnorm <- function (mean = 0, sd = 1)
{
    val <- sd^2 + mean^2 - meanHnorm (mean = mean, sd = sd)^2
    val
}

modeHnorm <- function (mean = 0, sd = 1)
{
    if (abs (mean) <= sd){
        0
    } else {
        gradient <- function (x, mean, sd){
            val <- dnorm ((x - mean) / sd) * (mean - x) -
                dnorm ((x + mean) / sd) * (mean + x)
            val / sd^3
        }
        eps <- sqrt (.Machine$double.eps)
        optimObj <- optim (mean/2, fn = dhnorm, gr = gradient,
                           mean = mean, sd = sd,
                           method = "L-BFGS-B", lower = eps, upper = mean + eps,
                           control = list (fnscale = -1))
        stopifnot (optimObj$convergence == 0)
        optimObj$par
    }
}

## Abkürzung: mathematischer Ausdruck in font.lab-Schrift, auch mehrere Argumente sind möglich
math <- function (..., font = 3)
{
    ## unpack dot-dot-dot argument
    dots <- as.list (substitute (...[]))
    dots <- dots[2:(length (dots) - 1)]

    ## font extension
    font <- switch (font, "plain", "bold", "italic", "bolditalic")
    dots <- paste (font, "(", dots, ")")

    ## parse and build expression
    do.call ("expression", as.list (parse (text = dots)))
}


## Einzeichnen des MLE
drawml <- function(x,                   # ML-Schätzer
                   y,                   # L(ML)-Wert
                   dezi=0,              # wieviele Nachkommastellen mindestens gezeigt werden
                   digi=3,              # wieviele gültige Ziffern
                   down=F)              # ob das Label nach unten verschoben werden soll
{
    segments(x, par("usr")[3], x, y, lty="dashed", col = "black")
    if (down) { axis(side=1, at=x, font=2, labels=format(x, nsmall=dezi, digits=digi), line=1, tick=F) }
    else { axis(side=1, at=x, font=2, labels=format(x, nsmall=dezi, digits=digi)) }
}


## malt vertikale (graue, falls col == NULL) gestrichelte Linie an der Stelle x bis fun (x)
funline <- function(x,                  # Abszisse
                    fun,                # Funktion
                    col = "gray",       # Farbe der Linie
                    label = NULL,       # Characterlabel an der x-Stelle
                    dezi = 0,           # Falls label = NULL, wird Funktionswert geschrieben: siehe drawml
                    digi = 3,           # dito
                    down = FALSE,       # dito
                    ...                 # weitere Argumente für fun
                    )
{
    segments(x, par("usr")[3], x, fun(x, ...), col = col, lty = 2)
    label <- ifelse (is.null (label), format(x, nsmall=dezi, digits=digi), label)
    axis(side = 1, at = x, labels = rep (label, length.out = length (x)),
         font = 2, padj = ifelse(down, 1, 0))
    ## falls nach unten gesetzt, nachträglich tick setzen:
    ## if(down) axis(side = 1, at = x, labels = rep("", length(x)), tick = TRUE)
}

## Funktion zum Plotten der Überdeckungswahrscheinlichkeiten bei Binomialverteilung
plot.coverage <- function(confidence.intervals, # Matrix mit 2 Spalten, die die unteren
                                        # und oberen Intervallgrenzen enthaelt und Zeilenzahl n+1 hat.
                          add = FALSE,  # soll der Plot hinzugefügt werden?
                          smooth = NULL, # falls nicht NULL, wird eine lokal gemittelte Überdeckungswkeit
                                        # mit dem epsilon-Parameter smooth eingezeichnet
                          faktor = 20,  # legt die Zahl der p-Werte, für die das Coverage berechnet wird,
                                        # als n * faktor - 1 fest
                          prob = 0.95,  # nominales Konfidenzniveau
                          ...
                          )
{
                                        # zu hohe Genauigkeiten (Wilson) vermeiden
  confidence.intervals = round(confidence.intervals, 12)
  n = nrow(confidence.intervals)-1
  x = 0:n
                                        # in welcher Spalte befindet sich die untere Grenze?
  lower.ind = which.min(confidence.intervals[1,])
  upper.ind = setdiff(c(1,2), lower.ind)
                                        # pvector berechnen : ohne 0 und 1
  pvector = seq(0, 1, length = faktor*n + 1)[-c(1, faktor*n+1)]
  gerade = !(length(pvector) %% 2)
  pvector.halb = pvector[1:ceiling(length(pvector)/2)]
                                        # Überdeckungswahrscheinlichkeiten berechnen:
                                        # nur für eine Hälfte, da Symmetrie um p = 0.5
  coverage.probs = sapply(pvector.halb,
    function(p){
      sum(dbinom(x, n, p)[which(p >= confidence.intervals[,lower.ind] & p <= confidence.intervals[,upper.ind])])
    }
    )
  if(gerade)
    coverage.probs = c(coverage.probs, rev(coverage.probs))
  else
    coverage.probs = c(coverage.probs, rev(coverage.probs)[-1])
                                        # zeichnen
  if(!add){
    plot(pvector, coverage.probs, type = "l", xlab = "True probability",
         ylab = "Coverage", col = "darkgrey", ..., axes=FALSE)
    axis(2)
    axis(1, at=c(0, 0.25, 0.5, 0.75, 1), labels=as.character(c(0, NA, 0.5, NA, 1)))
    box()
    abline(h = prob, lty = 2)
  }
  else
    {
      lines(pvector, coverage.probs, type = "l", col = "darkgrey", ...)
  }
                                        # evtl. Smooth einzeichnen (vgl. Bayarri und Berger)
  if(!is.null(smooth)){
                                        # allgemeine a-Funktion
    a = function(p){
      if(p==0) NA
      else if (p==1) NA
      else
      if(p <= smooth)
        NA#(p*(1-p)*p^(-2) - 1)*p#1 - 2*smooth
      else if(p >= 1 - smooth)
        NA#(p*(1-p)*(1-p)^(-2) - 1)*p #1/smooth - 3 + 2*smooth
      else
        (p*(1-p)*smooth^(-2) - 1)*p
    }
                                        # Funktion zur Berechnung des local coverage
    local.coverage = function(p){
      ap = a(p)
      a1mp = a(1-p)
      alpha = ap + x
      beta = a1mp + n - x
      values.gamma = (lchoose(n, x)
                    + lgamma(ap + a1mp) - lgamma(ap) - lgamma(a1mp)
                    + lgamma(ap + x) + lgamma(a1mp + n - x) - lgamma(ap + a1mp + n)
                      )
      values.integral = log(
        pbeta(confidence.intervals[,upper.ind], alpha, beta) - pbeta(confidence.intervals[,lower.ind], alpha, beta)
        )
      sum(exp(values.gamma + values.integral))
    }

    # berechnen und einzeichnen:
    coverage.average = sapply(pvector.halb, local.coverage)

    if(gerade)
      coverage.average = c(coverage.average, rev(coverage.average))
    else
      coverage.average = c(coverage.average, rev(coverage.average)[-1])

    lines(pvector, coverage.average, lwd = 2)
  }
}

## relict:
show.sc <- function(number,             # Zahl
                    digits              # gültige Ziffern
                    )
{
    stop("show.sc should no longer be used, use formatSc instead")
}


## Function to compute likelihood based confidence interval, basically
## the two solutions to
##            f(\theta) = l(\theta)-l(\hat{theta)) + 1/2 dchisq(1-alpha,df=1)=0
## are found. (by Michael Höhle)
likelihood.ci <- function(alpha=0.05,   # confidence level (see Equation 2.6 in Pawitan (2003))
                          loglik,       # Loglikelihoodfunktion
                          theta.hat,    # the MLE
                          lower,        # search interval [lower,theta.hat] for f=0
                          upper,        # search interval [theta.hat,upper] for f=0
                          comp.lower = TRUE, # should the lower boundary be computed?
                          comp.upper = TRUE, # should the upper boundary be computed?
                          ...
                          )
{
  ## Highest Likelihood intervall -- target function
  f <- function(theta,...) {
    loglik(theta,...) - loglik(theta.hat,...) + 1/2*qchisq(1-alpha, df=1)
  }

  #Compute upper and lower boundary numerically, if desired only one boundary
  if(comp.lower) hl.lower <- uniroot(f,interval=c(lower,theta.hat),...)$root
  if(comp.upper) hl.upper <- uniroot(f,interval=c(theta.hat,upper),...)$root

  ret <- NULL
  if(comp.lower) ret <- c (ret, hl.lower)
  if(comp.upper) ret <- c (ret, hl.upper)

  return (ret)
}

## Compute Clopper-Pearson interval for binomial success probability.
clopperPearson <- function (x,          # number of successes
                            n,          # total number of runs
                            level = 0.95# wished (minimum) confidence level
                            )
{
    alpha <- 1 - level

    lower <- qbeta (alpha / 2, x, n - x + 1)
    upper <- qbeta (1 - alpha / 2, x + 1, n - x)

##     warnOld <- options (warn = -1)
##     alpha <- 1 - level

##     lower <- 1 + (n - x + 1) / (x * qf (alpha / 2, 2 * x, 2 * (n - x + 1)))
##     lower <- lower^(-1)
##     lower[is.nan (lower)] <- 0

##     upper <- 1 + (n - x) / ((x + 1) * qf (1 - alpha / 2, 2 * (x + 1), 2 * (n - x)))
##     upper <- upper^(-1)
##     upper[is.nan (upper)] <- 1

##     on.exit (warn = warnOld)
    return (cbind (lower, upper))
}


## Nachverarbeitung von latex-Ausgaben
postLatex <- function (latexOutput,     # latex function output
                       widthFactor = 0.7,# tabular will have width (widthFactor * \textwidth)
                       minipage = TRUE,  # use minipage environment?
                       dropColTitles = FALSE # drop column titles?
                       )
{
    ## get string, skip comment lines
    x <- scan (file = latexOutput$file, what = character (0), sep = '\n', skip = 2, quiet = TRUE)

    ## optionally drop the column titles and the \midrule command.
    ## Both follow the \toprule command.
    if(dropColTitles)
    {
        colTitleIndex <- grep("toprule", x, fixed=TRUE) + 1
        x <- x[- c(colTitleIndex, colTitleIndex + 1)]
    }
    
    ## wrap around minipage
    if (minipage){
        x[1] <- paste (x[1], " \n ", "\\begin{minipage}{", widthFactor, "\\textwidth}", sep = "")
        x[length (x)] <- paste ("\\end{minipage} \n", x[length (x)])
    }

    ## replacements
    repl <- matrix (ncol = 2, byrow = TRUE, dimnames = list (NULL, c ("pattern", "replacement")),
                    data = c (
                                        # begin tabularx environment
                    "\\\\begin{tabular}",
                    paste ("\\\\begin{tabularx}{", ifelse (minipage, "", widthFactor), "\\\\textwidth}"),
                                        # end tabularx environment
                    "\\\\end{tabular}",
                    "\\\\end{tabularx}",
                                        # rule after column headings
                    "\\\\midrule",
                    "\\\\headendrule",
                                        # rule between column heading lines
                    "\\\\cline{.*}",
                    "\\\\midrule",
                                        # superfluous linebreak
                    "\\\\\\\\[ \t]*\\\\\\\\",
                    "\\\\\\\\"
                    ))

    ## process string
    for (i in seq_len (nrow (repl))){
        x <- gsub (repl[i, "pattern"],
                  repl[i, "replacement"],
                  x, perl = TRUE)
    }

    ## output
    cat(x, sep = '\n' )
}

## Abkürzung zur Formatierung von Zahlen mit Abstand nach Tausender Einheiten
formatBig <- function (x)
{
    mark <- ifelse (log10 (x) < 4, "", "\\\\\\\\,")
    format (x, big.mark = mark, scientific=FALSE)
}

## prints the number in scientific format
formatSc <- function(x, digits=3, backslash="\\\\")
{
    ## check which are exactly zero
    isZero <- x == 0.0

    ## start processing
    x <- signif(x, digits)
    exponent <- floor(log10(abs(x)))
    mantissa <- x / (10^exponent)

    ## format the mantissa
    mantissa <- format(mantissa, digits = digits)
    mantissa <-
        ifelse(mantissa == "1",
               "",
               paste(mantissa, backslash, "cdot ",
                     sep=""))

    result <- paste(mantissa,
                    "10^{", exponent,"}",
                    sep="")

    ## set to zero which were exactly zero
    result[isZero] <- "0"

    ## and return the result
    return(result)
}

## for rounding, such that formatRound(123.00, 1) is printed as 123.0 and not
## just 123.
formatRound <- function(x, digits=0,...)
{
   format(round(x, digits=digits),nsmall=digits, ...)
}


## einfache Konvertierung von Matrix/data.frame in Latexoutput
latexMatrix <- function (matrix)
    write.table (matrix, sep = " & ", eol="\\\\\n", quote=FALSE, col.names=FALSE, row.names = FALSE)

## empirisches HPD-Intervall
empiricalHpd <- function (theta,        # sample vector of parameter
                          level         # credibility level
                          )
{
    M <- length (theta)
    thetaorder <- sort.int (theta, method = "quick")

    alpha <- 1 - level
    maxSize <- ceiling (M * alpha)
    ind <- seq_len (maxSize)

    lower <- thetaorder[ind]
    upper <- thetaorder[M - maxSize + ind]
    size <- upper - lower
    sizeMin <- which.min(size)

    HPDLower <- thetaorder[sizeMin]
    HPDUpper <- thetaorder[M - maxSize + sizeMin]
    return (c (HPDLower, HPDUpper))
}


## Pakete ------------------------------------------------------------------------------------------

# für Tabellen etc
library(Hmisc)
## if(packageVersion("Hmisc")>"3.13.0")
##     stop("postLatex() needs old version of Hmisc to compile")

# für Huberschätzer, truehist:
library(MASS)

# für zweidim. Kerndichteschätzer:
library(KernSmooth)

## für dst:
library(sn)

## für gute Konfidenzintervall-Plots
library(gplots)
library(cubature)
library(lattice)

## für Caching von Sweave
#library (cacheSweave)

## für noncentral hypergeometric distribution,
## und rdirichlet
library(MCMCpack)

## für summaryBy
library(doBy)

## für formatPval
tmp <- library("biostatUZH", logical.return = TRUE)
if(! tmp)
{
    install.packages("Latexpakete/biostatUZH_1.4-7.tar.gz",
                     repos=NULL,
                     type="source")
    library("biostatUZH")
}

##################################### Daten vorbereiten ############################################

## Darmkrebs ---------------------------------------------------------------------------------------
colonCancer <- c(NA, 37, 22, 25, 29, 34, 49)

## Nildaten ----------------------------------------------------------------------------------------
nile <- matrix(scan("data/nile.txt"),ncol=2,byrow=2)

## PBC-Überlebenszeiten ----------------------------------------------------------------------------
library(survival)
pbc <- read.table("data/pbc_ems.csv", header = TRUE)
pbc$time <- pbc$time*365.25             # Umwandlung von Jahren in Tage

write.table(x=
            subset(pbc,
                   select=c(time, d, treat)),
            file="data/pbcFull.txt",
            row.names=FALSE,
            quote=FALSE)



pbcTreat <- pbc[pbc$treat==2,]           # nur Treatment-Patienten betrachten
pbcTreat <- pbcTreat[order(pbcTreat$time),]# aufsteigend nach ÜZeiten sortieren
pbcTreatSurv <- Surv(pbcTreat$time,pbcTreat$d) # d ist death-Variable, d.h. 1 heißt
                                        # beobachtet, 0 zensiert.
pbcTreat.nObs <- nrow(pbcTreat)
write.table(x=
            subset(pbcTreat,
                   select=c(time, d)),
            file="data/pbcTreat.txt",
            row.names=FALSE,
            quote=FALSE)

uncensored <- subset (pbcTreat, d == 1)   # tatsächliche Üzeiten in Treatmentgruppe
unc <- uncensored$time
uncN <- length (unc)
uncMean <- mean (unc)
uncSd <- sd (unc)
uncSum <- sum (unc)

## Fischstudie -------------------------------------------------------------------------------------
fischstudie <- read.table ("data/fischdaten")
fischstudie["148", "chol28"] <- fischstudie["148", "chol15"] # LVCF
fischstudie$diff28to0 <- with (fischstudie, chol0 - chol28)
ueberKeinFisch <- subset (fischstudie, fisch == 0 & kalorie == 1)
ueberMitFisch <- subset (fischstudie, fisch == 1 & kalorie == 1)
normalKeinFisch <- subset (fischstudie, fisch == 0 & kalorie == 0)
normalMitFisch <- subset (fischstudie, fisch == 1 & kalorie == 0)
## nrow (ueberKeinFisch) == 50                  # d.h. kalorie = 1 bedeutet übergewichtig
## mean (ueberKeinFisch$diff28to0)         # passt

## Lippenkrebs in Schottland -----------------------------------------------------------------------

scotlandCoordinates <-  matrix (scan ("data/scotland/scotkoord.txt"),
                                ncol = 3, byrow = T)
scotlandData <- read.table ("data/scotland/scotland.txt")
colnames (scotlandData) <- c("x", "e", "V3")
write.table(scotlandData[, -3],
            file="data/scotland/scotland-nice.txt",
            quote=FALSE,
            row.names=FALSE,
            col.names=TRUE)

## Präeklampsie ------------------------------------------------------------------------------------

preeclampsia <- read.table ("data/preeclampsia.txt", header = TRUE)

  # Funktion outerdens liefert für den Wert h die Wahrscheinlichkeit aller
  # Werte pi, deren Beta-Dichte (mit Parametern p1 und p2) kleiner als h ist.
  outerdens= function(h, p1, p2){
    modus = (p1-1)/(p1 + p2 - 2)
      if(x==0)
        schnitt.l <- 0
      else
        schnitt.l = uniroot(function(x){dbeta(x, p1, p2) - h}, interval = c(0, modus))$root
      if(x==n)
        schnitt.u <- 1
      else
        schnitt.u = uniroot(function(x){dbeta(x, p1, p2) - h}, interval = c(modus, 1))$root
    return(c(pbeta(schnitt.l, p1, p2) + pbeta(schnitt.u, p1, p2, lower.tail = FALSE), schnitt.l, schnitt.u))
  }

## Blood alcohol content --------------------------------------------------

iten <- read.table("data/TBS.Total_Neu.csv", header=TRUE)
iten$tf500a1 <- iten$BAK / iten$L_500a_1 * 2100
