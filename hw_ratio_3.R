# 3.  Using stratified random sampling and the strata identified on pages 6--9 of
# dataset.pdf for one of the populations that have data in a file *fv.csv (except
# Mexico2012Pfv.csv), estimate the mean number of eforensics-fraudulent votes
# (variable "Nfraudmean") using variable "Votes" or ("Votes"+1)/("NValid"+1) for
# the auxiliary variable.  Use a sample size of n=1500.  Compare the bounds on
# the error of estimation achieved using combined versus separate ratio
# estimators.  Compare the bounds on the error of estimation achieved using
# stratified ratio estimation to the bounds from STSRS with $n=1500$ and from SRS
# with $n=1500$.  Note that you may choose to combine strata that have too few
# observations to be reasonable to work with.

# work with California 2006 data

dat <- read.csv("California2006GOVfv.csv", row.names=1);
names(dat);
dim(dat);
#> names(dat);
# [1] "cname"       "cnum"        "county"      "precinct"    "eftype"     
# [6] "NVoters"     "NValid"      "Votes"       "Ntfraudmean" "Nfraudmean" 
#> dim(dat);
#[1] 22820    10

N <- dim(dat)[1];
N;

n <- 1500;

### combine small strata

Ns <- xtabs(~ cname, dat);
Ns;
cset <- names(Ns)[idx <- round((1500/N) * Ns)<35];
cset;
dat$cname2 <- ifelse(dat$cname %in% cset,"combined",as.character(dat$cname));
Ns <- xtabs(~ cname2, dat);
Ns;

strata <- names(Ns);

Mux <- sum(dat$Votes)/N;
Mux;
mux <- xtabs(Votes ~ cname2, dat)/Ns;
mux;

# sample size n=1500
sum(ns <- round((1500/N) * Ns));  # proportional allocation
ns;

sstrata <- vector(mode="character");
ef <- vector(mode="numeric");
Votes <- vector(mode="numeric");
for (j in strata) {
  sstrata <- c(sstrata,rep(j,ns[j]));
  sj <- sample(Ns[j],ns[j]);
  ef <- c(ef,dat$Nfraudmean[dat$cname2==j][sj]);
  Votes <- c(Votes,dat$Votes[dat$cname2==j][sj]);
}
sdat <- data.frame(sstrata,ef,Votes);

ybar <- xtabs(ef ~ sstrata, sdat)/ns;
ybar;
xbar <- xtabs(Votes ~ sstrata, sdat)/ns;
xbar;
if (any(xbar==0)) {
  ybar <- (xtabs(ef ~ sstrata, sdat)+.5)/ns;  # adjust for zeroes in x
  print(ybar);
  xbar <- (xtabs(Votes ~ sstrata, sdat)+.5)/ns;  # adjust for zeroes in x
  print(xbar);
}

# separate ratio estimation
r <- ybar/xbar;
r;
hatmuyRS <- sum((Ns/N)*r*mux);
hatmuyRS;
sr2 <- NA*ybar;
for (j in names(ns)) {
  jidx <- sdat$sstrata==j;
  sr2[j] <- sum((ef[jidx]-r[j]*Votes[jidx])^2)/(ns[j]-1);
}
sr2;
hatvarmuyRS <- sum((Ns/N)^2*((Ns-ns)/Ns)*sr2/ns);
hatvarmuyRS;
2*sqrt(hatvarmuyRS);

# combined ratio estimation
ybarst <- sum(Ns*ybar)/N;
ybarst;
xbarst <- sum(Ns*xbar)/N;
xbarst;
rC <- ybarst/xbarst;
rC;
hatmuyRC <- (ybarst/xbarst)*Mux;
hatmuyRC;
sr2 <- NA*ybar;
for (j in names(ns)) {
  jidx <- sdat$sstrata==j;
  sr2[j] <- sum((ef[jidx]-rC*Votes[jidx])^2)/(ns[j]-1);
}
sr2;
varhatmuyRC <- sum((Ns/N)^2*((Ns-ns)/Ns)*sr2/ns);
varhatmuyRC;
2*sqrt(varhatmuyRC);

# Monte Carlo sample of samples
K <- 1000;
N <- dim(dat)[1];
n <- 1500;
Ns <- xtabs(~ cname, dat);
cset <- names(Ns)[idx <- round((1500/N) * Ns)<35];
dat$cname2 <- ifelse(dat$cname %in% cset,"combined",as.character(dat$cname));
Ns <- xtabs(~ cname2, dat);
strata <- names(Ns);
Mux <- sum(dat$Votes)/N;
mux <- xtabs(Votes ~ cname2, dat)/Ns;
# sample size n=1500
sum(ns <- round((1500/N) * Ns));  # proportional allocation
hatmuyRS <- hatvarmuyRS <- boundmuyRS <- hatmuyRC <- hatvarmuyRC <- boundmuyRC <- rep(NA,K);
ybarSRS <- varybarSRS <- SRSbound <- rep(NA,K);
for (k in 1:K) {
  sstrata <- vector(mode="character");
  ef <- vector(mode="numeric");
  Votes <- vector(mode="numeric");
  for (j in strata) {
    sstrata <- c(sstrata,rep(j,ns[j]));
    sj <- sample(Ns[j],ns[j]);
    ef <- c(ef,dat$Nfraudmean[dat$cname2==j][sj]);
    Votes <- c(Votes,dat$Votes[dat$cname2==j][sj]);
  }
  sdat <- data.frame(sstrata,ef,Votes);
  ybar <- xtabs(ef ~ sstrata, sdat)/ns;
  xbar <- xtabs(Votes ~ sstrata, sdat)/ns;
  if (any(xbar==0)) {
    ybar <- (xtabs(ef ~ sstrata, sdat)+.5)/ns;  # adjust for zeroes in x
    xbar <- (xtabs(Votes ~ sstrata, sdat)+.5)/ns;  # adjust for zeroes in x
  }
  # separate ratio estimation
  r <- ybar/xbar;
  hatmuyRS[k] <- sum((Ns/N)*r*mux);
  sr2 <- NA*ybar;
  for (j in names(ns)) {
    jidx <- sdat$sstrata==j;
    sr2[j] <- sum((ef[jidx]-r[j]*Votes[jidx])^2)/(ns[j]-1);
  }
  hatvarmuyRS[k] <- sum((Ns/N)^2*((Ns-ns)/Ns)*sr2/ns);
  boundmuyRS[k] <- 2*sqrt(hatvarmuyRS[k]);
  
  # combined ratio estimation
  ybarst <- sum(Ns*ybar)/N;
  xbarst <- sum(Ns*xbar)/N;
  rC <- ybarst/xbarst;
  hatmuyRC[k] <- (ybarst/xbarst)*Mux;
  sr2 <- NA*ybar;
  for (j in names(ns)) {
    jidx <- sdat$sstrata==j;
    sr2[j] <- sum((ef[jidx]-rC*Votes[jidx])^2)/(ns[j]-1);
  }
  varhatmuyRC[k] <- sum((Ns/N)^2*((Ns-ns)/Ns)*sr2/ns);
  boundmuyRC[k] <- 2*sqrt(varhatmuyRC[k]);

  # SRS mean
  nSRS <- sum(ns);
  ybarSRS[k] <- mean(SRSy <- dat$Nfraudmean[sample(N,nSRS)]);
  varybarSRS[k] <- var(SRSy)/nSRS * (N-nSRS)/N;
  SRSbound[k] <- 2*sqrt(varybarSRS[k]);
}
mean(hatmuyRS);
2*sqrt(var(hatmuyRS));
mean(boundmuyRS);

mean(hatmuyRC);
2*sqrt(var(hatmuyRC));
mean(boundmuyRC);

mean(ybarSRS);
2*sqrt(var(ybarSRS));
mean(SRSbound);
