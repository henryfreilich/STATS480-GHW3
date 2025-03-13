# 1.  Using simple random sampling for one of the populations that have data in a
# file *fv.csv (except Mexico2012Pfv.csv), estimate the ratio of
# eforensics-fraudulent votes (variable "Nfraudmean") to the number of leader
# votes (variable "Votes") using a sample size of n=1500.  Also compute SRS ratio
# estimates designed to achieve a bound of B=.01, evaluating what bound on the
# error of estimation your design actually achieves (when designing the sample
# you may treat population variables "NVoters", "NValid" and "Votes" as known).

# work with California 2006 data

dat <- read.csv("Desktop/Stats 480/Data/California2006GOVfv.csv", row.names=1);
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

mux <- sum(dat$Votes)/N;
mux;

# sample
set.seed(426)
s <- sample(N,n);
y <- dat$Nfraudmean[s];
x <- dat$Votes[s];

# ratio
xbar <- sum(x)/n;
xbar;
ybar <- sum(y)/n;
ybar;
r <- sum(y)/sum(x);
r;
sr2 <- sum((y-r*x)^2)/(n-1);
sr2;
hatvarr <- sr2/n*(1/mux^2)*((N-n)/N);
hatvarr;
2*sqrt(hatvarr);

# sample size for B=.01
sigma2 <- var(.1*dat$Votes);  # guess for variance of Nfraudmean
sigma2;
B <- .01;
D <- B^2*mux^2/4;
D;
nB <- ceiling(N*sigma2/(N*D+sigma2));  # sample size
nB;

# sample using nB;
set.seed(197)
sB <- sample(N,nB);
y <- dat$Nfraudmean[sB];
x <- dat$Votes[sB];

# ratio
rB <- sum(y)/sum(x);
rB;
sr2B <- sum((y-rB*x)^2)/(nB-1);
sr2B;
hatvarrB <- sr2B/nB*(1/mux^2)*((N-nB)/N);
hatvarrB;
2*sqrt(hatvarrB);

2*sqrt(hatvarr);
