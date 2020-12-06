#library("devtools")
#library("roxygen2")
#devtools::install_github("r-lib/devtools")
#devtools::load_all(path = '/home/diego/GITHUP_REPO/Geodesy_Chile')

# Define decimal number format
options(digits = 10)
decimal=TRUE

# Ellipsoids
names_elip <- c('PSAD-56', 'SAD-69',	'WGS-84',	'GRS-80 (SIRGAS)')
a <-	c(6378388,	6378160,	6378137,	6378137)
divF <- c(297,	298.25,	298.257223563,	298.257222101)
PO <- 180/pi
Sin_1 <- pi/(180*3600)

Elipsoide <- as.data.frame(cbind(names_elip, a, divF, 1/divF,
                                 a-1/divF*a, 
                                 1-((a-1/divF*a)^2/a^2),
                                 (a^2/(a-1/divF*a)^2)-1,
                                 1+3/4*(1-((a-1/divF*a)^2/a^2))+45/64*(1-((a-1/divF*a)^2/a^2))^2+175/256*(1-((a-1/divF*a)^2/a^2))^3+11025/16384*(1-((a-1/divF*a)^2/a^2))^4+43659/65536*(1-((a-1/divF*a)^2/a^2))^5,
                                 3/4*(1-((a-1/divF*a)^2/a^2))+15/16*(1-((a-1/divF*a)^2/a^2))^2+525/512*(1-((a-1/divF*a)^2/a^2))^3+2205/2048*(1-((a-1/divF*a)^2/a^2))^4+72765/65536*(1-((a-1/divF*a)^2/a^2))^5,
                                 15/64*(1-((a-1/divF*a)^2/a^2))^2+105/256*(1-((a-1/divF*a)^2/a^2))^3+2205/4096*(1-((a-1/divF*a)^2/a^2))^4+10395/16384*(1-((a-1/divF*a)^2/a^2))^5,
                                 35/512*(1-((a-1/divF*a)^2/a^2))^3+315/2048*(1-((a-1/divF*a)^2/a^2))^4+31185/131072*(1-((a-1/divF*a)^2/a^2))^5,
                                 315/16384*(1-((a-1/divF*a)^2/a^2))^4+3465/65536*(1-((a-1/divF*a)^2/a^2))^5,
                                 639/131072*(1-((a-1/divF*a)^2/a^2))^5,
                                 (1+3/4*(1-((a-1/divF*a)^2/a^2))+45/64*(1-((a-1/divF*a)^2/a^2))^2+175/256*(1-((a-1/divF*a)^2/a^2))^3+11025/16384*(1-((a-1/divF*a)^2/a^2))^4+43659/65536*(1-((a-1/divF*a)^2/a^2))^5)*a*(1-(1-((a-1/divF*a)^2/a^2)))/PO,
                                 (3/4*(1-((a-1/divF*a)^2/a^2))+15/16*(1-((a-1/divF*a)^2/a^2))^2+525/512*(1-((a-1/divF*a)^2/a^2))^3+2205/2048*(1-((a-1/divF*a)^2/a^2))^4+72765/65536*(1-((a-1/divF*a)^2/a^2))^5)*a*(1-(1-((a-1/divF*a)^2/a^2)))/2,
                                 (15/64*(1-((a-1/divF*a)^2/a^2))^2+105/256*(1-((a-1/divF*a)^2/a^2))^3+2205/4096*(1-((a-1/divF*a)^2/a^2))^4+10395/16384*(1-((a-1/divF*a)^2/a^2))^5)*a*(1-(1-((a-1/divF*a)^2/a^2)))/4,
                                 (35/512*(1-((a-1/divF*a)^2/a^2))^3+315/2048*(1-((a-1/divF*a)^2/a^2))^4+31185/131072*(1-((a-1/divF*a)^2/a^2))^5)*a*(1-(1-((a-1/divF*a)^2/a^2)))/6,
                                 (315/16384*(1-((a-1/divF*a)^2/a^2))^4+3465/65536*(1-((a-1/divF*a)^2/a^2))^5)*a*(1-(1-((a-1/divF*a)^2/a^2)))/8,
                                 (639/131072*(1-((a-1/divF*a)^2/a^2))^5)*a*(1-(1-((a-1/divF*a)^2/a^2)))/10
                                 ))
names(Elipsoide) <- c("ELIPSOIDES", "a", "1/f", "f", "b", "e^2", "e´^2", 
                      "A", "B", "C", "D", "E", "F",
                      "Alfa", "Beta", "Gamma", "Delta", "Epsilon", "Zeta")

# Funtions
# do not reproduce
#f <- function(x) {
#  1/x
#}

#b <- function(x,y) {
#  x-1/y*x
#}

#e <- function(x,y) {
#  1-((x-1/y*x)^2/x^2)
#}

#e2 <- function(x,y) {
#  (x^2/(x-1/y*x)^2)-1
#}

#Elipsoide <- as.data.frame(cbind(names_elip, a, divF, f(divF), 
#                                 b(a,divF), e(a,divF),e2(a,divF)))
#names(Elipsoide) <- c("ELIPSOIDES", "a", "1/f", "f", "b", "e^2", "e´^2")

# Separation test of each Ellipsoid
# do not reproduce
#PSAD56 <- Elipsoide[1,1:7]
#SAD69 <- Elipsoide[2,1:7]
#WGS84 <- Elipsoide[3,1:7]
#GRS80 <- Elipsoide[4,1:7]

print(Elipsoide)

##
# Test data
# Lat
g <- -33
m <- 38
s <- 30.123456

# Long
g1 <- -70
m1 <- 41
s1 <- 35.123456
##

sexagesimal <- function(x,y,z){
  x-(y/60)-(z/3600)
}

radians <- function(x,y,z){
  (x-(y/60)-(z/3600))*pi/180
}

sexagesimal(g,m,s)
radians(g,m,s)

#  Radius of curvature SIRGAS
print(Elipsoide[4, 6])

#    (1-e^2)
# 1 = 'PSAD-56', 2 = 'SAD-69',	3 = 'WGS-84',	4 ='GRS-80 (SIRGAS)'
E2 <- function(x){
  1 - as.numeric(Elipsoide[x,6])
}

# "a" and "1 / f" are used from selected Ellipsoid
E2(4)

#    1-e^2*sen(lat)^2
# 1 = 'PSAD-56', 2 = 'SAD-69',	3 = 'WGS-84',	4 ='GRS-80 (SIRGAS)'
E3 <- function(x,y){
  (1 - as.numeric(Elipsoide[x,6])*sin(y)^2)
}

# Value in radians
rad <- radians(g,m,s)

E3(4,rad)

# Value of M
# 1 = 'PSAD-56', 2 = 'SAD-69',	3 = 'WGS-84',	4 ='GRS-80 (SIRGAS)'
M <- function(x,y){
  as.numeric(Elipsoide[x,2])*(1 - as.numeric(Elipsoide[x,6]))/(1 - as.numeric(Elipsoide[x,6])*sin(y)^2)^(3/2)
}

M(4,rad)

# Value of N
# 1 = 'PSAD-56', 2 = 'SAD-69',	3 = 'WGS-84',	4 ='GRS-80 (SIRGAS)'
N <- function(x,y){
  as.numeric(Elipsoide[x,2])/sqrt(1 - as.numeric(Elipsoide[x,6])*sin(y)^2)
}

N(4,rad)

## MERIDIAN AND PARALLEL ARCHES
# 1 = 'PSAD-56', 2 = 'SAD-69',	3 = 'WGS-84',	4 ='GRS-80 (SIRGAS)'
r <- function(x,y){
  as.numeric(Elipsoide[x,2])/sqrt(1 - as.numeric(Elipsoide[x,6])*sin(y)^2)*cos(y)
}

##
# Test data
# Lat
g <- -33
m <- 30
s <- 0

# Value in radians
rad <- radians(g,m,s)
##

r(4,rad)

S <- function(x,y){
  (as.numeric(Elipsoide[x,2])*(1 - as.numeric(Elipsoide[x,6]))/(1 - as.numeric(Elipsoide[x,6])*sin(y)^2)^(3/2))*y
}

S(4,rad)

L <- function(x,y){
  (as.numeric(Elipsoide[x,2])/sqrt(1 - as.numeric(Elipsoide[x,6])*sin(y)^2)*cos(y))*y
}

L(4,rad)

# CALCULATION OF DIFFERENCES IN LATITUDE AND LENGTH
# Generate formulas of these

##
# Test data
# Lat
g <- -33
m <- 12
s <- 27.11457

# Value in radians
rad_lat <- radians(g,m,s)

# Lon
g <- -71
m <- 18
s <- 44.86475

# Value in radians
rad_lon <- radians(g,m,s)

# ELLIPSOIDAL HEIGHT (h)
h <- 31.885
##

# GEODETIC TO CARTESIAN
# Letter a <- 1 = 'PSAD-56', 2 = 'SAD-69',	3 = 'WGS-84',	4 ='GRS-80 (SIRGAS)'
# Letter b <- ELLIPSOIDAL HEIGHT (h)
# Letter c <- rad Latitude
# Letter d <- rad longitude
cartesian <- function(a,b,c,d){
  valueX <- (as.numeric(Elipsoide[a,2])/sqrt(1 - as.numeric(Elipsoide[a,6])*sin(c)^2)+b)*cos(c)*cos(d)
  valueY <- (as.numeric(Elipsoide[a,2])/sqrt(1 - as.numeric(Elipsoide[a,6])*sin(c)^2)+b)*cos(c)*sin(d)
  valueZ <- ((as.numeric(Elipsoide[a,2])/sqrt(1 - as.numeric(Elipsoide[a,6])*sin(c)^2))*(1 - as.numeric(Elipsoide[a,6]))+b)*sin(c)
  values <- data.frame(valueX, valueY, valueZ)
  names(values) <- c("X", "Y", "Z")
  return(values)
}

cartesian(4,h, rad_lat, rad_lon)

# TRANSFORMATION FROM CARTESIAN TO GEODETIC
# AUXILIARY

##
# Test data
x <- 1711591.78090565
y <- -5060304.1659587
z <- -3473256.69328603
##

d <- function(x,y){
  sqrt(x^2+y^2)
}

d(x,y)

sata <- function(a,x,y,z){
  atan((as.numeric(Elipsoide[a,2])*z)/(as.numeric(Elipsoide[a,5])*sqrt(x^2+y^2)))
}

sata(4,x,y,z)

Latitude <- function(a,x,y,z){
  atan((z+as.numeric(Elipsoide[a,5])*as.numeric(Elipsoide[a,7])*sin(atan((as.numeric(Elipsoide[a,2])*z)/(as.numeric(Elipsoide[a,5])*sqrt(x^2+y^2))))^3)/((sqrt(x^2+y^2))-as.numeric(Elipsoide[a,2])*as.numeric(Elipsoide[a,6])*cos(atan((as.numeric(Elipsoide[a,2])*z)/(as.numeric(Elipsoide[a,5])*sqrt(x^2+y^2))))^3))
}

Latitude(4,x,y,z)

Longitude <- function(x,y){
  atan(y/x)
}

Longitude(x,y)

Height <- function(a,x,y,z){
  d <- sqrt(x^2+y^2)
  lat <- atan((z+as.numeric(Elipsoide[a,5])*as.numeric(Elipsoide[a,7])*sin(atan((as.numeric(Elipsoide[a,2])*z)/(as.numeric(Elipsoide[a,5])*sqrt(x^2+y^2))))^3)/((sqrt(x^2+y^2))-as.numeric(Elipsoide[a,2])*as.numeric(Elipsoide[a,6])*cos(atan((as.numeric(Elipsoide[a,2])*z)/(as.numeric(Elipsoide[a,5])*sqrt(x^2+y^2))))^3))
  N <- as.numeric(Elipsoide[a,2])/sqrt(1 - as.numeric(Elipsoide[a,6])*sin(lat)^2)
  H <- (d/cos(lat))-N
  return(H)
}

Height(4,x,y,z)

Lat_lon_height <- function(a,x,y,z){
  d <- sqrt(x^2+y^2)
  lon <- atan(y/x)
  lat <- atan((z+as.numeric(Elipsoide[a,5])*as.numeric(Elipsoide[a,7])*sin(atan((as.numeric(Elipsoide[a,2])*z)/(as.numeric(Elipsoide[a,5])*sqrt(x^2+y^2))))^3)/((sqrt(x^2+y^2))-as.numeric(Elipsoide[a,2])*as.numeric(Elipsoide[a,6])*cos(atan((as.numeric(Elipsoide[a,2])*z)/(as.numeric(Elipsoide[a,5])*sqrt(x^2+y^2))))^3))
  N <- as.numeric(Elipsoide[a,2])/sqrt(1 - as.numeric(Elipsoide[a,6])*sin(lat)^2)
  H <- (d/cos(lat))-N
  values <- data.frame(as.numeric(lat), as.numeric(lon), as.numeric(H))
  names(values) <- c("lat", "lon", "H")
  return(values)
}

Lat_lon_height(4,x,y,z)

# ELECTRONIC DISTANCE REDUCTION
# TODO

# REDUCTION OF HORIZONTAL DISTANCE TO THE ELLIPSOID (GEODESIC DISTANCE)
# Approximate radius of the Earth
# If h is constant, then (R + h) / R is also constant
# (R + h) / R = scale factor due to height kh
# Test data
R	<- 63780000
h <- 2500
Dhz <- 728.5

# Geodetic distance
Geodis <- function(x,y){
  A <- (63780000+x)/63780000
  B <- y/((63780000+x)/63780000)
  values <- data.frame(as.numeric(A), as.numeric(B))
  names(values) <- c("Kh(h)", "GEODESIC DISTANCE")
  return(values)
}

Geodis(h,Dhz)

# REDUCING THE ROPE TO THE ELLIPTICAL ARCH
# Test data
ROPE <- 50000

ARCH <- function(x){
  value1 <- (x^3)/(24*63780000*63780000)
  value2 <- value1*1000000
  values <- data.frame(as.numeric(value1), as.numeric(value2))
  names(values) <- c("ARCO (S)", "PPM")
  return(values)
}

ARCH(ROPE)

# CALCULATION OF SCALE FACTOR
# The origin of the distances is the ellipsoid:
# Geodesic dist * Scale factor K = Mapping distance (UTM)
# Test data
# Pichilemu EAST = 224200
EAST <- 224200
arch_data <- Geodis(h,Dhz)

SCALE_FACTOR <- function(x, y){
  valueX <- 500000-x
  K_UTM <- 0.9996*(1+(valueX^2/(2*6378000^2)))
  D_UTM <- y*K_UTM
  DIF_DS <- D_UTM-y
  PPM <- (DIF_DS/D_UTM)*1000000
  values <- data.frame(as.numeric(valueX), as.numeric(K_UTM), as.numeric(D_UTM), 
                       as.numeric(DIF_DS), as.numeric(PPM))
  names(values) <- c("X", "K UTM","D UTM", "DIF D-S", "PPM")
  return(values)
}
SCALE_FACTOR(EAST, arch_data)

# TRANSFORMATION OF GEODETIC COORDINATES TO TM

##
# Test data
# PARAMETERS
MC <- -69.00000
SC_FACTOR_Ko <- 0.99960
EF <- 500000.00000
NF <- 10000000.00000

# Lat
g <- -33
m <- 12
s <- 27.11457

# Value in radians
sexa_lat <- sexagesimal(g,m,s)
#rad_lat <- radians(g,m,s)

# Lon
g <- -71
m <- 18
s <- 44.86475

# Value in radians
sexa_lon <- sexagesimal(g,m,s)
#rad_lon <- radians(g,m,s)

# ELLIPSOIDAL HEIGHT (h)
h <- 31.885
##

# Letter a <- 1 = 'PSAD-56', 2 = 'SAD-69',	3 = 'WGS-84',	4 ='GRS-80 (SIRGAS)'
# Letter b <- sexagesimal latitude
# Letter c <- sexagesimal longitude
# Letter d <- MC
# Letter e <- SCALE FACTOR Ko
# Letter f <- EAST False (EF)
# Letter g <- North False (NF)
TO_TM <- function(a,b,c,d,e,f,g){
  N <- as.numeric(Elipsoide[a,2])/sqrt(1-as.numeric(Elipsoide[a,6])*sin(b*pi/180)^2)
  DELTA_LAMBA <- (c-d)*3600
  a1 <- as.numeric(Elipsoide[a,14])*b
  b1 <- as.numeric(Elipsoide[a,15])*sin(2*(b*pi/180))
  c1 <- as.numeric(Elipsoide[a,16])*sin(4*(b*pi/180))
  d1 <- as.numeric(Elipsoide[a,17])*sin(6*(b*pi/180))
  e1 <- as.numeric(Elipsoide[a,18])*sin(8*(b*pi/180))
  f1 <- as.numeric(Elipsoide[a,19])*sin(10*(b*pi/180))
  Be <- a1-b1+c1-d1+e1-f1
  t <- tan(b*pi/180)
  n <- sqrt(as.numeric(Elipsoide[a,7]))*cos(b*pi/180)
  N1 <- (1/2)*(DELTA_LAMBA^2)*N*sin(b*pi/180)*cos(b*pi/180)*(Sin_1^2)
  N2 <- (1/24)*(DELTA_LAMBA^4)*N*sin(b*pi/180)*(cos(b*pi/180)^3)*(Sin_1^4)*(5-(t^2)+(9*n^2)+(4*(n^4)))
  N3 <- (1/720)*(DELTA_LAMBA^6)*N*sin(b*pi/180)*(cos(b*pi/180)^5)*(Sin_1^6)*(61-(58*(t^2))+(720*(n^2))-(350*(t^2)*(n^2)))
  Y <- e*(N1+N2+N3+Be)
  North <- Y+g
  E1 <- DELTA_LAMBA*N*cos(b*pi/180)*Sin_1
  E2 <- (1/6)*(DELTA_LAMBA^3)*N*(cos(b*pi/180)^3)*(Sin_1^3)*(1-(t^2)+(n^2))
  E3 <- (1/120)*(DELTA_LAMBA^5)*N*(cos(b*pi/180)^5)*(Sin_1^5)*(5-(18*(t^2))+(t^4)+(14*(n^2))-(58*(t^2)*(n^2)))
  X <- e*(E1+E2+E3)
  East <- X+f
  values <- data.frame(as.numeric(East), as.numeric(North), as.numeric(X), as.numeric(Y))
  names(values) <- c("East", "North", "X", "Y")
  return(values)
}

# Review the final result
TO_TM(4, sexa_lat, sexa_lon, MC, SC_FACTOR_Ko, EF, NF)
