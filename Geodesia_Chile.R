# Definir formato de numero decimales
options(digits = 10)
decimal=TRUE

# Elipsoide

names_elip <- c('PSAD-56', 'SAD-69',	'WGS-84',	'GRS-80 (SIRGAS)')
a <-	c(6378388,	6378160,	6378137,	6378137)
divF <- c(297,	298.25,	298.257223563,	298.257222101)

# Funtions

f <- function(x) {
  1/x
}

b <- function(x,y) {
  x-1/y*x
}

e <- function(x,y) {
  1-((x-1/y*x)^2/x^2)
}

e2 <- function(x,y) {
  (x^2/(x-1/y*x)^2)-1
}

Elipsoide <- as.data.frame(cbind(names_elip, a, divF, f(divF), 
                                 b(a,divF), e(a,divF),e2(a,divF)))
names(Elipsoide) <- c("ELIPSOIDES", "a", "1/f", "f", "b", "e^2", "e´^2")

# Separacion de cada Elipsoide
#PSAD56 <- Elipsoide[1,1:7]
#SAD69 <- Elipsoide[2,1:7]
#WGS84 <- Elipsoide[3,1:7]
#GRS80 <- Elipsoide[4,1:7]

print(Elipsoide)

# Lat

g <- -33
m <- 38
s <- 30.123456

# Long

g1 <- -70
m1 <- 41
s1 <- 35.123456

sexagesimal <- function(x,y,z){
  x-(y/60)-(z/3600)
}

radianes <- function(x,y,z){
  (x-(y/60)-(z/3600))*pi/180
}

sexagesimal(g,m,s)
radianes(g,m,s)

#  Radius of curvature SIRGAS
print(Elipsoide[4, 6])

#    (1-e^2)
# 1 = 'PSAD-56', 2 = 'SAD-69',	3 = 'WGS-84',	4 ='GRS-80 (SIRGAS)'
E2 <- function(x){
  1 - as.numeric(Elipsoide[x,6])
}

# se usan "a" y "1/f" desde Elipsoide seleccionado
E2(4)

#    1-e^2*sen(lat)^2
# 1 = 'PSAD-56', 2 = 'SAD-69',	3 = 'WGS-84',	4 ='GRS-80 (SIRGAS)'
E3 <- function(x,y){
  (1 - as.numeric(Elipsoide[x,6])*sin(y)^2)
}

# Valor en radianes
rad <- radianes(g,m,s)

E3(4,rad)

# Valor de M
# 1 = 'PSAD-56', 2 = 'SAD-69',	3 = 'WGS-84',	4 ='GRS-80 (SIRGAS)'
M <- function(x,y){
  as.numeric(Elipsoide[x,2])*(1 - as.numeric(Elipsoide[x,6]))/(1 - as.numeric(Elipsoide[x,6])*sin(y)^2)^(3/2)
}

M(4,rad)

# Valor de N

N <- function(x,y){
  as.numeric(Elipsoide[x,2])/sqrt(1 - as.numeric(Elipsoide[x,6])*sin(y)^2)
}

N(4,rad)

## ARCOS MERIDIANOS Y PARALELOS

r <- function(x,y){
  x*cos(y)
}

# Lat

g <- -33
m <- 30
s <- 0

# Valor en radianes
rad <- radianes(g,m,s)
nn <- N(4,rad)

r(nn,rad)
