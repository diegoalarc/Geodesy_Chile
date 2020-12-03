# Elipsoide

names_elip <- c('PSAD-56',	'SAD-69',	'WGS-84',	'GRS-80 (SIRGAS)')
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

Elipsoide <- as.data.frame(cbind(names_elip, a, divF, f(divF), b(a,divF), e(a,divF),e2(a,divF)))
names(Elipsoide) <- c("ELIPSOIDES", "a", "1/f", "f", "b", "e^2", "e´^2")

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
E2 <- function(x){
  1 - x
}

#    1-e^2*sen(lat)^2
E3 <- (1 - 0.006694380022900*sin(-0.5871584)^2)

M <- 