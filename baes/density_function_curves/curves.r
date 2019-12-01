line <- function(m,A)
{
	determ<-det(A)
	
	sol<-solve(A)
	
	c <- sol[1,1]
	d <- sol[1,2]
	e <- sol[2,1]
	f <- sol[2,2]
	
	g <- m[1]
	h <- m[2]
	
	x <- seq(-2.5, 2.5, 0.1)
	y <- seq(-2.5, 2.5, 0.1)
	
	A <- c
	B <- f
	C <- e+d
	D <- -2*g*c-h*e-h*d
	E <- -2*h*f-g*e-g*d
	F <- g*g*c-g*h*e+g*h*d+h*h*f
	
	func <- function(x, y) {
	  1/(2*pi*sqrt(determ))*exp((-1/2)*(x^2*A + y^2*B + x*y*C + x*D + y*E + F))
	}
	
	z <- outer(x, y, func)
	
	contour(x, y, z)
}

line(c(0,0),matrix(c(1,0,0,0.4),2,2))

line(c(0,0),matrix(c(0.4,0,0,1),2,2))
line(c(0,0),matrix(c(1,0,1,1),2,2))
  
