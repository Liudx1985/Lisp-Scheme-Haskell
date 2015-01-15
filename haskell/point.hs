{-- type class --}
---------------------------------------------------------
-- shapes
data Point = Point {x, y ::Float}  deriving (Show) 
data Shape = Circle {c ::Point, r :: Float} | Rectangle {lt, rb :: Point}  deriving (Show)

-- defined a function calculate hsqrt(pt)
absPoint :: Point -> Float
absPoint pt = sqrt((x pt * x pt) + (y pt * y pt))


area :: Shape ->Float
area (Circle c rad) = pi * (rad * rad)
area Rectangle{lt = _lt, rb = _rb} = abs (x _lt - x _rb) * abs (y _lt - y _rb)


---------------------------------------------------------
-- vector define
data Vector a = Vector a a a deriving (Show, Eq) 
vplus :: (Num t) => Vector t -> Vector t -> Vector t 
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n) 
vectMult :: (Num t) => Vector t -> t -> Vector t 
(Vector i j k) `vectMult` m = Vector (i*m) (j*m) (k*m) 
scalarMult :: (Num t) => Vector t -> Vector t -> t 
(Vector i j k) `scalarMult` (Vector l m n) = i*l + j*m + k*n
---------------------------------------------------------
-- Complex define

data Complex = R{real, imag::Float} deriving (Show, Read) 

absComplex :: Complex -> Float
absComplex pt = sqrt((real pt * real pt) + (imag pt * imag pt))

add :: Complex -> Complex -> Complex
add a b = R{real = real a + real b, imag = imag a + imag b}
---------------------------------------------------------
main = do {
	print(absPoint (Point {x = 3, y = 4}))
	let c = Circle {c = Point 0 0, r = 4} in print (area c);
	let r = Rectangle {lt = Point{y = 4, x = 2}, rb = Point{y = 1, x = 1}} in print (area r);
	let (c1, c2) = (R 1 2, R 3 4) in print (add c1 c2);
	print (read "R {real = 4.0, imag = 6.0}" :: Complex) -- use Complex deriving(Show, Read)
	let (a, b) = (Vector 1 2 3, Vector 1 2 3) in print (a == b); -- use vector deriving(Eq)
}


