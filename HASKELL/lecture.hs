--Variable Declaration
x = 17 

--Functions
addsix x = x + 6
square x = x * x

factorial 0 = 1
factorial x = x * factorial(x-1)

--Guards
relu n
	| n < 0		= 0
	| otherwise = n

add x y = x + y

addten x = add 10 x

--Pairs

vector = (2.5, 3.5)

total (x1, y1) (x2, y2) = x1+x2+y1+y2
