module Note where

pitch :: Int -> Int -> Int
pitch nt oct = nt+oct*12

c,d,e,f,g,a,b :: Int -> Int
c = pitch  0
d = pitch  2
e = pitch  4
f = pitch  5
g = pitch  7
a = pitch  9
b = pitch 11

(#) :: (a -> Int) -> a -> Int
noteFunc # oct = noteFunc oct + 1


pitches30 :: [Int]
pitches30 =
   c 0 : d 0 : g 0 : a 0 : b 0 :
   c 1 : d 1 : e 1 : f 1 : f#1 : g 1 : g#1 : a 1 : a#1 : b 1 :
   c 2 : c#2 : d 2 : d#2 : e 2 : f 2 : f#2 : g 2 : g#2 : a 2 : a#2 : b 2 :
   c 3 : d 3 : e 3 :
   []
