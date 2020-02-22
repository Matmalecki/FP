module Lab1 where 

gcd1 a 0 = a
gcd1 a b = gcd1 b (mod a b)

lcm1 a b = div (a * b) (gcd a b)


-- zad 3
smaller x y = x < y

greater x y = y < x

equal x y = (&&) (not (smaller x y))  (not (greater x y))

smaller_equal x y = not (greater x y)

greater_equal x y = not (smaller x y)

not_equal x y = not (equal x y)


--zad4

even1 0 = True
even1 n = odd1 (n-1)

odd1 0 = False
odd1 n = even1 (n-1)

-- zad5

plus x y = x + y
times x y = x * y


same_values p1 p2 x y = equal (p1 x y) (p2 x y)

--zad6

delta a b c = (b*b) -(4*a*c)

kwad_str a b c = case (delta a b c) of
                0 -> show (-b / (2 * a))
                d -> if d < 0 then "Brak miejsc zerowych"
                    else (show (-b + sqrt(d) / 2 * a) ++ " " ++ show (-b - sqrt(d) / 2 * a))


kwad_lst a b c = case (delta a b c) of
                0 -> [(-b / (2 * a))]
                d -> case (smaller d 0) of
                    True -> []
                    False -> [(-b + sqrt(d) / 2 * a) ,(-b - sqrt(d) / 2 * a)]
