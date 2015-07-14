-- :l van_der_corput.hs
add1 x = if length x == 0 then [1] 
			else do
				let y = [x !! n | n <- [1..((length x)-1)] ]
				if x !! 0 == 0 then 1:y
				else 0:(add1 y)

bins n = if n == 0 then [0] else add1 (bins (n-1))

vdc_val x = if length x == 1 then 0.5*(x!!0) 
			else 0.5 * (x !! 0) + 0.5 * vdc_val [x !! n | n <- [1..((length x)-1)] ]
			
vdc_seq = [vdc_val (bins n) | n <-[1..]]
