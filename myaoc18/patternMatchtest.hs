f :: String -> Integer
f "zero" = 0
f ('a' : xs ) = 1
f ( _ : "abb" ) = 11
f ( _ : _ : "abb" ) = 4449
f ( _ : 'd' : _) = 171
f ('b' : 'b' : _) = 8
f ('c' : _) = 12
f _ = 444