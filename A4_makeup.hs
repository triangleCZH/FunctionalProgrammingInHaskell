

--in case these are base cases==========================================
Base case : l = x:[] where q x = False
filter p . filter q l
= {l = x:[]}
filter p . filter q (x:[])
= {Definition of filter, q x = False}
filter p . filter q []
= {Definition of filter}
filter p []
= {Definition of filter}
[]
= {Definition of filter, p x && q x = p x && False = False}
filter (\x -> q x && p x) []
= {and p q x = p x && q x}
filter (\x -> and p q x) []
= {Simplification}
filter (and p q) []
= {Definition of filter, and p q x = False}
filter (and p q) (x:[])
= {l = x:[]}
filter (and p q) l

Base case: l = x:[] where p x = False, q x = True
filter p . filter q l
= {l = x:[]}
filter p . filter q (x:[])
= {Definition of filterm q x = True}
filter p . (x : filter p [])
= {Definition of filter, p x = False}
filter p (filter q [])
= {Definition of filter}
filter p []
= {Definition of filter}
[]
= {Definition of filter, q x && p x = False}
filter (\x -> q x && p x) []
= {and p q x = p x && q x}
filter (\x -> and p q x) []
= {Simplification}
filter (and p q) []
= {Definition of filter, and p q x = False}
filter (and p q) x:[]
= {l = x:[]}
filter (and p q) l

Base case: l = x:[] where p x && q x = True
filter p . filter q l
= {l = x:[]}
filter p . filter q (x:[])
= {Definition of filterm q x = True}
filter p . (x : filter p [])
= {Definition of filter, p x = True}
x : (filter p (filter q []))
= {Definition of filter}
x : (filter p [])
= {Definition of filter}
x : []
= {Definition of filter}
x : filter (and p q) []
= {Definition of filter, and p q x = p x && q x = True}
filter (and p q) (x:[])
= {l = x:[]}
filter (and p q) l