(fn [f & maps]
  (reduce 
  	(fn [m1 m2]
		(reduce 
		(fn [m [k v]]
		    (assoc m k
		      (if (contains? m k) 
		        (f (m k) v) 
		        v))) 
	  	m1 m2)) 
  {} maps))