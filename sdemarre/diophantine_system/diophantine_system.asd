(defsystem diophantine-system
	   :defsystem-depends-on ("maxima-file" "info-index")
	   :name "diophantine-system"
	   :maintainer "Serge De Marre"
	   :author "Serge De Marre"
	   :licence ""
	   :description "diophantine system of linear equations solver"
	   :long-description "Maxima program to solve a system of linear diophantine equations A.x=b with integer elements of A,x and b."

	   :components
	   ((:info-index "diophantine-system-index")
	    (:maxima-file "smith_normal_form")
	    (:maxima-file "diophantine_system")))
