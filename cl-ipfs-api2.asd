(defsystem "cl-ipfs-api2"
	   :version "0.1"
	   :author "Jaidyn Ann <jadedctrl@teknik.io>"
           :license "AGPLv3"
	   :depends-on (:drakma :yason :arnesi :uiop)
	   :components ((:file "package")
                 	(:file "main")))
