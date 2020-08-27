(defsystem "cl-ipfs-api2"
	   :version "0.51"
           :license "GPLv3"
	   :author "Jaidyn Ann <jadedctrl@teknik.io>"
	   :description "Bindings for the IPFS HTTP API."
	   :depends-on (:drakma :yason :arnesi :uiop)
	   :components ((:file "package")
                 	(:file "main")))
