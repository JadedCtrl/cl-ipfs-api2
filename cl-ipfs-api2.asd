(defsystem "cl-ipfs-api2"
	   :version "0.51"
           :license "LGPLv3"
	   :author "Jaidyn Ann <jadedctrl@teknik.io>"
	   :description "Bindings for the IPFS HTTP API."
	   :depends-on (:alexandria :drakma :yason :arnesi :uiop)
	   :components ((:file "package")
                 	(:file "main")))
