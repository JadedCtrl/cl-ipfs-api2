(defsystem "cl-ipfs-api²"
	   :version "0.1"
	   :author "Jaidyn Ann <jadedctrl@teknik.io>"
           :license "AGPLv3"
	   :depends-on ("drakma" "yason" "arnesi")
	   :components ((:file "package")
                 	(:file "main")))
