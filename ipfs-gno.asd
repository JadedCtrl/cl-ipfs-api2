(defsystem "ipfs-gno"
	   :version "0.1"
	   :author "Jaidyn Ann <jadedctrl@teknik.io>"
           :license "Cooperative Software License"
	   :depends-on ("drakma" "yason" "arnesi")
	   :components ((:file "package")
                 	(:file "main")))
