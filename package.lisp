(defpackage :cl-ipfs-api2
  (:use :cl :arnesi)
  (:nicknames :cl-ipfs :ipfs :cl-ipfs-api²)
  (:export
    *api-host*
    *api-root*

    ;; / calls
    :dl
    :cat
    :add
    :dns
    :id
    :ls
    :resolve
    :shutdown

    ;; / block calls
    :block/get
    :block/put
    :block/rm
    :block/stat

    ;; / bootstrap calls
    :bootstrap
    :bootstrap/list
    :bootstrap/add
    :bootstrap/add/default
    :bootstrap/rm
    :bootstrap/rm/all

    ;; / cid calls
    :cid/base32
    :cid/bases
    ;; / config calls
    :config
    :config/show

    ;; /version calls
    :version
    :version/deps))

(in-package :cl-ipfs-api²)
(in-package :cl-ipfs-api2)

(defparameter *api-host* "http://127.0.0.1:5001")
(defparameter *api-root* "/api/v0/")


;; STRING LIST [:LIST :BOOLEAN :SYMBOL] → STRING | HASHTABLE | (NIL STRING)
(defun ipfs-call (call arguments &key (parameters nil) (want-stream nil)
			   (method :GET))
  "Make an IPFS HTTP API call. Quite commonly used.
   Some calls return strings/raw data, and others return JSON.
   When strings/arbitrary data are recieved, they're returned verbatim.
   But, when JSON is returned, it is parsed into a hashtable.
   If the JSON is 'error JSON', I.E., it signals that an error has been
   recieved, two values are returned: NIL and the string-error-message."
  (let ((result
	  (drakma:http-request
	    (make-call-url *api-host* *api-root* call arguments)
	    :method method
	    :parameters parameters
	    :want-stream want-stream)))

    (cond (want-stream result)
	  ((stringp result) result)
	  ((vectorp result)
	   (let ((json (yason:parse (flexi-streams:octets-to-string result))))
	     (cond ((equal "error" (ignore-errors (gethash "Type" json)))
		    (values nil (gethash "Message" json)))
		   ('T json)))))))


;; STRING STRING LIST → STRING
(defun make-call-url (host root call arguments)
  "Create the URL of an API call, as per the given arguments.
  Symbols are assumed to be something like 'T (so boolean), nil likewise.
  Arguments should look like this:
    (('recursive' nil)('name' 'xabbu'))"
  (let ((call-url (string+ host root call))
	(first-arg 'T))
    (mapcar (lambda (arg-pair)
	      (when arg-pair
		(setq call-url
		      (string+
			call-url
			(if first-arg "?" "&")
			(first arg-pair) "="
			(cond ((not (second arg-pair))
			       "false")
			      ((symbolp (second arg-pair))
			       "true")
			      ('T (second arg-pair)))))
		(setq first-arg nil)))
	    arguments)
    call-url))


;; FORM FORM → FORM
(defmacro bind-api-result (call form)
  "Wrap around an #'ipfs-call form; if #'call returns an error, then
  return NIL and the error message-- (NIL STRING)-- otherwise, execute
  #'form.
  Binds the result of the API call to… you guessed it, the variable 'result'.
  The error message is assigned to 'message', if such a thing exists."
  `(multiple-value-bind (result message)
     ,call
     (if message
       (values nil message)
       ,form)))



;; -------------------------------------
;; ROOT CALLS

;; PATHNAME → (HASH-STRING SIZE-NUMBER) || (NIL STRING)
(defun add (pathname &key (pin 't) (only-hash nil))
  "Add a file to IPFS, return it's hash.
  http://127.0.0.1:8080/ipns/docs.ipfs.io/reference/api/http/#api-v0-add"
  (bind-api-result
    (ipfs-call "add" `(("pin" ,pin) ("only-hash" ,only-hash))
	       :method :post :parameters `(("file" . ,pathname)))

      (values (gethash "Hash" result)
	      (parse-integer (gethash "Size" result))
	      (gethash "Name" result))))

;; STRING :NUMBER :NUMBER → STRING || (NIL STRING)
(defun cat (ipfs-path &key (offset nil) (length nil))
  "Return a string of the data at the given IPFS path.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-cat"
  (bind-api-result
    (ipfs-call "cat"
	       `(("arg" ,ipfs-path)
		 ,(if offset `("offset" ,offset))
		 ,(if length `("length" ,length))))
    result))

;; STRING [:BOOLEAN :BOOLEAN] → (LIST LIST LIST LIST) || (NIL STRING)
(defun ls (ipfs-path &key (resolve-type 't) (size 't))
  "Returns all sub-objects (IPFS hashes) under a given IPFS/IPNS directory
  path. Returns four lists; hashes, sizes, names, types. They are related
  positionally-- aka, item #2 in the hash-list, will have a size of #2 in the
  size-list, etc.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-ls"
  (bind-api-result
    (ipfs-call "ls" `(("arg" ,ipfs-path)
		      ("resolve-type" ,resolve-type) ("size" ,size)))

    (let ((links (gethash "Links"
			  (car (gethash "Objects" result)))))
      (values (mapcar (lambda (link) (gethash "Hash" link)) links)
	      (mapcar (lambda (link) (gethash "Size" link)) links)
	      (mapcar (lambda (link) (gethash "Name" link)) links)
	      (mapcar (lambda (link) (gethash "Type" link)) links)))))

;; STRING PATHNAME → NIL
(defun dl (ipfs-path out-file)
  "Write an IPFS file directly to a file on the local file-system.
  Non-recursive, in the case of directories… for now.
  (Thanks to this thread ♥: https://stackoverflow.com/a/12607423)
  Is a general replacement for the 'get' API call, but actually just uses
  the 'cat' call, due to some issues with using 'get'.
  Will not actually return NIL when an error is reached (like other functions)
  with an error-message, it'lll just write the error JSON to the file.
  Whoops."
  (with-open-file (out-stream out-file :direction :output
			      :element-type '(unsigned-byte 8)
			      :if-exists :overwrite :if-does-not-exist :create)
    (let ((in-stream
	    (ipfs-call "cat" `(("arg" ,ipfs-path)) :want-stream 'T)))

      (awhile (read-byte in-stream nil nil)
	      (write-byte it out-stream))
      (close in-stream))))

;; -----------------

;; [:STRING] → (STRING LIST STRING STRING STRING)
(defun id (&key (peer-id nil))
  "Return info on a node by ID. Has multiple return-values, as follows:
  public-key, address-list, agent-version, protocol-version, and peer ID.
  The last might be redundant if it was specified as an argument; otherwise,
  it tells you your own node's ID.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-id"
  (bind-api-result
    (ipfs-call "id" `(,(if peer-id (list "arg" peer-id))))

    (values (gethash "PublicKey" result)
	    (gethash "Addresses" result)
	    (gethash "AgentVersion" result)
	    (gethash "ProtocolVersion" result)
	    (gethash "ID" result))))

;; -----------------

;; STRING → (STRING || (NIL STRING)
(defun dns (domain &key (recursive 't))
  "Resolve a domain into a path (usually /ipfs/).
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-dns"
  (bind-api-result
    (ipfs-call "dns" `(("arg" ,domain) ("recursive" ,recursive)))
    (gethash "Path" result)))

;; STRING [:BOOLEAN :NUMBER :NUMBER] → STRING || (NIL STRING)
(defun resolve (ipfs-path &key (recursive 't) (dht-record-count nil)
			  (dht-timeout 30))
  "Resolve a given name to an IPFS path."
  (bind-api-result
    (ipfs-call "resolve" `(("arg" ,ipfs-path) ("recursive" ,recursive)
                           ,(if dht-record-count
			      (list "dht-record-count" dht-record-count))
			   ("dht-timeout" ,(string+ dht-timeout "s"))))
    (gethash "Path" result)))
					      
;; -----------------

;; NIL → NIL
(defun shutdown ()
  "Shut down the connected IPFS node.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-shutdown"
  (ipfs-call "shutdown" '()))



;; -------------------------------------
;; BLOCK CALLS

;; STRING → STRING
(defun block/get (hash)
  "Get a raw IPFS block.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-block-get"
  (bind-api-result
    (ipfs-call "block/get" `(("arg" ,hash)))
    result))

;; PATHNAME [:STRING :STRING :NUMBER :BOOLEAN] → (STRING NUMBER)
(defun block/put (pathname &key (format nil) (mhtype "sha2-256") (mhlen -1)
		       (pin nil))
  "Store input as an IPFS block.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-block-put"
  (bind-api-result
    (ipfs-call "block/put" `(,(if format (list "format" format))
			     ("mhtype" ,mhtype)
			     ("mhlen" ,mhlen)
			     ("pin" ,pin))
	       :method :POST :parameters `(("data" . ,pathname)))
    (values (gethash "Key" result)
	    (gethash "Size" result))))

;; STRING → BOOLEAN
(defun block/rm (hash &key (force nil))
  "Delete an IPFS block(s).
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-block-rm"
  (bind-api-result
    (ipfs-call "block/rm" `(("arg" ,hash) ,(if force (list "force" force))))
    't))

;; STRING → (STRING NUMBER)
(defun block/stat (hash)
  "Print info about a raw IPFS block
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-block-stat"
  (bind-api-result
    (ipfs-call "block/stat" `(("arg" ,hash)))
    (values (gethash "Key" result)
	    (gethash "Size" result))))



;; -------------------------------------
;; BOOTSTRAP CALLS

;; NIL → LIST
(defun bootstrap ()
  "Return a list of bootstrap peers
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-bootstrap"
  (bind-api-result
    (ipfs-call "bootstrap" '())
    (gethash "Peers" result)))

;; NIL → LIST
(defun bootstrap/list ()
  "Return a list of bootstrap peers
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-bootstrap-list"
  (bootstrap))

;; STRING → LIST
(defun bootstrap/add (peer)
  "Add a peer to the bootstrap list
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-bootstrap-add"
  (bind-api-result
    (ipfs-call "bootstrap/add" `(("arg" ,peer)))
    (gethash "Peers" result)))

;; NIL → LIST
(defun bootstrap/add/default ()
  "Add default peers to the bootstrap list
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-bootstrap-add-default"
  (bind-api-result
    (ipfs-call "bootstrap/add/default" '())
    (gethash "Peers" result)))

;; STRING → LIST
(defun bootstrap/rm (peer)
  "Remove a peer from the bootstrap list
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-bootstrap-rm"
  (bind-api-result
    (ipfs-call "bootstrap/rm" `(("arg" ,peer)))
    (gethash "Peers" result)))

;; NIL → LIST
(defun bootstrap/rm/all (peer)
  "Remove a peer from the bootstrap list
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-bootstrap-rm"
  (bind-api-result
    (ipfs-call "bootstrap/rm/all" '())
    (gethash "Peers" result)))



;; -------------------------------------
;; CID CALLS

;; STRING → STRING || (NIL STRING)
(defun cid/base32 (cid)
  "Convert a CID into Base32 CIDv1
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-cid-base32"
  (bind-api-result
    (ipfs-call "cid/base32" `(("arg" ,cid)))
    (if (zerop (length (gethash "ErrorMsg" result)))
      (gethash "Formatted" result)
      (values nil (gethash "ErrorMsg" result)))))

;; NIL → LIST || (NIL STRING)
(defun cid/bases ()
  "Return a list of available bases in plist format; each base's name is a
  assigned a given code-number.
    (CODE-A NAME-A CODE-B NAME-B … CODE-N NAME-N)
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-cid-bases"
  (bind-api-result
    (ipfs-call "cid/bases" '())
    (mapcan (lambda (base)
	      (list (gethash "Code" base)
		    (gethash "Name" base)))
	    result)))

;; NIL → LIST || (NIL STRING)
(defun cid/bases ()
  "Convert a CID into Base32 CIDv1
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-cid-base32"
  (bind-api-result
    (ipfs-call "cid/bases" '())
    (mapcan (lambda (base)
	      (list (gethash "Code" base)
		    (gethash "Name" base)))
	    result)))


;; -------------------------------------
;; CONFIG CALLS

;; STRING [:STRING :BOOLEAN :BOOLEAN] → STRING || (NIL STRING)
(defun config (key &key (value nil) (bool nil) (json nil))
  "Get/set a config key's value.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-config"
  (bind-api-result
    (ipfs-call "config" `(("arg" ,key) ,(if value (list "value" value))
				       ("bool" ,bool) ("json" ,json)))
    (cons (gethash "Key" result) (gethash "Value" result))))

;; NIL → ALIST
(defun config/show ()
  "Return the config file's contents, in plist-format.
  Doesn't quite line up with #api-v0-config-show"
  (bind-api-result
    (ipfs-call "config/show" '())
    (hash-table-alist-recursive result)))
;;    (mapcar (lambda (key)
;;	      (list key (gethash key result)))
;;	    (hash-table-keys result))))



;; -------------------------------------
;; VERSION CALLS

;; NIL → (STRING ALIST)
(defun version ()
  "Return versioning information on this IPFS node
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-version"
  (bind-api-result
    (ipfs-call "version" '())
    (values (gethash "Version" result)
	    `(("Commit" ,(gethash "Commit" result))
	      ("Repo"   ,(gethash "Repo" result))
	      ("System" ,(gethash "System" result))
	      ("Golang" ,(gethash "Golang" result)))))A)

;; NIL → PLIST
(defun version/deps ()
  "Return info about dependencies used for build.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-version"
  (bind-api-result
    (ipfs-call "version/deps" '())
    `(("Path"       ,(gethash "Path" result))
      ("ReplacedBy" ,(gethash "ReplacedBy" result))
      ("Sum"        ,(gethash "Sum" result)))))




;; -------------------------------------
;; UTIL

(defun test-apply (test function data)
  (format t "DATA: ~A ~A~%" data (not (hash-table-p data)))
  (if (hash-table-p data) (format t "HELL NO")
  (cond ((and (listp data) (not (hash-table-p data)))
	 (mapcar (lambda (item)
		   (test-apply test function item))
		 data))
	
	((apply test `(,data))
	 (let ((new-data (apply function `(,data))))
	   (cond
	     ((or (listp new-data)
		  (ignore-errors (apply test `(,new-data))))
	      (test-apply test function new-data))
	     ('T new-data))))
	('T data))))

;; STRING-A STRING-B … STRING-N → STRING
(defun string+ (&rest strings)
  "Combine an arbitrary amount of strings into a single string."
  (reduce (lambda (a b) (format nil "~A~A" a b)) strings))

(defun hash-table-alist-recursive (hash-table)
  (test-apply #'hash-table-p #'alexandria:hash-table-alist (alexandria:hash-table-alist hash-table)))
