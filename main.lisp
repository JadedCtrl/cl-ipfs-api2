;; This file is free software: you can redistribute it and/or modify
;; it under the terms of version 3 of the GNU General Public License
;; as published by the Free Software Foundation.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

(in-package :cl-ipfs-api2)

(defparameter *api-host* "http://127.0.0.1:5001")
(defparameter *api-root* "/api/v0/")
(defparameter *ipfs-root* nil) ;; correlates to the env variable $IPFS_PATH,
                               ;; only necessary if yours deviates from the
                               ;; default path. only used for #'pubsub-*



;; —————————————————————————————————————
;; BASE

;; STRING LIST [:LIST :BOOLEAN :SYMBOL] → STRING | ALIST | (NIL STRING)
(defun ipfs-call (call arguments &key (parameters nil) (want-stream nil)
                        (method :POST))
  "Make an IPFS HTTP API call. Quite commonly used.
   Some calls return strings/raw data, and others return JSON.
   When strings/arbitrary data are recieved, they're returned verbatim.
   But, when JSON is returned, it is parsed into a hashtable.
   If the JSON is 'error JSON', I.E., it signals that an error has been
   recieved, two values are returned: NIL and the string-error-message."
  (let ((result
          (multiple-value-list
           (drakma:http-request
            ;; We ensure the string is of 'character elements and not 'base-char
            ;; which would break Puri.
            (alexandria:copy-array (make-call-url call arguments) :element-type 'character)
            :method method
            :url-encoder #'ipfs::url-encode
            :parameters parameters
            :want-stream want-stream))))
    (if want-stream
        (car result)
        (apply #'process-result result))))

(defun process-result (body status-code headers uri http-stream must-close status-text)
  (declare (ignore uri http-stream must-close status-text))
  (let* ((result (cond ((stringp body) body)
                       ((vectorp body) (flexi-streams:octets-to-string body))))
         (result (if (search "application/json" (cdr (assoc :content-type headers)))
                     (mapcar (lambda (line)
                               (simplify (yason:parse line :object-as :alist)))
                             (delete "" (uiop:split-string result :separator (string #\newline))
                                     :test 'string=))
                     result)))
    (if (eql 200 status-code)
        result
        (values nil (if (stringp result)
                        result
                        (ignore-errors (cdr (s-assoc "Message" result))))))))

;; STRING LIST &key STRING STRING → STRING
(defun make-call-url (call arguments &key (host *api-host*) (root *api-root*))
  "Create the URL of an API call, as per the given arguments.
  Symbols are assumed to be something like 'T (so boolean), nil likewise.
  Arguments should look like this:
    (('recursive' nil)('name' 'xabbu'))"
  (let ((call-url (string+ host root call))
	(first-arg 'T))
    (mapcar
      (lambda (arg-pair)
        (when arg-pair
          (setq call-url
            (string+ call-url
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


;; —————————————————————————————————————
;; ROOT CALLS

(defun parent-directory (path)
  (if (uiop:directory-pathname-p path)
      (uiop:pathname-parent-directory-pathname path)
      (uiop:pathname-directory-pathname path)))

(defun directory->parameters (dir)
  (let* ((result '())
         (root (uiop:ensure-pathname dir :truenamize t))
         (parent (parent-directory root)))
    (uiop:collect-sub*directories
     root
     #'uiop:directory-exists-p
     (constantly t)
     (lambda (subdirectory)
       (setf result
             (cons `("file" ,subdirectory
                            :content-type "application/x-directory"
                            :filename ,(uiop:native-namestring (uiop:enough-pathname subdirectory parent)))
                   (append result
                           (mapcar (lambda (file)
                                     `("file" ,file
                                              :filename ,(uiop:native-namestring (uiop:enough-pathname file parent))))
                                   (uiop:directory-files subdirectory)))))))
    result))

;; PATHNAME → (HASH-STRING SIZE-NUMBER) || (NIL STRING)
(defun add (pathname &key (pin 't) (only-hash nil) (cid-version 0))
  "Add a file to IPFS, return it's hash.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-add"
  (ipfs-call "add" `(("pin" ,pin) ("only-hash" ,only-hash) ("cid-version" ,cid-version))
             :parameters (if (uiop:directory-exists-p pathname)
                             (directory->parameters pathname)
                             `(("file" . ,pathname)))))

;; STRING :NUMBER :NUMBER → STRING || (NIL STRING)
(defun cat (ipfs-path &key (offset nil) (length nil))
  "Return a string of the data at the given IPFS path.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-cat"
  (ipfs-call "cat"
             `(("arg" ,ipfs-path)
               ,(if offset `("offset" ,offset))
               ,(if length `("length" ,length)))))

;; STRING [:BOOLEAN :BOOLEAN] → ALIST || (NIL STRING)
(defun ls (ipfs-path &key (resolve-type 't) (size 't))
  "Returns all sub-objects (IPFS hashes) under a given IPFS/IPNS directory
  path. Returns as an associative list.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-ls"
  (ipfs-call "ls" `(("arg" ,ipfs-path)
                    ("resolve-type" ,resolve-type) ("size" ,size))))

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

;; ——————————————————

;; [STRING] → ALIST
(defun id (&optional peer-id)
  "Return info on a node by ID. Returns as an associative list, the public key,
  agent version, etc. If no node ID is specified, then your own is assumed.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-id"
  (ipfs-call "id" `(,(if peer-id (list "arg" peer-id)))))

;; ——————————————————

;; STRING → STRING || (NIL STRING
(defun dns (domain &key (recursive 't))
  "Resolve a domain into a path (usually /ipfs/).
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-dns"
  (ipfs-call "dns" `(("arg" ,domain) ("recursive" ,recursive))))

;; STRING [:BOOLEAN :NUMBER :NUMBER] → STRING || (NIL STRING)
(defun resolve (ipfs-path &key (recursive 't) (dht-record-count nil)
			  (dht-timeout 30))
  "Resolve a given name to an IPFS path."
  (ipfs-call "resolve" `(("arg" ,ipfs-path) ("recursive" ,recursive)
                        ,(if dht-record-count
                           (list "dht-record-count" dht-record-count))
                        ("dht-timeout" ,(string+ dht-timeout "s")))))

;; ——————————————————

;; NIL → NIL
(defun shutdown ()
  "Shut down the connected IPFS node.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-shutdown"
  (ipfs-call "shutdown" '()))



;; —————————————————————————————————————
;; BITSWAP CALLS

;; STRING → ALIST || (NIL STRING)
(defun bitswap-ledger (peer-id)
  "Show the current ledger for a peer.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-bitswap-ledger"
  (ipfs-call "bitswap/ledger" `(("arg" ,peer-id))))

;; NIL → NIL
(defun bitswap-reprovide ()
  "Trigger the reprovider.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-bitswap-reprovide"
  (ipfs-call "bitswap/reprovide" '()))

;; NIL → ALIST || (NIL STRING)
(defun bitswap-stat ()
  "Show diagnostic info on the bitswap agent.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-bitswap-stat"
  (ipfs-call "bitswap/stat" '()))

;; STRING → ALIST || (NIL STRING)
(defun bitswap-wantlist (&optional peer-id)
  "Show blocks currently on the wantlist.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-bitswap-wantlist"
  (ipfs-call "bitswap/wantlist" `(,(if peer-id (list "peer" peer-id)))))



;; —————————————————————————————————————
;; BLOCK CALLS

;; STRING → STRING || (NIL STRING)
(defun block-get (hash)
  "Get a raw IPFS block.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-block-get"
  (ipfs-call "block/get" `(("arg" ,hash))))

;; PATHNAME [:STRING :STRING :NUMBER :BOOLEAN] → ALIST || (NIL STRING)
(defun block-put (pathname &key (format nil) (mhtype "sha2-256") (mhlen -1)
		       (pin nil))
  "Store input as an IPFS block.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-block-put"
  (ipfs-call "block/put" `(,(if format (list "format" format))
                            ("mhtype" ,mhtype)
                            ("mhlen" ,mhlen)
                            ("pin" ,pin))
             :parameters `(("data" . ,pathname))))

;; STRING → NIL
(defun block-rm (hash &key (force nil))
  "Delete an IPFS block(s).
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-block-rm"
  (ipfs-call "block/rm" `(("arg" ,hash) ,(if force (list "force" force))))
  nil)

;; STRING → ALIST || (NIL STRING)
(defun block-stat (hash)
  "Print info about a raw IPFS block
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-block-stat"
  (ipfs-call "block/stat" `(("arg" ,hash))))



;; —————————————————————————————————————
;; BOOTSTRAP CALLS

;; NIL → LIST || (NIL STRING)
(defun bootstrap ()
  "Return a list of bootstrap peers
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-bootstrap"
  (cdr (ipfs-call "bootstrap" '())))

;; NIL → LIST || (NIL STRING)
(defun bootstrap-list ()
  "Return a list of bootstrap peers
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-bootstrap-list"
  (bootstrap))

;; STRING → LIST || (NIL STRING)
(defun bootstrap-add (peer)
  "Add a peer to the bootstrap list
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-bootstrap-add"
  (cdr (ipfs-call "bootstrap/add" `(("arg" ,peer)))))

;; NIL → LIST || (NIL STRING)
(defun bootstrap-add-default ()
  "Add default peers to the bootstrap list
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-bootstrap-add-default"
  (cdr (ipfs-call "bootstrap/add/default" '())))

;; STRING → LIST || (NIL STRING)
(defun bootstrap-rm (peer)
  "Remove a peer from the bootstrap list
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-bootstrap-rm"
  (cdr (ipfs-call "bootstrap/rm" `(("arg" ,peer)))))

;; NIL → LIST || (NIL STRING)
(defun bootstrap-rm-all ()
  "Remove a peer from the bootstrap list
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-bootstrap-rm"
  (cdr (ipfs-call "bootstrap/rm/all" '())))



;; —————————————————————————————————————
;; CID CALLS

;; STRING → STRING || (NIL STRING)
(defun cid-base32 (cid)
  "Convert a CID into Base32 CIDv1
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-cid-base32"
  (let ((result (ipfs-call "cid/base32" `(("arg" ,cid)))))
    (if (zerop (length (cdr (s-assoc "ErrorMsg" result))))
      (cdr (s-assoc "Formatted" result))
      (values nil (cdr (s-assoc "ErrorMsg" result))))))

;; NIL → ALIST || (NIL STRING)
(defun cid-bases ()
  "Return a associative list of available bases in plist format; each base's
  name is a assigned a given code-number.
    ((CODE-A . NAME-A) (CODE-B . NAME-B) … (CODE-N . NAME-N))
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-cid-bases"
  (ipfs-call "cid/bases" '()))



;; —————————————————————————————————————
;; CONFIG CALLS

;; STRING [:STRING :BOOLEAN :BOOLEAN] → STRING || (NIL STRING)
(defun config (key &key (value nil) (bool nil) (json nil))
  "Get/set a config key's value.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-config"
  (cdr (s-assoc "Value"
    (ipfs-call "config" `(("arg" ,key) ,(if value (list "value" value))
                          ("bool" ,bool) ("json" ,json))))))

;; NIL → ALIST
(defun config-show ()
  "Return the config file's contents, in alist-format… y'know, with several
  sub-alists.
  Doesn't quite line up with #api-v0-config-show
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-config-show"
  (ipfs-call "config/show" '()))

;; STRING → STRING || (NIL STRING)
(defun config-get (key)
  "Get a config key's value.
  Doesn't map with any existant API call; it's just a convenience wrapper
  around #'config."
  (config key))

;; STRING → STRING || (NIL STRING)
(defun config-set (key value &key (bool nil) (json nil))
  "Set a config key's value.
  Doesn't map with any existant API call; it's just a convenience wrapper
  around #'config."
  (config key :value value :bool bool :json json))



;; —————————————————————————————————————
;; DAG CALLS

;; STRING → STRING || (NIL STRING)
(defun dag-get (dag-node)
  "Get a dag node from IPFS.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-dag-get"
  (ipfs-call "dag/get" `(("arg" ,dag-node))))

;; STRING [:STRING :STRING :BOOLEAN] → STRING || (NIL STRING
(defun dag-put (dag-node &key (format "cbor") (input-enc "json") (pin 'T))
  "Add a dag node to IPFS. Returns CID string.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-dag-put"
  (ipfs-call "dag/put" `(("arg" ,dag-node) ("format" ,format)
                         ("input-enc" ,input-enc) ("pin" ,pin))))
;;    (gethash "/" (gethash "Cid" result))))

;; STRING → ALIST || (NIL STRING)
(defun dag-resolve (path)
  "Resolve an IPLD block.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-dag-resolve"
  (ipfs-call "dag/resolve" `(("arg" ,path))))



;; —————————————————————————————————————
;; DHT CALLS

;; STRING → LIST || (NIL STRING)
(defun dht-findpeer (peer-id)
  "Find the multiaddresses associated with a peer ID.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-dht-findpeer"
  (cdr (s-assoc "Addrs" (cadr (s-assoc "Responses"
    (ipfs-call "dht/findpeer" `(("arg" ,peer-id))))))))

;; STRING [:NUMBER] → LIST || (NIL STRING)
(defun dht-findprovs (key &key (provider-quantity 20))
  "Find peers that can provide a specific value, given a key.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-dht-findprovs"
  (ipfs-call "dht/findprovs"
   `(("arg" ,key)("num-providers" ,provider-quantity))))

;; STRING → LIST || (NIL STRING)
(defun dht-get (key)
  "Query the routing system for a key's best value.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-dht-get"
  (cdr (s-assoc "Addrs" (cadr (s-assoc "Responses"
    (ipfs-call "dht/get" `(("arg" ,key))))))))

;; STRING [:BOOLEAN] → NIL
(defun dht-provide (key &key (recursive nil))
  "Announce to the network that you're providing the given values.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-dht-provide"
  (ipfs-call "dht/provide" `(("arg" ,key)("recursive" ,recursive))))

;; STRING STRING → NIL || (NIL STRING)
(defun dht-put (key value)
  "Write a key-value pair to the routing system.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-dht-put"
  (ipfs-call "dht/put" `(("arg" ,key)("arg" ,value))))

;; STRING → ALIST || (NIL STRING)
(defun dht-query (peer-id)
  "Find the closest peer IDs to the given one by querying the DHT.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-dht-query"
  (cdr (s-assoc "Responses"
    (ipfs-call "dht/query" `(("arg" ,peer-id))))))



;; —————————————————————————————————————
;; DIAG CALLS

;; NIL → ALIST
(defun diag-cmds ()
  "List commands run on this IPFS node.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-diag-cmds"
  (ipfs-call "diag/cmds" NIL))

;; NIL → NIL || (NIL STRING)
(defun diag-cmds-clear ()
  "Clear inactive requests from the log.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-diag-cmds-clear"
  (ipfs-call "diag/cmds/clear" NIL))

;; NUMBER → NIL || (NIL STRING)
(defun diag-cmds-set-time (time)
  "Set how long to keep inactive requests in the log.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-diag-cmds-set-time"
  (ipfs-call "diag/cmds/set-time" `(("arg" ,time))))

;; NIL → STRING || (NIL STRING)
(defun diag-sys ()
  "Print system diagnostic info.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-diag-sys"
  (ipfs-call "diag/sys" NIL))



;; —————————————————————————————————————
;; FILE CALLS

(defun file-ls (path)
  "List directory contents for UNIX filesystem objects.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-file-ls"
  (cdr (s-assoc "Objects"
    (ipfs-call "file/ls" `(("arg" ,path))))))



;; —————————————————————————————————————
;; FILES CALLS

;; STRING [:NUMBER :STRING] → NIL
(defun files-chcid (path &key (cid-version nil) (hash nil))
  "Change the cid version or hash function of the root node of a given path.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-files-chcid"
  (ipfs-call "files/chcid" `(("arg" ,path)
                             ,(if cid-version `("cid-version" ,cid-version))
                             ,(if hash (list "hash" hash)))))

;; STRING STRING → NIL || (NIL STRING)
(defun files-cp (source destination)
  "Copy files into mfs.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-files-cp"
  (ipfs-call "files/cp" `(("arg" ,source)("arg" ,destination))))

;; STRING → STRING
(defun files-flush (&optional (path "/"))
  "Flush a given path's data to disk. Returns CID.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-files-flush"
  (ipfs-call "files/flush" `(("arg" ,path))))

;; [STRING] → ALIST || (NIL STRING)
(defun files-ls (&optional (path "/"))
  "List directories in local mutable namespace.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-files-ls"
  (ipfs-call "files/ls" `(("arg" ,path) ("long" "true"))))

;; STRING [:BOOLEAN :NUMBER :STRING] → NIL || (NIL STRING)
(defun files-mkdir (path &key (parents nil) (cid-version nil) (hash nil))
  "Make a directory.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-files-mkdir"
  (ipfs-call "files/mkdir" `(("arg" ,path)
                             ,(if parents (list "parents" parents))
                             ,(if cid-version `("cid-version" ,cid-version))
                             ,(if hash (list "hash" hash)))))

;; STRING STRING → NIL || (NIL STRING)
(defun files-mv (source destination)
  "Move a file.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-files-mv"
  (ipfs-call "files/mv" `(("arg" ,source)("arg" ,destination))))

;; STRING [:NUMBER :NUMBER] → STRING || (NIL STRING)
(defun files-read (path &key (offset nil) (max nil))
  "Read a file in given mfs.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-files-read"
  (ipfs-call "files/read" `(("arg" ,path)
                            ,(if offset (list "offset" offset))
                            ,(if max (list "max" max)))))

;; STRING [:BOOLEAN :BOOLEAN] → NIL || (NIL STRING)
(defun files-rm (path &key (recursive nil) (force nil))
  "Remove a given file.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-files-rm"
  (ipfs-call "files/rm" `(("arg" ,path) ("recursive" ,recursive)
                          ("force" ,force))))

;; STRING → ALIST || (NIL STRING)
(defun files-stat (path)
  "Remove a given file.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-files-rm"
  (ipfs-call "files/stat" `(("arg" ,path))))

;; PATHNAME STRING [:NUMBER :BOOLEAN :BOOLEAN :BOOLEAN :NUMBER :BOOLEAN
;;                  :NUMBER :STRING]
;;                 → NIL || (NIL STRING)
(defun files-write (path-or-string dest-path
			     &key (offset nil) (create nil) (parents nil)
			     (truncate nil) (count nil) (raw-leaves nil)
			     (cid-version nil) (hash nil))
  "Write to a given file. First parameter can be a string or a path to
a local file.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-files-rm"
  (let ((result
          (multiple-value-list
           (drakma:http-request
            (make-call-url
             "files/write"
             `(("arg" ,dest-path) ("create", create) ("parents" ,parents)
                                  ("truncate" ,truncate) ("raw-leaves" ,raw-leaves)
                                  ,@(when offset (list "offset" offset))
                                  ,@(when count (list "count" count))
                                  ,@(when cid-version `("cid-version" ,cid-version))
                                  ,@(when hash (list "hash" hash))))
            :method :post
            :parameters `(("data" . ,path-or-string))
            :form-data t))))
    (apply #'process-result result)))

(defmacro with-files-write ((stream dest-path &rest params) &body body)
  "A convenience macro for files-write. In the body of the macro, any writes
to the stream named by STREAM will be sent to the mfs file at DEST-PATH. PARAMS
will be passed directly to the files-write function."
  (let ((fn (gensym "FN")))
    ;;FIXME: Would be nice to write the stream directly to files-write.
    ;; This feels a little less efficient.
    `(uiop:with-temporary-file (:stream ,stream :pathname ,fn)
       ,@body
       :close-stream
       (files-write ,fn ,dest-path ,@params))))

;; —————————————————————————————————————
;; FILESTORE CALLS

;; NIL → ALIST || (NIL STRING)
(defun filestore-dups ()
  "List blocks that're both in the filestore and standard block storage.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-filestore-dups"
  (ipfs-call "filestore/dups" '()))

;; [STRING] → ALIST || (NIL STRING)
(defun filestore-ls (&optional cid)
  "List objects in filestore.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-filestore-ls"
  (ipfs-call "filestore/ls" `(,(if cid (list "arg" cid)))))

;; [STRING] → ALIST || (NIL STRING)
(defun filestore-verify (&optional cid)
  "Verify objects in filestore.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-filestore-verify"
  (ipfs-call "filestore/verify" `(,(if cid (list "arg" cid)))))



;; —————————————————————————————————————
;; KEY CALLS

;; STRING [:STRING :NUMBER] → ALIST || (NIL STRING)
(defun key-gen (name &key (type nil) (size nil))
  "Create a new keypair.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-key-gen"
  (ipfs-call "key/gen" `(("name" ,name) ,(if type (list "type" type))
                                        ,(if size (list "size" size)))))

;; NIL → ALIST || (NIL STRING)
(defun key-list ()
  "List all local keypairs.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-key-list"
  (ipfs-call "key/list" '()))

;; STRING STRING [:BOOLEAN] → ALIST || (NIL STRING)
(defun key-rename (old-name new-name &key (force nil))
  "Rename a local keypair.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-key-rename"
  (ipfs-call "key/rename" `(("arg" ,old-name) ("arg" ,new-name)
                                              ("force" ,force))))
;; STRING → ALIST || (NIL STRING)
(defun key-remove (name)
  "Remove a keypair, based on name.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-key-remove"
  (ipfs-call "key/remove" `(("arg" ,name))))



;; —————————————————————————————————————
;; LOG CALLS

;; STRING STRING → STRING || (NIL STRING)
(defun log-level (subsystem level)
  "Change the logging level of a subsystem.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-log-level"
  (cdr (s-assoc "Message"
    (ipfs-call "log/level" `(("arg" ,subsystem)("arg" ,level))))))

;; NIL → LIST || (NIL STRING)
(defun log-ls ()
  "List the logging subsystems.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-log-ls"
  (cdr (ipfs-call "log/ls" '())))

;; NIL → STRING || (NIL STRING)
(defun log-tail ()
  "Read the event log.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-log-tail"
  (ipfs-call "log/tail" '()) result)



;; —————————————————————————————————————
;; NAME CALLS

;; STRING [:BOOLEAN :STRING :BOOLEAN :STRING] → ALIST || (NIL STRING)
(defun name-publish (ipfs-path &key (resolve 'T) (lifetime "24h")
			       (allow-offline 'T) (ttl nil))
  "Publish an IPNS name-- associate it with an IPFS path.
   /ipns/docs.ipfs.io/reference/api/http/#api-v0-name-publish"
  (ipfs-call "name/publish" `(("arg" ,ipfs-path)("resolve" ,resolve)
                              ("lifetime" ,lifetime)
                              ("allow-offline" ,allow-offline)
                              ,(if ttl (list "ttl" ttl)))))

;; STRING → STRING || (NIL STRING)
(defun name-pubsub-cancel (name)
  "Cancel subscription to a name.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-name-pubsub-cancel"
  (cdr (s-assoc "Cancelled"
    (ipfs-call "name/pubsub/cancel" `(("arg" ,name))))))

;; NIL → STRING || (NIL STRING)
(defun name-pubsub-state ()
  "Query the state of IPNS pubsub.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-name-pubsub-state"
  (cdr (s-assoc "Enabled"
    (ipfs-call "name/pubsub/state" '()))))

;; NIL → STRING || (NIL STRING)
(defun name-pubsub-subs ()
  "Show current name subscriptions.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-name-pubsub-subs"
  (cdr (s-assoc "Strings"
    (ipfs-call "name/pubsub/subs" '()))))

;; STRING [:BOOLEAN :BOOLEAN :NUMBER :STRING] → STRING || (NIL STRING)
(defun name-resolve (name &key (recursive 't) (nocache "")
			  (dht-record-count nil) (dht-timeout nil))
  "Resolve a given IPNS name.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-name-resolve"
  (ipfs-call "name/resolve" `(("arg" ,name)("recursive" ,recursive)
                              ,(when (not (empty-string-p nocache))
                                 (list "nocache" nocache))
                              ,(when dht-record-count
                                 (list "dht-record-count" dht-record-count))
                              ,(when dht-timeout
                                 (list "dht-timeout" dht-timeout)))))



;; —————————————————————————————————————
;; OBJECT CALLS

;; STRING → STRING || (NIL STRING)
(defun object-data (key)
  "Output the raw data of an IPFS object.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-object-data"
  (ipfs-call "object/data" `(("arg" ,key))))

;; STRING STRING → ALIST || (NIL STRING)
(defun object-diff (object-a object-b)
  "Display the differences between two IPFS objects.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-object-diff"
  (ipfs-call "object/diff" `(("arg" ,object-a)("arg" ,object-b))))

;; STRING [:STRING] → STRING || (NIL STRING)
(defun object-get (key &key (data-encoding "text"))
  "Get and serialize the named DAG node.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-object-get"
  (ipfs-call "object/get" `(("arg" ,key)("data-encoding" ,data-encoding))))

;; STRING → ALIST || (NIL STRING)
(defun object-links (key)
  "Output the links pointed to by the specified object.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-object-links"
  (ipfs-call "object/links" `(("arg" ,key))))

;; [:STRING] → ALIST || (NIL STRING)
(defun object-new (&key (template nil))
  "Create a new object from an IPFS template.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-object-new"
  (ipfs-call "object/new"`(,(if template `("template" ,template)))))

;; STRING STRING STRING [:BOOLEAN] → ALIST || (NIL STRING)
(defun object-patch-add-link (hash name object &key (create ""))
  "Add a link to a given object.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-object-patch-add-link"
  (ipfs-call "object/patch/add-link"
             `(("arg" ,hash)("arg" ,name)("arg" ,object)
               ,(when (not (empty-string-p create)) `("create" ,create)))))

;; STRING STRING → ALIST || (NIL STRING)
(defun object-patch-rm-link (hash name)
  "Remove a link from a given object.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-object-patch-rm-link"
  (ipfs-call "object/patch/rm-link" `(("arg" ,hash)("arg" ,name))))

;; STRING → ALIST || (NIL STRING)
(defun object-stat (key)
  "Get stats for a DAG node.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-object-stat"
  (ipfs-call "object/stat" `(("arg" ,key))))



;; —————————————————————————————————————
;; P2P CALLS

;; [:BOOLEAN :STRING :STRING :STRING :STRING] → NUMBER || (NIL STRING)
(defun p2p-close (&key (all "") (protocol nil) (listen-address nil)
		       (target-address nil))
  "Stop listening for new connections to forward.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-p2p-close"
  (ipfs-call "p2p/close" `(,(when (not (empty-string-p all)) `("all" ,all))
                            ,(when protocol `("protocol" ,protocol))
                            ,(when listen-address
                               `("listen-address" ,listen-address))
                            ,(when target-address
                               `("target-address" ,target-address)))))

;; STRING STRING STRING [:BOOLEAN] → STRING || (NIL STRING)
(defun p2p-forward (protocol listening-endpoint target-endpoint
			     &key (allow-custom-protocol ""))
  "Forward connections to libp2p service.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-p2p-forward"
  (ipfs-call "p2p/forward" `(("arg" ,protocol)("arg" ,listening-endpoint)
                             ("arg" ,target-endpoint)
                             ,(when
                                (not (empty-string-p allow-custom-protocol))
                                `("allow-custom-protocol"
                                  ,allow-custom-protocol)))))

;; STRING STRING [:BOOLEAN :BOOLEAN] → STRING || (NIL STRING)
(defun p2p-listen (protocol target-endpoint
			    &key (allow-custom-protocol "") (report-peer-id ""))
  "Create libp2p service.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-p2p-listen"
  (ipfs-call "p2p/listen" `(("arg" ,protocol)("arg" ,target-endpoint)
                            ,(when
                               (not (empty-string-p allow-custom-protocol))
                               `("allow-custom-protocol"
                                 ,allow-custom-protocol))
                            ,(when (not (empty-string-p report-peer-id))
                               `("report-peer-id" ,report-peer-id)))))

;; NIL → ALIST || (NIL STRING)
(defun p2p-ls ()
  "List active p2p listeners.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-p2p-ls"
  (ipfs-call "p2p/ls" '()))

;; [:STRING :BOOLEAN] → STRING || (NIL STRING)
(defun p2p-stream-close (&key (identifier nil) (all ""))
  "Close an active p2p stream.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-p2p-stream-close"
  (ipfs-call "p2p/stream/close" `(,(when identifier `("arg" ,identifier))
                                   ,(when (not (empty-string-p all))
                                      `("all" ,all)))))

;; NIL → ALIST || (NIL STRING)
(defun p2p-stream-ls ()
  "List active p2p streams.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-p2p-stream-ls"
  (ipfs-call "p2p/stream/ls" '()))



;; —————————————————————————————————————
;; PIN CALLS

;; STRING [:BOOLEAN] → ALIST || (NIL STRING)
(defun pin-add (path &key (recursive 'T))
  "Pin an object to local storage.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-pin-add"
  (ipfs-call "pin/add" `(("arg" ,path)("recursive" ,recursive))))

;; [:STRING :STRING] → ALIST || (NIL STRING)
(defun pin-ls (&key (path nil) (type "all"))
  "List objects pinned to local storage.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-pin-ls"
  (let ((res (ipfs-call "pin/ls" `(,(when path `("arg" ,path)) ("type" ,type)))))
    (if (equal res '("Keys")) nil res)))

;; STRING [:BOOLEAN] → ALIAS || (NIL STRING)
(defun pin-rm (path &key (recursive 'T))
  "Remove pinned objects from local storage.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-pin-rm"
  (ipfs-call "pin/rm" `(("arg" ,path)("recursive" ,recursive))))

;; STRING STRING [:BOOLEAN] → ALIST || (NIL STRING)
(defun pin-update (old-path new-path &key (unpin 'T))
  "Update a recursive pin.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-pin-update"
  (ipfs-call "pin/update"
             `(("arg" ,old-path)("arg" ,new-path)("unpin" ,unpin))))

;; NIL → ALIST || (NIL STRING)
(defun pin-verify ()
  "Verify that recursive pins are complete.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-pin-verify"
  (ipfs-call "pin/verify" '()))



;; —————————————————————————————————————
;; PUBSUB CALLS

;; STRING [:STRING] → PROCESS-INFO-STREAM
(defun pubsub-sub (topic &key (env ""))
  "Subscribe to a given pubsub topic— this function requires go-ipfs to be
  installed on the current machine, and that `ipfs` is in the current $PATH.
  This probably will only work on *nix systems (sorry Windows nerds).
  Returns a uiop/launch-program::process-info socket-- can be used in
  conjunction with the #'pubsub-sub-* functions, or with :uiop/launch-program's
  functions.
  A system-dependent replacement for
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-pubsub-sub"
  (when (and *ipfs-root* (empty-string-p env))
    (setq env (string+ "env IPFS_PATH=" *ipfs-root* " > /dev/null;")))

  (uiop:launch-program (string+ env "ipfs pubsub sub " topic) :output :stream))

;; PROCESS-INFO-STREAM → FD-STREAM
(defun pubsub-sub-process (pubsub-socket)
  "Turn a uiop process-info-stream ('pubsub stream') into a fd-stream that
  is #'read-char-able, etc."
  (uiop/launch-program:process-info-output pubsub-socket))

;; PROCESS-INFO-STREAM → CHARACTER
(defun pubsub-sub-read-char (pubsub-socket)
  "Process a 'pubsub stream' (process-info-stream) and #'readchar it."
  (read-char (pubsub-sub-process pubsub-socket)))

;; PROCESS-INFO-STREAM → BOOLEAN
(defun pubsub-sub-listen (pubsub-socket)
  "Process a 'pubsub stream' (process-info-stream) and #'listen it."
  (listen (pubsub-sub-process pubsub-socket)))

;; PROCESS-INFO-STREAM → NIL
(defun pubsub-sub-close (pubsub-socket)
  "Close a 'pubsub stream' (process-info-stream) and related processes."
  (and (uiop/launch-program:terminate-process pubsub-socket :urgent 't)
       (uiop/launch-program:close-streams pubsub-socket)))

;; —————————————————

;; STRING STRING [:STRING] → NIL
(defun pubsub-pub (topic string &key (env ""))
  "Publish a string to a given pubsub topic.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-pubsub-pub"
  (when (and *ipfs-root* (empty-string-p env))
    (setq env (string+ "env IPFS_PATH=" *ipfs-root* " > /dev/null;")))

  (uiop:run-program (string+ env "ipfs pubsub pub " topic " \"" string "\""))
  nil)

;; —————————————————

;; NIL → LIST || (NIL STRING)
(defun pubsub-ls ()
  "Return a list of subscribed topics.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-pubsub-ls"
  (s-assoc "Strings"
    (ipfs-call "pubsub/ls" '())))

;; [STRING] → LIST || (NIL STRING)
(defun pubsub-peers (&optional topic)
  "Return a list of peers with pubsub enabled.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-pubsub-peers"
  (s-assoc "Strings"
    (ipfs-call "pubsub/peers" `(,(if topic (list "arg" topic))))))



;; —————————————————————————————————————
;; REFS CALLS

;; STRING [:BOOLEAN :BOOLEAN :NUMBER] → ALIST || (NIL STRING)
(defun refs (path &key (unique "") (recursive "") (max-depth -1))
  "List links (references) from an object.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-refs"
  (ipfs-call "refs" `(("arg" ,path)("max-depth" ,max-depth)
                      ,(if (not (empty-string-p recursive))
                         `("recursive" ,recursive))
                      ,(if (not (empty-string-p unique))
                         `("unique" ,unique)))))

;; NIL → ALIST || (NIL STRING)
(defun refs-local ()
  "List all local references.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-refs-local"
  (ipfs-call "refs/local" '()))



;; —————————————————————————————————————
;; REPO CALLS

;; NIL → STRING || (NIL STRING)
(defun repo-fsck ()
  "Remove repo lock-files.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-repo-fsck"
  (cdr (s-assoc "Message" (ipfs-call "repo/fsck" '()))))

;; NIL → ALIST || (NIL STRING)
(defun repo-gc ()
  "Perform garbage collection on the repo.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-repo-gc"
  (ipfs-call "repo/gc" '()))

;; NIL → ALIST || (NIL STRING)
(defun repo-stat ()
  "Get stats for the current repo.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-repo-stat"
  (ipfs-call "repo/stat" '()))

;; NIL → ALIST || (NIL STRING)
(defun repo-verify ()
  "Verify that all repo blocks aren't corrupted.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-repo-verify"
  (ipfs-call "repo/verify" '()))

;; NIL → NUMBER || (NIL STRING)
(defun repo-version ()
  "Show the repo version.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-repo-version"
  (parse-integer (ipfs-call "repo/version" '())))



;; —————————————————————————————————————
;; STATS CALLS

;; NIL → ALIST || (NIL STRING)
(defun stats-bitswap ()
  "Show diagnostics on bitswap.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-stats-bitswap"
  (ipfs-call "stats/bitswap" '()))

;; [:STRING :STRING :STRING] → ALIST || (NIL STRING)
(defun stats-bw (&key (peer nil) (proto nil) (interval nil))
  "Return bandwidth information.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-stats-bw"
  (ipfs-call "stats/bitswap"
             `(,(when peer `("peer" ,peer)) ,(when proto `("proto" ,proto))
               ,(when interval `("interval" ,interval))
               ,(when interval `("poll" 'T)))))

;; NIL → ALIST || (NIL STRING)
(defun stats-repo ()
  "Show diagnostics on current repo.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-stats-repo"
  (ipfs-call "stats/repo" '()))



;; —————————————————————————————————————
;; SWARM CALLS

;; NIL → ALIST || (NIL STRING)
(defun swarm-addrs ()
  "List known addresses.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-swarm-addrs"
  (ipfs-call "swarm/addrs" '()))

;; NIL → LIST || (NIL STRING)
(defun swarm-addrs-listen ()
  "List interface listening addresses.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-swarm-addrs-listen"
  (cdr (ipfs-call "swarm/addrs/listen" '())))

;; NIL → LIST || (NIL STRING)
(defun swarm-addrs-local ()
  "List local addresses.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-swarm-addrs-local"
  (cdr (ipfs-call "swarm/addrs/local" '())))

;; STRING → LIST || (NIL STRING)
(defun swarm-connect (address)
  "Open connection to a given address.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-swarm-connect"
  (cdr (ipfs-call "swarm/connect" `(("arg" ,address)))))

;; STRING → LIST || (NIL STRING)
(defun swarm-disconnect (address)
  "Close connection to a given address.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-swarm-disconnect"
  (cdr (ipfs-call "swarm/disconnect" `(("arg" ,address)))))

;; NIL → LIST || (NIL STRING)
(defun swarm-filters ()
  "List address filters.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-swarm-filters"
  (ipfs-call "swarm/filters" '()))

;; STRING → LIST || (NIL STRING)
(defun swarm-filters-add (multiaddr)
  "Add an address filter.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-swarm-filters-add"
  (ipfs-call "swarm/filters/add" `(("arg" ,multiaddr))))

;; STRING → LIST || (NIL STRING)
(defun swarm-filters-rm (multiaddr)
  "Remove an address filter.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-swarm-filters-rm"
  (ipfs-call "swarm/filters/rm" `(("arg" ,multiaddr))))

;; NIL → ALIST || (NIL STRING)
(defun swarm-peers ()
  "List peers with open connections.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-swarm-peers"
 (ipfs-call "swarm/peers" '()))



;; —————————————————————————————————————
;; URLSTORE CALLS

;; STRING [:BOOLEAN :BOOLEAN] → ALIST || (NIL STRING)
(defun urlstore-add (url &key (pin 'T) (trickle ""))
  "Add a URL via urlstore.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-urlstore-add"
  (ipfs-call "urlstore/add"`(("arg" ,url)("pin" ,pin)
                              ,(when (not (empty-string-p trickle))
                                 `("trickle" ,trickle)))))



;; —————————————————————————————————————
;; VERSION CALLS

;; NIL → LIST || (NIL STRING)
(defun version ()
  "Return the current golang, system, repo, and IPFS versions.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-version"
  (ipfs-call "version" nil))

;; NIL → ALIST
(defun version-deps ()
  "Return info about dependencies used for build; I.E., Go version, OS, etc.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-version"
  (ipfs-call "version/deps" '()))



;; —————————————————————————————————————
;; UTIL

;; LIST -> LIST
(defun simplify (list)
  "'Simplify' a list. Remove any extraneous sublisting [ ((2 3)) -> (2 3) ],
  and remove extraneous strings in otherwise pure alists, e.g.
  [ (``Apple'' (2 2) (3 3) (4 4)) -> ((2 2) (3 3) (4 4)) ]"
  (cond ((and (stringp (car list))
              (stringp (cdr list)))
         (cdr list))
        ((and (eq 1 (length list))
              (consp (car list)))
         (simplify (car list)))
        ((and (consp list)
              (stringp (car list))
              (consp (cadr list)))
         (simplify (cdr list)))
        ('T list)))

;; STRING LIST
(defun s-assoc (key alist)
  "Get the value of an associative list using a string key."
  (assoc key alist :test #'string-equal))

;; STRING-A STRING-B … STRING-N → STRING
(defun string+ (&rest strings)
  "Combine an arbitrary amount of strings into a single string."
  (reduce (lambda (a b) (format nil "~A~A" a b)) strings))

;; STRING → BOOLEAN
(defun empty-string-p (string)
  "Return whether or not a given item is an empty string."
  (and (stringp string) (zerop (length string))))

;; STRING → STRING
(defun url-encode (string &rest ignored)
  "Wrap around drakma's url encoder, with a slight change-- instead of using
  plus-signs for spaces, we want to use %20."
  ignored
  (cl-ppcre:regex-replace-all
    "%2520" (drakma:url-encode
             (cl-ppcre:regex-replace-all " " string "%20")
             drakma:*drakma-default-external-format*)
    "%20"))
