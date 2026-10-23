;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

;;;; uBlock Origin / Adblock Plus static filters.
;;;;
;;;; Network filters are indexed by hostname or by a literal token, which is
;;;; what keeps a full EasyList match off the linear scan. Cosmetic filters
;;;; become one stylesheet per page. A literal `$replace' rewrites the response
;;;; body instead of blocking the request. Scriptlets, HTML filters, regex
;;;; `$replace', and the other modifiers that do not block (`$csp',
;;;; `$removeparam', `$rewrite', `$header') are skipped: applying them as
;;;; blocks would change their meaning.
;;;;
;;;; YouTube's pre-roll is decided before the video starts, from `adPlacements'
;;;; and `adSlots' in the player response. Firefox uBlock Origin renames those
;;;; keys with `$replace' on the player request, and undefines them on
;;;; `ytInitialPlayerResponse' for the document itself. The same renames are
;;;; applied here to those responses.

(in-package :cef)

(defparameter *network-bits*
  '((:script . 1)
    (:image . 2)
    (:stylesheet . 4)
    (:object . 8)
    (:xmlhttprequest . 16)
    (:subdocument . 32)
    (:document . 64)
    (:ping . 128)
    (:websocket . 256)
    (:media . 512)
    (:font . 1024)
    (:other . 2048)
    (:popup . 4096)
    (:webrtc . 8192))
  "Request-type bits. Pseudo-types such as elemhide sit above this range.")

(defparameter +network-mask+ (1- (ash 1 14)))
(defparameter +elemhide-bit+ (ash 1 16))
(defparameter +generichide-bit+ (ash 1 17))
(defparameter +specificelemhide-bit+ (ash 1 18))
(defparameter +genericblock-bit+ (ash 1 19))
(defparameter +default-block-mask+ (logand +network-mask+ (lognot (logior 64 4096))))
(defparameter +default-exception-mask+ +network-mask+)

(defparameter *type-aliases*
  '(("xhr" . :xmlhttprequest)
    ("css" . :stylesheet)
    ("doc" . :document)
    ("frame" . :subdocument)
    ("object-subrequest" . :object)
    ("dtd" . :other))
  "Short and legacy type names used in published filter lists.")

(defparameter *weak-tokens*
  (let ((table (make-hash-table :test 'equal)))
    (dolist (token '("com" "net" "org" "www" "http" "https" "html" "php" "jpg"
                     "jpeg" "png" "gif" "css" "img" "cdn" "static" "images"
                     "image" "media" "assets" "files" "file" "content" "upload"))
      (setf (gethash token table) t))
    table)
  "Tokens that show up in too many URLs to be a selective index key.")

(defparameter *multi-suffixes*
  (let ((table (make-hash-table :test 'equal)))
    (dolist (suffix '("co.uk" "org.uk" "ac.uk" "gov.uk" "co.jp" "ne.jp" "or.jp"
                      "com.au" "net.au" "org.au" "co.nz" "co.za" "com.br" "com.mx"
                      "com.tr" "com.tw" "com.hk" "com.sg" "co.in" "com.cn" "com.ar"
                      "co.kr" "com.co" "com.ua" "co.il" "com.pl" "co.id" "com.my"
                      "com.ph" "com.vn" "co.th" "com.pk" "com.ng" "com.pe" "com.eg"))
      (setf (gethash suffix table) t))
    table)
  "Multi-label public suffixes used to tell first-party from third-party.
A host under one of these shares a site with its sibling subdomains.")

(defparameter *unsupported-selector-markers*
  '(":has-text" ":matches-css" ":matches-attr" ":xpath(" ":upward("
    ":nth-ancestor" ":min-text-length" ":watch-attr" ":remove-attr"
    ":remove-class" ":remove(" ":style(" ":-abp-contains" ":contains(" "+js(")
  "Cosmetic syntax that is not CSS. One bad selector voids the whole rule.")

(defparameter *default-filter-list-max-age* (* 4 24 60 60)
  "Seconds before a cached default list is downloaded again.")

(defparameter *default-filter-lists*
  '(("easylist.txt" "https://easylist.to/easylist/easylist.txt")
    ("easyprivacy.txt" "https://easylist.to/easylist/easyprivacy.txt")
    ("ublock-filters.txt" "https://raw.githubusercontent.com/uBlockOrigin/uAssets/master/filters/filters.txt")
    ("ublock-badware.txt" "https://raw.githubusercontent.com/uBlockOrigin/uAssets/master/filters/badware.txt")
    ("ublock-privacy.txt" "https://raw.githubusercontent.com/uBlockOrigin/uAssets/master/filters/privacy.txt")
    ("ublock-quick-fixes.txt" "https://raw.githubusercontent.com/uBlockOrigin/uAssets/master/filters/quick-fixes.txt")
    ("ublock-unbreak.txt" "https://raw.githubusercontent.com/uBlockOrigin/uAssets/master/filters/unbreak.txt")
    ("peter-lowe.txt" "https://pgl.yoyo.org/adservers/serverlist.php?hostformat=adblockplus&showintro=1&mimetype=plaintext"))
  "uBlock Origin's default lists, minus regional and annoyance lists.")

(export-always '*default-filter-lists*)

(defun split-by (char text)
  (loop with start = 0
        for pos = (position char text :start start)
        collect (subseq text start (or pos (length text)))
        while pos
        do (setf start (1+ pos))))

(defun separator-char-p (char)
  "ABP separator: anything but a letter, a digit, or one of `_', `-', `.', `%'."
  (let ((code (char-code char)))
    (and (< code 128)
         (not (alphanumericp char))
         (not (member char '(#\_ #\- #\. #\%))))))

(defun strip-fragment (url)
  (let ((hash (position #\# url)))
    (if hash (subseq url 0 hash) url)))

(defun host-bounds (url)
  "Return start and end of the host in URL, without userinfo or port."
  (let* ((scheme (search "://" url))
         (start (if scheme (+ scheme 3) 0))
         (limit (or (position-if (lambda (char) (member char '(#\/ #\? #\#)))
                                 url :start start)
                    (length url)))
         (at (position #\@ url :start start :end limit)))
    (when at
      (setf start (1+ at)))
    (when (< start limit)
      (let ((colon (position #\: url :start start :end limit)))
        (values start (or colon limit))))))

(defun url-host (url)
  (multiple-value-bind (start end) (host-bounds url)
    (when start
      (let ((host (subseq url start end)))
        (when (and (plusp (length host)) (char= (char host 0) #\[))
          (let ((close (position #\] host)))
            (when close
              (setf host (subseq host 1 close)))))
        (when (plusp (length host))
          host)))))

(defun host-suffixes (host)
  "HOST and each shorter suffix, so `a.b.com' yields `a.b.com', `b.com', and `com'."
  (when (and host (plusp (length host)))
    (let ((suffixes (list host))
          (from 0))
      (loop for dot = (position #\. host :start from)
            while dot
            do (setf from (1+ dot))
               (push (subseq host from) suffixes))
      suffixes)))

(defun host-match-positions (url)
  (multiple-value-bind (host-start host-end) (host-bounds url)
    (when host-start
      (let ((positions (list host-start))
            (from host-start))
        (loop for dot = (position #\. url :start from :end host-end)
              while dot
              do (setf from (1+ dot))
                 (push from positions))
        (nreverse positions)))))

(defun ip-address-p (host)
  (and host
       (plusp (length host))
       (every (lambda (char) (or (digit-char-p char) (char= char #\.))) host)
       (find #\. host)))

(defun registrable-domain (host)
  "eTLD+1 of HOST, using a small multi-label suffix list."
  (cond
    ((or (null host) (zerop (length host))) nil)
    ((or (find #\: host) (ip-address-p host)) host)
    (t
     (let* ((labels (split-by #\. host))
            (count (length labels)))
       (cond
         ((< count 2) host)
         ((and (>= count 3)
               (gethash (concatenate 'string
                                     (nth (- count 2) labels) "."
                                     (nth (1- count) labels))
                        *multi-suffixes*))
          (concatenate 'string
                       (nth (- count 3) labels) "."
                       (nth (- count 2) labels) "."
                       (nth (1- count) labels)))
         (t
          (concatenate 'string
                       (nth (- count 2) labels) "."
                       (nth (1- count) labels))))))))

(defun same-party-p (request-host document-host)
  (or (null request-host)
      (null document-host)
      (string= (registrable-domain request-host)
               (registrable-domain document-host))))

(defun host-matches-domain-p (host domain)
  "Whether HOST is DOMAIN or a subdomain of it. `name.*' matches that site's eTLD+1."
  (cond
    ((or (null host) (null domain) (zerop (length domain))) nil)
    ((and (alexandria:ends-with-subseq ".*" domain)
          (> (length domain) 2))
     (let ((name (subseq domain 0 (- (length domain) 1)))
           (site (registrable-domain host)))
       (and site
            (or (string= site (subseq name 0 (1- (length name))))
                (and (alexandria:starts-with-subseq name site)
                     (> (length site) (length name))
                     (char= (char site (1- (length name))) #\.))))))
    (t
     (or (string= host domain)
         (and (> (length host) (length domain))
              (alexandria:ends-with-subseq domain host)
              (char= (char host (- (length host) (length domain) 1)) #\.))))))

(defun domains-match-p (host domains excluded)
  (and (or (null domains)
           (some (lambda (domain) (host-matches-domain-p host domain)) domains))
       (notany (lambda (domain) (host-matches-domain-p host domain)) excluded)))

(defun pattern-parts (body)
  (let ((parts nil)
        (literal (make-array 16 :element-type 'character :fill-pointer 0 :adjustable t)))
    (flet ((flush ()
             (when (plusp (fill-pointer literal))
               (push (copy-seq literal) parts)
               (setf (fill-pointer literal) 0))))
      (loop for char across body
            do (case char
                 (#\* (flush)
                  (unless (eq (first parts) :any)
                    (push :any parts)))
                 (#\^ (flush) (push :sep parts))
                 (t (vector-push-extend char literal))))
      (flush))
    (coerce (nreverse parts) 'simple-vector)))

(defun match-from (parts text part ti right)
  (cond
    ((>= part (length parts))
     (or (not right) (>= ti (length text))))
    ((eq (aref parts part) :any)
     (if (= (1+ part) (length parts))
         t
         (loop for next from ti to (length text)
               thereis (match-from parts text (1+ part) next right))))
    ((eq (aref parts part) :sep)
     (or (and (>= ti (length text))
              (match-from parts text (1+ part) ti right))
         (and (< ti (length text))
              (separator-char-p (char text ti))
              (match-from parts text (1+ part) (1+ ti) right))))
    (t
     (let* ((literal (aref parts part))
            (end (+ ti (length literal))))
       (and (<= end (length text))
            (string= literal text :start2 ti :end2 end)
            (match-from parts text (1+ part) end right))))))

(defun unanchored-match (parts text right)
  (cond
    ((zerop (length parts)) (or (not right) (zerop (length text))))
    ((stringp (aref parts 0))
     (let ((literal (aref parts 0))
           (from 0))
       (loop for pos = (search literal text :start2 from)
             while pos
             thereis (match-from parts text 0 pos right)
             do (setf from (1+ pos)))))
    (t
     (loop for pos from 0 to (length text)
           thereis (match-from parts text 0 pos right)))))

(defun parse-anchors (pattern)
  (let ((body pattern)
        (hostname nil)
        (left nil)
        (right nil))
    (when (alexandria:starts-with-subseq "||" body)
      (setf hostname t
            body (subseq body 2)))
    (when (and (plusp (length body)) (char= (char body 0) #\|))
      (setf left t
            body (subseq body 1)))
    (when (and (plusp (length body)) (char= (char body (1- (length body))) #\|))
      (setf right t
            body (subseq body 0 (1- (length body)))))
    (values body hostname left right)))

(defun hostname-literal-sep (url literal)
  (multiple-value-bind (host-start host-end) (host-bounds url)
    (when host-start
      (let ((pos host-start)
            (size (length literal)))
        (loop
          (let ((next (+ pos size)))
            (when (and (<= next (length url))
                       (string= literal url :start2 pos :end2 next)
                       (or (= next (length url))
                           (separator-char-p (char url next))))
              (return t)))
          (let ((dot (position #\. url :start pos :end host-end)))
            (unless dot
              (return nil))
            (setf pos (1+ dot))))))))

(defun compile-matcher (pattern match-case)
  "Return a function of (original folded) that matches PATTERN against a URL.
FOLDED is PATTERN's URL downcased and without a fragment. Matchers that are
not `$match-case' read FOLDED. Regex filters keep their own case."
  (let ((regex (regex-parts pattern)))
    (if regex
        (regex-matcher (car regex) (cdr regex))
        (let ((source (if match-case pattern (string-downcase pattern))))
          (multiple-value-bind (body hostname left right) (parse-anchors source)
          (let ((parts (pattern-parts body)))
            (cond
              ((zerop (length parts))
               (constantly t))
              ((and hostname (not right) (not match-case)
                    (= (length parts) 2)
                    (stringp (aref parts 0))
                    (eq (aref parts 1) :sep)
                    (not (find #\/ (aref parts 0))))
               (let ((literal (aref parts 0)))
                 (lambda (original folded)
                   (declare (ignore original))
                   (hostname-literal-sep folded literal))))
              (t
               (lambda (original folded)
                 (let ((text (if match-case original folded)))
                   (cond
                     (hostname
                      (some (lambda (pos)
                              (match-from parts text 0 pos right))
                            (host-match-positions text)))
                     (left (match-from parts text 0 0 right))
                     (t (unanchored-match parts text right)))))))))))))

(defun regex-delimiter (pattern)
  "Index of the closing slash of a `/regex/flags' filter, or NIL.
Flags are letters. A path such as `/banner/*/img^' starts with a slash and
contains another one, and that is still a plain filter."
  (when (and (>= (length pattern) 2) (char= (char pattern 0) #\/))
    (let ((close (loop with escaped = nil
                       with found = nil
                       for index from 1 below (length pattern)
                       for char = (char pattern index)
                       do (cond
                            (escaped (setf escaped nil))
                            ((char= char #\\) (setf escaped t))
                            ((char= char #\/) (setf found index)))
                       finally (return found))))
      ;; uBlock's only regex flag is `i`. A trailing word such as `/amp/analytics'
      ;; is a path, not a flag list.
      (when (and close
                 (> close 1)
                 (loop for index from (1+ close) below (length pattern)
                       always (char= (char pattern index) #\i)))
        close))))

(defun regex-parts (pattern)
  "Split a `/pattern/flags' filter. NIL when PATTERN is not a regex filter."
  (let ((close (regex-delimiter pattern)))
    (when close
      (cons (subseq pattern 1 close)
            (subseq pattern (1+ close))))))

(defun regex-matcher (source flags)
  (handler-case
      (let ((scanner (ppcre:create-scanner
                      source
                      :case-insensitive-mode (find #\i flags))))
        (lambda (original folded)
          (declare (ignore folded))
          (ppcre:scan scanner original)))
    (error ()
      (constantly nil))))

(defun split-filter-options (body)
  "Split a filter body into pattern and option text. `$` inside a regex stays put."
  (let* ((close (regex-delimiter body))
         (dollar (if close
                     (position #\$ body :start (1+ close))
                     (position #\$ body))))
    (if dollar
        (values (subseq body 0 dollar) (subseq body (1+ dollar)))
        (values body nil))))

(defun literal-host-key (pattern)
  "Hostname of a `||host' filter, when that hostname is a literal index key."
  (when (alexandria:starts-with-subseq "||" pattern)
    (let* ((rest (subseq pattern 2))
           (end (position-if (lambda (char)
                               (member char '(#\^ #\/ #\* #\? #\| #\#)))
                             rest)))
      (when (and end (char= (char rest end) #\*))
        (return-from literal-host-key nil))
      (let ((host (string-downcase (subseq rest 0 (or end (length rest))))))
        (when (and (plusp (length host))
                   (every (lambda (char)
                            (or (alphanumericp char) (member char '(#\. #\-))))
                          host)
                   (not (char= (char host 0) #\.))
                   (not (char= (char host (1- (length host))) #\.)))
          host)))))

(defun text-tokens (text)
  (let ((tokens nil)
        (start nil))
    (loop for index from 0 to (length text)
          for char = (when (< index (length text)) (char text index))
          do (cond
               ((and char (alphanumericp char))
                (unless start
                  (setf start index)))
               (start
                (when (>= (- index start) 3)
                  (push (subseq text start index) tokens))
                (setf start nil))))
    tokens))

(defun choose-token (pattern)
  (let ((tokens (text-tokens (string-downcase pattern))))
    (flet ((longest (candidates)
             (reduce (lambda (left right)
                       (if (> (length left) (length right)) left right))
                     candidates)))
      (let* ((strong (remove-if (lambda (token)
                                  (or (gethash token *weak-tokens*)
                                      (< (length token) 5)))
                                tokens))
             (usable (or strong
                         (remove-if (lambda (token) (gethash token *weak-tokens*)) tokens)
                         tokens)))
        (when usable
          (longest usable))))))

(defstruct network-filter
  (pattern "" :type string)
  (key "" :type string)
  (matcher (constantly nil) :type function)
  (type-mask 0 :type integer)
  (party nil)
  (domains nil :type list)
  (excluded-domains nil :type list)
  (to-domains nil :type list)
  (to-excluded nil :type list)
  (exception-p nil)
  (important-p nil)
  (active-p t)
  (host-key nil)
  (token-key nil)
  (replacements nil :type list)
  (stamp 0 :type integer))

(defstruct cosmetic-rule
  (selector "" :type string)
  (domains nil :type list)
  (excluded-domains nil :type list)
  (exception-p nil))

(defstruct pp-frame
  active-p
  parent-active-p
  condition-p
  else-seen-p)

(defun preprocessor-ident-true-p (name)
  (member name '("true" "ext_ublock" "env_chromium" "cap_user_stylesheet")
          :test #'string-equal))

(defun preprocessor-true-p (expression)
  (let ((index 0)
        (text (or expression "")))
    (labels ((peek ()
               (loop while (and (< index (length text))
                                (member (char text index) '(#\Space #\Tab)))
                     do (incf index))
               (when (< index (length text))
                 (char text index)))
             (ident ()
               (let ((start index))
                 (loop while (and (< index (length text))
                                  (or (alphanumericp (char text index))
                                      (char= (char text index) #\_)))
                       do (incf index))
                 (subseq text start index)))
             (primary ()
               (cond
                 ((eql (peek) #\()
                  (incf index)
                  (let ((value (parse-or)))
                    (when (eql (peek) #\))
                      (incf index))
                    value))
                 ((null (peek)) nil)
                 (t (preprocessor-ident-true-p (ident)))))
             (parse-not ()
               (if (eql (peek) #\!)
                   (progn (incf index) (not (parse-not)))
                   (primary)))
             (parse-and ()
               (let ((value (parse-not)))
                 (loop
                   (if (and (eql (peek) #\&)
                            (< (1+ index) (length text))
                            (char= (char text (1+ index)) #\&))
                       (let ((right (progn (incf index 2) (parse-not))))
                         (setf value (and value right)))
                       (return value)))))
             (parse-or ()
               (let ((value (parse-and)))
                 (loop
                   (if (and (eql (peek) #\|)
                            (< (1+ index) (length text))
                            (char= (char text (1+ index)) #\|))
                       (let ((right (progn (incf index 2) (parse-and))))
                         (setf value (or value right)))
                       (return value))))))
      (parse-or))))

(defun including-p (stack)
  (or (null stack) (pp-frame-active-p (first stack))))

(defun type-bit (type)
  (or (cdr (assoc type *network-bits*))
      (cdr (assoc :other *network-bits*))))

(defun canonicalize-type (name)
  (let* ((text (string-downcase (or name "other")))
         (alias (cdr (assoc text *type-aliases* :test #'string=)))
         (keyword (or alias (intern (string-upcase text) :keyword))))
    (if (assoc keyword *network-bits*)
        keyword
        :other)))

(defun parse-domain-list (value)
  (let ((yes nil)
        (no nil))
    (dolist (part (split-by #\| (or value "")))
      (let ((part (string-downcase (string-trim '(#\Space #\Tab) part))))
        (cond
          ((zerop (length part)))
          ((char= (char part 0) #\~) (push (subseq part 1) no))
          (t (push part yes)))))
    (values yes no)))

(defun option-type (name)
  (let ((alias (cdr (assoc name *type-aliases* :test #'string=))))
    (cond
      (alias alias)
      ((string= name "all") :all)
      (t
       (let ((keyword (intern (string-upcase name) :keyword)))
         (when (assoc keyword *network-bits*)
           keyword))))))

(defun pseudo-bit (name)
  (cond
    ((or (string= name "elemhide") (string= name "ehide")) +elemhide-bit+)
    ((or (string= name "generichide") (string= name "ghide")) +generichide-bit+)
    ((or (string= name "specificelemhide") (string= name "shide")) +specificelemhide-bit+)
    ((string= name "genericblock") +genericblock-bit+)))

(defun known-bare-option-p (name)
  (or (option-type name)
      (pseudo-bit name)
      (member name '("important" "match-case" "badfilter" "third-party" "first-party"
                     "strict1p" "strict3p" "1p" "3p" "all" "empty" "redirect")
              :test #'string=)))

(defun unsupported-option-p (options)
  (some (lambda (marker)
          (search marker options))
        '("csp=" "removeparam=" "rewrite=" "redirect-rule" "header=" "method="
          "permissions=" "denyallow=" "urlskip=" "reason=" "queryprune=")))

(defun replace-metacharacter-p (char)
  "Characters that make a `$replace' pattern a regular expression."
  (member char '(#\. #\* #\+ #\? #\^ #\$ #\{ #\} #\( #\) #\| #\[ #\] #\\)))

(defun parse-replace-value (value)
  "Parse a literal `$replace=/pattern/replacement/' option.
Returns (PATTERN . REPLACEMENT), or NIL when VALUE is absent or a regex.
uBlock applies a regex here. A regex left in place would be searched for as
text, so those options are refused and the filter is dropped."
  (when (and value (>= (length value) 4) (char= (char value 0) #\/))
    (let ((index 1))
      (labels ((read-piece ()
                 (let ((chars nil)
                       (escaped nil))
                   (loop while (< index (length value))
                         for char = (char value index)
                         do (incf index)
                         (cond
                           (escaped
                            (push char chars)
                            (setf escaped nil))
                           ((char= char #\\)
                            (setf escaped t))
                           ((char= char #\/)
                            (return (coerce (nreverse chars) 'string)))
                           (t (push char chars)))
                         finally (return nil)))))
        (let ((pattern (read-piece))
              (replacement (read-piece)))
          (when (and pattern replacement (plusp (length pattern))
                     (notany #'replace-metacharacter-p pattern))
            (cons pattern replacement)))))))

(defun filter-key (exception-p body tokens match-case)
  (format nil "~a~a~@[~a~]"
          (if exception-p "@@" "")
          (if match-case body (string-downcase body))
          (when tokens
            (format nil "$~{~a~^,~}" (sort (copy-list tokens) #'string<)))))

(defun parse-network-filter (line)
  "Parse one network filter. Returns :badfilter and a key, :filter and a filter, or NIL."
  (let* ((exception (alexandria:starts-with-subseq "@@" line))
         (rest (if exception (subseq line 2) line)))
    (multiple-value-bind (body options) (split-filter-options rest)
      (when (and options (unsupported-option-p options))
        (return-from parse-network-filter nil))
      (let ((mask (if exception +default-exception-mask+ +default-block-mask+))
            (explicit nil)
            (tokens nil)
            (badfilter nil)
            (party nil)
            (domains nil)
            (excluded nil)
            (to-domains nil)
            (to-excluded nil)
            (important nil)
            (match-case nil)
            (replacements nil)
            (unknown nil))
        (dolist (raw (if options (split-by #\, options) nil))
          (let* ((text (string-trim '(#\Space #\Tab) raw))
                 (negated (and (plusp (length text)) (char= (char text 0) #\~)))
                 (name (if negated (subseq text 1) text))
                 (equals (position #\= name))
                 (key (string-downcase (if equals (subseq name 0 equals) name)))
                 (value (when equals (subseq name (1+ equals)))))
            (cond
              ((zerop (length key)))
              ((string= key "badfilter") (setf badfilter t))
              ((string= key "important") (setf important (not negated)))
              ((string= key "match-case") (setf match-case (not negated)))
              ((or (string= key "domain") (string= key "from"))
               (multiple-value-bind (yes no) (parse-domain-list value)
                 (setf domains yes excluded no))
               (push (format nil "domain=~a" (string-downcase (or value ""))) tokens))
              ((string= key "to")
               (multiple-value-bind (yes no) (parse-domain-list value)
                 (setf to-domains yes to-excluded no))
               (push (format nil "to=~a" (string-downcase (or value ""))) tokens))
              ((string= key "replace")
               (let ((parsed (parse-replace-value value)))
                 (unless parsed
                   (setf unknown t))
                 (when parsed
                   (setf replacements (append replacements (list parsed)))
                   (push (format nil "replace=~a" (string-downcase (or value ""))) tokens))))
              ((member key '("redirect" "empty") :test #'string=)
               (push (if value (format nil "~a=~a" key value) key) tokens))
              ((or (string= key "third-party") (string= key "3p"))
               (setf party (if negated :first :third))
               (push (if negated "~third-party" "third-party") tokens))
              ((or (string= key "first-party") (string= key "1p"))
               (setf party (if negated :third :first))
               (push (if negated "~first-party" "first-party") tokens))
              ((string= key "strict3p")
               (setf party (if negated :strict-first :strict-third))
               (push (if negated "~strict3p" "strict3p") tokens))
              ((string= key "strict1p")
               (setf party (if negated :strict-third :strict-first))
               (push (if negated "~strict1p" "strict1p") tokens))
              ((pseudo-bit key)
               (let ((bit (pseudo-bit key)))
                 ;; A positive type replaces the default mask. A negation only
                 ;; clears that bit, so `$~script' still matches images.
                 (cond
                   (negated
                    (setf mask (logand mask (lognot bit))))
                   (t
                    (unless explicit
                      (setf mask 0
                            explicit t))
                    (setf mask (logior mask bit)))))
               (push (if negated (concatenate 'string "~" key) key) tokens))
              ((option-type key)
               (let* ((type (option-type key))
                      (bit (if (eq type :all) +network-mask+ (type-bit type))))
                 (cond
                   (negated
                    (setf mask (logand mask (lognot bit))))
                   (t
                    (unless explicit
                      (setf mask 0
                            explicit t))
                    (setf mask (logior mask bit))))
                 (push (if negated
                           (concatenate 'string "~" (string-downcase (symbol-name type)))
                           (if (eq type :all)
                               "all"
                               (string-downcase (symbol-name type))))
                       tokens)))
              ((not (known-bare-option-p key))
               (setf unknown t)))))
        (when unknown
          (return-from parse-network-filter nil))
        (when important
          (push "important" tokens))
        (when match-case
          (push "match-case" tokens))
        (let ((key (filter-key exception body tokens match-case)))
          (if badfilter
              (values :badfilter key)
              (let* ((regex (regex-parts body))
                     (folded (if match-case body (string-downcase body)))
                     (host (unless regex
                             (literal-host-key folded)))
                     ;; A literal sliced out of a regex is not a standalone
                     ;; URL token: /ads[0-9]+\.js/ matches ads12.js. Regex
                     ;; filters stay on the list checked for every request.
                     (token (unless (or regex host)
                              (choose-token folded))))
                (values :filter
                        (make-network-filter
                         :pattern body
                         :key key
                         :matcher (compile-matcher body match-case)
                         :type-mask mask
                         :party party
                         :domains domains
                         :excluded-domains excluded
                         :to-domains to-domains
                         :to-excluded to-excluded
                         :exception-p exception
                         :important-p important
                         :host-key host
                         :token-key (unless host token)
                         :replacements replacements)))))))))

(defun normalize-selector (selector)
  (let ((text (string-trim '(#\Space #\Tab #\Return) selector)))
    (loop for start = (search ":-abp-has(" text)
          while start
          do (setf text (concatenate 'string
                                     (subseq text 0 start)
                                     ":has("
                                     (subseq text (+ start (length ":-abp-has("))))))
    text))

(defun usable-selector-p (selector)
  (and (plusp (length selector))
       (not (char= (char selector 0) #\^))
       (not (char= (char selector 0) #\+))
       (not (find #\{ selector))
       (not (find #\} selector))
       (not (find #\< selector))
       (not (find #\> selector))
       (not (search ":-abp-" selector))
       (not (some (lambda (marker) (search marker selector))
                  *unsupported-selector-markers*))))

(defun cosmetic-anchor (line)
  (dolist (anchor '(("#@?#" . :extended-exception)
                    ("#?#" . :extended)
                    ("#@$#" . :snippet-exception)
                    ("#$#" . :snippet)
                    ("#@#" . :exception)
                    ("##" . :hide)))
    (let ((pos (search (car anchor) line)))
      (when pos
        (return (values (car anchor) pos (cdr anchor)))))))

(defun cosmetic-domains-p (text)
  (and (not (search "://" text))
       (not (search "/" text))
       (not (search "|" text))
       (not (search "$" text))
       (not (alexandria:starts-with-subseq "@@" text))))

(defun parse-domain-list-comma (text)
  (let ((yes nil)
        (no nil))
    (dolist (part (split-by #\, text))
      (let ((part (string-downcase (string-trim '(#\Space #\Tab) part))))
        (cond
          ((zerop (length part)))
          ((char= (char part 0) #\~) (push (subseq part 1) no))
          (t (push part yes)))))
    (values yes no)))

(defun parse-cosmetic-filter (line)
  (multiple-value-bind (anchor pos kind) (cosmetic-anchor line)
    (declare (ignore anchor))
    (when (and kind
               (not (member kind '(:snippet :snippet-exception)))
               (cosmetic-domains-p (subseq line 0 pos)))
      (let ((selector (normalize-selector
                       (subseq line (+ pos (ecase kind
                                            (:hide 2)
                                            (:exception 3)
                                            (:extended 3)
                                            (:extended-exception 4)))))))
        (when (usable-selector-p selector)
          (multiple-value-bind (domains excluded)
              (parse-domain-list-comma (subseq line 0 pos))
            (make-cosmetic-rule :selector selector
                                :domains domains
                                :excluded-domains excluded
                                :exception-p (member kind '(:exception :extended-exception)))))))))

(defun clean-line (line)
  (let ((text (string-trim '(#\Space #\Tab #\Return #\Newline #\Page) line)))
    (if (and (plusp (length text)) (char= (char text 0) #\ufeff))
        (string-left-trim '(#\ufeff) text)
        text)))

(defun directive (line)
  (cond
    ((or (string= line "!#if") (alexandria:starts-with-subseq "!#if " line))
     (values :if (string-trim '(#\Space #\Tab) (subseq line (min 4 (length line))))))
    ((or (string= line "!#else") (alexandria:starts-with-subseq "!#else " line))
     :else)
    ((or (string= line "!#endif") (alexandria:starts-with-subseq "!#endif " line))
     :endif)
    ((or (string= line "!#include") (alexandria:starts-with-subseq "!#include " line))
     (values :include (string-trim '(#\Space #\Tab #\") (subseq line (min 9 (length line))))))))

(defstruct parsed-batch
  (filters nil :type list)
  (badfilters nil :type list)
  (cosmetics nil :type list))

(defun parse-filter-stream (stream &optional directory seen)
  (let ((batch (make-parsed-batch))
        (stack nil)
        (seen (or seen (make-hash-table :test 'equal))))
    (loop for raw = (read-line stream nil)
          while raw
          for line = (clean-line raw)
          do (multiple-value-bind (kind argument) (directive line)
               (case kind
                 (:if
                  (let* ((parent (including-p stack))
                         (condition (preprocessor-true-p argument)))
                    (push (make-pp-frame :active-p (and parent condition)
                                         :parent-active-p parent
                                         :condition-p condition)
                          stack)))
                 (:else
                  (when (and stack (not (pp-frame-else-seen-p (first stack))))
                    (setf (pp-frame-active-p (first stack))
                          (and (pp-frame-parent-active-p (first stack))
                               (not (pp-frame-condition-p (first stack))))
                          (pp-frame-else-seen-p (first stack)) t)))
                 (:endif
                  (when stack
                    (pop stack)))
                 (:include
                  (when (and (including-p stack) directory argument (plusp (length argument)))
                    (let ((path (uiop:merge-pathnames* argument directory)))
                      (when (uiop:file-exists-p path)
                        (let ((name (namestring (truename path))))
                          (unless (gethash name seen)
                            (setf (gethash name seen) t)
                            (with-open-file (included name :external-format :utf-8)
                              (let ((nested (parse-filter-stream included directory seen)))
                                (setf (parsed-batch-filters batch)
                                      (nconc (parsed-batch-filters nested)
                                             (parsed-batch-filters batch))
                                      (parsed-batch-badfilters batch)
                                      (nconc (parsed-batch-badfilters nested)
                                             (parsed-batch-badfilters batch))
                                      (parsed-batch-cosmetics batch)
                                      (nconc (parsed-batch-cosmetics nested)
                                             (parsed-batch-cosmetics batch)))))))))))
                 (t
                  (when (and (including-p stack)
                             (plusp (length line))
                             (not (char= (char line 0) #\!))
                             (not (char= (char line 0) #\[)))
                    (let ((cosmetic (parse-cosmetic-filter line)))
                      (cond
                        (cosmetic
                         (push cosmetic (parsed-batch-cosmetics batch)))
                        ;; A cosmetic anchor that we cannot apply (`##+js`, `:has-text')
                        ;; must not fall through and block URLs that contain the line.
                        ((nth-value 2 (cosmetic-anchor line)))
                        (t
                         (multiple-value-bind (parsed-kind payload) (parse-network-filter line)
                           (case parsed-kind
                             (:filter (push payload (parsed-batch-filters batch)))
                             (:badfilter (push payload (parsed-batch-badfilters batch)))))))))))))
    batch))

(defun parse-filter-text (text &optional directory)
  (with-input-from-string (stream text)
    (parse-filter-stream stream directory)))

(define-class adblocker ()
  ((update-lock
    (bt:make-recursive-lock "cef-adblock")
    :documentation "Held while indexes are read or replaced.")
   (host-index
    (make-hash-table :test 'equal)
    :documentation "Hostname to the filters anchored at that host.")
   (token-index
    (make-hash-table :test 'equal)
    :documentation "Literal token to the filters that contain it.")
   (generic-filters
    nil
    :type list
    :documentation "Filters with no hostname and no usable token.")
   (page-exceptions
    nil
    :type list
    :documentation "elemhide, generichide, specificelemhide, and genericblock rules.")
   (by-key
    (make-hash-table :test 'equal)
    :documentation "Filter key to the filters it names, so `$badfilter' can retire them.")
   (disabled
    (make-hash-table :test 'equal)
    :documentation "Keys removed by `$badfilter'.")
   (generic-selectors
    nil
    :type list)
   (specific-rules
    nil
    :type list)
   (css-cache
    (make-hash-table :test 'equal)
    :documentation "Cache key to the stylesheet for that page.")
   (genericblock-cache
    (make-hash-table :test 'equal))
   (generation
    0
    :type integer
    :documentation "Bumped on each match so a filter is tested once."))
  (:export-class-name-p t)
  (:documentation "Engine for uBlock Origin and Adblock Plus filter lists.
`load-filters' reads lists. `blocked-p' answers a request. `enable' installs
the engine on CEF's network path, and `default-block' loads uBlock Origin's
default lists."))

(defun network-match-p (mask)
  (plusp (logand mask +network-mask+)))

(defun page-exception-p (mask)
  (plusp (logand mask (logior +elemhide-bit+ +generichide-bit+
                              +specificelemhide-bit+ +genericblock-bit+))))

(defun index-filter (adblocker filter)
  (when (page-exception-p (network-filter-type-mask filter))
    (push filter (page-exceptions adblocker)))
  (when (network-match-p (network-filter-type-mask filter))
    (cond
      ((network-filter-host-key filter)
       (push filter (gethash (network-filter-host-key filter) (host-index adblocker))))
      ((network-filter-token-key filter)
       (push filter (gethash (network-filter-token-key filter) (token-index adblocker))))
      (t
       (push filter (generic-filters adblocker)))))
  (push filter (gethash (network-filter-key filter) (by-key adblocker))))

(defun index-cosmetic (adblocker rule)
  (if (and (null (cosmetic-rule-domains rule))
           (null (cosmetic-rule-excluded-domains rule))
           (not (cosmetic-rule-exception-p rule)))
      (push (cosmetic-rule-selector rule) (generic-selectors adblocker))
      (push rule (specific-rules adblocker))))

(defun commit-batch (adblocker batch)
  (bt:with-recursive-lock-held ((update-lock adblocker))
    (dolist (key (parsed-batch-badfilters batch))
      (setf (gethash key (disabled adblocker)) t)
      (dolist (filter (gethash key (by-key adblocker)))
        (setf (network-filter-active-p filter) nil)))
    (dolist (filter (parsed-batch-filters batch))
      (unless (gethash (network-filter-key filter) (disabled adblocker))
        (index-filter adblocker filter)))
    (dolist (rule (parsed-batch-cosmetics batch))
      (index-cosmetic adblocker rule))
    (clrhash (css-cache adblocker))
    (clrhash (genericblock-cache adblocker)))
  (reset-adblock-cosmetic)
  adblocker)

(defun party-matches-p (filter request-host document-host)
  (case (network-filter-party filter)
    ((nil) t)
    (:third (not (same-party-p request-host document-host)))
    (:first (same-party-p request-host document-host))
    (:strict-third
     (and request-host document-host (not (string= request-host document-host))))
    (:strict-first
     (and request-host document-host (string= request-host document-host)))))

(defun filter-matches-p (filter url folded type request-host document-host)
  (and (network-filter-active-p filter)
       (plusp (logand (network-filter-type-mask filter) (type-bit type)))
       (party-matches-p filter request-host document-host)
       (domains-match-p document-host
                        (network-filter-domains filter)
                        (network-filter-excluded-domains filter))
       (domains-match-p request-host
                        (network-filter-to-domains filter)
                        (network-filter-to-excluded filter))
       (funcall (network-filter-matcher filter) url folded)))

(defun each-candidate (adblocker host folded function)
  (let ((generation (incf (generation adblocker))))
    (flet ((visit (filter)
             (unless (= (network-filter-stamp filter) generation)
               (setf (network-filter-stamp filter) generation)
               (funcall function filter))))
      (dolist (filter (generic-filters adblocker))
        (visit filter))
      (dolist (suffix (host-suffixes host))
        (dolist (filter (gethash suffix (host-index adblocker)))
          (visit filter)))
      (dolist (token (text-tokens folded))
        (dolist (filter (gethash token (token-index adblocker)))
          (visit filter))))))

(defun generic-filter-p (filter)
  (and (null (network-filter-domains filter))
       (null (network-filter-excluded-domains filter))))

(defun document-genericblock-p (adblocker document-url folded-document document-host)
  (let ((cached (gethash folded-document (genericblock-cache adblocker))))
    (cond
      ((eq cached :yes) t)
      ((eq cached :no) nil)
      (t
       (let ((answer
               (some (lambda (filter)
                       (and (network-filter-active-p filter)
                            (plusp (logand (network-filter-type-mask filter) +genericblock-bit+))
                            (party-matches-p filter document-host document-host)
                            (domains-match-p document-host
                                             (network-filter-domains filter)
                                             (network-filter-excluded-domains filter))
                            (funcall (network-filter-matcher filter)
                                     document-url folded-document)))
                     (page-exceptions adblocker))))
         (when (> (hash-table-count (genericblock-cache adblocker)) 1024)
           (clrhash (genericblock-cache adblocker)))
         (setf (gethash folded-document (genericblock-cache adblocker))
               (if answer :yes :no))
         answer)))))

(defun decide-blocked-p (adblocker url document-url type)
  (let* ((url (strip-fragment url))
         (document-url (strip-fragment (or document-url url)))
         (folded (string-downcase url))
         (folded-document (if (string= url document-url)
                              folded
                              (string-downcase document-url)))
         (request-host (url-host folded))
         (document-host (url-host folded-document))
         (genericblock (document-genericblock-p adblocker document-url folded-document document-host))
         (important-allow nil)
         (important-block nil)
         (allow nil)
         (block nil))
    (each-candidate
     adblocker request-host folded
     (lambda (filter)
       (when (and (null (network-filter-replacements filter))
                  (or (not genericblock)
                      (network-filter-exception-p filter)
                      (not (generic-filter-p filter)))
                  (filter-matches-p filter url folded type request-host document-host))
         (cond
           ((and (network-filter-exception-p filter) (network-filter-important-p filter))
            (setf important-allow t))
           ((network-filter-important-p filter)
            (setf important-block t))
           ((network-filter-exception-p filter)
            (setf allow t))
           (t
            (setf block t))))))
    (cond
      (important-allow nil)
      (important-block t)
      (allow nil)
      (t block))))

(defun page-flag-p (adblocker url folded host bit)
  (some (lambda (filter)
          (and (network-filter-active-p filter)
               (plusp (logand (network-filter-type-mask filter) bit))
               (domains-match-p host
                                (network-filter-domains filter)
                                (network-filter-excluded-domains filter))
               (funcall (network-filter-matcher filter) url folded)))
        (page-exceptions adblocker)))

(defun selectors-to-css (selectors)
  (if (null selectors)
      ""
      (with-output-to-string (stream)
        (loop while selectors
              for chunk = (loop repeat 500
                                while selectors
                                collect (pop selectors))
              do (format stream "~{~a~^,~} { display: none !important; }~%" chunk)))))

(defun matching-selectors (rules host excluded)
  (let ((found nil))
    (dolist (rule rules)
      (when (and (not (cosmetic-rule-exception-p rule))
                 (domains-match-p host
                                  (cosmetic-rule-domains rule)
                                  (cosmetic-rule-excluded-domains rule))
                 (not (gethash (cosmetic-rule-selector rule) excluded)))
        (push (cosmetic-rule-selector rule) found)))
    found))

(defun excluded-selectors (rules host)
  (let ((table (make-hash-table :test 'equal)))
    (dolist (rule rules)
      (when (and (cosmetic-rule-exception-p rule)
                 (domains-match-p host
                                  (cosmetic-rule-domains rule)
                                  (cosmetic-rule-excluded-domains rule)))
        (setf (gethash (cosmetic-rule-selector rule) table) t)))
    table))

(defun build-cosmetic-css (adblocker url)
  (let* ((url (strip-fragment url))
         (folded (string-downcase url))
         (host (or (url-host folded) ""))
         (elemhide (page-flag-p adblocker url folded host +elemhide-bit+))
         (generichide (page-flag-p adblocker url folded host +generichide-bit+))
         (specific (page-flag-p adblocker url folded host +specificelemhide-bit+))
         ;; The sheet depends on the host and on which hide-exceptions matched.
         ;; Keying that way shares one copy across every URL on the host.
         (key (format nil "~a|~:[~;e~]~:[~;g~]~:[~;s~]"
                      host elemhide generichide specific))
         (cached (gethash key (css-cache adblocker))))
    (if cached
        cached
        (progn
          (when (> (hash-table-count (css-cache adblocker)) 256)
            (clrhash (css-cache adblocker)))
          (setf (gethash key (css-cache adblocker))
                (if elemhide
                    ""
                    (let ((excluded (excluded-selectors (specific-rules adblocker) host))
                          (selectors nil))
                      (unless generichide
                        (dolist (selector (generic-selectors adblocker))
                          (unless (gethash selector excluded)
                            (push selector selectors))))
                      (unless specific
                        (setf selectors
                              (nconc (matching-selectors (specific-rules adblocker) host excluded)
                                     selectors)))
                      (selectors-to-css selectors))))))))

(defun add-replacement (pairs pair)
  (if (member pair pairs :test #'equal)
      pairs
      (append pairs (list pair))))

(defun youtube-host-p (host)
  "Whether HOST is a YouTube site whose player response carries pre-roll ads."
  (and host
       (or (member host '("youtube.com" "youtube-nocookie.com" "youtubekids.com")
                   :test #'string=)
           (alexandria:ends-with-subseq ".youtube.com" host)
           (alexandria:ends-with-subseq ".youtube-nocookie.com" host)
           (alexandria:ends-with-subseq ".youtubekids.com" host))))

(defparameter *youtube-player-replacements*
  '(("\"adPlacements\"" . "\"no_ads\"")
    ("\"adSlots\"" . "\"no_ads\""))
  "Renames Firefox uBlock Origin applies to YouTube player responses.
The player looks the original keys up before it starts the video. A missing
key means there is no pre-roll.")

(defparameter *youtube-document-replacements*
  '(("\"adPlacements\"" . "\"no_ads\"")
    ("\"playerAds\"" . "\"no_ads\"")
    ("\"adSlots\"" . "\"no_ads\""))
  "Renames for the watch document. `ytInitialPlayerResponse' is embedded in
the HTML, and uBlock's `set' scriptlet undefines these three properties on it
before the player reads them.")

(defun youtube-player-response-p (folded host document-host type)
  "Whether this is a YouTube player fetch rather than the video stream.
`fetch' is reported as xhr on some CEF builds and as other on others. The
document has to be a YouTube page, so a third-party site that mentions the
player URL is left alone."
  (and (member type '(:xmlhttprequest :other))
       (youtube-host-p document-host)
       (or (search "/youtubei/v1/player" folded)
           (search "/youtubei/v1/get_watch" folded)
           (and (host-matches-domain-p host "www.youtube.com")
                (or (search "/watch?" folded)
                    (search "/playlist?list=" folded))))))

(defun youtube-ad-replacements (url document-url type)
  "Body renames that remove a YouTube pre-roll before the player sees it.
NIL when URL is not one of those responses."
  (let* ((folded (string-downcase (strip-fragment url)))
         (document (string-downcase (strip-fragment (or document-url url))))
         (host (url-host folded))
         (document-host (url-host document)))
    (cond
      ((youtube-player-response-p folded host document-host type)
       *youtube-player-replacements*)
      ((and (member type '(:document :subdocument))
            (youtube-host-p host))
       *youtube-document-replacements*))))

(defun collect-replacements (adblocker url document-url type)
  (let* ((url (strip-fragment url))
         (document-url (strip-fragment (or document-url url)))
         (folded (string-downcase url))
         (folded-document (if (string= url document-url)
                              folded
                              (string-downcase document-url)))
         (request-host (url-host folded))
         (document-host (url-host folded-document))
         (pairs nil))
    (each-candidate
     adblocker request-host folded
     (lambda (filter)
       (when (and (network-filter-replacements filter)
                  (not (network-filter-exception-p filter))
                  (filter-matches-p filter url folded type request-host document-host))
         (dolist (pair (network-filter-replacements filter))
           (setf pairs (add-replacement pairs pair))))))
    (dolist (pair (youtube-ad-replacements url document-url type))
      (setf pairs (add-replacement pairs pair)))
    pairs))

(defun encode-replacements (pairs)
  "Length-prefixed UTF-8 encoding of (NEEDLE . REPLACEMENT) pairs.
The C bridge reads the lengths as bytes. Empty when there is nothing to rewrite."
  (if (null pairs)
      ""
      (with-output-to-string (stream)
        (format stream "~d~%" (length pairs))
        (dolist (pair pairs)
          (let ((from (car pair))
                (to (cdr pair)))
            (format stream "~d~%" (length (babel:string-to-octets from :encoding :utf-8)))
            (write-string from stream)
            (format stream "~d~%" (length (babel:string-to-octets to :encoding :utf-8)))
            (write-string to stream))))))

(export-always 'response-replacements)
(defmethod response-replacements ((adblocker adblocker) url &key (document-url url) (type :other))
  "Alist of literal body substitutions for URL.
Each entry is (NEEDLE . REPLACEMENT). The request is still loaded. Empty when
the body should pass through unchanged."
  (let ((type (if (keywordp type) type (canonicalize-type type))))
    (bt:with-recursive-lock-held ((update-lock adblocker))
      (collect-replacements adblocker url document-url type))))

(export-always 'blocked-p)
(defmethod blocked-p ((adblocker adblocker) url &key (document-url url) (type :other))
  "Whether ADBLOCKER blocks URL for the page DOCUMENT-URL.
TYPE is a keyword such as :script, :image, :document, :subdocument, or
:xmlhttprequest. A request is first-party when DOCUMENT-URL is omitted."
  (bt:with-recursive-lock-held ((update-lock adblocker))
    (decide-blocked-p adblocker url document-url type)))

(export-always 'cosmetic-css)
(defmethod cosmetic-css ((adblocker adblocker) url)
  "Stylesheet that hides elements on the page URL. Empty when there is nothing to hide."
  (bt:with-recursive-lock-held ((update-lock adblocker))
    (build-cosmetic-css adblocker url)))

(export-always 'load-filters)
(defgeneric load-filters (adblocker source)
  (:documentation "Add uBlock Origin and Adblock Plus filters from SOURCE.
SOURCE is a filter list string, a pathname, a stream, or a list of those.
A literal `$replace' rewrites the response. Scriptlets, regex `$replace', and
modifiers that rewrite a request instead of blocking it are skipped.")
  (:method ((adblocker adblocker) (source string))
    (commit-batch adblocker (parse-filter-text source)))
  (:method ((adblocker adblocker) (source pathname))
    (with-open-file (stream source :external-format :utf-8)
      (let ((name (ignore-errors (namestring (truename source)))))
        (commit-batch adblocker
                      (parse-filter-stream stream
                                           (uiop:pathname-directory-pathname source)
                                           (when name
                                             (let ((seen (make-hash-table :test 'equal)))
                                               (setf (gethash name seen) t)
                                               seen)))))))
  (:method ((adblocker adblocker) (source stream))
    (commit-batch adblocker (parse-filter-stream source)))
  (:method ((adblocker adblocker) (source list))
    (dolist (item source)
      (load-filters adblocker item))
    adblocker))

(defun filter-cache-path (name)
  (let ((directory (ensure-directory (uiop:xdg-cache-home "cl-cef/adblock/"))))
    (uiop:merge-pathnames* name directory)))

(defun cache-fresh-p (path)
  (and (uiop:file-exists-p path)
       (< (- (get-universal-time) (or (file-write-date path) 0))
          *default-filter-list-max-age*)))

(defun read-text-file (path)
  (alexandria:read-file-into-string path :external-format :utf-8))

(defun write-text-file (path text)
  (alexandria:write-string-into-file text path
                                     :if-exists :supersede
                                     :if-does-not-exist :create
                                     :external-format :utf-8))

(defun fetch-text (url)
  (if (find-package :dexador)
      (uiop:symbol-call :dexador :get url
                        :force-string t
                        :connect-timeout 10
                        :read-timeout 30)
      (uiop:run-program (list "curl" "-fsSL" "--max-time" "30" url)
                        :output :string
                        :external-format :utf-8)))

(defun ensure-list-text (name url)
  (let ((path (filter-cache-path name)))
    (if (cache-fresh-p path)
        (read-text-file path)
        (handler-case
            (let ((text (fetch-text url)))
              (write-text-file path text)
              text)
          (error (condition)
            (cond
              ((uiop:file-exists-p path)
               (warn "cl-cef adblocker kept the cached ~a (~a)." name condition)
               (read-text-file path))
              (t
               (warn "cl-cef adblocker could not load ~a (~a)." name condition)
               nil)))))))

(export-always 'load-default-lists)
(defmethod load-default-lists ((adblocker adblocker))
  "Download uBlock Origin's default lists, or read a copy newer than four days.
A download failure falls back to the cached copy of that list."
  (dolist (entry *default-filter-lists*)
    (destructuring-bind (name url) entry
      (let ((text (ensure-list-text name url)))
        (when (and text (plusp (length text)))
          (load-filters adblocker text)))))
  adblocker)

(defun adblock-decider (adblocker)
  (lambda (url document-url type-name)
    (let ((type (canonicalize-type type-name)))
      (if (blocked-p adblocker url :document-url document-url :type type)
          (values t "" "")
          (values nil
                  (if (member type '(:document :subdocument))
                      (cosmetic-css adblocker url)
                      "")
                  (encode-replacements
                   (response-replacements adblocker url
                                          :document-url document-url
                                          :type type)))))))

(export-always '*adblocker*)
(defvar *adblocker* nil
  "The adblocker installed on CEF's request path, or NIL.")

(export-always 'enable)
(defmethod enable ((adblocker adblocker))
  "Block requests with ADBLOCKER. Safe to call before or after `launch'."
  (setf *adblocker* adblocker
        *request-decider* (adblock-decider adblocker))
  (when *library-loaded-p*
    (install-request-blocker))
  adblocker)

(export-always 'default-block)
(defmethod default-block ((adblocker adblocker))
  "Enable ADBLOCKER and load uBlock Origin's default lists in the background.
Lists are cached under the XDG cache directory and refreshed every four days.
Requests made before the lists arrive are allowed."
  (enable adblocker)
  (let ((thread (bt:make-thread
                 (lambda ()
                   (handler-case (load-default-lists adblocker)
                     (error (condition)
                       (warn "cl-cef adblocker could not load the default lists: ~a"
                             condition))))
                 :name "cl-cef adblock update")))
    thread))
