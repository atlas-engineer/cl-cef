;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :cef/tests)

(defun adblock-from (text)
  (let ((engine (make-instance 'cef:adblocker)))
    (cef:load-filters engine text)
    engine))

(define-test hostname-anchor-blocks-subdomains ()
  (let ((engine (adblock-from "||doubleclick.net^")))
    (assert-true (cef:blocked-p engine "https://ad.doubleclick.net/pagead/js" :type :script))
    (assert-true (cef:blocked-p engine "http://doubleclick.net/x" :type :image))
    (assert-false (cef:blocked-p engine "https://notdoubleclick.net/x" :type :script))
    (assert-false (cef:blocked-p engine "https://doubleclick.net.evil.com/x" :type :script))
    ;; Blocking filters do not match the document itself unless `$document' says so.
    (assert-false (cef:blocked-p engine "https://doubleclick.net/" :type :document))))

(define-test anchors-wildcards-and-separators ()
  (let ((engine (adblock-from "|https://cdn.example/banner.js|
^banner^
banner*tracker
||widgets.example/ads/")))
    (assert-true (cef:blocked-p engine "https://cdn.example/banner.js" :type :script))
    (assert-false (cef:blocked-p engine "https://cdn.example/banner.js?x=1" :type :script))
    (assert-true (cef:blocked-p engine "https://example.com/banner?id=1" :type :image))
    (assert-false (cef:blocked-p engine "https://example.com/mybanner/x" :type :image))
    (assert-true (cef:blocked-p engine "https://example.com/banner/v2/tracker.js" :type :script))
    (assert-true (cef:blocked-p engine "https://widgets.example/ads/unit.js" :type :script))
    (assert-true (cef:blocked-p engine "https://a.widgets.example/ads/unit.js" :type :script))
    (assert-false (cef:blocked-p engine "https://widgets.example/other.js" :type :script))))

(define-test type-party-and-domain-options ()
  (let ((engine (adblock-from "||static.example^$script
||cdn.example.co.uk^$third-party
||tracker.test^$domain=news.test
||pixels.test^$~script")))
    (assert-true (cef:blocked-p engine "https://static.example/app.js" :type :script))
    (assert-false (cef:blocked-p engine "https://static.example/logo.png" :type :image))
    (assert-false (cef:blocked-p engine "https://cdn.example.co.uk/lib.js"
                                  :document-url "https://www.example.co.uk/"
                                  :type :script))
    (assert-true (cef:blocked-p engine "https://cdn.example.co.uk/lib.js"
                                 :document-url "https://www.other.co.uk/"
                                 :type :script))
    (assert-true (cef:blocked-p engine "https://tracker.test/collect"
                                 :document-url "https://news.test/story"
                                 :type :xmlhttprequest))
    (assert-false (cef:blocked-p engine "https://tracker.test/collect"
                                  :document-url "https://other.test/"
                                  :type :xmlhttprequest))
    (assert-true (cef:blocked-p engine "https://pixels.test/px.gif" :type :image))
    (assert-false (cef:blocked-p engine "https://pixels.test/px.js" :type :script))))

(define-test exceptions-and-important ()
  (let ((engine (adblock-from "||ads.example^
@@||ads.example^$script
||forced.example^
@@||forced.example^
||forced.example^$important
||kept.example^$important
@@||kept.example^$important")))
    (assert-false (cef:blocked-p engine "https://ads.example/a.js" :type :script))
    (assert-true (cef:blocked-p engine "https://ads.example/a.png" :type :image))
    (assert-true (cef:blocked-p engine "https://forced.example/a.js" :type :script))
    (assert-false (cef:blocked-p engine "https://kept.example/a.js" :type :script))))

(define-test badfilter-csp-and-preprocessor ()
  (let ((engine (adblock-from "! comment
[Adblock Plus 2.0]
||gone.example^
||gone.example^$badfilter
||page.example^$csp=script-src 'none'
!#if env_firefox
||firefox.example^
!#endif
!#if env_chromium
||chromium.example^
!#endif
!#if !env_firefox
||notfirefox.example^
!#else
||infirefox.example^
!#endif
example.com##+js(set, x, 1)")))
    (assert-false (cef:blocked-p engine "https://gone.example/a.js" :type :script))
    (assert-false (cef:blocked-p engine "https://page.example/" :type :document))
    (assert-false (cef:blocked-p engine "https://firefox.example/a.js" :type :script))
    (assert-false (cef:blocked-p engine "https://infirefox.example/a.js" :type :script))
    (assert-true (cef:blocked-p engine "https://chromium.example/a.js" :type :script))
    (assert-true (cef:blocked-p engine "https://notfirefox.example/a.js" :type :script))
    (assert-false (search "set" (cef:cosmetic-css engine "https://example.com/")))))

(define-test regex-and-popup ()
  (let ((engine (adblock-from "/ads[0-9]+\\.js/
/AdsOnly/
||pop.example^$popup
||pop.example^")))
    (assert-true (cef:blocked-p engine "https://example.com/ads12.js" :type :script))
    (assert-false (cef:blocked-p engine "https://example.com/ads.js" :type :script))
    (assert-true (cef:blocked-p engine "https://example.com/AdsOnly.js" :type :script))
    (assert-false (cef:blocked-p engine "https://example.com/adsonly.js" :type :script))
    ;; A path that starts with `/' is not a regex just because it contains one.
    (let ((path (adblock-from "/n/ax4mqlu25efi/b/*$document
/or/.espace-.client/(;authentication.htm^$document
/amp/analytics")))
      (assert-false (cef:blocked-p path "https://news.ycombinator.com/" :type :document))
      (assert-false (cef:blocked-p path "https://www.cnn.com/" :type :document))
      (assert-false (cef:blocked-p path "https://example.com/s.js" :type :script))
      (assert-true (cef:blocked-p path "https://evil.example/n/ax4mqlu25efi/b/1" :type :document)))
    (assert-true (cef:blocked-p engine "https://pop.example/win" :type :popup))
    (assert-false (cef:blocked-p engine "https://pop.example/win" :type :document))
    (assert-true (cef:blocked-p engine "https://pop.example/lib.js" :type :script))))

(define-test popup-filter-is-explicit ()
  ;; The second engine only has `$popup', so the plain pattern above is not in play.
  (let ((engine (adblock-from "||pop.example^$popup")))
    (assert-true (cef:blocked-p engine "https://pop.example/win" :type :popup))
    (assert-false (cef:blocked-p engine "https://pop.example/win" :type :document))
    (assert-false (cef:blocked-p engine "https://pop.example/lib.js" :type :script))))

(define-test cosmetic-filters ()
  (let* ((engine (adblock-from "##.ad-banner
example.com##.sponsor
example.com#@#.ad-banner
other.com##.other
google.*##.gad
example.com##.ad:has-text(buy)
example.com##div:-abp-has(.ad)"))
         (example (cef:cosmetic-css engine "https://example.com/story"))
         (other (cef:cosmetic-css engine "https://other.com/"))
         (third (cef:cosmetic-css engine "https://third.test/"))
         (google (cef:cosmetic-css engine "https://www.google.co.uk/search"))
         (evil (cef:cosmetic-css engine "https://google.evil.com/")))
    (assert-false (search ".ad-banner" example))
    (assert-true (search ".sponsor" example))
    (assert-true (search ":has(.ad)" example))
    (assert-false (search "has-text" example))
    (assert-true (search ".ad-banner" other))
    (assert-true (search ".other" other))
    (assert-false (search ".sponsor" other))
    (assert-true (search ".ad-banner" third))
    (assert-false (search ".sponsor" third))
    (assert-true (search ".gad" google))
    (assert-false (search ".gad" evil))))

(define-test elemhide-and-generichide ()
  (let ((engine (adblock-from "##.ad-banner
example.com##.sponsor
@@||example.com^$generichide
@@||clean.example^$elemhide")))
    (assert-false (search ".ad-banner" (cef:cosmetic-css engine "https://example.com/")))
    (assert-true (search ".sponsor" (cef:cosmetic-css engine "https://example.com/")))
    (assert-true (search ".ad-banner" (cef:cosmetic-css engine "https://elsewhere.test/")))
    (assert-string= "" (cef:cosmetic-css engine "https://clean.example/account"))))

(define-test youtube-preroll-is-removed-before-playback ()
  ;; Firefox uBlock Origin renames these keys in the player response. The player
  ;; reads them before starting the video, so the pre-roll never begins.
  (let ((engine (make-instance 'cef:adblocker))
        (player "https://www.youtube.com/youtubei/v1/player?prettyPrint=false")
        (watch "https://www.youtube.com/watch?v=abc"))
    (let ((xhr (cef:response-replacements engine player
                                          :document-url watch
                                          :type :xmlhttprequest)))
      (assert-true (find "\"adPlacements\"" xhr :key #'car :test #'string=))
      (assert-true (find "\"adSlots\"" xhr :key #'car :test #'string=))
      (assert-false (find "\"playerAds\"" xhr :key #'car :test #'string=)))
    (assert-false (cef:blocked-p engine player :document-url watch :type :xmlhttprequest))
    (let ((document (cef:response-replacements engine watch :type :document)))
      (assert-true (find "\"adPlacements\"" document :key #'car :test #'string=))
      (assert-true (find "\"playerAds\"" document :key #'car :test #'string=))
      (assert-true (find "\"adSlots\"" document :key #'car :test #'string=)))
    ;; The same player URL fetched by some other site is not a YouTube pre-roll.
    (assert-false (cef:response-replacements engine player
                                             :document-url "https://example.com/"
                                             :type :xmlhttprequest))
    (assert-false (cef:response-replacements engine "https://example.com/" :type :document))
    ;; An embed's player call is first-party to the frame, including googleapis.
    (assert-true (cef:response-replacements
                  engine
                  "https://youtubei.googleapis.com/youtubei/v1/player?key=abc"
                  :document-url "https://www.youtube-nocookie.com/embed/abc"
                  :type :other))
    ;; The video bytes themselves are not rewritten.
    (assert-false (cef:response-replacements
                   engine
                   "https://rr3---sn.googlevideo.com/videoplayback?id=1"
                   :document-url watch
                   :type :media))))

(define-test literal-replace-rewrites-and-does-not-block ()
  (let ((engine (adblock-from "||ads.example/player?$xhr,1p,replace=/\"adPlacements\"/\"no_ads\"/
||ads.example^$xhr,replace=/a.+b/c/")))
    (assert-false (cef:blocked-p engine "https://ads.example/player?x=1"
                                  :document-url "https://ads.example/"
                                  :type :xmlhttprequest))
    (let ((pairs (cef:response-replacements engine "https://ads.example/player?x=1"
                                            :document-url "https://ads.example/"
                                            :type :xmlhttprequest)))
      (assert-equal "\"no_ads\"" (cdr (assoc "\"adPlacements\"" pairs :test #'string=))))
    ;; A regex `$replace' is not searched for as literal text, and it is not a block.
    (assert-false (cef:blocked-p engine "https://ads.example/banner" :type :xmlhttprequest))
    (assert-false (cef:response-replacements engine "https://ads.example/banner"
                                             :document-url "https://ads.example/"
                                             :type :xmlhttprequest))))

(define-test official-css-cosmetic-filters ()
  (let* ((engine (adblock-from "###ccf #a1 .fail
###ccf #a2 .fail:not(.a2)
example.com###ccf #a3 .fail
example.com###ccf #a4 .fail:not(.a4)
example.com###ccf #a5 .fail:style(visibility: hidden)
###ccf #a6 .fail-pseudo::before
example.com###ccf #a7 .fail-pseudo::before
example.com###ccf #a8 .fail-pseudo::before:style(visibility: hidden)
example.com##div > .ad"))
         (css (cef:cosmetic-css engine "https://example.com/tests/css-selector-based-cosmetic-filters.html")))
    (dolist (selector '("#ccf #a1 .fail"
                        "#ccf #a2 .fail:not(.a2)"
                        "#ccf #a3 .fail"
                        "#ccf #a4 .fail:not(.a4)"
                        "#ccf #a6 .fail-pseudo::before"
                        "#ccf #a7 .fail-pseudo::before"
                        "div > .ad"))
      (assert-true (search selector css)))
    (assert-true (search "#ccf #a5 .fail { visibility: hidden !important }" css))
    (assert-true (search "#ccf #a8 .fail-pseudo::before { visibility: hidden !important }" css))
    (assert-false (search ":style(" css))))

(define-test official-procedural-and-html-filters ()
  (let* ((host "example.com")
         (procedural
           '(" .fail:has(b)"
             " .fail:has(> a > b)"
             " .fail:has(+ a > b)"
             " .fail:has(:scope + a > b)"
             " .fail:has(:is(.pass a > b))"
             " .fail:not(:has(c))"
             " .fail:has-text(needle)"
             " .fail:has-text(/NEEDLE/i)"
             " .fail:not(:has-text(haystack))"
             " .fail:matches-css(position: absolute)"
             " .fail:has(a:matches-css-before(opacity: 0))"
             " .fail:has(b:matches-css-after(opacity: 0))"
             " .fail > a > b:nth-ancestor(2)"
             ":xpath(.//b/../..)"
             " .fail:min-text-length(300)"
             " .pass > a:has(b) + .fail"
             " .pass > a:has(b) + .fail:has(b)"
             " .pass:watch-attr(class) > .fail:has(b.notok)"
             " .fail:has(+ a)"
             " .fail:has(~ a:has(b))"
             " .fail:remove()"
             " b:upward(2)"
             " b:upward(.fail)"
             " b:upward(.fail):style(visibility: hidden !important)"))
         (html
           '("^#phf #a1 .fail:has(b)"
             "^#phf #a2 .fail:has(> a > b)"
             "^#phf #a4 .fail:has-text(needle)"
             "^#phf #a5 .fail:has-text(/NEEDLE/i)"
             "^#phf #a7 .fail > a > b:nth-ancestor(2)"
             "^#phf #a8:xpath(.//b/../..)"
             "^#phf #a9 .fail:min-text-length(300)"
             "^#phf #a14 b:upward(2)"
             "^#phf #a15 b:upward(.fail)"))
         (text (with-output-to-string (stream)
                 (loop for selector in procedural
                       for index from 1
                       do (format stream "~a#?##pcf #a~d~a~%" host index selector))
                 (dolist (selector html)
                   (format stream "~a##~a~%" host selector))))
         (engine (adblock-from text))
         (css (cef:cosmetic-css engine "https://example.com/pcf"))
         (script (cef:cosmetic-script engine "https://example.com/pcf")))
    (assert-true (search ".fail:has(b)" css))
    (assert-true (search ":has(> a > b)" css))
    (assert-false (search "has-text" css))
    (assert-false (search "xpath" css))
    (assert-true (search "hasText" script))
    (assert-true (search "needle" script))
    (assert-true (search "/NEEDLE/i" script))
    (assert-true (search "haystack" script))
    (assert-true (search "position: absolute" script))
    (assert-true (search "::before" script))
    (assert-true (search "::after" script))
    (assert-true (search "upward" script))
    (assert-true (search "xpathNodes" script))
    (assert-true (search ".//b/../.." script))
    (assert-true (search "minText" script))
    (assert-true (search "300" script))
    (assert-true (search ".fail:has(b.notok)" script))
    (assert-true (search "drop(" script))
    (assert-true (search "paint(" script))
    (assert-true (search "visibility: hidden !important" script))
    (assert-true (search "#phf #a1" script))
    (assert-true (search "MutationObserver" script))))

(define-test official-nostif-scriptlet ()
  (let ((cases '(("example.com##+js(nostif, bad)" "bad" "")
                 ("example.com##+js(nostif, !good)" "!good" "")
                 ("example.com##+js(nostif, , 33)" "" "33")
                 ("example.com##+js(nostif, , !66)" "" "!66")
                 ("example.com##+js(nostif, bad, 33)" "bad" "33")
                 ("example.com##+js(nostif, !good, !66)" "!good" "!66")
                 ("example.com##+js(nostif, bad, !33)" "bad" "!33")
                 ("example.com##+js(nostif, !good, 66)" "!good" "66")
                 ("example.com##+js(nostif, !bad, 33)" "!bad" "33")
                 ("example.com##+js(nostif, good, !66)" "good" "!66"))))
    (dolist (case cases)
      (destructuring-bind (line needle delay) case
        (let* ((engine (adblock-from line))
               (script (cef:cosmetic-script engine "https://example.com/sif")))
          (assert-true (search "installNostif" script))
          (assert-true (search (format nil "installNostif(~s,~s)" needle delay) script))
          (assert-false (search "installNostif" (cef:cosmetic-css engine "https://example.com/sif"))))))))

(defparameter *parser-checklist-accept*
  '("a*"
    "*$xhr"
    "|*$xhr"
    "|$xhr"
    "||*$xhr"
    "||$xhr"
    "||*|$xhr"
    ":: ab"
    ":: AB"
    ":: ab # comment"
    "0.0.0.0 0.0.0.0.example.com"
    "0.0.0.0 localhost.example.com"
    "0.0.0.0 local-host.example.com"
    "0.0.0.0 local.com"
    "0.0.0.0 localhost.com"
    "0.0.0.0 local-host.com"
    "$script,redirect=noop.js"
    "*$redirect=noop.js"
    "*$empty"
    "*$xhr,empty"
    "*$xhr,redirect=empty"
    "*$csp=default-src 'none'"
    "*$all,~document"
    "*$all,~popup"
    "*$all,~inline-script"
    "*$all,~inline-font"
    "*$domain=toto.com|toto.*|~toto.com|~toto.*|tôtó.ça|tôtó.*|~tôtó.ça|[ff00::0]|1.1.1.1"
    "*$denyallow=toto.com|tôtó.ça|[ff00::0]|1.1.1.1,domain=toto.com"))

(defparameter *parser-checklist-reject*
  '("a"
    "|"
    "||"
    "$"
    "*"
    "|*"
    "||*"
    "||*|"
    ":: a"
    ":: ab/"
    ":: ab/ # comment"
    "::/ ab"
    ":: ab$"
    "0.0.0.0 0.0.0.0"
    "0.0.0.0 local"
    "0.0.0.0 localhost"
    "/(abc|def/$xhr"
    "*$beacon,redirect-rule=empty"
    "*$ping,redirect-rule=empty"
    "*$websocket,redirect-rule=empty"
    "*$ghide,redirect=noop.js"
    "*$csp=default-src 'none',empty"
    "*$csp=default-src 'none',redirect=empty"
    "*$redirect=empty,csp=default-src 'none'"
    "*$csp=default-src 'none',xhr"
    "*$csp=default-src 'none',ghide"
    "*$csp=default-src 'none',csp=script-src 'none'"
    "*$~document"
    "*$~popup"
    "*$~inline-script"
    "*$~inline-font"
    "*$domain=.toto.com|toto.com.|[ff00::00000]|1.1.1.1111"
    "*$denyallow=.toto.com|toto.com.|toto.*|~toto.com|~toto.*|[ff00::00000]|1.1.1.1111,domain=toto.com"
    "*$script,denyallow=toto.com"))

(define-test official-static-filtering-parser-checklist ()
  (let* ((text (with-output-to-string (stream)
                 (dolist (line *parser-checklist-accept*)
                   (write-line line stream))
                 (dolist (line *parser-checklist-reject*)
                   (write-line line stream))))
         (engine (adblock-from text)))
    (dolist (line *parser-checklist-accept*)
      (assert-true (cef:filter-parsed-p engine line)))
    (dolist (line *parser-checklist-reject*)
      (assert-false (cef:filter-parsed-p engine line)))
    (let ((hosts (adblock-from ":: ab
:: AB
0.0.0.0 localhost.example.com
0.0.0.0 localhost")))
      (assert-true (cef:blocked-p hosts "http://ab/x" :type :script))
      (assert-true (cef:blocked-p hosts "https://localhost.example.com/a" :type :script))
      (assert-false (cef:blocked-p hosts "https://localhost/a" :type :script))
      (assert-false (cef:filter-parsed-p hosts "0.0.0.0 localhost")))))

(define-test indexed-host-rules-do-not-match-unrelated-urls ()
  (let ((engine (make-instance 'cef:adblocker)))
    (cef:load-filters
     engine
     (with-output-to-string (stream)
       (dotimes (index 300)
         (format stream "||ad~4,'0d.invalid^~%" index))))
    (assert-true (cef:blocked-p engine "https://ad0150.invalid/x.js" :type :script))
    (assert-false (cef:blocked-p engine "https://example.com/index.js" :type :script))
    (assert-false (cef:blocked-p engine "https://ad0150.invalid.evil/x.js" :type :script))))
