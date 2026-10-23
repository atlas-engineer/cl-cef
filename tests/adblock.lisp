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
