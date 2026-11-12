;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

;;;; Procedural cosmetic filters, HTML filters, and the no-setTimeout-if
;;;; scriptlet. Plain CSS stays in the stylesheet. Everything else becomes
;;;; one script injected with the document.

(in-package :cef)

(defparameter *procedural-operators*
  '("matches-css-before" "matches-css-after" "matches-css" "min-text-length"
    "nth-ancestor" "watch-attr" "has-text" "upward" "xpath" "remove" "style")
  "Operators compiled to script, longest name first.")

(defparameter *unimplemented-selector-markers*
  '(":matches-attr" ":remove-attr" ":remove-class" ":-abp-contains" ":contains(")
  "Cosmetic syntax this engine does not run. The selector is dropped.")

(defun blank-selector-p (text)
  (or (null text)
      (every (lambda (char) (member char '(#\Space #\Tab #\Newline #\Return))) text)))

(defun balanced-end (text open)
  "Index just past the parenthesis that closes the one at OPEN."
  (when (and (< open (length text)) (char= (char text open) #\())
    (loop with depth = 0
          for index from open below (length text)
          for char = (char text index)
          do (cond
               ((char= char #\() (incf depth))
               ((char= char #\))
                (decf depth)
                (when (zerop depth)
                  (return (1+ index))))))))

(defun unsafe-has-argument-p (argument)
  "Native :has() rejects :scope, a leading ~, and a nested :has()."
  (let ((text (string-left-trim '(#\Space #\Tab) (or argument ""))))
    (or (search ":scope" text)
        (search ":has(" text)
        (and (plusp (length text)) (char= (char text 0) #\~)))))

(defun unsafe-has-p (text)
  (loop for start = 0 then close
        for pos = (search ":has(" text :start2 start)
        while pos
        for open = (+ pos 4)
        for close = (balanced-end text open)
        thereis (and close (unsafe-has-argument-p (subseq text (1+ open) (1- close))))))

(defun contains-procedural-p (text)
  (or (unsafe-has-p text)
      (some (lambda (name)
              (search (concatenate 'string ":" name "(") text))
            *procedural-operators*)))

(defun named-operator-at (text index)
  (when (and (< index (length text)) (char= (char text index) #\:))
    (dolist (name *procedural-operators*)
      (let ((end (+ index 1 (length name))))
        (when (and (<= end (length text))
                   (string= name text :start2 (1+ index) :end2 end)
                   (< end (length text))
                   (char= (char text end) #\())
          (let ((close (balanced-end text end)))
            (when close
              (return-from named-operator-at
                (list name (subseq text (1+ end) (1- close)) index close)))))))))

(defun wrapper-operator-at (text index)
  (dolist (wrap '("not" "has"))
    (let ((end (+ index 1 (length wrap))))
      (when (and (< index (length text))
                 (char= (char text index) #\:)
                 (<= end (length text))
                 (string= wrap text :start2 (1+ index) :end2 end)
                 (< end (length text))
                 (char= (char text end) #\())
        (let ((close (balanced-end text end)))
          (let ((argument (subseq text (1+ end) (1- close))))
            (when (or (unsafe-has-argument-p argument)
                      (contains-procedural-p argument))
              (return-from wrapper-operator-at
                (list (cond
                        ((and (string= wrap "has") (unsafe-has-argument-p argument))
                         "has-rel")
                        ((string= wrap "has") "has-inner")
                        (t (concatenate 'string wrap "-inner")))
                      argument
                      index
                      close)))))))))

(defun next-operator (text)
  "The first procedural operator at parenthesis depth zero."
  (loop with depth = 0
        for index from 0 below (length text)
        for char = (char text index)
        do (cond
             ((char= char #\() (incf depth))
             ((char= char #\)) (when (plusp depth) (decf depth)))
             ((zerop depth)
              (let ((operator (or (named-operator-at text index)
                                  (wrapper-operator-at text index))))
                (when operator
                  (return operator)))))))

(defun split-first-operator (selector)
  "Values are the CSS before the operator, its name, its argument, and the tail.
Name is NIL when SELECTOR has no procedural operator."
  (let ((operator (next-operator selector)))
    (if (null operator)
        (values selector nil nil "")
        (values (subseq selector 0 (third operator))
                (first operator)
                (second operator)
                (subseq selector (fourth operator))))))

(defun js-string (text)
  (with-output-to-string (stream)
    (write-char #\" stream)
    (loop for char across (or text "")
          do (case char
               (#\\ (write-string "\\\\" stream))
               (#\" (write-string "\\\"" stream))
               (#\Newline (write-string "\\n" stream))
               (#\Return (write-string "\\r" stream))
               (#\Tab (write-string "\\t" stream))
               (t
                (if (< (char-code char) 32)
                    (format stream "\\u~4,'0x" (char-code char))
                    (write-char char stream)))))
    (write-char #\" stream)))

(defun js-pick (roots selector)
  (if (blank-selector-p selector)
      roots
      (format nil "pick(~a,~a)" roots (js-string selector))))

(defun normalize-important (declaration)
  (let ((text (string-trim '(#\Space #\Tab) (or declaration ""))))
    (if (search "!important" text :test #'char-equal)
        text
        (concatenate 'string text " !important"))))

(defun pure-style-rule (selector)
  "A cons of (CSS-SELECTOR . DECLARATION) when SELECTOR is CSS plus :style().
NIL when the selector needs the procedural script."
  (multiple-value-bind (css name argument after) (split-first-operator selector)
    (when (and (string= name "style")
               (blank-selector-p after)
               (not (contains-procedural-p css)))
      (cons (string-trim '(#\Space #\Tab #\Newline) css)
            (normalize-important argument)))))

(defun filter-set (base argument keep)
  (let ((inner (compile-set argument "[n]")))
    (when (stringp inner)
      (format nil "(~a).filter(function(n){return (~a).length~a0;})"
              base inner (if keep "!==" "===")))))

(defun compile-set (selector roots)
  "JavaScript expression for the elements SELECTOR matches under ROOTS.
A list (:paint EXPR DECL) or (:drop EXPR) when the chain ends in an action."
  (multiple-value-bind (css name argument after) (split-first-operator selector)
    (let ((base (js-pick roots css)))
      (cond
        ((null name) base)
        ((string= name "style")
         (list :paint base (normalize-important argument)))
        ((string= name "remove")
         (list :drop base))
        ((member name '("upward" "nth-ancestor") :test #'string=)
         (compile-set after (format nil "upward(~a,~a)" base (js-string argument))))
        ((string= name "xpath")
         (compile-set after (format nil "xpathNodes(~a,~a)" base (js-string argument))))
        ((string= name "has-text")
         (compile-set after (format nil "hasText(~a,~a)" base (js-string argument))))
        ((string= name "min-text-length")
         (compile-set after (format nil "minText(~a,~a)" base (js-string argument))))
        ((string= name "matches-css")
         (compile-set after (format nil "matchCss(~a,null,~a)" base (js-string argument))))
        ((string= name "matches-css-before")
         (compile-set after
                      (format nil "matchCss(~a,\"::before\",~a)" base (js-string argument))))
        ((string= name "matches-css-after")
         (compile-set after
                      (format nil "matchCss(~a,\"::after\",~a)" base (js-string argument))))
        ((string= name "watch-attr")
         (compile-set after base))
        ((string= name "not-inner")
         (let ((filtered (filter-set base argument nil)))
           (when filtered
             (compile-set after filtered))))
        ((string= name "has-inner")
         (let ((filtered (filter-set base argument t)))
           (when filtered
             (compile-set after filtered))))
        ((string= name "has-rel")
         (compile-set after (format nil "relativeHas(~a,~a)" base (js-string argument))))))))

(defun cosmetic-statement (selector action)
  "One JavaScript statement that applies SELECTOR.
ACTION is :hide or :remove. NIL when SELECTOR is plain CSS or pure :style()."
  (let ((source (if (and (plusp (length selector)) (char= (char selector 0) #\^))
                    (subseq selector 1)
                    selector))
        (remove (or (eq action :remove)
                    (and (plusp (length selector)) (char= (char selector 0) #\^)))))
    (cond
      ((or (blank-selector-p source) (pure-style-rule source)) nil)
      ((not (contains-procedural-p source))
       (format nil "~a(pick(DOC,~a));" (if remove "drop" "hide") (js-string source)))
      (t
       (let ((result (compile-set source "DOC")))
         (cond
           ((null result) nil)
           ((and (consp result) (eq (first result) :paint))
            (format nil "paint(~a,~a);" (second result) (js-string (third result))))
           ((and (consp result) (eq (first result) :drop))
            (format nil "drop(~a);" (second result)))
           ((stringp result)
            (format nil "~a(~a);" (if remove "drop" "hide") result))))))))

(defun scriptlet-parts (selector)
  "Values are the scriptlet name and its argument list.
NIL when SELECTOR is not +js(...)."
  (let ((text (string-trim '(#\Space #\Tab) selector)))
    (when (and (alexandria:starts-with-subseq "+js(" text)
               (char= (char text (1- (length text))) #\)))
      (let* ((inner (subseq text 4 (1- (length text))))
             (parts nil)
             (start 0)
             (depth 0))
        (loop for index from 0 below (length inner)
              for char = (char inner index)
              do (case char
                   (#\( (incf depth))
                   (#\) (when (plusp depth) (decf depth)))
                   (#\,
                    (when (zerop depth)
                      (push (string-trim '(#\Space #\Tab) (subseq inner start index)) parts)
                      (setf start (1+ index))))))
        (push (string-trim '(#\Space #\Tab) (subseq inner start)) parts)
        (setf parts (nreverse parts))
        (when (and parts (plusp (length (first parts))))
          (values (string-downcase (first parts)) (rest parts)))))))

(defun scriptlet-statement (selector)
  "JavaScript that installs SELECTOR's scriptlet, or NIL when it is not one.
The official scriptlet test is no-setTimeout-if, also written nostif."
  (multiple-value-bind (name arguments) (scriptlet-parts selector)
    (when (member name '("nostif" "no-settimeout-if") :test #'string=)
      (format nil "installNostif(~a,~a);"
              (js-string (or (first arguments) ""))
              (js-string (or (second arguments) ""))))))

(defparameter *page-script-prelude*
  "function pick(roots,sel){var out=[],relative=/^[\\s>+~]/.test(sel);for(var i=0;i<roots.length;i++){var root=roots[i];if(!root||!root.querySelectorAll)continue;var query=sel;if(relative){if(root.nodeType===9)continue;query=':scope'+sel;}try{var found=root.querySelectorAll(query);for(var j=0;j<found.length;j++)out.push(found[j]);}catch(e){}}return out;}
function hide(nodes){for(var i=0;i<nodes.length;i++){var node=nodes[i];if(node&&node.style)node.style.setProperty('display','none','important');}}
function drop(nodes){for(var i=0;i<nodes.length;i++){var node=nodes[i];if(node&&node.remove)node.remove();}}
function paint(nodes,decl){var important=/!important/i.test(decl);var body=decl.replace(/!important/ig,'').trim();var cut=body.indexOf(':');if(cut<0)return;var prop=body.slice(0,cut).trim();var value=body.slice(cut+1).trim();for(var i=0;i<nodes.length;i++){var node=nodes[i];if(node&&node.style)node.style.setProperty(prop,value,important?'important':'');}}
function textHit(text,needle){if(needle.length>2&&needle.charAt(0)==='/'&&needle.lastIndexOf('/')>0){var cut=needle.lastIndexOf('/');try{return new RegExp(needle.slice(1,cut),needle.slice(cut+1)).test(text);}catch(e){return false;}}return text.indexOf(needle)!==-1;}
function hasText(nodes,needle){return nodes.filter(function(node){return textHit(node.textContent||'',needle);});}
function minText(nodes,spec){var n=Number(spec);return nodes.filter(function(node){return (node.textContent||'').length>=n;});}
function matchCss(nodes,pseudo,decl){var cut=decl.indexOf(':');var prop=decl.slice(0,cut).trim();var value=decl.slice(cut+1).trim();return nodes.filter(function(node){return getComputedStyle(node,pseudo||undefined).getPropertyValue(prop).trim()===value;});}
function upward(nodes,spec){var byCount=/^[0-9]+$/.test(spec);var count=byCount?Number(spec):0;var out=[];for(var i=0;i<nodes.length;i++){var node=nodes[i];if(byCount){for(var step=0;step<count&&node;step++)node=node.parentElement;if(node)out.push(node);}else{var parent=node.parentElement;while(parent&&!parent.matches(spec))parent=parent.parentElement;if(parent)out.push(parent);}}return out;}
function xpathNodes(nodes,expr){var out=[];for(var i=0;i<nodes.length;i++){var node=nodes[i];var doc=node.nodeType===9?node:node.ownerDocument;if(!doc)continue;var snap=doc.evaluate(expr,node.nodeType===9?doc:node,null,7,null);for(var j=0;j<snap.snapshotLength;j++){var item=snap.snapshotItem(j);if(item&&item.nodeType===1)out.push(item);}}return out;}
function needleHit(src,needle){var neg=false;if(needle.charAt(0)==='!'){neg=true;needle=needle.slice(1);}var hit=false;if(needle==='')hit=true;else if(needle.charAt(0)==='/'&&needle.lastIndexOf('/')>0){var cut=needle.lastIndexOf('/');try{hit=new RegExp(needle.slice(1,cut),needle.slice(cut+1)).test(src);}catch(e){hit=false;}}else hit=src.indexOf(needle)!==-1;return neg?!hit:hit;}
function delayHit(ms,spec){if(!spec)return true;var neg=false;if(spec.charAt(0)==='!'){neg=true;spec=spec.slice(1);}if(spec==='')return true;var hit=Number(ms)===Number(spec);return neg?!hit:hit;}
function relativeMatch(node,spec){spec=spec.replace(/^:scope\\s*/,'');if(!spec)return false;var op=spec.charAt(0);if(op==='+'||op==='~'||op==='>'){var rest=spec.slice(1).trim();if(op==='+'){var next=node.nextElementSibling;return !!(next&&elementMatch(next,rest));}if(op==='~'){var sib=node.nextElementSibling;while(sib){if(elementMatch(sib,rest))return true;sib=sib.nextElementSibling;}return false;}var kids=node.children;for(var i=0;i<kids.length;i++)if(elementMatch(kids[i],rest))return true;return false;}try{return node.querySelector(spec)!=null;}catch(e){return false;}}
function elementMatch(el,sel){var has=sel.indexOf(':has(');var child=sel.indexOf('>');if(has!==-1&&(child===-1||has<child)){var head=sel.slice(0,has).trim();var arg=sel.slice(has+5,sel.lastIndexOf(')'));if(head&&!el.matches(head))return false;return relativeMatch(el,arg);}if(child!==-1){var left=sel.slice(0,child).trim();var right=sel.slice(child+1).trim();if(left){try{if(!el.matches(left))return false;}catch(e){return false;}}var kids=el.children;for(var i=0;i<kids.length;i++)if(elementMatch(kids[i],right))return true;return false;}try{return el.matches(sel);}catch(e){return false;}}
function relativeHas(nodes,spec){return nodes.filter(function(node){return relativeMatch(node,spec);});}
function installNostif(needle,delay){var orig=setTimeout;setTimeout=function(fn,ms){var src='';try{src=Function.prototype.toString.call(fn);}catch(e){}if(needleHit(src,needle)&&delayHit(ms,delay))return 0;return orig.apply(this,arguments);};window.setTimeout=setTimeout;}
")

(defun wrap-page-script (immediate jobs)
  "One page script. IMMEDIATE statements run at once. JOBS re-run as the DOM changes.
Scriptlets run before the guard is set, so a throw cannot skip a second try."
  (when (or immediate jobs)
    (format nil "(function(){~a~{~a~}if(window.__clCefUbo)return;window.__clCefUbo=1;~@[~a~]})();"
            *page-script-prelude*
            immediate
            (when jobs
              (format nil "var DOC;var running=false;function run(){if(running||!DOC)return;running=true;try{~{~a~}}finally{running=false;}}function boot(){DOC=[document];run();if(!document.documentElement)return;new MutationObserver(run).observe(document.documentElement,{subtree:true,childList:true,attributes:true,characterData:true});}if(document.readyState==='loading')document.addEventListener('DOMContentLoaded',boot,{once:true});else boot();"
                      jobs)))))
