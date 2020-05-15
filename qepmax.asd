;; maxima-asdf support file for qepmax package
;; written by Yasuaki Honda
;; Licensed under GPL 3.0 .
;; See LICENSE file for the terms of the license

;; Visit https://github.com/robert-dodier/maxima-asdf for installing maxima-asdf in your environment.
;; Then you can load the package from github directly:

;; install_github("YasuakiHonda","qepmax","master");
;; asdf_load("qepmax");

;; Then you can play with functions in qepmax package.


(defsystem qepmax
  :defsystem-depends-on ("maxima-file")
  :name "Qepmax package"
  :author "Yasuaki Honda"
  :license "GNU General Public License, version 3"
  :description "Maxima package for Quantifier Elimination, using QEPCAD B"

  :components
    ((:maxima-file "qepmax") 
     (:maxima-file "nns") 
     (:maxima-file "ratsimpx2")
     (:maxima-file "toimplx")
     (:file "qepmax-support")))
