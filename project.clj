;; Copyright © 2013, JUXT LTD. All Rights Reserved.
;;
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;;
;; By using this software in any fashion, you are agreeing to be bound by the
;; terms of this license.
;;
;; You must not remove this notice, or any other, from this software.

(defproject bidi "1.10.6-KIBU"
  :description "Bidirectional URI routing"
  :url "https://github.com/juxt/bidi"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/clojurescript "0.0-2202"]
                 [com.cemerick/url "0.1.1"]
                 [ring/ring-core "1.2.1"]]
  :lein-release {:deploy-via :clojars}
  :jar-exclusions [#"\.cljx|\.swp|\.swo|\.DS_Store"]
  :profiles {:dev {:dependencies [[ring-mock "0.1.5"]
                                  [compojure "1.1.6"]]
                   :hooks [cljx.hooks]
                   :plugins [[com.keminglabs/cljx "0.4.0"]
                             [lein-cljsbuild "1.0.2"]
                             [com.cemerick/clojurescript.test "0.3.1"]]
                   :cljx {:builds [{:source-paths ["src/cljx"]
                                    :output-path "target/classes"
                                    :rules :clj}

                                   {:source-paths ["src/cljx"]
                                    :output-path "target/classes"
                                    :rules :cljs}]}}})
