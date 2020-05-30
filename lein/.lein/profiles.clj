{:user         {:plugins [[lein-ancient "0.6.15"]]}
 :repl         {:src          [sidebar]
                :dependencies [[org.clojure/tools.namespace "0.2.11"]]}
 :rebl         {:source-paths   ["/Users/ddeaguiar/src/rebl/src"]
                :resource-paths ["/Users/ddeaguiar/src/rebl/resources"]
                :dependencies   [[org.clojure/core.async "0.4.490"]
                                 [org.clojure/spec.alpha "0.2.176"]]
                :main           cognitect.rebl}
 :rebl-jar     {:dependencies [[com.cognitect/REBL "0.9.172"]
                               [org.clojure/core.async "0.4.490"]
                               ;; due to project conflicts
                               [org.clojure/core.memoize "0.8.2"]
                               [org.clojure/core.cache "0.8.2"]]
                :main         cognitect.rebl}
 :socket       {:jvm-opts ["-Dclojure.server.repl={:port,60606,:accept,clojure.core.server/repl}"]}
 :socket-prepl {:jvm-opts ["-Dclojure.server.io-prepl={:port,40404,:accept,clojure.core.server/io-prepl}"]}
 :socket-rebl  {:jvm-opts ["-Dclojure.server.rebl={:port 60606 :accept cognitect.rebl/-main}"]}}
