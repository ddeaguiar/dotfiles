{:user     {:plugins [[lein-ancient "0.6.15"]]}
 :repl     {:src          [sidebar]
            :dependencies [[org.clojure/tools.namespace "0.2.11"]]}
 :socket   {:jvm-opts ["-Dclojure.server.repl={:port,50505,:accept,clojure.core.server/repl}"]}
 :io-prepl {:jvm-opts ["-Dclojure.server.jvm={:port,55555,:accept,clojure.core.server/io-prepl}"]}
 :rebl     {:source-paths   ["/Users/ddeaguiar/src/rebl/src"]
            :resource-paths ["/Users/ddeaguiar/src/rebl/resources"]
            :dependencies   [[org.clojure/core.async "0.4.490"]
                             [org.clojure/spec.alpha "0.2.176"]]
            :main           cognitect.rebl}
 :rebl-jar {:dependencies [[com.cognitect/REBL "0.9.172"]
                           [org.clojure/core.async "0.4.490"]]
            :main         cognitect.rebl}}
