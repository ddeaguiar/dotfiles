{:user {:plugins [[lein-ancient "0.6.15"]]}
 :repl {:src [sidebar]
        :dependencies [[org.clojure/tools.namespace "0.2.11"]]}
 :socket {:jvm-opts ["-Dclojure.server.repl={:port,50505,:accept,clojure.core.server/repl}"]}
 :io-prepl {:jvm-opts ["-Dclojure.server.jvm={:port,55555,:accept,clojure.core.server/io-prepl}"]}
 :rebl-jar {:dependencies [[com.cognitect/REBL "0.9.172"]
                           [org.clojure/core.async "0.4.490"]]
            :main cognitect.rebl}
 }
