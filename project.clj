(defproject gbtmago.mcts "0.1.0-SNAPSHOT"
  :description "Monte Carlo Tree Search implemented in Clojure"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0-RC1" :exclusions [[org.clojure/core.async]]]
                 [org.clojure/core.async "0.3.442"]
                 [org.clojure/tools.trace "0.7.9"]
                 [rhizome "0.2.9"]]
  :main ^:skip-aot gbtmago.mcts.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
