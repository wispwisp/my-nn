(defproject my-nn "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [net.mikera/core.matrix "0.63.0"]
                 [net.mikera/vectorz-clj "0.48.0"]
                 [ring "1.10.0"]
                 [ring/ring-anti-forgery "1.3.0"]
                 [ring/ring-defaults "0.3.4"]
                 [compojure "1.7.0"]
                 [hiccup "1.0.5"]]
  :main ^:skip-aot my-nn.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
