(defproject hnc-gen "0.1.0-SNAPSHOT"
  :description "hnc-gen: generate natural langauge from semantic expressions"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"]]
;  :repl-options {:init-ns hnc-gen.core}
  :repl-options {:init-ns hnc-gen.bible}
  :jvm-opts ["-Dfile.encoding=UTF-8"])
