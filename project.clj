(defproject advent2019 "0.1.0-SNAPSHOT"
  :description "Advent of code - 2019 solutions. https://adventofcode.com/2019"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [metasoarous/oz "1.6.0-alpha5"]
                 [org.clojure/core.async "0.6.532"]
                 [org.clojure/math.combinatorics "0.1.6"]]
  :repl-options {:init-ns advent2019.core}
  :source-paths ["src"])
