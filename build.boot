(set-env!
 :source-paths #{"src/cljs"}
 :resource-paths #{"html"}

 :dependencies '[[org.clojure/clojure "1.7.0"]
                 [org.clojure/clojurescript "1.7.170"]
                 [adzerk/boot-cljs "1.7.170-3"]
                 [pandeiro/boot-http "0.7.0"]
                 [adzerk/boot-reload "0.4.2"]
                 [adzerk/boot-cljs-repl "0.3.0"]
                 [com.cemerick/piggieback "0.2.1"]
                 [weasel "0.7.0"]
                 [org.clojure/tools.nrepl "0.2.12"]
                 [org.clojars.magomimmo/domina "2.0.0-SNAPSHOT"]
                 [crisptrutski/boot-cljs-test "0.2.2-SNAPSHOT"]
                 ])

(require '[adzerk.boot-cljs :refer [cljs]]
         '[pandeiro.boot-http :refer [serve]]
         '[adzerk.boot-reload :refer [reload]]
         '[adzerk.boot-cljs-repl :refer [cljs-repl start-repl]]
         '[crisptrutski.boot-cljs-test :refer [test-cljs exit!]])

(deftask deps [] identity)

(deftask dev
  "Launch immediate feedback dev environment"
  []
  (comp
   (serve :dir "target")
   (watch)
   (reload)
   (cljs-repl)
   (cljs)
   (target :dir #{"target"})))

(deftask test-and-watch []
  (comp
   (watch)
   (test-cljs)))

(deftask test-common-and-watch []
  (comp 
   (watch)
   (test-cljs :namespaces [#".\.common.*"])))

(deftask test-distributions-and-watch []
  (comp
   (watch)
   (test-cljs :namespaces [#".\.distributions.*"])))

(set-env! :source-paths #(conj % "test/cljs"))
