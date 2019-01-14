(require '[cljs.build.api :as b])

(b/watch "src"
  {:main 'two.core
   :output-to "out/two.js"
   :output-dir "out"})
