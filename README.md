# Evident

## Toy Example

```clojure
(ns evident-example.core
  (:require [evident.core :refer [! deffn with-effects]]))

(deffn log [& msgs] [:log]
  (apply (! :log) msgs))

(defn make-counter [] (atom 0))

(deffn get-count [] [:counter]
  (deref (! :counter)))

(deffn inc-count [] [:counter]
  (swap! (! :counter) inc))

(deffn up-to-eleven [] [:counter :log]
  (dotimes [_ 11]
    (! log "incrementing counter from" (! get-count))
    (! inc-count)))

(defn -main [& _]
  (with-effects {:counter (make-counter), :log println}
    (! up-to-eleven)))
```
