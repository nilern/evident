(ns evident.core
  (:require [clojure.tools.analyzer.jvm :as ana]
            [clojure.tools.analyzer.passes.jvm.emit-form :refer [emit-form]]))

(defn ! [& _] (assert false "evident.core/! used outside evident.core/effn"))

(defn- map-children [f ast]
  (if-let [children (:children ast)]
    (reduce (fn [ast child-name]
              (let [child (child-name ast)]
                (if (vector? child)
                  (assoc ast child-name (mapv f child))
                  (assoc ast child-name (f child)))))
            ast children)
    ast))

(defn- ctx-convert [ctx node]
  (let [node (if (and (= (-> node :fn :op) :var) (= (-> node :fn :var) #'!))
               ;; (! foo bar) => (foo effs# bar)
               (let [{:keys [ctx-name ctx-atom]} ctx
                     {[f & args] :args} node]
                 (assert f)
                 {:op       :invoke
                  :fn       f
                  :args     (into [{:op          :local
                                    :form        ctx-name
                                    :assignable? false
                                    :name        ctx-name
                                    :local       :let
                                    :atom        ctx-atom}]
                                  args)
                  :children [:fn :args]})
               node)]
    (map-children (partial ctx-convert ctx) node)))

(defmacro effn [args ctx-proj & body]
  (let [ctx-name (gensym 'effs)]
    (->> `(fn [tmp-ctx-name# ~@args]
            (let [~ctx-name (select-keys tmp-ctx-name# ~ctx-proj)]
              ~@body))
         ana/analyze
         (ctx-convert {:ctx-name ctx-name, :ctx-atom (atom nil)})
         emit-form)))
