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

(defn- convert-ast [ctx node]
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
    (map-children (partial convert-ast ctx) node)))

(defn- ctx-convert [env ctx-name form]
  (let [locals (into {} (map (fn [[name _]] [name {:op :local, :form name, :name name}])) env)
        analyzer-env (assoc (ana/empty-env) :locals locals)
        ast (ana/analyze form analyzer-env)]
    (->> ast (convert-ast {:ctx-name ctx-name, :ctx-atom (atom nil)}) emit-form)))

(defmacro effn [args ctx-proj & body]
  (let [ctx-name (gensym 'effs)]
    (ctx-convert &env ctx-name `(fn [tmp-ctx-name# ~@args]
                                  (let [~ctx-name (select-keys tmp-ctx-name# ~ctx-proj)]
                                    ~@body)))))

(defmacro deffn [name args ctx-proj & body]
  (let [ctx-name (gensym 'effs)]
    (ctx-convert &env ctx-name `(defn ~name [tmp-ctx-name# ~@args]
                                  (let [~ctx-name (select-keys tmp-ctx-name# ~ctx-proj)]
                                    ~@body)))))

(defn call-with-effects [f effs & args]
  (apply f effs args))

;; OPTIMIZE: Get rid of the beta-redex.
(defmacro with-effects [effs & body]
  `(let [effs# ~effs]
     (call-with-effects (effn [] (keys effs#) ~@body) effs#)))