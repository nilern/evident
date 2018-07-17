(ns evident.core
  (:require [clojure.tools.analyzer.jvm :as ana]
            [clojure.tools.analyzer.passes.jvm.emit-form :refer [emit-form]]))

(defn ! [& args] (assert false "evident.core/! used outside evident.core/effn"))

(defn- map-children [f ast]
  (reduce (fn [ast child-name]
            (let [child (child-name ast)]
              (if (vector? child)
                (assoc ast child-name (mapv f child))
                (assoc ast child-name (f child)))))
          ast (:children ast)))

(defmulti ctx-convert (fn [_ {:keys [op]}] op))

;; TODO: Use select-keys with ctx-proj
(defmethod ctx-convert :fn-method [{:keys [effn? ctx-proj] :as ctx} ast]
  (let [ctx (if effn?
              {:effn?    false
               :ctx-proj ctx-proj
               :ctx-name (gensym 'effs)
               :ctx-atom (atom {:status :ready, :val {:tag Object}})}
              ctx)
        ast (if effn?
              (let [ctx-binding {:op        :binding
                                 :form      (:ctx-name ctx)
                                 :name      (:ctx-name ctx)
                                 :local     :arg
                                 :arg-id    0
                                 :variadic? false
                                 :atom      (:ctx-atom ctx)
                                 :children  []}]
                (-> ast
                    (update :params (partial into [ctx-binding] (map #(update % :arg-id inc))))
                    (update :fixed-arity inc)))
              ast)]
    (map-children (partial ctx-convert ctx) ast)))

(defmethod ctx-convert :invoke [ctx ast]
  (let [ast (if (and (= (-> ast :fn :op) :var)
                     (= (-> ast :fn :var) #'!))
              (-> ast
                  (assoc :fn (first (:args ast)))
                  (update :args (fn [args]
                                  (->> args
                                       rest
                                       (cons {:op          :local
                                              :form        (:ctx-name ctx)
                                              :assignable? false
                                              :name        (:ctx-name ctx)
                                              :local       :arg
                                              :arg-id      0
                                              :variadic?   false
                                              :atom        (:ctx-atom ctx)})
                                       vec))))
              ast)]
    (map-children (partial ctx-convert ctx) ast)))

(defmethod ctx-convert :fn [ctx ast] (map-children (partial ctx-convert ctx) ast))

(defmethod ctx-convert :binding [_ ast] ast)
(defmethod ctx-convert :local [_ ast] ast)
(defmethod ctx-convert :const [_ ast] ast)

(defmacro effn [args ctx-proj & body]
  (->> `(fn ~args ~@body) ana/analyze (ctx-convert {:effn? true, :ctx-proj ctx-proj}) emit-form))

