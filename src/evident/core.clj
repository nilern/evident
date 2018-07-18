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

(defmulti ctx-convert (fn [_ {:keys [op]}] op))

(defmethod ctx-convert :fn-method [{:keys [effn? ctx-proj] :as ctx} ast]
  (let [tmp-ctx-name (gensym 'tmp-effs)
        tmp-ctx-atom (atom {:status :ready, :val {:tag Object}})
        ctx (if effn?
              {:effn?    false
               :ctx-name (gensym 'effs)
               :ctx-atom (atom {:status :ready, :val {:tag Object}})}
              ctx)
        ast (if effn?
              (let [ctx-binding {:op        :binding
                                 :form      tmp-ctx-name
                                 :name      tmp-ctx-name
                                 :local     :arg
                                 :arg-id    0
                                 :variadic? false
                                 :atom      tmp-ctx-atom
                                 :children  []}
                    ;; (select-keys tmp-effs# ~ctx-proj)
                    ctx-projection {:op       :invoke
                                    :fn       {:op          :var
                                               :form        `select-keys
                                               :var         #'select-keys
                                               :assignable? (not (-> #'select-keys meta :static))}
                                    :args     [{:op          :local
                                                :form        tmp-ctx-name
                                                :assignable? false
                                                :name        tmp-ctx-name
                                                :local       :arg
                                                :arg-id      0
                                                :variadic?   false
                                                :atom        tmp-ctx-atom}
                                               (ana/analyze ctx-proj)] ;; HACK
                                    :children [:fn :args]}]
                (-> ast
                    ;; ([x] ...) => ([tmp-effs# x] ...)
                    (update :params (partial into [ctx-binding] (map #(update % :arg-id inc))))
                    (update :fixed-arity inc)

                    ;; (do ...) => (let [effs# ~ctx-projection] ...)
                    (update :body (fn [body]
                                    {:op         :do
                                     :statements []
                                     :ret        {:op       :let
                                                  :bindings [{:op       :binding
                                                              :form     (:ctx-name ctx)
                                                              :name     (:ctx-name ctx)
                                                              :local    :let
                                                              ;; TODO: select-keys ctx-proj
                                                              :init     ctx-projection
                                                              :atom     (:ctx-atom ctx)
                                                              :children [:init]}]
                                                  :body     body
                                                  :children [:bindings :body]}
                                     :body?      true
                                     :children   [:statements :ret]}))))
              ast)]
    (map-children (partial ctx-convert ctx) ast)))

(defmethod ctx-convert :invoke [ctx ast]
  (let [ast (if (and (= (-> ast :fn :op) :var) (= (-> ast :fn :var) #'!))
              ;; (! :foo bar "baz") => (bar (:foo effs#) "baz")
              (let [{callee :fn [selector action & args] :args} ast]
                (assert selector)
                (assert action)
                {:op       :invoke
                 :fn       action
                 :args     (into [{:op       :invoke
                                   :fn       selector
                                   :args     [{:op          :local
                                               :form        (:ctx-name ctx)
                                               :assignable? false
                                               :name        (:ctx-name ctx)
                                               :local       :let
                                               :atom        (:ctx-atom ctx)}]
                                   :children [:fn :args]}]
                                 args)
                 :children [:fn :args]})
              ast)]
    (map-children (partial ctx-convert ctx) ast)))

(defmethod ctx-convert :do [ctx ast] (map-children (partial ctx-convert ctx) ast))
(defmethod ctx-convert :fn [ctx ast] (map-children (partial ctx-convert ctx) ast))
(defmethod ctx-convert :let [ctx ast] (map-children (partial ctx-convert ctx) ast))

(defmethod ctx-convert :binding [_ ast] ast)
(defmethod ctx-convert :const [_ ast] ast)
(defmethod ctx-convert :local [_ ast] ast)
(defmethod ctx-convert :var [_ ast] ast)

(defmacro effn [args ctx-proj & body]
  (->> `(fn ~args ~@body) ana/analyze (ctx-convert {:effn? true, :ctx-proj ctx-proj}) emit-form))

