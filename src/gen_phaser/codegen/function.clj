(ns gen-phaser.codegen.function
  (:require [camel-snake-kebab.core :as csk]
            [cljfmt.core :as cfmt]
            [cuerdas.core :as str]))


(defn ^:private instance-arg-name
  [class-name]
  (csk/->kebab-case-string (last (str/split class-name #"\."))))

(def ^:private class-param-template
  " * %s (%s) - Targeted instance for method")

(defn ^:private req-param?
  [p]
  (#{false ""} (:optional p)))

(def ^:private param-template
  " * %s (%s)%s - %s")

(defn ^:private clean-param-type
  [param-type]
  (cond
    (coll? param-type)
    (str/join " | " (map str/trim param-type))

    :else
    param-type))

(def ^:private crosslink-regex
  #"\{\{\#crossLink \"(.*?)\"\}\}(.*?)\{\{\/crossLink\}\}")

(defn ^:private replace-crosslink
  [matches]
  (last (remove empty? matches)))

(defn ^:private quote-str
  [s]
  (str/replace s #"\"" (clojure.string/re-quote-replacement "\\\"")))

(defn ^:private indent-desc
  [s]
  (str/replace s #"\n" "\n  "))

(defn ^:private clean-param-desc
  [param-desc]
  (str/replace param-desc crosslink-regex replace-crosslink))

(defn ^:private build-param-name
  [p]
  (let [p-name (or (:name p) "args")]
    (case p-name
      "args"      "args"
      "arguments" "args"
      (csk/->kebab-case-string p-name))))

(defn ^:private param-docstring
  ([params]
   (when-not (empty? params)
     (str "  Parameters:\n   "
          (str/join
           "\n   "
           (for [p params
                 :let [opt (if-not (req-param? p) " {optional}" "")]]
             (format param-template
                     (build-param-name p)
                     (clean-param-type (:type p))
                     opt
                     (-> (if-not (empty? (:description p))
                           (:description p)
                           "No description")
                         (clean-param-desc)
                         (quote-str))))))))
  ([class-name params]
   (when-not (empty? params)
     (let [instance-arg (instance-arg-name class-name)]
       (str "  Parameters:\n   "
            (str/join
             "\n   "
             (concat [(format class-param-template instance-arg class-name)]
                     (for [p params
                           :let [opt (if-not (req-param? p) " {optional} " "")]]
                       (format param-template
                               (build-param-name p)
                               (clean-param-type (:type p))
                               opt
                               (-> (if-not (empty? (:description p))
                                     (:description p)
                                     "No description")
                                   (clean-param-desc)
                                   (quote-str)))))))))))

(defn ^:private return-docstring
  [returns]
  (when returns
    (let [type-str (as-> (:type returns) x
                     (str/replace x #"\"" "")
                     (str/trim x)
                     (str/split x #"\s+")
                     (str/join " | " x))
          desc-str (quote-str (:description returns))]
      (str "  Returns:  " type-str " - " desc-str))))

(defn ^:private build-docstring
  [class-name f]
  (let [desc     (indent-desc (quote-str (:description f)))
        params   (:parameters f)
        returns  (:returns f)
        doc-strs [desc
                  (param-docstring class-name params)
                  (return-docstring returns)]]
    (str/join "\n\n" (filter identity doc-strs))))

(defn ^:private params-missing-name?
  [params]
  (some #(not (contains? % :name)) params))

(defn ^:private parameter-permutations
  [req-params opt-params]
  (map #(concat req-params %)
       (map #(take % opt-params)
            (range 0 (inc (count opt-params))))))

(def ^:private fn-template
  "(defn %s\n  \"%s\"\n %s)")

(def ^:private fn-overload-body-template
  "([%s]\n (phaser->clj\n (.%s %s\n %s)))")

(def ^:private fn-vararg-body-template
  (str "([%s]\n (phaser->clj\n (.apply (.-%s %s) %s\n"
       "(into-array (concat [%s] args)))))"))

(def ^:private fn-arg-template
  "(clj->phaser %s)")

(defn ^:private build-vararg-fn-body
  [class-name fn-name params]
  (let [instance-arg (instance-arg-name class-name)
        params       (remove #(not (contains? % :name)) params)
        param-strs   (map build-param-name params)
        arg-strs     (map #(format fn-arg-template %) param-strs)]
    (format fn-vararg-body-template
            (str/join " " (concat [instance-arg] param-strs ["& args"]))
            fn-name
            instance-arg
            instance-arg
            (str/join "\n" arg-strs))))

(defn ^:private build-overload-fn-body
  [class-name fn-name params]
  (let [instance-arg (instance-arg-name class-name)
        param-strs   (map build-param-name params)
        arg-strs     (map #(format fn-arg-template %) param-strs)]
    (format fn-overload-body-template
            (str/join " " (concat [instance-arg] param-strs))
            fn-name
            instance-arg
            (str/join "\n" arg-strs))))

(defn ^:private build-fn-bodies
  [class-name fn-name params]
  (if (params-missing-name? params)
    (vector (build-vararg-fn-body class-name fn-name params))
    (let [req-params  (filter req-param? params)
          opt-params  (remove req-param? params)
          param-perms (parameter-permutations req-params opt-params)]
      (for [p param-perms]
        (build-overload-fn-body class-name fn-name p)))))

(defn gen-function
  [class-name f]
  (let [fn-name       (:name f)
        fn-name-kebab (csk/->kebab-case-string fn-name)
        docstring     (build-docstring class-name f)
        fn-params     (:parameters f)
        bodies        (build-fn-bodies class-name fn-name fn-params)]
    (cfmt/reformat-string
     (format fn-template
             fn-name-kebab
             docstring
             (str/join "\n" bodies)))))

(defn ^:private build-constructor-docstring
  [c]
  (let [desc     (indent-desc (quote-str (:description c)))
        params   (:parameters c)
        doc-strs [desc
                  (param-docstring params)]]
    (str/join "\n\n" (filter identity doc-strs))))

(def ^:private constructor-body-template
  "([%s]\n (js/%s. %s))")

(defn ^:private build-constructor-body
  [class-name params]
  (let [param-strs (map build-param-name params)
        arg-strs   (map #(format fn-arg-template %) param-strs)]
    (format constructor-body-template
            (str/join " " param-strs)
            class-name
            (str/join "\n" arg-strs))))

(defn gen-constructor
  [class-name c]
  (let [fn-name     (str "->" (:name c))
        docstring   (build-constructor-docstring c)
        fn-params   (:parameters c)
        req-params  (filter req-param? fn-params)
        opt-params  (remove req-param? fn-params)
        param-perms (parameter-permutations req-params opt-params)
        bodies      (map #(build-constructor-body class-name %) param-perms)]
    (cfmt/reformat-string
     (format fn-template
             fn-name
             docstring
             (str/join "\n" bodies)))))
