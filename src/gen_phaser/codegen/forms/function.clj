(ns gen-phzr.codegen.forms.function
  (:require [cljfmt.core :as cfmt]
            [cuerdas.core :as str]
            [gen-phzr.util :as u]))


(def ^:private class-param-template
  " * %s (%s) - Targeted instance for method")

(defn ^:private req-param?
  [p]
  (contains? #{false "" nil} (:optional p)))

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
  (-> s
      (str/replace #"\"" "'")
      (str/replace #"\\" (clojure.string/re-quote-replacement "\\\\"))))

(defn ^:private indent-desc
  [s]
  (-> s
      (str/replace #"\n" "\n  ")
      (str/replace #"\n  \n" "\n\n")))

(defn ^:private clean-param-desc
  [param-desc]
  (str/replace param-desc crosslink-regex replace-crosslink))

(defn ^:private build-param-name
  [p]
  (let [p-name (or (:name p) "args")]
    (case p-name
      "args"      "args"
      "arguments" "args"
      (u/name->kebab p-name))))

(defn ^:private build-param-strings
  [params]
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
                (quote-str)))))

(defn ^:private param-docstring
  ([params]
   (when-not (empty? params)
     (str "  Parameters:\n   "
          (str/join  "\n   " (build-param-strings params)))))
  ([class-name params]
   (when-not (empty? params)
     (let [instance-arg (u/instance-arg-name class-name)]
       (str "  Parameters:\n   "
            (str/join "\n   " (concat [(format class-param-template
                                               instance-arg
                                               class-name)]
                                      (build-param-strings params))))))))

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
        static?  (:static f)
        params   (:parameters f)
        returns  (:returns f)
        doc-strs [desc
                  (if static?
                    (param-docstring params)
                    (param-docstring class-name params))
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

(def ^:private fn-vararg-body-template
  (str "([%s]\n (phaser->clj\n (.apply (aget %s %s) %s\n"
       "(into-array (concat [%s] args)))))"))

(def ^:private fn-vararg-body-template-static
  (str "([%s]\n (phaser->clj\n (.apply (aget %s %s)\n"
       "(into-array (concat [%s] args)))))"))

(def ^:private fn-overload-body-template
  "([%s]\n (phaser->clj\n (.%s %s\n %s)))")

(def ^:private fn-overload-body-template-static
  "([%s]\n (phaser->clj\n (.%s %s\n %s)))")

(def ^:private fn-arg-template
  "(clj->phaser %s)")

(defn ^:private build-vararg-fn-body
  [class-name fn-name params static?]
  (let [instance-arg (u/instance-arg-name class-name)
        params       (remove #(not (contains? % :name)) params)
        param-strs   (map build-param-name params)
        arg-strs     (map #(format fn-arg-template %) param-strs)]
    (if-not static?
      (format fn-vararg-body-template
              (str/join " " (concat [instance-arg] param-strs ["& args"]))
              instance-arg
              fn-name
              instance-arg
              (str/join "\n" arg-strs))
      (format fn-vararg-body-template-static
              (str/join " " (concat param-strs ["& args"]))
              (str "js/" class-name)
              fn-name
              (str/join "\n" arg-strs)))))

(defn ^:private build-overload-fn-body
  [class-name fn-name params static?]
  (let [instance-arg (u/instance-arg-name class-name)
        param-strs   (map build-param-name params)
        arg-strs     (map #(format fn-arg-template %) param-strs)]
    (if-not static?
      (format fn-overload-body-template
              (str/join " " (concat [instance-arg] param-strs))
              fn-name
              instance-arg
              (str/join "\n" arg-strs))
      (format fn-overload-body-template-static
              (str/join " " param-strs)
              fn-name
              (str "js/" class-name)
              (str/join "\n" arg-strs)))))

(defn ^:private build-fn-bodies
  [class-name fn-name params static?]
  (if (params-missing-name? params)
    (vector (build-vararg-fn-body class-name fn-name params static?))
    (let [req-params  (filter req-param? params)
          opt-params  (remove req-param? params)
          param-perms (parameter-permutations req-params opt-params)]
      (for [p param-perms]
        (build-overload-fn-body class-name fn-name p static?)))))

(defn gen-function
  [class-name f]
  (let [fn-name       (:name f)
        static?       (:static f)
        fn-name-kebab (u/name->kebab fn-name)
        docstring     (build-docstring class-name f)
        fn-params     (:parameters f)
        bodies        (build-fn-bodies class-name fn-name fn-params static?)]
    (cfmt/reformat-string
     (format fn-template
             (if-not static?
               fn-name-kebab
               (str fn-name-kebab "-"))
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
  (when-not (u/raw-phaser-objs class-name)
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
               (str/join "\n" bodies))))))
