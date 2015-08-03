(ns gen-phaser.externs
  (:require [cheshire.core :as c]
            [clojure.pprint :refer [pprint]]
            [clojure.set :as s]
            [cuerdas.core :as str]))

(def ^:private blacklist-constructors
  #{"AABB"
    "AngleLockEquation"
    "Body"
    "Box"
    "Broadphase"
    "Capsule"
    "Circle"
    "Constraint"
    "ContactEquation"
    "ContactEquationPool"
    "ContactMaterial"
    "Convex"
    "DistanceConstraint"
    "Equation"
    "EventEmitter"
    "FrictionEquation"
    "FrictionEquationPool"
    "GSSolver"
    "GearConstraint"
    "Heightfield"
    "Island"
    "IslandManager"
    "IslandNode"
    "IslandNodePool"
    "IslandPool"
    "Line"
    "LinearSpring"
    "LockConstraint"
    "Material"
    "NaiveBroadphase"
    "Narrowphase"
    "OverlapKeeper"
    "OverlapKeeperRecord"
    "OverlapKeeperRecordPool"
    "Particle"
    "Phaser.FlexGrid"
    "Phaser.FlexLayer"
    "Plane"
    "Point"
    "Polygon"
    "Pool"
    "PrismaticConstraint"
    "Ray"
    "RaycastResult"
    "RevoluteConstraint"
    "RotationalLockEquation"
    "RotationalSpring"
    "RotationalVelocityEquation"
    "SAPBroadphase"
    "Scalar"
    "Shape"
    "Solver"
    "Spring"
    "TupleDictionary"
    "TopDownVehicle"
    "Utils"
    "WheelConstraint"
    "World"
    "vec2"})

(defn ^:private kind?
  [node kind]
  (= kind (:kind node)))

(defn ^:private remove-blacklist-nodes
  [data]
  (remove #(and (blacklist-constructors (get % :longname))
                (kind? % "constructor"))
          data))

(defn ^:private public?
  [node]
  (not (#{"protected" "private"} (get node :access))))

(defn ^:private public-access
  [data]
  (filter public? data))

(defn ^:private exposed-constructors
  [data]
  (let [class-set (into #{} (map #(get % "memberof")
                                 (public-access data)))]
    (flatten
     (for [c class-set]
       (filter #(and (= c (get % "longname"))
                     (kind? % "constructor")
                     (public? %)
                     (not (#{"Phaser.Device"} (get % "longname"))))
               data)))))

(defn ^:private instance-property?
  [node]
  (and (kind? node "member")
       (= "instance" (get node "scope"))
       (public? node)))

(defn ^:private inst-props-for-constructor
  [data constructor]
  (let [c-name (get constructor "longname")]
    (->> (filterv #(= c-name (get % "memberof")) data)
         (filterv instance-property?))))

(defn ^:private instance-method?
  [node]
  (and (kind? node "function")
       (= "instance" (get node "scope"))
       (public? node)))

(defn ^:private inst-methods-for-constructor
  [data constructor]
  (let [c-name (get constructor "longname")]
    (->> (filterv #(= c-name (get % "memberof")) data)
         (filterv instance-method?))))

(defn ^:private build-exts
  [data]
  (let [constructors (exposed-constructors data)]
    (into {} (for [c constructors
                   :let [ip (inst-props-for-constructor data c)
                         im (inst-methods-for-constructor data c)]]
               [c {:instance-properties ip
                   :instance-methods    im}]))))

(defn ^:private format-c-lname
  [constructor]
  (let [lname (get constructor "longname")]
    (cond
      (str/contains? lname "module:")
      (let [matches (re-matches #"^module\:(.*)\s*" lname)]
        (nth matches 1))

      :else
      lname)))

(defn ^:private format-constructor
  [constructor]
  (let [c-lname   (format-c-lname constructor)
        params    (get constructor "params")
        param-str (str/join ", "
                            (remove #(str/contains? % ".")
                                    (map #(get % "name") params)))]
    (format "%s = function(%s) {};"
            c-lname
            param-str)))

(defn ^:private format-inst-property
  [constructor inst-property]
  (let [c-lname (format-c-lname constructor)
        ip-name (get inst-property "name")]
    (format "%s.prototype.%s;" c-lname ip-name)))

(defn ^:private format-inst-method
  [constructor inst-method]
  (let [c-lname   (get constructor "longname")
        im-name   (get inst-method "name")
        params    (get inst-method "params")
        param-str (str/join ", "
                            (remove #(str/contains? % ".")
                                    (map #(get % "name") params)))]
    (format "%s.prototype.%s = function(%s) {};"
            c-lname
            im-name
            param-str)))

(defn ^:private format-ext
  [[c ext]]
  (let [c-lname (format-c-lname c)
        c-str   (format-constructor c)
        ip-strs (map #(format-inst-property c %) (:instance-properties ext))
        im-strs (map #(format-inst-method c %) (:instance-methods ext))]
    (str
     "/********************************************/\n"
     "/* " c-lname " */\n"
     "\n"
     c-str "\n"
     "\n"
     (when-not (empty? ip-strs)
       (str (str/join "\n" (sort ip-strs)) "\n\n"))
     (when-not (empty? im-strs)
       (str (str/join "\n" (sort im-strs)) "\n\n"))

     "/********************************************/")))

(defn ^:private format-exts
  [exts]
  (str
   "var Phaser = {};\n"
   "var PIXI = {};\n"
   "\n"
   (->> (sort (map format-ext exts))
        (str/join "\n\n\n"))))

(defn gen-exts
  [data]
  (let [exts (build-exts (remove-blacklist-nodes data))]
    (format-exts exts)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ^:private classes
  [data]
  (filter #(kind? % "class") data))

(defn ^:private remove-blacklist-longnames
  [data]
  (remove #(blacklist-constructors (get % :longname)) data))

(defn ^:private fix-class-longname
  [longname]
  (str/replace longname #"module\:" ""))

(defn ^:private public-member-of?
  [node c]
  (and (= (:memberof node) (:longname c))
       (public? node)))

(defn ^:private class-def
  [data c]
  (let [members (filter #(public-member-of? % c) data)
        cd      {:class c}]
    (reduce
     (fn [col m]
       (case (:kind m)
         "constructor" (assoc col :constructors (conj (:constructors col) m))
         "function"    (assoc col :functions (conj (:functions col) m))
         "member"      (assoc col :members (conj (:members col) m))
         (assoc col :misc (conj (:misc col) m ))))
     cd
     members)))

(defn ^:private build-class-data
  [data]
  (let [cs (remove-blacklist-longnames (classes data))]
    (into {} (for [c cs]
               [(fix-class-longname (:longname c))
                (class-def data c)]))))

(defn ^:private find-class-def
  [class-data longname]
  (or (get class-data longname)
      (get class-data (str "PIXI." longname))
      (get class-data (str "Phaser." longname))))

(defn ^:private augment-class-def
  [class-data c-def]
  (let [c    (:class c-def)
        alns (:augments c)
        acs  (map #(find-class-def class-data %) alns)]
    (reduce
     (fn [base ac-def]
       (let [updated-augment (augment-class-def class-data ac-def)]
         (-> base
             (update-in [:functions] #(concat % (:functions updated-augment)))
             (update-in [:members]   #(concat % (:members updated-augment))))))
     c-def
     acs)))

(defn ^:private gen-ext2
  [data]
  (let []))
