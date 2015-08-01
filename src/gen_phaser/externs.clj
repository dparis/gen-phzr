(ns gen-phaser.externs
  (:require [cheshire.core :as json]
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
    "ContactMaterial"
    "Convex"
    "DistanceConstraint"
    "Equation"
    "EventEmitter"
    "FrictionEquation"
    "GSSolver"
    "GearConstraint"
    "Heightfield"
    "Island"
    "IslandManager"
    "IslandNode"
    "Line"
    "LinearSpring"
    "LockConstraint"
    "Material"
    "NaiveBroadphase"
    "Narrowphase"
    "OverlapKeeper"
    "OverlapKeeperRecord"
    "Particle"
    "Phaser.FlexGrid"
    "Phaser.FlexLayer"
    "Plane"
    "Point"
    "Polygon"
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
    "TopDownVehicle"
    "Utils"
    "WheelConstraint"
    "World"
    "vec2"})

(defn ^:private remove-blacklist-nodes
  [data]
  (remove #(and (blacklist-constructors (get % "longname"))
                (= "constructor" (get % "kind")))
          data))

(defn ^:private public?
  [node]
  (not (#{"protected" "private"} (get node "access"))))

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
                     (= "constructor" (get % "kind"))
                     (public? %)
                     (not (#{"Phaser.Device"} (get % "longname"))))
               data)))))

(defn ^:private instance-property?
  [node]
  (and (= "member" (get node "kind"))
       (= "instance" (get node "scope"))
       (public? node)))

(defn ^:private inst-props-for-constructor
  [data constructor]
  (let [c-name (get constructor "longname")]
    (->> (filterv #(= c-name (get % "memberof")) data)
         (filterv instance-property?))))

(defn ^:private instance-method?
  [node]
  (and (= "function" (get node "kind"))
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
