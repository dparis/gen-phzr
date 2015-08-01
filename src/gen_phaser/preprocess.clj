(ns gen-phaser.preprocess
  (:require [cuerdas.core :as str]))

(defn ^:private remove-constructor-tags
  [js]
  (str/replace js #"\s*\*\s*\@constructor\s*" "\n"))

(defn ^:private fix-empty-todo-tags
  [js]
  (str/replace js #"\s*\*\s*\@todo\s*\n" "\n * @todo Placeholder Todo\n"))

(defn ^:private class-tag-replacement
  [tag-matches]
  (let [[_ body fn-line] tag-matches
        trim-line        (str/trim fn-line)
        a-match          (second (re-find #"^function\s+(.*)\s*\(" trim-line))
        b-match          (second (re-find #"^(.*?) = function" trim-line))]
    (cond
      a-match (str "@class " a-match "\n" body)
      b-match (str "@class " b-match "\n" body)
      :else   "BAD CLASS REPLACEMENT")))

(defn ^:private fix-empty-class-tags
  [js]
  (str/replace js
               #"(?s)@class\s*\n+(.*?\*\/\n+(.*?)\{\s)"
               class-tag-replacement))

(def draw-modes-regex
  (re-pattern
   (str "(?s)Different drawing buffer modes supported\\n"
        "\\s*\\*\\s*\\n"
        "\\s*\\*\\s*@property\\s*\\n"
        ".*?"
        "PIXI\\.Strip\\.DrawModes")))

(def draw-modes-fixed-block
  "Different drawing buffer modes supported\n
   *
   * @property DrawModes
   * @type {{TRIANGLE_STRIP: number, TRIANGLES: number}}
   * @static
   */
  PIXI.Strip.DrawModes")

(defn ^:private fix-empty-property-tags
  [js]
  (str/replace js
               draw-modes-regex
               draw-modes-fixed-block))

(defn ^:private add-missing-memberof
  [js]
  (-> js
      (str/replace #"(?s)\s*\*\/\s*\n+Pool\.prototype.(resize|get|release) = "
                   (str "\n"
                        " * @memberof Pool\n"
                        " */\n"
                        "Pool.prototype.$1 = "))
      (str/replace #"\* @param canvas \{HTMLCanvasElement\} the current canvas"
                   (str "* @param canvas {HTMLCanvasElement} the current canvas\n"
                        " * @memberof PIXI.CanvasTinter"))))

(defn preprocess-js
  [js]
  (-> js
      (remove-constructor-tags)
      (fix-empty-todo-tags)
      (fix-empty-class-tags)
      (fix-empty-property-tags)
      (add-missing-memberof)))

(defn ^:private remove-anonymous-prefixes
  [json]
  (str/replace json #"\<anonymous\>\~" ""))

(defn ^:private replace-tildes
  [json]
  (-> json
      (str/replace #"I\~" "I.")
      (str/replace #"Box\~" "Box.")
      (str/replace #"initialize\~" "initialize.")))

(defn ^:private update-anonymous-memberof
  [json]
  (str/replace json
               #"\"memberof\"\: \"<anonymous>\""
               "\"memberof\": \"Pool\""))

(defn preprocess-json
  [json]
  (-> json
      (remove-anonymous-prefixes)
      (replace-tildes)
      (update-anonymous-memberof)))
