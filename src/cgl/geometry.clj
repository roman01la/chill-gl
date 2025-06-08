(ns cgl.geometry
  (:import [org.joml Vector3f Vector2f Matrix4f]))

;; Path generation

(defn create-line-path
  "Creates a straight line path between two points with a given number of segments.
   Returns a sequence of Vector3f points along the path."
  [start end num-segments]
  (let [start-vec (Vector3f. (.x start) (.y start) (.z start))
        end-vec (Vector3f. (.x end) (.y end) (.z end))
        direction (-> (Vector3f.)
                      (.set end-vec)
                      (.sub start-vec))
        segment-length (/ (.length direction) num-segments)
        unit-direction (-> direction (.normalize))]

    (for [i (range (inc num-segments))]
      (-> (Vector3f.)
          (.set unit-direction)
          (.mul (float (* i segment-length)))
          (.add start-vec)))))

(defn create-bezier-path
  "Creates a cubic Bezier curve path with the given control points and number of segments.
   Returns a sequence of Vector3f points along the path."
  [p0 p1 p2 p3 num-segments]
  (for [i (range (inc num-segments))]
    (let [t (/ i num-segments)
          t2 (* t t)
          t3 (* t2 t)
          mt (- 1 t)
          mt2 (* mt mt)
          mt3 (* mt2 mt)
          b0 (* mt3)                  ; (1-t)^3
          b1 (* 3 mt2 t)              ; 3(1-t)^2 * t
          b2 (* 3 mt t2)              ; 3(1-t) * t^2
          b3 (* t3)]                  ; t^3
      (-> (Vector3f.)
          (.set (* b0 (.x p0)) (* b0 (.y p0)) (* b0 (.z p0)))
          (.add (* b1 (.x p1)) (* b1 (.y p1)) (* b1 (.z p1)))
          (.add (* b2 (.x p2)) (* b2 (.y p2)) (* b2 (.z p2)))
          (.add (* b3 (.x p3)) (* b3 (.y p3)) (* b3 (.z p3)))))))

(defn create-infinity-path
  "Creates a path in the shape of an infinity symbol (figure eight)"
  [num-segments radius-x radius-y]
  (let [t-step (/ (* 2 Math/PI) num-segments)]
    (vec
      (for [i (range num-segments)]
        (let [t (* i t-step)
              ; Parametric equation for a lemniscate (figure eight)
              ; x = radius-x * sin(t) / (1 + cos(t)^2)
              ; y = radius-y * sin(t) * cos(t) / (1 + cos(t)^2)
              ; z = 0
              denominator (+ 1.0 (Math/pow (Math/cos t) 2))
              x (* radius-x (Math/sin t) (/ 1.0 denominator))
              y (* radius-y (Math/sin t) (Math/cos t) (/ 1.0 denominator))
              z 0.0]
          (Vector3f. x y z))))))

;; Circle generation

(defn create-circle
  "Creates a circle in 3D space with the given parameters:
   - center: Vector3f center point of the circle
   - radius: radius of the circle
   - normal: Vector3f normal vector to the circle plane
   - num-segments: number of segments to divide the circle into

   Returns a sequence of Vector3f points along the circle."
  [center radius normal num-segments]
  (let [normal-vec (-> (Vector3f.)
                       (.set normal)
                       (.normalize))

        ; Create a coordinate system with normal as z-axis
        ; First, find an arbitrary vector perpendicular to normal
        perpendicular (if (> (Math/abs (.x normal-vec)) 0.1)
                        (Vector3f. 0 1 0)  ; Use up vector if normal is not close to up
                        (Vector3f. 1 0 0)) ; Use right vector if normal is close to up

        ; Create two perpendicular vectors in the circle plane
        right-vec (-> (Vector3f.)
                      (.set perpendicular)
                      (.cross normal-vec)
                      (.normalize)
                      (.mul (float radius)))

        up-vec (-> (Vector3f.)
                   (.set normal-vec)
                   (.cross right-vec)
                   (.normalize)
                   (.mul (float radius)))]

    ; Generate points around the circle
    (for [i (range num-segments)]
      (let [angle (* i (/ (* 2 Math/PI) num-segments))
            x-component (-> (Vector3f.)
                            (.set right-vec)
                            (.mul (float (Math/cos angle))))
            y-component (-> (Vector3f.)
                            (.set up-vec)
                            (.mul (float (Math/sin angle))))]
        (-> (Vector3f.)
            (.set center)
            (.add x-component)
            (.add y-component))))))

;; Tube generation

(defn build-frame [^Vector3f tangent ^Vector3f prev-n]
  ;; Remove the component of prev-n that is parallel to the new tangent
  (let [n' (-> (Vector3f. prev-n)
               (.sub (-> (Vector3f.)
                         (.set tangent)
                         (.mul (.dot tangent prev-n))))
               (.normalize))
        b' (-> (Vector3f.) (.set tangent) (.cross n') (.normalize))]
    [n' b']))          ;; n' = normal, b' = binormal


(defn create-tube
  [path radius segments]
  (let [v (Vector3f.)
        n (atom (Vector3f. 0 0 1))     ;; start-up vector = global Z
        ;; accumulate vertices, normals and indices
        {:keys [verts norms inds]}
        (reduce
          (fn [{:keys [verts norms inds] :as acc} [pNext pCurr idx]]
            ;; tangent
            (-> v (.set pNext) (.sub pCurr) (.normalize))
            ;; transport the old normal so it stays ⟂  to new tangent
            (let [[^Vector3f n' b'] (build-frame v @n)]
              (reset! n n')                       ;; keep it for the next step
              ;; generate one ring
              (let [ring (for [j (range segments)]
                           (let [f (* (/ (* 2 Math/PI) segments) j)]
                             (-> (Vector3f.)
                                 (.set n')
                                 (.mul (Math/cos f))
                                 (.add (-> (Vector3f.) (.set ^Vector3f b') (.mul (Math/sin f))))
                                 (.mul ^float radius)
                                 (.add pCurr))))]
                {:verts (into verts ring)
                 :norms (into norms
                              (map (fn [vtx] (-> (Vector3f. ^Vector3f vtx) (.sub pCurr) (.normalize))) ring))
                 :inds  (into inds
                              (for [j (range segments)]
                                (let [a (+ idx j)
                                      b (+ idx (mod (inc j) segments))
                                      c (+ idx segments j)
                                      d (+ idx segments (mod (inc j) segments))]
                                  [a c b  b c d])))})))
          {:verts [] :norms [] :inds []}
          (map vector (rest path) path (iterate #(+ % segments) 0)))]
    {:vertices verts :normals norms :indices (apply concat inds)}))

(defn tube-to-vertex-data
  "Convert tube data to a flat array suitable for OpenGL buffers.
   Each vertex has position (x,y,z) and normal (nx,ny,nz).

   Returns a map with:
   - :vertex-array - a float array of [x,y,z,nx,ny,nz, ...]
   - :index-array - an int array of triangle indices"
  [tube-data]
  (let [{:keys [vertices indices normals]} tube-data

        ; Create flattened vertex array with positions and normals
        vertex-count (count vertices)
        vertex-array (float-array (* vertex-count 6))  ; 3 for position, 3 for normal

        ; Fill vertex array
        _ (doseq [i (range vertex-count)]
            (let [^Vector3f vertex (nth vertices i)
                  ^Vector3f normal (nth normals i)
                  base-idx (* i 6)]
              (aset vertex-array (+ base-idx 0) (.x vertex))
              (aset vertex-array (+ base-idx 1) (.y vertex))
              (aset vertex-array (+ base-idx 2) (.z vertex))
              (aset vertex-array (+ base-idx 3) (.x normal))
              (aset vertex-array (+ base-idx 4) (.y normal))
              (aset vertex-array (+ base-idx 5) (.z normal))))

        ; Convert indices to primitive array
        index-array (int-array indices)]

    {:vertex-array vertex-array
     :index-array index-array}))

;; Helper functions for geometry transformation

(defn transform-vertices
  "Apply a transformation matrix to a sequence of vertices"
  [vertices transform-matrix]
  (mapv
   (fn [vertex]
     (-> (Vector3f. (.x vertex) (.y vertex) (.z vertex))
         (.mulPosition transform-matrix)))
   vertices))
